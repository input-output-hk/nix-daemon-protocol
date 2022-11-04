{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module HsshOld where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import Network.SSH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.SSH.Server as Server
import qualified System.Socket as S
import qualified System.Socket.Family.Inet6 as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream as S
import Data.Default (Default(def))
import Formatting ((%), fprint)
import Formatting.ShortFormatters (sh)
import Utils
import System.Exit (ExitCode(ExitSuccess))
import SocketDuplex ()
import System.IO (Handle, hClose, hFlush, stdout)
import System.Process (createPipe)
import Control.Concurrent.Async
import GHC.Stack (HasCallStack)

data Ident = JustUsername UserName

type SessionHandler = HasCallStack => Handle -> Handle -> Handle -> IO ExitCode

startSshServer :: SessionHandler -> IO ()
startSshServer handler = do
  file <- BS.readFile "ssh-host-key"
  -- ssh-keygen -t ed25519 -f ssh-host-key -N ''
  (privateKey, _):_ <- decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  print privateKey
  bracket open close (accept config privateKey)
  where
    onAuthRequest :: UserName -> ServiceName -> PublicKey -> IO (Maybe Ident)
    onAuthRequest username service key = do
      fprint
        ("auth request for user " % sh % " service " % sh %
         " with pubkey fingerprint " %
         sh %
         "\n")
        username
        service $
        pubkeyFingerprint key
      pure (Just $ JustUsername username)
    config =
      Server.Config
        { Server.transportConfig = Data.Default.def
        , Server.userAuthConfig =
            Data.Default.def {Server.onAuthRequest = onAuthRequest}
        , Server.connectionConfig =
            Data.Default.def
              { Server.onSessionRequest = handleSessionRequest handler
              , Server.channelMaxQueueSize = 64 * 1024
              }
        }
    open :: IO (S.Socket S.Inet6 S.Stream S.Default)
    open = S.socket :: IO (S.Socket S.Inet6 S.Stream S.Default)
    close :: S.Socket S.Inet6 S.Stream S.Default -> IO ()
    close = S.close
    accept ::
         Server.Config identity
      -> KeyPair
      -> S.Socket S.Inet6 S.Stream S.Default
      -> IO b
    accept cfg agent s = do
      print @T.Text "running"
      S.setSocketOption s (S.ReuseAddress True)
      S.setSocketOption s (S.V6Only False)
      S.bind s (S.SocketAddressInet6 S.inet6Any 2222 0 0)
      S.listen s 5
      forever $
        bracketOnError (S.accept s) (S.close . fst) $ \(stream, peer) -> do
          putStrLn $ "Connection from " ++ show peer
          void $
            forkFinally (Server.serve cfg agent stream >>= print) $ \emsg -> do
              print emsg
              S.close stream

handleSessionRequest :: SessionHandler -> Ident -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest handler _idnt _req = do
  pure $ Just $ Server.SessionHandler $ actualHandler handler

actualHandler :: (InputStream stdin, OutputStream stdout, OutputStream stderr) => SessionHandler -> Server.Environment -> Maybe Server.TermInfo -> Maybe Server.Command -> stdin -> stdout -> stderr -> IO ExitCode
actualHandler handler _env _terminfo _command stdin stdout stderr = do
  (stdinRead, stdinWrite) <- createPipe
  (stdoutRead, stdoutWrite) <- createPipe
  (stderrRead, stderrWrite) <- createPipe
  stdinThread <- async $ do
    stdinLoop stdinWrite
  stdoutTread <- async $ do
    outLoop stdoutRead stdout
  stderrTread <- async $ do
    outLoop stderrRead stderr
  handlerThread <- async $ do
    handler stdinRead stdoutWrite stderrWrite

  --res <- waitAnyCatchCancel [ stdinThread, stdoutTread, stderrTread, handlerThread]
  res <- waitCatch handlerThread
  print res
  mapM_ hClose [ stdinRead, stdinWrite, stdoutRead, stdoutWrite, stderrRead, stderrWrite]
  pure ExitSuccess
  where
    stdinLoop :: Handle -> IO ExitCode
    stdinLoop hnd = do
      bs <- receive stdin 1024
      case (BS.null bs) of
        True -> pure ExitSuccess
        False -> do
          BS.hPut hnd bs
          hFlush hnd
          stdinLoop hnd
    outLoop :: OutputStream stdout => Handle -> stdout -> IO ExitCode
    outLoop input out = do
      bs <- BS.hGetSome input 1024
      case (BS.null bs) of
        True -> pure ExitSuccess
        False -> do
          sendAll out bs
          outLoop input out

