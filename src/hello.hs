{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, bracketOnError)
import Control.Monad (forever, void)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Default (Default(def))
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import qualified System.Socket as S
import qualified System.Socket.Family.Inet6 as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream as S

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (left)
import Data.Serialize (putByteString, runGet)
import Data.Serialize.Get (getWord64le)
import Data.Serialize.Put (runPut)
import Data.Serialize.Put (putWord64le)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word64)
import GHC.Base (when)
import Network.SSH
  ( InputStream(..)
  , KeyPair
  , OutputStream(..)
  , UserName
  , decodePrivateKeyFile
  , sendAll
  )
import qualified Network.SSH.Server as Server

import Formatting ((%), fprint)
import Formatting.ShortFormatters (sh)
import NixProtocol
  ( NixInt(..)
  , StdErrError(..)
  , handleOneOpcode
  , newConnection
  , sendMessage
  )
import qualified NixProtocol
import SocketDuplex ()

data Ident =
  JustUsername UserName

main :: IO ()
main
  -- ssh-keygen -t ed25519 -f ssh-host-key -N ''
 = do
  file <- BS.readFile "ssh-host-key"
  (privateKey, _):_ <-
    decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  bracket open close (accept config privateKey)
  where
    config =
      Server.Config
        { Server.transportConfig = Data.Default.def
        , Server.userAuthConfig =
            Data.Default.def
              { Server.onAuthRequest =
                  \username _ _ -> pure (Just $ JustUsername username)
              }
        , Server.connectionConfig =
            Data.Default.def
              { Server.onSessionRequest = handleSessionRequest
              , Server.onDirectTcpIpRequest = handleDirectTcpIpRequest
              }
        }
    open = S.socket :: IO (S.Socket S.Inet6 S.Stream S.Default)
    close = S.close
    accept :: Server.Config identity -> KeyPair -> S.Socket S.Inet6 S.Stream S.Default -> IO b
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
            forkFinally
              (Server.serve cfg agent stream >>= print) $
              \emsg -> do
                print emsg
                S.close stream

-- for when the user tries to do tcp forwarding over ssh
handleDirectTcpIpRequest ::
     Ident -> Server.DirectTcpIpRequest -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest _idnt req =
  pure $
  Just $
  Server.DirectTcpIpHandler $ \stream -> do
    bs <- receive stream 4096
    sendAll stream "HTTP/1.1 200 OK\n"
    sendAll stream "Content-Type: text/plain\n\n"
    sendAll stream $! BS.pack $ fmap (fromIntegral . fromEnum) $ show req
    sendAll stream "\n\n"
    sendAll stream bs
    print bs

handleSessionRequest ::
     Ident -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest _idnt _req =
  pure $
  Just $
  Server.SessionHandler $ \_ _ _ stdin stdout _ -> do
    conn <- newConnection stdin stdout
    eResult <-
      runExceptT $ do
        clientVersion <- NixProtocol.handshake conn
        sendMessage conn $ NixInt stderrLast
        liftIO $ do fprint ("client with " % sh % " connected\n") clientVersion
        pure "handshake done"
    case eResult of
      Left err -> do
        print err
        pure $ ExitFailure 1
      Right success -> do
        print @T.Text success
        eMainLoopError <-
          runExceptT $ do
            handleOneOpcode conn
            pure "done main loop"
        case eMainLoopError of
          Left err -> do
            fprint ("main loop error: " % sh % "\n") err
            _ <- runExceptT $ do
              sendMessage conn $ StdErrError 1 $ T.pack $ show err
              sendMessage conn $ NixInt stderrLast
            pure $ ExitFailure 1
          Right success' -> do
            fprint ("main loop finished without error: " % sh @T.Text % "\n") success'
            _ <- runExceptT $ do
              sendMessage conn $ NixInt stderrLast
            pure ExitSuccess

--    handshake stdin stdout
--    -- sendAll stdout $ Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.word64LE workerMagic2
--    -- sendAll stdout $ pack $ show $ wm1
--    pure ExitSuccess

--readWord64 :: InputStream istream => istream -> IO (Either String Word64)
--readWord64 stdin = runGet getWord64le <$> receive stdin 8
data ErrorType
  = ErrorTypeParsingWord String
  | ErrorTypeParsingSomethingElse String
  | ErrorTypeInvalidWorkerMagic
  deriving (Show)

readWord64 :: InputStream istream => istream -> ExceptT ErrorType IO Word64
readWord64 stdin = do
  eWord <- liftIO $ runGet getWord64le <$> receive stdin 8
  case eWord of
    Left err -> left $ ErrorTypeParsingWord err
    Right word -> pure word

writeWord64 ::
     OutputStream ostream => ostream -> Word64 -> ExceptT ErrorType IO ()
writeWord64 stdout n = do
  liftIO $ sendAll stdout $ runPut $ putWord64le n

readString :: InputStream istream => istream -> ExceptT ErrorType IO T.Text
readString stdin = do
  len <- readWord64 stdin
  bytes <- liftIO $ receive stdin $ fromIntegral len
  when (len /= 0) $ do
    _padding <- liftIO $ receive stdin $ 8 - ((fromIntegral len) `mod` 8)
    pure ()
  pure $ T.decodeUtf8 bytes

writeString ::
     OutputStream ostream => ostream -> T.Text -> ExceptT ErrorType IO ()
writeString stdout text = do
  let rawBytes = T.encodeUtf8 text
      rawLength = BS.length rawBytes
      paddingLength =
        if (rawLength == 0)
          then 0
          else 8 - ((fromIntegral rawLength) `mod` 8)
      padding = replicate paddingLength 0
      encodedBytes =
        runPut $ do
          putWord64le $ fromIntegral rawLength
          putByteString rawBytes
          putByteString $ BS.pack padding
  liftIO $ sendAll stdout encodedBytes

-- either leftFunc rightFunc bs
--
-- if workerMagic1, err := readInt(s); err != nil {
-- 	return errors.WithMessage(err, "reading magic1")
-- } else if workerMagic1 != WorkerMagic1 {
-- 	return errors.WithMessagef(err, "worker magic 1 mismatch: %x != %x", workerMagic1, WorkerMagic1)
-- } else if err := writeInt(s, WorkerMagic2); err != nil {
-- 	return errors.WithMessage(err, "writing magic2")
-- } else if err := writeInt(s, ProtocolVersion); err != nil {
-- 	return errors.WithMessage(err, "writing protocol version")
-- } else if _, err := readInt(s); err != nil { // clientProtocolVersion
-- 	return errors.WithMessage(err, "reading protocol version")
-- } else if err := writeString(s, "2.11.2"); err != nil {
-- 	return errors.WithMessage(err, "writing version")
-- } else if err := writeInt(s, StderrLast); err != nil {
-- 	return errors.WithMessage(err, "writing StderrLast")
-- } else {
-- // throw away bytes used by old versions (cpu affinity and reserve space)
-- s.Read(make([]byte, 16))
-- readInt :: Get Word64
-- readInt = do
--   runGet getWord64le rd
stderrLast :: Word64
stderrLast = 0x616C7473 -- stla

