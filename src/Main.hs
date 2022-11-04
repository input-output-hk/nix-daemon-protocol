{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Applicative ((<|>), many)
import Control.Exception (SomeException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (left)
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.Hash.Algorithms ()
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Parse as BP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Serialize (putByteString, runGet)
import Data.Serialize.Get (getWord64le)
import Data.Serialize.Put (runPut)
import Data.Serialize.Put (putWord64le)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word32, Word64, Word8)
import Formatting ((%), fprint)
import Formatting.ShortFormatters (sh)
import GHC.Base (when)
import Network.SSH
  ( InputStream(..)
  , OutputStream(..)
  , PublicKey(..)
  , sendAll
  )
import qualified Network.SSH.Server as Server
import NixProtocol
  ( StdErrError(..)
  , handleOneOpcode
  , newConnection
  , stderrLast
  )
import qualified NixProtocol
import SocketDuplex ()
import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import HsshOld
import Utils
import Types (sendMessage, newHandleConnection, NixInt(..))
import GHC.Stack (HasCallStack)
import Options.Applicative
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (stdin,stdout,stderr, hFlush, hSetBuffering, BufferMode(LineBuffering))

data StartMode = StartModeSSH | StartModeStdio

modeParser :: Parser StartMode
modeParser = sshModeParser <|> stdioModeParser

sshModeParser = flag' StartModeSSH (long "ssh")

stdioModeParser = flag' StartModeStdio (long "stdio")

main :: IO ()
main = do
  let
    opts = info (modeParser <**> helper) (fullDesc)
  mode <- execParser opts
  when False $ do
    pubfilebytes <- BS.readFile "ssh-host-key.pub"
    fprint ("raw pubkey file: " % sh % "\n") pubfilebytes
    pubkey <- decodePublicKey pubfilebytes
    fprint ("raw parse result " % sh % "\n") pubkey
    fprint ("fingerprint: " % sh % "\n") $ pubkeyFingerprint pubkey
  case mode of
    StartModeSSH -> HsshOld.startSshServer sessionHandler
    StartModeStdio -> do
      protocolOut <- hDuplicate stdout
      hSetBuffering stderr LineBuffering
      hDuplicateTo stderr stdout
      hSetBuffering stdout LineBuffering
      code <- sessionHandler stdin protocolOut stderr
      hFlush stderr
      exitWith code

decodePublicKey :: (MonadFail m, BA.ByteArray ba) => ba -> m PublicKey
decodePublicKey = f . BP.parse parsePubkey . BA.convert
  where
    f (BP.ParseOK _ a) = pure a
    f (BP.ParseFail e) = fail e
    f (BP.ParseMore c) = f (c Nothing)

parsePubkey :: BP.Parser BS.ByteString PublicKey
parsePubkey = do
  BP.bytes "ssh-ed25519"
  void $ many space
  bs <- parseBase64
  void $ many space
  _comment <- BP.takeAll
  case BP.parse parseRawPubKey bs of
    BP.ParseOK _ key -> pure key
    BP.ParseFail e -> fail e
    BP.ParseMore _ -> syntaxError
  where
    syntaxError :: BP.Parser ba a
    syntaxError = fail "Syntax error"
    parseRawPubKey :: BP.Parser BS.ByteString PublicKey
    parseRawPubKey = do
      key <-
        getString >>= \algo ->
          case algo of
            "ssh-ed25519" -> do
              BP.skip 3
              BP.byte 32 -- length field (is always 32 for ssh-ed25519)
              pubkey <- BP.take Ed25519.publicKeySize
              case Ed25519.publicKey pubkey of
                CryptoPassed a -> pure $ PublicKeyEd25519 a
                CryptoFailed _ ->
                  fail $
                  "Invalid " ++ show (BA.convert algo :: BA.Bytes) ++ " key"
            _ ->
              fail $
              "Unsupported algorithm " ++ show (BA.convert algo :: BA.Bytes)
      pure key
    getWord64be :: BA.ByteArray ba => BP.Parser ba Word32
    getWord64be = do
      x0 <- fromIntegral <$> BP.anyByte
      x1 <- fromIntegral <$> BP.anyByte
      x2 <- fromIntegral <$> BP.anyByte
      x3 <- fromIntegral <$> BP.anyByte
      pure $ shiftR x0 24 .|. shiftR x1 16 .|. shiftR x2 8 .|. x3
    getString :: BA.ByteArray ba => BP.Parser ba ba
    getString = BP.take =<< (fromIntegral <$> getWord64be)
    fe :: Char -> Word8
    fe = fromIntegral . fromEnum
    space :: (BA.ByteArray ba) => BP.Parser ba ()
    space =
      BP.anyByte >>= \c ->
        if | c == fe ' ' -> pure ()
           | c == fe '\n' -> pure ()
           | c == fe '\r' -> pure ()
           | c == fe '\t' -> pure ()
           | otherwise -> fail ""
    parseBase64 :: (BA.ByteArray ba) => BP.Parser ba ba
    parseBase64 = s0 []
          -- Initial state and final state.
      where
        s0 xs = (char >>= s1 xs) <|> pure (BA.pack $ reverse xs)
          -- One character read (i). Three more characters expected.
        s1 xs i = (char >>= s2 xs i)
          -- Two characters read (i and j). Either '==' or two more character expected.
        s2 xs i j = r2 xs i j <|> (char >>= s3 xs i j)
          -- Three characters read (i, j and k). Either a '=' or one more character expected.
        s3 xs i j k = r3 xs i j k <|> (char >>= s4 xs i j k)
          -- Four characters read (i, j, k and l). Computation of result and transition back to s0.
        s4 xs i j k l = s0 $ byte3 : byte2 : byte1 : xs
          where
            byte1 = (i `shiftL` 2) + (j `shiftR` 4)
            byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
            byte3 = ((k .&. 3) `shiftL` 6) + l
          -- Read two '=' chars as finalizer. Only valid from state s2.
        r2 xs i j = padding >> padding >> pure (BA.pack $ reverse $ byte1 : xs)
          where
            byte1 = (i `shiftL` 2) + (j `shiftR` 4)
          -- Read one '=' char as finalizer. Only valid from state s1.
        r3 xs i j k = padding >> pure (BA.pack $ reverse $ byte2 : byte1 : xs)
          where
            byte1 = (i `shiftL` 2) + (j `shiftR` 4)
            byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
        char :: (BA.ByteArray ba) => BP.Parser ba Word8
        char =
          BP.anyByte >>= \c ->
            if | c >= fe 'A' && c <= fe 'Z' -> pure (c - fe 'A')
               | c >= fe 'a' && c <= fe 'z' -> pure (c - fe 'a' + 26)
               | c >= fe '0' && c <= fe '9' -> pure (c - fe '0' + 52)
               | c == fe '+' -> pure 62
               | c == fe '/' -> pure 63
               | otherwise -> fail ""
        padding :: (BA.ByteArray ba) => BP.Parser ba ()
        padding = BP.byte 61 -- 61 == fromEnum '='

handleSessionRequest ::
     Ident -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest _idnt _req =
  pure $
  Just $
  Server.SessionHandler $ \_env _terminfo _command stdin stdout stderr -> do
    eFinalResult <-
      try $ do
        conn <- newConnection stdin stdout
        eResult <-
          runExceptT $ do
            clientVersion <- NixProtocol.handshake conn
            sendMessage conn $ NixInt stderrLast
            liftIO $ do
              fprint ("client with " % sh % " connected\n") clientVersion
            pure "handshake done"
        case eResult of
          Left err -> do
            print err
            pure $ ExitFailure 1
          Right success -> do
            print @T.Text success
            eMainLoopError <-
              runExceptT $ do
                _ <- forever $ handleOneOpcode conn
                pure "done main loop"
            case eMainLoopError of
              Left "eof" -> do
                fprint ("main loop finished without error\n")
                pure ExitSuccess
              Left err -> do
                fprint ("main loop error: " % sh % "\n") err
                _ <-
                  runExceptT $ do
                    sendMessage conn $ StdErrError 1 $ T.pack $ show err
                    sendMessage conn $ NixInt stderrLast
                pure $ ExitFailure 1
              Right success' -> do
                fprint
                  ("main loop finished without error: " % sh @T.Text % "\n")
                  success'
                _ <- runExceptT $ do sendMessage conn $ NixInt stderrLast
                pure ExitSuccess
    case eFinalResult of
      Left err -> do
        print @SomeException err
        sendAll stderr $ BSC.pack $ show err
        pure $ ExitFailure 1
      Right res -> pure res

sessionHandler :: HasCallStack => SessionHandler
sessionHandler stdin stdout stderr = do
  eFinalResult <- try $ do
    conn <- newHandleConnection stdin stdout
    eResult <- runExceptT $ do
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
            _ <- forever $ handleOneOpcode conn
            pure "done main loop"
        case eMainLoopError of
          Left "eof" -> do
            fprint ("main loop finished without error\n")
            pure ExitSuccess
          Left err -> do
            fprint ("main loop error: " % sh % "\n") err
            _ <-
              runExceptT $ do
                sendMessage conn $ StdErrError 1 $ T.pack $ show err
                sendMessage conn $ NixInt stderrLast
            pure $ ExitFailure 1
          Right success' -> do
            fprint
              ("main loop finished without error: " % sh @T.Text % "\n")
              success'
            _ <- runExceptT $ do sendMessage conn $ NixInt stderrLast
            pure ExitSuccess
  case eFinalResult of
    Left err -> do
      print @SomeException err
      BS.hPut stderr $ BSC.pack $ show err
      pure $ ExitFailure 1
    Right res -> pure res

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


