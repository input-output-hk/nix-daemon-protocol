{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiWayIf #-}

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
import Data.Word (Word64, Word8, Word32)
import GHC.Base (when)
import Network.SSH
  ( InputStream(..)
  , KeyPair
  , OutputStream(..)
  , UserName
  , decodePrivateKeyFile
  , sendAll
  , PublicKey(..)
  , ServiceName
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

import qualified Data.ByteArray.Parse   as BP
import           Control.Applicative    (many, (<|>))
import Data.Bits (shiftL, (.&.), shiftR, (.|.))
import qualified Crypto.PubKey.Ed25519  as Ed25519
import           Crypto.Error

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.Hash            as Hash
import qualified Data.ByteString.Base64              as Base64

data Ident =
  JustUsername UserName

main :: IO ()
main
  -- ssh-keygen -t ed25519 -f ssh-host-key -N ''
 = do
  file <- BS.readFile "ssh-host-key"
  (privateKey, _):_ <-
    decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  print privateKey

  pubfilebytes <- BS.readFile "ssh-host-key.pub"
  fprint ("raw pubkey file: " % sh % "\n") pubfilebytes
  pubkey <- decodePublicKey pubfilebytes
  fprint ("raw parse result " % sh % "\n") pubkey
  fprint ("fingerprint: " % sh % "\n") $ pubkeyFingerprint pubkey

  bracket open close (accept config privateKey)
  where
    onAuthRequest :: UserName -> ServiceName -> PublicKey -> IO (Maybe Ident)
    onAuthRequest username service key = do
      fprint ("auth request for user " % sh % " service " % sh % " with pubkey fingerprint " % sh % "\n") username service $ pubkeyFingerprint key
      pure (Just $ JustUsername username)
    config =
      Server.Config
        { Server.transportConfig = Data.Default.def
        , Server.userAuthConfig =
            Data.Default.def
              { Server.onAuthRequest = onAuthRequest
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
    BP.ParseFail e   -> fail e
    BP.ParseMore _   -> syntaxError

    where
      syntaxError :: BP.Parser ba a
      syntaxError = fail "Syntax error"

      parseRawPubKey :: BP.Parser BS.ByteString PublicKey
      parseRawPubKey = do
        key <- getString >>= \algo -> case algo of
          "ssh-ed25519" -> do
            BP.skip 3
            BP.byte 32 -- length field (is always 32 for ssh-ed25519)
            pubkey <- BP.take Ed25519.publicKeySize
            case Ed25519.publicKey pubkey of
              CryptoPassed a -> pure $ PublicKeyEd25519 a
              CryptoFailed _ -> fail $ "Invalid " ++ show (BA.convert algo :: BA.Bytes) ++ " key"
          _ -> fail $ "Unsupported algorithm " ++ show (BA.convert algo :: BA.Bytes)
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
      space = BP.anyByte >>= \c-> if
        | c == fe ' '  -> pure ()
        | c == fe '\n' -> pure ()
        | c == fe '\r' -> pure ()
        | c == fe '\t' -> pure ()
        | otherwise    -> fail ""

      parseBase64 :: (BA.ByteArray ba) => BP.Parser ba ba
      parseBase64 = s0 []
        where
          -- Initial state and final state.
          s0 xs         =                 (char >>= s1 xs)       <|> pure (BA.pack $ reverse xs)
          -- One character read (i). Three more characters expected.
          s1 xs i       =                 (char >>= s2 xs i)
          -- Two characters read (i and j). Either '==' or two more character expected.
          s2 xs i j     = r2 xs i j   <|> (char >>= s3 xs i j)
          -- Three characters read (i, j and k). Either a '=' or one more character expected.
          s3 xs i j k   = r3 xs i j k <|> (char >>= s4 xs i j k)
          -- Four characters read (i, j, k and l). Computation of result and transition back to s0.
          s4 xs i j k l = s0 $ byte3 : byte2 : byte1: xs
            where
              byte1 = ( i         `shiftL` 2) + (j `shiftR` 4)
              byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
              byte3 = ((k .&.  3) `shiftL` 6) + l
          -- Read two '=' chars as finalizer. Only valid from state s2.
          r2 xs i j     = padding >> padding >> pure (BA.pack $ reverse $ byte1 : xs)
            where
              byte1 = (i `shiftL` 2) + (j `shiftR` 4)
          -- Read one '=' char as finalizer. Only valid from state s1.
          r3 xs i j k   = padding >> pure (BA.pack $ reverse $ byte2 : byte1 : xs)
            where
              byte1 = (i          `shiftL` 2) + (j `shiftR` 4)
              byte2 = ((j .&. 15) `shiftL` 4) + (k `shiftR` 2)
          char :: (BA.ByteArray ba) => BP.Parser ba Word8
          char = BP.anyByte >>= \c-> if
               | c >= fe 'A' && c <= fe 'Z' -> pure (c - fe 'A')
               | c >= fe 'a' && c <= fe 'z' -> pure (c - fe 'a' + 26)
               | c >= fe '0' && c <= fe '9' -> pure (c - fe '0' + 52)
               | c == fe '+'                -> pure 62
               | c == fe '/'                -> pure 63
               | otherwise                  -> fail ""
          padding :: (BA.ByteArray ba) => BP.Parser ba ()
          padding = BP.byte 61 -- 61 == fromEnum '='

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

hashSHA256 :: BS.ByteString -> BS.ByteString
hashSHA256 bs = BA.convert (Hash.hash bs :: Hash.Digest Hash.SHA256)

pubkeyFingerprint :: PublicKey -> T.Text
pubkeyFingerprint pubkey = maybe "unsupported algo" (T.decodeLatin1 . Base64.encode . hashSHA256) $ keyToRawBytes pubkey
    where
      keyToRawBytes :: PublicKey -> Maybe BS.ByteString
      keyToRawBytes (PublicKeyEd25519 key') = Just $ "\NUL\NUL\NUL\vssh-ed25519\NUL\NUL\NUL " <> (BA.convert key')
      keyToRawBytes _ = Nothing
