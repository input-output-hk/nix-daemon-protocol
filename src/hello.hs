{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkFinally)
import Control.Exception (bracket, bracketOnError, handle, throwIO)
import Control.Monad (forever, void)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Default (Default(def))
import System.Exit (ExitCode(ExitFailure, ExitSuccess))

import qualified System.Socket as S
import qualified System.Socket.Family.Inet6 as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream as S
import qualified System.Socket.Unsafe as S

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Except.Extra (left)
import qualified Data.Binary as B
import Data.Bits (Bits((.|.), shiftL))
import qualified Data.ByteString.Builder
import Data.Serialize (runGet, putByteString, getBytes, Get)
import Data.Serialize.Get (getWord64le)
import Data.Serialize.Put (runPut)
import Data.Serialize.Put (putWord64le)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Base (when)
import Network.SSH
  ( DuplexStream
  , InputStream(..)
  , KeyPair
  , OutputStream(..)
  , decodePrivateKeyFile
  , sendAll
  , UserName
  )
import qualified Network.SSH.Server as Server
import Data.Word (Word64)

import NixProtocol

data Ident = JustUsername UserName

main :: IO ()
main = do
  file <- BS.readFile "../spongix/user1"
  (privateKey, _):_ <-
    decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  bracket open close (accept config privateKey)
  where
    config =
      Server.Config
        { Server.transportConfig = Data.Default.def
        , Server.userAuthConfig =
            Data.Default.def
              {Server.onAuthRequest = \username _ _ -> pure (Just $ JustUsername username)}
        , Server.connectionConfig =
            Data.Default.def
              { Server.onSessionRequest = handleSessionRequest
              , Server.onDirectTcpIpRequest = handleDirectTcpIpRequest
              }
        }
    open = S.socket :: IO (S.Socket S.Inet6 S.Stream S.Default)
    close = S.close
    accept config agent s = do
      S.setSocketOption s (S.ReuseAddress True)
      S.setSocketOption s (S.V6Only False)
      S.bind s (S.SocketAddressInet6 S.inet6Any 2222 0 0)
      S.listen s 5
      forever $
        bracketOnError (S.accept s) (S.close . fst) $ \(stream, peer) -> do
          putStrLn $ "Connection from " ++ show peer
          void $
            forkFinally
              (Server.serve config agent stream >>= print)
              (const $ S.close stream)

-- for when the user tries to do tcp forwarding over ssh
handleDirectTcpIpRequest ::
     Ident
  -> Server.DirectTcpIpRequest
  -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest idnt req =
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
handleSessionRequest idnt req =
  pure $
  Just $
  Server.SessionHandler $ \_ _ _ stdin stdout _ -> do
    eResult <- runExceptT $ do handshake stdin stdout
    case eResult of
      Left err -> do
        print err
        pure $ ExitFailure 1
      Right success -> do
        print success
        pure ExitSuccess

--    handshake stdin stdout
--    -- sendAll stdout $ Data.ByteString.Builder.toLazyByteString $ Data.ByteString.Builder.word64LE workerMagic2
--    -- sendAll stdout $ pack $ show $ wm1
--    pure ExitSuccess
--
--
--			writeInt(s, StderrError)
--			writeString(s, "Error")
--			writeInt(s, 1)
--			writeString(s, "error-name")
--			writeString(s, err.Error())
--			writeInt(s, 0)
--			writeInt(s, 0)
handshake ::
     (InputStream istream, OutputStream ostream)
  => istream
  -> ostream
  -> ExceptT ErrorType IO ()
handshake stdin stdout = do
  workerMagic1 <- readWord64 stdin
  case workerMagic1 of
    0x6E697863 -> handshake2 stdin stdout
    _ -> left ErrorTypeInvalidWorkerMagic

handshake2 ::
     (InputStream istream, OutputStream ostream)
  => istream
  -> ostream
  -> ExceptT ErrorType IO ()
handshake2 stdin stdout = do
  liftIO $
    sendAll stdout $
    runPut $ do
      putWord64le workerMagic2
      putWord64le protocolVersion
  clientProtocolVersion <- readWord64 stdin
  liftIO $ sendAll stdout $ runPut $ putByteString "2.11.2"

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

writeWord64 :: OutputStream ostream => ostream -> Word64 -> ExceptT ErrorType IO ()
writeWord64 stdout n = do
  liftIO $ sendAll stdout $ runPut $ putWord64le n

readString :: InputStream istream => istream -> ExceptT ErrorType IO T.Text
readString stdin = do
  len <- readWord64 stdin
  bytes <- liftIO $ receive stdin $ fromIntegral len
  when (len /= 0) $ do
    liftIO $ receive stdin $ 8 - ((fromIntegral len) `mod` 8)
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

stderrError :: Word64
stderrError = 0x63787470 -- ptxc

workerMagic1 :: Word64
workerMagic1 = 0x6E697863 -- cxin

workerMagic2 :: Word64
workerMagic2 = 0x6478696F -- ioxd

protocolVersion :: Word64
protocolVersion = 1 `shiftL` 8 .|. 34 -- 290

-------------------------------------------------------------------------------
-- Instances for use with the socket library
-------------------------------------------------------------------------------
instance DuplexStream (S.Socket f S.Stream p)

instance OutputStream (S.Socket f S.Stream p) where
  send stream bytes = handle f $ S.send stream bytes S.msgNoSignal
    where
      f e
        | e == S.ePipe = pure 0
        | otherwise = throwIO e
  sendUnsafe stream (BA.MemView ptr n) =
    fromIntegral <$>
    handle f (S.unsafeSend stream ptr (fromIntegral n) S.msgNoSignal)
    where
      f e
        | e == S.ePipe = pure 0
        | otherwise = throwIO e

instance InputStream (S.Socket f S.Stream p) where
  peek stream len = S.receive stream len (S.msgNoSignal <> S.msgPeek)
  receive stream len = S.receive stream len S.msgNoSignal
  receiveUnsafe stream (BA.MemView ptr n) =
    fromIntegral <$> S.unsafeReceive stream ptr (fromIntegral n) S.msgNoSignal
-- module Main (main) where
-- 
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Binary.Get
-- import Data.Word
-- 
-- deserialiseHeader :: Get (Word32, Word32, Word32)
-- deserialiseHeader = do
--   alen <- getWord32be
--   plen <- getWord32be
--   chksum <- getWord32be
--   return (alen, plen, chksum)
-- 
-- main :: IO ()
-- main = do
--   input <- BL.getContents
--   print $ runGet deserialiseHeader input
