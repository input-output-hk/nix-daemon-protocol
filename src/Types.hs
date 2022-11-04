{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

module Types where

import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, takeTMVar)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad (replicateM)
import Control.Monad.Trans.Except.Extra (left)
import Data.Binary
import Data.Binary.Get (runGetIncremental, Decoder(..), getWord64le, getByteString)
import Data.Binary.Put (runPut, putWord64le, putByteString)
import Data.ByteString (ByteString)
import Data.Text (Text)
import System.IO (Handle, hFlush)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T

paddingSize :: Word64 -> Int
paddingSize 0 = 0
paddingSize n =
  fromIntegral $
  if modded == 0
    then 0
    else 8 - modded
  where
    modded = n `mod` 8

newtype NixInt = NixInt Word64

instance Binary NixInt where
  put (NixInt word) = putWord64le word
  get = NixInt <$> getWord64le

newtype NixString =
  NixString Text
  deriving (Show)

instance Binary NixString where
  get = do
    NixByteString bs <- get
    pure $ NixString $ T.decodeUtf8 bs
  put (NixString msg) = do
    put $ NixByteString $ T.encodeUtf8 msg

newtype NixByteString =
  NixByteString ByteString

instance Binary NixByteString where
  get = do
    NixInt len <- get
    bs <- getByteString $ fromIntegral len
    _padding <- getByteString $ paddingSize len
    pure $ NixByteString bs
  put (NixByteString bs) = do
    put $ NixInt $ fromIntegral $ BS.length bs
    putByteString bs
    putByteString $
      BS.pack $ replicate (paddingSize $ fromIntegral $ BS.length bs) 0

newtype NixList a =
  NixList [a]
  deriving (Show)

instance Binary a => Binary (NixList a) where
  put (NixList list) = do
    put $ length list
    mapM_ put list
  get = do
    NixInt listLength <- get
    NixList <$> replicateM (fromIntegral listLength) get

-- based on code from https://github.com/lpeterse/haskell-ssh
class MessageStream a where
  sendMessage ::
       forall msg. Binary msg
    => a
    -> msg
    -> ExceptT Text IO ()
  receiveMessage ::
       forall msg. Binary msg
    => a
    -> ExceptT Text IO msg
  receiveFramedMessage :: a -> ExceptT Text IO LBS.ByteString

data HandleConnection = HandleConnection
  { hcStdIn :: Handle
  , hcStdOut :: Handle
  , hcUnconsumed :: TMVar ByteString
  }

instance MessageStream HandleConnection where
  receiveMessage HandleConnection{hcStdIn,hcUnconsumed} = do
    buf <- liftIO $ atomically $ takeTMVar hcUnconsumed
    loop (Just buf) $ runGetIncremental get
    where
      loop :: Maybe ByteString -> Decoder msg -> ExceptT Text IO msg
      loop _ (Fail _unconsumed _offset _errmsg) = left "todo"
      loop (Just extra) (Partial fun) = do
        loop Nothing $ fun $ Just extra
      loop Nothing (Partial fun) = do
        bs <- liftIO $ BS.hGetSome hcStdIn 1024
        case BS.null bs of
          True -> left "eof"
          False -> do
            loop Nothing $ fun $ Just bs
      loop _ (Done remain _offset res) = do
        liftIO $ atomically $ putTMVar hcUnconsumed remain
        pure res
  receiveFramedMessage conn@HandleConnection{hcUnconsumed,hcStdIn} = do
    NixInt frameSize <- receiveMessage conn
    --liftIO $ fprint ("frame size: " % d % "\n") frameSize
    loop mempty frameSize
    where
      takePayload :: MonadIO m => Int -> m LBS.ByteString
      takePayload len = do
        extra <- liftIO $ atomically $ takeTMVar hcUnconsumed
        if | (BS.length extra == 0)
              --liftIO $ print "direct receiveAll"
            ->
             do liftIO $ do
                  atomically $ putTMVar hcUnconsumed mempty
                  LBS.hGet hcStdIn len
           | (BS.length extra) < len ->
             do liftIO $ atomically $ putTMVar hcUnconsumed mempty
                more <- takePayload (len - (BS.length extra))
                pure $ (LBS.fromStrict extra) <> more
           | (BS.length extra) == len
              --liftIO $ print "case2"
            ->
             do liftIO $ atomically $ putTMVar hcUnconsumed mempty
                pure $ LBS.fromStrict extra
           | (BS.length extra) > len
              --liftIO $ print "case3"
            ->
             do let (want, moreExtra) = BS.splitAt len extra
                liftIO $ atomically $ putTMVar hcUnconsumed moreExtra
                pure $ LBS.fromStrict want
           | otherwise ->
             do liftIO $ print @Text "case4"
                liftIO $ do
                  atomically $ putTMVar hcUnconsumed mempty
                  LBS.hGet hcStdIn len
      loop :: LBS.ByteString -> Word64 -> ExceptT Text IO LBS.ByteString
      loop xs 0 = pure $ xs
      loop xs len
          --liftIO $ fprint ("loop(" % d % "," % d % ")\n") (LBS.length xs) len
       = do
        payload <- takePayload $ fromIntegral len
        NixInt frameSize <- receiveMessage conn
          --liftIO $ fprint ("frame size: " % d % "\n") frameSize
        loop (xs <> payload) frameSize
  sendMessage HandleConnection{hcStdOut} msg = liftIO $ do
    LBS.hPut hcStdOut $ runPut $ put msg
    hFlush hcStdOut

newHandleConnection :: Handle -> Handle -> IO HandleConnection
newHandleConnection stdin stdout = do
  leftover <- newTMVarIO mempty
  pure $ HandleConnection stdin stdout leftover
