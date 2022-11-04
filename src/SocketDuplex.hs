{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module SocketDuplex where

import Control.Exception (handle, throwIO)
import qualified Data.ByteArray as BA
import Network.SSH (DuplexStream, InputStream(..), OutputStream(..))
import qualified System.Socket as S
import qualified System.Socket.Type.Stream as S
import qualified System.Socket.Unsafe as S

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
