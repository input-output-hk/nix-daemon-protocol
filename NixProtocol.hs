{-# LANGUAGE RankNTypes #-}

module NixProtocol where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy as LBS

-- based on code from https://github.com/lpeterse/haskell-ssh

class Encoding a where
  put :: a -> Put

class Decoding a where
  get :: Get a

class MessageStream a where
  sendMessage :: forall msg. Encoding msg => a -> msg -> IO ()
  receiveMessage :: forall msg. Decoding msg => a -> IO msg

instance Decoding Word64 where
  get = getWord64le

instance Encoding Word64 where
  put = putWord64le

instance Decoding Text where
  get 
