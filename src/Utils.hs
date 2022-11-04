{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Network.SSH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Crypto.Hash as Hash

pubkeyFingerprint :: PublicKey -> T.Text
pubkeyFingerprint pubkey =
  maybe "unsupported algo" (T.decodeLatin1 . Base64.encode . hashSHA256) $
  keyToRawBytes pubkey
  where
    keyToRawBytes :: PublicKey -> Maybe BS.ByteString
    keyToRawBytes (PublicKeyEd25519 key') =
      Just $ "\NUL\NUL\NUL\vssh-ed25519\NUL\NUL\NUL " <> (BA.convert key')
    keyToRawBytes _ = Nothing

hashSHA256 :: BS.ByteString -> BS.ByteString
hashSHA256 bs = BA.convert (Hash.hash bs :: Hash.Digest Hash.SHA256)
