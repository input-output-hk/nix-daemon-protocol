{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}

module NixProtocol where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left)
import Control.Monad (replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (Bits((.|.), shiftL), (.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Base (when)

import Control.Concurrent.STM.TMVar
  ( TMVar
  , newTMVarIO
  , putTMVar
  , takeTMVar
  )
import Control.Monad.STM (atomically)
--import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as T
import Network.SSH (InputStream, OutputStream, receive, sendAll)
import Formatting ((%), fprint, sformat)
import Formatting.ShortFormatters (sh)

-- based on code from https://github.com/lpeterse/haskell-ssh
newtype NixInt =
  NixInt Word64

newtype NixString =
  NixString Text deriving Show

newtype NixByteString =
  NixByteString ByteString

newtype NixList a = NixList [a] deriving Show

data ClientVersion =
  ClientVersion
    { major :: Word8
    , minor :: Word8
    }
  deriving (Show)

data WorkerOpcode
  = WOPIsValidPath
  | WOPHasSubstitutes
  | WOPQueryReferrers
  | WOPAddToStore
  | WOPAddTextToStore
  | WOPBuildPaths
  | WOPEnsurePath
  | WOPAddTempRoot
  | WOPAddIndirectRoot
  | WOPSyncWithGC
  | WOPFindRoots
  | WOPSetOptions
  | WOPCollectGarbage
  | WOPQuerySubstitutablePathInfo
  | WOPQueryAllValidPaths
  | WOPQueryFailedPaths
  | WOPClearFailedPaths
  | WOPQueryPathInfo
  | WOPQueryPathFromHashPart
  | WOPQuerySubstitutablePathInfos
  | WOPQueryValidPaths
  | WOPQuerySubstitutablePaths
  | WOPQueryValidDerivers
  | WOPOptimiseStore
  | WOPVerifyStore
  | WOPBuildDerivation
  | WOPAddSignatures
  | WOPNarFromPath
  | WOPAddToStoreNar
  | WOPQueryMissing
  | WOPQueryDerivationOutputMap
  | WOPRegisterDrvOutput
  | WOPQueryRealisation
  | WOPAddMultipleToStore
  | WOPAddBuildLog
  | WOPBuildPathsWithResults
  | WOPUnhandled Word64
  deriving Show

intToOpcode :: Word64 -> WorkerOpcode
intToOpcode 31 = WOPQueryValidPaths
intToOpcode w = WOPUnhandled w

instance Binary WorkerOpcode where
  get = do
    NixInt opcode <- get
    pure $ intToOpcode opcode
  put = undefined

paddingSize :: Word64 -> Int
paddingSize 0 = 0
paddingSize n = fromIntegral $ 8 - (n `mod` 8)

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

data (InputStream istream, OutputStream ostream) =>
     Connection istream ostream =
  Connection
    { istream :: istream
    , ostream :: ostream
    , unconsumed :: TMVar ByteString
    }

instance (InputStream istream, OutputStream ostream) =>
         MessageStream (Connection istream ostream) where
  receiveMessage Connection {istream, unconsumed} = do
    todo <- liftIO $ atomically $ takeTMVar unconsumed
    loop (Just todo) $ runGetIncremental get
    where
      loop :: Maybe ByteString -> Decoder msg -> ExceptT Text IO msg
      loop _ (Fail _unconsumed _offset _errmsg) = left "todo"
      loop (Just extra) (Partial fun) = do
        liftIO $ fprint ("leftover from last read "%sh%"\n") extra
        loop Nothing $ fun $ Just extra
      loop Nothing (Partial fun) = do
        bs <- liftIO $ receive istream 4096
        liftIO $ fprint ("did ssh read of " % sh % "\n") bs
        loop Nothing $ fun $ Just bs
      loop _ (Done remain _offset res) = do
        liftIO $ atomically $ putTMVar unconsumed remain
        pure res
  sendMessage Connection {ostream} msg = liftIO $ do
    let lbs = runPut $ put msg
    mapM_ (sendAll ostream) $ LBS.toChunks lbs
    pure ()

instance Binary NixInt where
  put (NixInt word) = putWord64le word
  get = NixInt <$> getWord64le

instance Binary NixString where
  get = do
    NixByteString bs <- get
    pure $ NixString $ T.decodeUtf8 bs
  put (NixString msg) = do
    put $ NixByteString $ T.encodeUtf8 msg

instance Binary a => Binary (NixList a) where
  put = undefined
  get = do
    NixInt listLength <- get
    NixList <$> replicateM (fromIntegral listLength) get

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

workerMagic1 :: Word64
workerMagic1 = 0x6E697863 -- cxin

workerMagic2 :: Word64
workerMagic2 = 0x6478696F -- ioxd

stderrError :: Word64
stderrError = 0x63787470 -- ptxc

protocolVersion :: Word64
protocolVersion = 1 `shiftL` 8 .|. 34 -- 290

handshake :: MessageStream s => s -> ExceptT Text IO ClientVersion
handshake stream = do
  NixInt magic <- receiveMessage stream
  when (magic /= workerMagic1) $ left "invalid magic"
  sendMessage stream $ NixInt workerMagic2
  sendMessage stream $ NixInt protocolVersion
  NixInt clientVersion <- receiveMessage stream
  let major = fromIntegral $ (clientVersion `shiftR` 8) .&. 0xff
      minor = fromIntegral $ clientVersion .&. 0xff
      cv = ClientVersion major minor
  NixInt hasAffinity <- receiveMessage stream
  when (hasAffinity /= 0) $ do
      NixInt _pinnedCpu <- receiveMessage stream
      pure ()
  NixInt _unk <- receiveMessage stream
  pure cv

handleOneOpcode ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
handleOneOpcode conn = do
  opcode <- receiveMessage conn
  liftIO $ do
    print @WorkerOpcode opcode
  case opcode of
    WOPQueryValidPaths -> do
      paths <- getStringList conn
      liftIO $ print paths
      pure ()
    other -> do
      left $ sformat ("unhandled opcode: " % sh) other

data StdErrError =
  StdErrError
    { verbosity :: Int
    , msg :: Text
    }

instance Binary StdErrError where
  get = undefined
  put StdErrError {verbosity, msg} = do
    put $ NixInt stderrError
    put $ NixString "Error"
    put $ NixInt $ fromIntegral verbosity -- verbosity
    put $ NixString "" -- ignored
    put $ NixString msg
    put $ NixInt 0 -- pos
    put $ NixInt 0 -- number of traces

newConnection ::
     (InputStream istream, OutputStream ostream)
  => istream
  -> ostream
  -> IO (Connection istream ostream)
newConnection stdin stdout = do
  leftover <- newTMVarIO mempty
  pure $ Connection {istream = stdin, ostream = stdout, unconsumed = leftover}

getStringList :: (InputStream stdin, OutputStream stdout) => Connection stdin stdout -> ExceptT Text IO [Text]
getStringList conn = do
  NixList l <- (receiveMessage conn :: ExceptT Text IO (NixList NixString))
  pure $ map (\(NixString str) -> str) l
