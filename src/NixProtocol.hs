{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module NixProtocol where

import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, takeTMVar)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Except.Extra (left)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits (Bits((.|.), shiftL), (.&.), shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Formatting ((%), fprint, sformat)
import Formatting.ShortFormatters (sh)
import GHC.Base (when)
import Network.SSH (InputStream, OutputStream, receive, receiveAll, sendAll)

-- based on code from https://github.com/lpeterse/haskell-ssh
newtype NixInt =
  NixInt Word64

newtype NixString =
  NixString Text
  deriving (Show)

newtype NixByteString =
  NixByteString ByteString

newtype NixList a =
  NixList [a]
  deriving (Show)

data ClientVersion =
  ClientVersion
    { major :: Word8
    , minor :: Word8
    }
  deriving (Show)

data WorkerOpcode
  = WOPQuitObsolete -- 0
  | WOPIsValidPath -- 1
  | WOPQuerySubstitutesObsolete -- 2
  | WOPQuerySubstitutes -- 3
  | WOPQueryPathHashObsolete -- 4
  | WOPQueryReferencesObsolete -- 5
  | WOPQueryReferrers -- 6
  | WOPAddToStore -- 7
  | WOPAddTextToStore -- 8
  | WOPBuildPaths -- 9
  | WOPEnsurePath -- 10
  | WOPAddTempRoot -- 11
  | WOPAddIndirectRoot -- 12
  | WOPSyncWithGC -- 13
  | WOPFindRoots -- 14
  | WOPCollectGarbageObsolete -- 15
  | WOPExportPathObsolete -- 16
  | WOPImportPathObsolete -- 17
  | WOPQueryDeriverObsolete -- 18
  | WOPSetOptions -- 19
  | WOPCollectGarbage -- 20
  | WOPQuerySubstitutablePathInfo -- 21
  | WOPQueryDerivationOutputsObsolete -- 22
  | WOPQueryAllValidPaths -- 23
  | WOPQueryFailedPaths -- 24
  | WOPClearFailedPaths -- 25
  | WOPQueryPathInfo -- 26
  | WOPImportPathsObsolete -- 27
  | WOPQueryDerivationOutputNamesObsolete -- 28
  | WOPQueryPathFromHashPart -- 29
  | WOPQuerySubstitutablePathInfos -- 30
  | WOPQueryValidPaths -- 31
  | WOPQuerySubstitutablePaths -- 32
  | WOPQueryValidDerivers -- 33
  | WOPOptimiseStore -- 34
  | WOPVerifyStore -- 35
  | WOPBuildDerivation -- 36
  | WOPAddSignatures -- 37
  | WOPNarFromPath -- 38
  | WOPAddToStoreNar -- 39
  | WOPQueryMissing -- 40
  | WOPQueryDerivationOutputMap -- 41
  | WOPRegisterDrvOutput -- 42
  | WOPQueryRealisation -- 43
  | WOPAddMultipleToStore -- 44
  | WOPAddBuildLog -- 45
  | WOPBuildPathsWithResults -- 46
  | WOPUnhandled Word64
  deriving (Show)

intToOpcode :: Word64 -> WorkerOpcode
intToOpcode 1 = WOPIsValidPath
intToOpcode 11 = WOPAddTempRoot
intToOpcode 26 = WOPQueryPathInfo
intToOpcode 31 = WOPQueryValidPaths
intToOpcode 42 = WOPRegisterDrvOutput
intToOpcode 44 = WOPAddMultipleToStore
intToOpcode w = WOPUnhandled w

instance Binary WorkerOpcode where
  get = do
    NixInt opcode <- get
    pure $ intToOpcode opcode
  put = undefined

paddingSize :: Word64 -> Int
paddingSize 0 = 0
paddingSize n =
  fromIntegral $
  if modded == 0
    then 0
    else 8 - modded
  where
    modded = n `mod` 8

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
      loop (Just extra) (Partial fun)
        --liftIO $ fprint ("leftover from last read "%sh%"\n") extra
       = do
        loop Nothing $ fun $ Just extra
      loop Nothing (Partial fun) = do
        bs <- liftIO $ receive istream 4096
        --liftIO $ fprint ("did ssh read of " % sh % "\n") bs
        loop Nothing $ fun $ Just bs
      loop _ (Done remain _offset res) = do
        liftIO $ atomically $ putTMVar unconsumed remain
        pure res
  receiveFramedMessage conn@Connection {istream, unconsumed} = do
    NixInt frameSize <- receiveMessage conn
    --liftIO $ fprint ("frame size: " % d % "\n") frameSize
    loop mempty frameSize
    where
      takePayload :: MonadIO m => Int -> m LBS.ByteString
      takePayload len = do
        extra <- liftIO $ atomically $ takeTMVar unconsumed
        if | (BS.length extra == 0)
              --liftIO $ print "direct receiveAll"
            ->
             do liftIO $ do
                  atomically $ putTMVar unconsumed mempty
                  BS.fromStrict <$> receiveAll istream len
           | (BS.length extra) < len ->
             do liftIO $ atomically $ putTMVar unconsumed mempty
                more <- takePayload (len - (BS.length extra))
                pure $ (BS.fromStrict extra) <> more
           | (BS.length extra) == len
              --liftIO $ print "case2"
            ->
             do liftIO $ atomically $ putTMVar unconsumed mempty
                pure $ BS.fromStrict extra
           | (BS.length extra) > len
              --liftIO $ print "case3"
            ->
             do let (want, moreExtra) = BS.splitAt len extra
                liftIO $ atomically $ putTMVar unconsumed moreExtra
                pure $ BS.fromStrict want
           | otherwise ->
             do liftIO $ print @Text "case4"
                liftIO $ do
                  atomically $ putTMVar unconsumed mempty
                  BS.fromStrict <$> receiveAll istream len
      loop :: LBS.ByteString -> Word64 -> ExceptT Text IO LBS.ByteString
      loop xs 0 = pure $ xs
      loop xs len
          --liftIO $ fprint ("loop(" % d % "," % d % ")\n") (LBS.length xs) len
       = do
        payload <- takePayload $ fromIntegral len
        NixInt frameSize <- receiveMessage conn
          --liftIO $ fprint ("frame size: " % d % "\n") frameSize
        loop (xs <> payload) frameSize
  sendMessage Connection {ostream} msg =
    liftIO $ do
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
  put (NixList list) = do
    put $ length list
    mapM_ put list
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
protocolVersion = 1 `shiftL` 8 .|. 34

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
  when (minor >= 33) $ do sendMessage stream $ NixByteString "2.11.1"
  NixInt _unk <- receiveMessage stream
  pure cv

handleOneOpcode ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
handleOneOpcode conn = do
  opcode <- receiveMessage conn
  liftIO $ do print @WorkerOpcode opcode
  case opcode of
    WOPAddTempRoot -> addTempRoot conn
    WOPQueryValidPaths -> queryValidPaths conn
    WOPAddMultipleToStore -> addMultipleToStore conn
    WOPRegisterDrvOutput -> registerDrvOutput conn
    WOPIsValidPath -> isValidPath conn
    WOPQueryPathInfo -> queryPathInfo conn
    other -> do
      left $ sformat ("unhandled opcode: " % sh) other

queryPathInfo ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
queryPathInfo conn = do
  NixByteString storePath <- receiveMessage conn
  liftIO $ fprint ("queryPathInfo storePath: " % sh % "\n") storePath
  sendMessage conn $ NixInt stderrLast
  -- TODO ValidPathInfo::write
  sendMessage conn $ NixInt 0

isValidPath ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
isValidPath conn = do
  NixByteString storePath <- receiveMessage conn
  liftIO $ fprint ("isValidPath storePath: " % sh % "\n") storePath
  sendMessage conn $ NixInt stderrLast
  sendMessage conn $ NixInt 1

addTempRoot ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
addTempRoot conn = do
  NixByteString storePath <- receiveMessage conn
  liftIO $ fprint ("addTempRoot storePath: " % sh % "\n") storePath
  sendMessage conn $ NixInt stderrLast
  sendMessage conn $ NixInt 1

queryValidPaths ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
queryValidPaths conn = do
  paths <- getStringList conn
  NixInt buildersUseSubstitutes <- receiveMessage conn
  liftIO $
    fprint
      ("paths: " % sh % " use binary cache: " % sh % "\n")
      paths
      buildersUseSubstitutes
  sendMessage conn $ NixInt stderrLast
  sendMessage conn emptyPathList
  sendMessage conn $ NixInt stderrLast

addMultipleToStore ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
addMultipleToStore conn = do
  NixInt repair <- receiveMessage conn
  NixInt dontCheckSigs <- receiveMessage conn
  liftIO $
    fprint
      ("add multiple, repair: " % sh % " dontCheck: " % sh % "\n")
      repair
      dontCheckSigs
  narstream <- receiveFramedMessage conn
  let ListOfNars nars = runGet get narstream
  liftIO $ mapM_ (print . fst) nars
  sendMessage conn $ NixInt stderrLast

registerDrvOutput ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO ()
registerDrvOutput conn = do
  NixByteString realisation <- receiveMessage conn
  liftIO $ fprint ("realisation:" % sh % "\n") realisation
  sendMessage conn $ NixInt stderrLast

emptyPathList :: NixList NixString
emptyPathList = NixList []

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

getStringList ::
     (InputStream stdin, OutputStream stdout)
  => Connection stdin stdout
  -> ExceptT Text IO [Text]
getStringList conn = do
  NixList l <- (receiveMessage conn :: ExceptT Text IO (NixList NixString))
  pure $ map (\(NixString str) -> str) l

stderrLast :: Word64
stderrLast = 0x616C7473 -- stla

data ValidPathInfo =
  ValidPathInfo
    { vpiOutPath :: Text
    , vpiDeriver :: Text
    , vpiNarHash :: Text
    , vpiReferences :: [Text]
    , vpiRegistrationTime :: Word64
    , vpiNarSize :: Word64
    , vpiUltimate :: Bool
    , vpiSigs :: [Text]
    , vpiContentAddress :: Text
    }
  deriving (Show)

instance Binary ValidPathInfo where
  put = undefined
  get = do
    NixString outPath <- get
    NixString drv <- get
    NixString narHash <- get
    NixList l <- get
    let references = map (\(NixString str) -> str) l
    NixInt regtime <- get
    NixInt narSize <- get
    NixInt ultimate <- get
    NixList rawSigs <- get
    let sigs = map (\(NixString str) -> str) rawSigs
    NixString ca <- get
    pure $
      ValidPathInfo
        outPath
        drv
        narHash
        references
        regtime
        narSize
        (ultimate /= 0)
        sigs
        ca

newtype ListOfNars =
  ListOfNars [(ValidPathInfo, ByteString)]

instance Binary ListOfNars where
  put = undefined
  get = do
    NixInt narCount <- get
    l <-
      replicateM (fromIntegral narCount) $ do
        vpi <- get
        nar <- getByteString (fromIntegral $ vpiNarSize vpi)
        pure (vpi, nar)
    pure $ ListOfNars l
