{-# LANGUAGE ForeignFunctionInterface, LambdaCase #-}
module Concordium.External where

import Foreign
import Foreign.C

import Control.Concurrent.Chan
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Serialize
import Data.Serialize.Put as P
import Data.IORef
import Data.Foldable(forM_)

import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text as AET

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Data.FixedByteString as FBS

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization(FinalizationIndex(..))

import Concordium.Scheduler.Utils.Init.Example (initialState)

import Concordium.Birk.Bake
import Concordium.Runner
import Concordium.Show
import Concordium.Skov (SkovFinalizationState)
import Concordium.Logger

import qualified Concordium.Getters as Get
import qualified Concordium.Startup as S

type PeerID = Word64

data BakerRunner = BakerRunner {
    bakerInChan :: Chan (InMessage PeerID),
    bakerOutChan :: Chan (OutMessage PeerID),
    bakerState :: IORef SkovFinalizationState,
    bakerLogger :: LogMethod IO
}

type CStringCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" callCStringCallback :: FunPtr CStringCallback -> CStringCallback

foreign import ccall "dynamic" callCStringCallbackInstance :: FunPtr (Int64 -> CStringCallback) -> Int64 -> CStringCallback

makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word64 -- ^Number of bakers
    -> FunPtr CStringCallback -- ^Function to process the generated genesis data.
    -> FunPtr (Int64 -> CStringCallback) -- ^Function to process each baker identity. Will be called repeatedly with different baker ids.
    -> IO ()
makeGenesisData genTime nBakers cbkgen cbkbaker = do
    BS.useAsCStringLen (encode genData) $ \(cdata, clen) -> callCStringCallback cbkgen cdata (fromIntegral clen)
    mapM_ (\bkr@(BakerIdentity bid _ _ _ _) -> BS.useAsCStringLen (encode bkr) $ \(cdata, clen) -> callCStringCallbackInstance cbkbaker (fromIntegral bid) cdata (fromIntegral clen)) bakersPrivate
    where
        bakers = S.makeBakers (fromIntegral nBakers)
        genData = S.makeGenesisData genTime bakers
        bakersPrivate = map fst bakers

type BlockCallback = Int64 -> CString -> Int64 -> IO ()

type MissingByBlockDeltaCallback = PeerID -> BlockReference -> Word64 -> IO ()

type MissingByBlockCallback = PeerID -> BlockReference -> IO ()

type MissingByFinalizationIndexCallback = PeerID -> Word64 -> IO ()

-- | External function that logs in Rust a message using standard Rust log output
--
-- The first argument represents the Identifier which shows in which module the message has been emited.
-- The current mapping is as follows:
--
-- +----------+---------+
-- |Identifier|Module   |
-- +==========+=========+
-- |1         |Runner   |
-- +----------+---------+
-- |2         |Afgjort  |
-- +----------+---------+
-- |3         |Birk     |
-- +----------+---------+
-- |4         |Crypto   |
-- +----------+---------+
-- |5         |Kontrol  |
-- +----------+---------+
-- |6         |Skov     |
-- +----------+---------+
-- |7         |Baker    |
-- +----------+---------+
-- |8         |External |
-- +----------+---------+
--
-- The second argument represents the Log Level which is interpreted as follows:
--
-- +-----+--------+
-- |Value|LogLevel|
-- +=====+========+
-- |1    |Error   |
-- +-----+--------+
-- |2    |Warning |
-- +-----+--------+
-- |3    |Info    |
-- +-----+--------+
-- |4    |Debug   |
-- +-----+--------+
-- |Other|Trace   |
-- +-----+--------+
--
-- The third argument is the log message that is emited.

type LogCallback = Word8 -> Word8 -> CString -> IO()

foreign import ccall "dynamic" callBlockCallback :: FunPtr BlockCallback -> BlockCallback
foreign import ccall "dynamic" callLogCallback :: FunPtr LogCallback -> LogCallback
foreign import ccall "dynamic" callMissingBlockDelta :: FunPtr MissingByBlockDeltaCallback -> MissingByBlockDeltaCallback
foreign import ccall "dynamic" callMissingBlock :: FunPtr MissingByBlockCallback -> MissingByBlockCallback
foreign import ccall "dynamic" callMissingFin :: FunPtr MissingByFinalizationIndexCallback -> MissingByFinalizationIndexCallback

toLogMethod :: FunPtr LogCallback -> LogMethod IO
toLogMethod logCallbackPtr = le
    where
        logCallback = callLogCallback logCallbackPtr
        le src lvl msg = BS.useAsCString (BS.pack msg) $
                            logCallback (logSourceId src) (logLevelId lvl)

outLoop :: LogMethod IO -> Chan (OutMessage PeerID) -> BlockCallback -> MissingByBlockDeltaCallback -> MissingByBlockCallback -> MissingByFinalizationIndexCallback -> IO ()
outLoop logm chan cbk missingBlock missingFinBlock missingFinIx = do
    readChan chan >>= \case
        MsgNewBlock block -> do
            let bbs = runPut (put (NormalBlock block))
            logm External LLDebug $ "Sending block data size = " ++ show (BS.length bbs)
            BS.useAsCStringLen bbs $ \(cstr, l) -> cbk 0 cstr (fromIntegral l)
        MsgFinalization finMsg -> do
            logm External LLDebug $ "Sending finalization message size = " ++ show (BS.length finMsg)
            BS.useAsCStringLen finMsg $ \(cstr, l) -> cbk 1 cstr (fromIntegral l)
        MsgFinalizationRecord finRec -> do
            let bs = runPut (put finRec)
            logm External LLDebug $ "Sending finalization record data size = " ++ show (BS.length bs)
            BS.useAsCStringLen bs $ \(cstr, l) -> cbk 2 cstr (fromIntegral l)
        MsgMissingBlock src bh delta -> do
            logm External LLDebug $ "Requesting missing block " ++ show bh ++ "+ delta " ++ show (theBlockHeight delta) ++ " from peer " ++ show src
            withBlockReference bh $ \blockRef -> missingBlock src blockRef (theBlockHeight delta)
        MsgMissingFinalization src (Left bh) -> do
            logm External LLDebug $ "Requesting missing finalization record for block " ++ show bh ++ " from peer " ++ show src
            withBlockReference bh $ missingFinBlock src
        MsgMissingFinalization src (Right (FinalizationIndex finIx)) -> do
            logm External LLDebug $ "Requestion missing finalization record at index " ++ show finIx ++ " from peer " ++ show src
            missingFinIx src finIx

    outLoop logm chan cbk missingBlock missingFinBlock missingFinIx

startBaker ::
           CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> CString -> Int64 -- ^Serialized baker identity (c string + len)
           -> FunPtr BlockCallback -- ^Handler for new blocks
           -> FunPtr LogCallback -- ^Handler for log events
           -> FunPtr MissingByBlockDeltaCallback -- ^Handler for missing blocks
           -> FunPtr MissingByBlockCallback -- ^Handler for missing finalization records by block hash
           -> FunPtr MissingByFinalizationIndexCallback -- ^Handler for missing finalization records by finalization index
            -> IO (StablePtr BakerRunner)
startBaker gdataC gdataLenC bidC bidLenC bcbk lcbk missingBlock missingFinBlock missingFinIx = do
        gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        bdata <- BS.packCStringLen (bidC, fromIntegral bidLenC)
        case (decode gdata, decode bdata) of
            (Right genData, Right bid) -> do
                (cin, cout, out) <- makeRunner logM bid genData (initialState 2)
                _ <- forkIO $ outLoop logM cout (callBlockCallback bcbk) (callMissingBlockDelta missingBlock) (callMissingBlock missingFinBlock) (callMissingFin missingFinIx)
                newStablePtr (BakerRunner cin cout out logM)
            _ -> ioError (userError $ "Error decoding serialized data.")
    where
        logM = toLogMethod lcbk

stopBaker :: StablePtr BakerRunner -> IO ()
stopBaker bptr = do
    BakerRunner cin _ _ _ <- deRefStablePtr bptr
    freeStablePtr bptr
    writeChan cin MsgShutdown

receiveBlock :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
receiveBlock bptr src cstr l = do
    BakerRunner cin _ _ logm <- deRefStablePtr bptr
    logm External LLDebug $ "Received block data size = " ++ show l ++ ". Decoding ..."
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get blockBS of
        Left _ -> do
          logm External LLDebug "Block deserialization failed. Ignoring the block."
          return 1
        Right (GenesisBlock _) -> do
            logm External LLDebug $ "Genesis block deserialized. Ignoring the block."
            return 1
        Right (NormalBlock block) -> do
                        logm External LLInfo $ "Block deserialized. Sending to consensus."
                        writeChan cin $ MsgBlockReceived src block
                        return 0

receiveFinalization :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO ()
receiveFinalization bptr src cstr l = do
    BakerRunner cin _ _ logm <- deRefStablePtr bptr
    logm External LLDebug $ "Received finalization message size = " ++ show l
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    writeChan cin $ MsgFinalizationReceived src bs

receiveFinalizationRecord :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
receiveFinalizationRecord bptr src cstr l = do
    BakerRunner cin _ _ logm <- deRefStablePtr bptr
    logm External LLDebug $ "Received finalization record data size = " ++ show l ++ ". Decoding ..."
    finRecBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get finRecBS of
        Left _ -> do
          logm External LLDebug "Deserialization of finalization record failed."
          return 1
        Right finRec -> do
          logm External LLDebug "Finalization record deserialized."
          writeChan cin $ MsgFinalizationRecordReceived src finRec
          return 0

printBlock :: CString -> Int64 -> IO ()
printBlock cstr l = do
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get blockBS of
        Left _ -> putStrLn "<Bad Block>"
        Right block -> putStrLn $ showsBlock block ""

receiveTransaction :: StablePtr BakerRunner -> CString -> Int64 -> IO Int64
receiveTransaction bptr tdata len = do
    BakerRunner cin _ _ logm <- deRefStablePtr bptr
    logm External LLInfo $ "Received transaction, data size=" ++ show len ++ ". Decoding ..."
    tbs <- BS.packCStringLen (tdata, fromIntegral len)
    case runGet get tbs of
      Left _ -> do logm External LLDebug "Could not decode transaction into header + body."
                   return 1
      Right tr -> do
        logm External LLInfo $ "Transaction decoded. Its header is: " ++ show (trHeader tr)
        writeChan cin (MsgTransactionReceived tr) >> return 0


-- |Returns a null-terminated string with a JSON representation of the current status of Consensus.
getConsensusStatus :: StablePtr BakerRunner -> IO CString
getConsensusStatus bptr = do
    sfsRef <- bakerState <$> deRefStablePtr bptr
    status <- Get.getConsensusStatus sfsRef
    newCString $ LT.unpack $ AET.encodeToLazyText status

-- |Given a null-terminated string that represents a block hash (base 16), returns a null-terminated
-- string containing a JSON representation of the block.
getBlockInfo :: StablePtr BakerRunner -> CString -> IO CString
getBlockInfo bptr blockcstr = do
    sfsRef <- bakerState <$> deRefStablePtr bptr
    block <- peekCString blockcstr
    blockInfo <- Get.getBlockInfo sfsRef block
    newCString $ LT.unpack $ AET.encodeToLazyText blockInfo

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
getAncestors :: StablePtr BakerRunner -> CString -> Word64 -> IO CString
getAncestors bptr blockcstr depth = do
    sfsRef <- bakerState <$> deRefStablePtr bptr
    block <- peekCString blockcstr
    ancestors <- Get.getAncestors sfsRef block (fromIntegral depth)
    newCString $ LT.unpack $ AET.encodeToLazyText ancestors

-- |Returns a null-terminated string with a JSON representation of the current branches from the
-- last finalized block (inclusive).
getBranches :: StablePtr BakerRunner -> IO CString
getBranches bptr = do
    sfsRef <- bakerState <$> deRefStablePtr bptr
    branches <- Get.getBranches sfsRef
    newCString $ LT.unpack $ AET.encodeToLazyText branches



byteStringToCString :: BS.ByteString -> IO CString
byteStringToCString bs = do
  let bsp = BS.concat [P.runPut (P.putWord32be (fromIntegral (BS.length bs))), bs]
  BS.unsafeUseAsCStringLen bsp $ \(cstr, len) -> do dest <- mallocBytes len
                                                    copyBytes dest cstr len
                                                    return dest

getLastFinalAccountList :: StablePtr BakerRunner -> IO CString
getLastFinalAccountList bptr = do
  BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
  logm External LLInfo "Received account list request."
  alist <- Get.getLastFinalAccountList sfsRef
  logm External LLInfo $ "Replying with the list: " ++ show alist
  let s = encode alist
  byteStringToCString s

getLastFinalInstances :: StablePtr BakerRunner -> IO CString
getLastFinalInstances bptr = do
  BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
  logm External LLInfo "Received instance list request."
  alist <- Get.getLastFinalInstances sfsRef
  logm External LLInfo $ "Replying with the list: " ++ (show alist)
  let s = encode alist
  byteStringToCString s
  
getLastFinalAccountInfo :: StablePtr BakerRunner -> CString -> IO CString
getLastFinalAccountInfo bptr cstr = do
  BakerRunner _ _ sfsRef logm <-deRefStablePtr bptr
  logm External LLInfo "Received account info request."
  bs <- BS.packCStringLen (cstr, 21)
  case decode bs of
    Left _ -> do logm External LLInfo "Could not decode address."
                 byteStringToCString BS.empty
    Right acc -> do logm External LLInfo $ "Decoded address to: " ++ show acc
                    ainfo <- Get.getLastFinalAccountInfo sfsRef acc
                    logm External LLInfo $ "Replying with: " ++ show ainfo
                    byteStringToCString (encode ainfo)

getLastFinalInstanceInfo :: StablePtr BakerRunner -> CString -> IO CString
getLastFinalInstanceInfo bptr cstr = do
  BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
  logm External LLInfo "Received account info request."
  bs <- BS.packCStringLen (cstr, 16)
  case decode bs of
    Left _ -> do logm External LLInfo "Could not decode address."
                 byteStringToCString BS.empty
    Right ii -> do logm External LLInfo $ "Decoded address to: " ++ show ii
                   iinfo <- Get.getLastFinalContractInfo sfsRef ii
                   logm External LLInfo $ "Replying with: " ++ show ii
                   byteStringToCString (encode iinfo)

freeCStr :: CString -> IO ()
freeCStr = free

type BlockReference = Ptr Word8

withBlockReference :: BlockHash -> (BlockReference -> IO a) -> IO a
withBlockReference (Hash.Hash fbs) = FBS.withPtr fbs

blockReferenceToBlockHash :: BlockReference -> IO BlockHash
blockReferenceToBlockHash src = Hash.Hash <$> FBS.create cp
    where
        cp dest = copyBytes dest src (FBS.fixedLength (undefined :: Hash.DigestSize))

-- |Get a block for the given block hash.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlock :: StablePtr BakerRunner -> BlockReference -> IO CString
getBlock bptr blockRef = do
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for block: " ++ show bh
        b <- runLoggerT (Get.getBlockData sfsRef bh) logm
        case b of
            Nothing -> do
                logm External LLInfo $ "Block not available"
                byteStringToCString BS.empty
            Just block -> do
                logm External LLInfo $ "Block found"
                byteStringToCString $ P.runPut $ put block

-- |Get a block that is descended from the given block hash by the given number of generations.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlockDelta :: StablePtr BakerRunner -> BlockReference -> Word64 -> IO CString
getBlockDelta bptr blockRef 0 = getBlock bptr blockRef
getBlockDelta bptr blockRef delta = do
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for descendent of block " ++ show bh ++ " with delta " ++ show delta
        b <- runLoggerT (Get.getBlockDescendant sfsRef bh (BlockHeight delta)) logm
        case b of
            Nothing -> do
                logm External LLInfo $ "Block not available"
                byteStringToCString BS.empty
            Just block -> do
                logm External LLInfo $ "Block found"
                byteStringToCString $ P.runPut $ put block

-- |Get a finalization record for the given block.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlockFinalization :: StablePtr BakerRunner -> BlockReference -> IO CString
getBlockFinalization bptr blockRef = do
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for finalization record for block: " ++ show bh
        f <- runLoggerT (Get.getBlockFinalization sfsRef bh) logm
        case f of
            Nothing -> do
                logm External LLInfo $ "Finalization record not available"
                byteStringToCString BS.empty
            Just finRec -> do
                logm External LLInfo $ "Finalization record found"
                byteStringToCString $ P.runPut $ put finRec

-- |Get a finalization record for the given finalization index.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getIndexedFinalization :: StablePtr BakerRunner -> Word64 -> IO CString
getIndexedFinalization bptr finInd = do
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        logm External LLInfo $ "Received request for finalization record at index " ++ show finInd
        f <- runLoggerT (Get.getIndexedFinalization sfsRef (fromIntegral finInd)) logm
        case f of
            Nothing -> do
                logm External LLInfo $ "Finalization record not available"
                byteStringToCString BS.empty
            Just finRec -> do
                logm External LLInfo $ "Finalization record found"
                byteStringToCString $ P.runPut $ put finRec

type FinalizationMessageCallback = PeerID -> CString -> Int64 -> IO ()

foreign import ccall "dynamic" callFinalizationMessageCallback :: FunPtr FinalizationMessageCallback -> FinalizationMessageCallback

-- |Get pending finalization messages beyond a given point.
-- The finalization messages are returned by calling the provided
-- callback for each message, which receives the peer id, a string and
-- the length of the string.  The call returns once all messages are handled.
-- A return value of 0 indicates success.  A return value of 1 indicates
-- that the finalization point could not be deserialized.
-- Note: this function is thread safe; it will not block, but the data can in
-- principle be out of date.
getFinalizationMessages :: StablePtr BakerRunner 
    -> PeerID -- ^Peer id (used in callback)
    -> CString -> Int64 -- ^Data, length of finalization point
    -> FunPtr FinalizationMessageCallback -> IO Int64
getFinalizationMessages bptr peer finPtStr finPtLen callback = do
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        logm External LLInfo $ "Received request for finalization messages"
        finPtBS <- BS.packCStringLen (finPtStr, fromIntegral finPtLen)
        case runGet get finPtBS of
            Left _ -> do
                logm External LLDebug "Finalization point deserialization failed"
                return 1
            Right fpt -> do
                finMsgs <- runLoggerT (Get.getFinalizationMessages sfsRef fpt) logm
                forM_  finMsgs $ \(toLog, finMsg) -> do logm External LLDebug $ "Sending finalization catchup, data = " ++ toLog
                                                        BS.useAsCStringLen finMsg $ \(cstr, l) -> callFinalizationMessageCallback callback peer cstr (fromIntegral l)
                return 0

-- |Get the current point in the finalization protocol.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
getFinalizationPoint :: StablePtr BakerRunner -> IO CString
getFinalizationPoint bptr = do 
        BakerRunner _ _ sfsRef logm <- deRefStablePtr bptr
        logm External LLInfo $ "Received request for finalization point"
        finPt <- Get.getFinalizationPoint sfsRef
        logm External LLDebug $ "Replying with finalization point = " ++ show finPt
        byteStringToCString $ P.runPut $ put finPt

foreign export ccall makeGenesisData :: Timestamp -> Word64 -> FunPtr CStringCallback -> FunPtr (Int64 -> CStringCallback) -> IO ()
foreign export ccall startBaker :: CString -> Int64 -> CString -> Int64 -> FunPtr BlockCallback -> FunPtr LogCallback -> FunPtr MissingByBlockDeltaCallback -> FunPtr MissingByBlockCallback -> FunPtr MissingByFinalizationIndexCallback -> IO (StablePtr BakerRunner)
foreign export ccall stopBaker :: StablePtr BakerRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalization :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO ()
foreign export ccall receiveFinalizationRecord :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
foreign export ccall printBlock :: CString -> Int64 -> IO ()
foreign export ccall receiveTransaction :: StablePtr BakerRunner -> CString -> Int64 -> IO Int64

foreign export ccall getConsensusStatus :: StablePtr BakerRunner -> IO CString
foreign export ccall getBlockInfo :: StablePtr BakerRunner -> CString -> IO CString
foreign export ccall getAncestors :: StablePtr BakerRunner -> CString -> Word64 -> IO CString
foreign export ccall getBranches :: StablePtr BakerRunner -> IO CString
foreign export ccall freeCStr :: CString -> IO ()

foreign export ccall getBlock :: StablePtr BakerRunner -> BlockReference -> IO CString
foreign export ccall getBlockDelta :: StablePtr BakerRunner -> BlockReference -> Word64 -> IO CString
foreign export ccall getBlockFinalization :: StablePtr BakerRunner -> BlockReference -> IO CString
foreign export ccall getIndexedFinalization :: StablePtr BakerRunner -> Word64 -> IO CString
foreign export ccall getFinalizationMessages :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> FunPtr FinalizationMessageCallback -> IO Int64
foreign export ccall getFinalizationPoint :: StablePtr BakerRunner -> IO CString

-- report global state information will be removed in the future when global
-- state is handled better
foreign export ccall getLastFinalAccountList :: StablePtr BakerRunner -> IO CString
foreign export ccall getLastFinalInstances :: StablePtr BakerRunner -> IO CString
foreign export ccall getLastFinalAccountInfo :: StablePtr BakerRunner -> CString -> IO CString
foreign export ccall getLastFinalInstanceInfo :: StablePtr BakerRunner -> CString -> IO CString
