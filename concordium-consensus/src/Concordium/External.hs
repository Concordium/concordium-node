{-# LANGUAGE ForeignFunctionInterface, LambdaCase, RecordWildCards #-}
module Concordium.External where

import Foreign
import Foreign.C

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Serialize
import Data.Serialize.Put as P
import Data.Foldable(forM_)
import Control.Exception
import Control.Monad

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
import Concordium.Skov (SkovFinalizationEvent(..))
import Concordium.Afgjort.Finalize (FinalizationOutputEvent(..))
import Concordium.Logger

import qualified Concordium.Getters as Get
import qualified Concordium.Startup as S

-- |A 'PeerID' identifies peer at the p2p layer.
type PeerID = Word64

-- |A 'BlockReference' is a pointer to a block hash as a sequence of 32 bytes.
type BlockReference = Ptr Word8

-- |Use a 'BlockHash' as a 'BlockReference'.  The 'BlockReference' may not
-- be valid after the function has returned.
withBlockReference :: BlockHash -> (BlockReference -> IO a) -> IO a
withBlockReference (Hash.Hash fbs) = FBS.withPtr fbs

-- |Create a 'BlockHash' from a 'BlockReference'.  This creates a copy
-- of the block hash.
blockReferenceToBlockHash :: BlockReference -> IO BlockHash
blockReferenceToBlockHash src = Hash.Hash <$> FBS.create cp
    where
        cp dest = copyBytes dest src (FBS.fixedLength (undefined :: Hash.DigestSize))


-- |Callback for handling genesis data.
type GenesisDataCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeGenesisDataCallback :: FunPtr GenesisDataCallback -> GenesisDataCallback
callGenesisDataCallback :: FunPtr GenesisDataCallback -> BS.ByteString -> IO ()
callGenesisDataCallback cb bs = BS.useAsCStringLen bs $ \(cdata, clen) -> (invokeGenesisDataCallback cb) cdata (fromIntegral clen)

-- |Callback for handling a generated baker identity.
-- The first argument is the identity of the baker (TODO: this should really by a 'Word64').
-- The second argument is the pointer to the data, and the third is the length of the data.
type BakerIdentityCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBakerIdentityCallback :: FunPtr BakerIdentityCallback -> BakerIdentityCallback
callBakerIdentityCallback :: FunPtr BakerIdentityCallback -> Word64 -> BS.ByteString -> IO ()
callBakerIdentityCallback cb bix bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBakerIdentityCallback cb (fromIntegral bix) cdata (fromIntegral clen)

-- |Generate genesis data given a genesis time (in seconds since the UNIX epoch) and
-- number of bakers.  This function is deterministic: calling with the same genesis
-- time and number of bakers should generate the same genesis data and baker identities.
makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word64 -- ^Number of bakers
    -> FunPtr GenesisDataCallback -- ^Function to process the generated genesis data.
    -> FunPtr BakerIdentityCallback -- ^Function to process each baker identity. Will be called repeatedly with different baker ids.
    -> IO ()
makeGenesisData genTime nBakers cbkgen cbkbaker = do
    callGenesisDataCallback cbkgen (encode genData)
    mapM_ (\bkr@(BakerIdentity bid _ _ _ _) -> callBakerIdentityCallback cbkbaker bid (encode bkr)) bakersPrivate
    where
        bakers = S.makeBakers (fromIntegral nBakers)
        genData = S.makeGenesisData genTime bakers
        bakersPrivate = map fst bakers

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
type LogCallback = Word8 -> Word8 -> CString -> IO ()
foreign import ccall "dynamic" callLogCallback :: FunPtr LogCallback -> LogCallback

-- |Wrap a log callback as a log method.
toLogMethod :: FunPtr LogCallback -> LogMethod IO
toLogMethod logCallbackPtr = le
    where
        logCallback = callLogCallback logCallbackPtr
        le src lvl msg = BS.useAsCString (BS.pack msg) $
                            logCallback (logSourceId src) (logLevelId lvl)


-- |Callback for broadcasting a message to the network.
-- The first argument indicates the message type.
-- The second argument is a pointer to the data to broadcast.
-- The third argument is the length of the data in bytes.
type BroadcastCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastCallback
data BroadcastMessageType 
    = BMTBlock
    | BMTFinalization
    | BMTFinalizationRecord
callBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastMessageType -> BS.ByteString -> IO ()
callBroadcastCallback cbk mt bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBroadcastCallback cbk mti cdata (fromIntegral clen)
    where
        mti = case mt of
            BMTBlock -> 0
            BMTFinalization -> 1
            BMTFinalizationRecord -> 2

-- |Callback to signal that a block is missing by a block hash of a (possible) ancestor and
-- the difference in height from this ancestor.
type MissingByBlockDeltaCallback = PeerID -> BlockReference -> Word64 -> IO ()
foreign import ccall "dynamic" invokeMissingByBlockDeltaCallback :: FunPtr MissingByBlockDeltaCallback -> MissingByBlockDeltaCallback
callMissingByBlockDeltaCallback :: FunPtr MissingByBlockDeltaCallback -> PeerID -> BlockHash -> BlockHeight -> IO ()
callMissingByBlockDeltaCallback cbk peer bh delta = withBlockReference bh $ \bref -> invokeMissingByBlockDeltaCallback cbk peer bref (theBlockHeight delta)

-- |Callback to signal that an item is missing for a specific block.
type MissingByBlockCallback = PeerID -> BlockReference -> IO ()
foreign import ccall "dynamic" invokeMissingByBlockCallback :: FunPtr MissingByBlockCallback -> MissingByBlockCallback
callMissingByBlockCallback :: FunPtr MissingByBlockCallback -> PeerID -> BlockHash -> IO ()
callMissingByBlockCallback cbk peer bh = withBlockReference bh $ invokeMissingByBlockCallback cbk peer

-- |Callback to signal that a finalization record is missing at a given finalization index.
type MissingByFinalizationIndexCallback = PeerID -> Word64 -> IO ()
foreign import ccall "dynamic" invokeMissingByFinalizationIndexCallback :: FunPtr MissingByFinalizationIndexCallback -> MissingByFinalizationIndexCallback
callMissingByFinalizationIndexCallback :: FunPtr MissingByFinalizationIndexCallback -> PeerID -> FinalizationIndex -> IO ()
callMissingByFinalizationIndexCallback cbk peer (FinalizationIndex finIx) = invokeMissingByFinalizationIndexCallback cbk peer finIx

-- |A 'BakerRunner' encapsulates a running baker thread.
data BakerRunner = BakerRunner {
    bakerSyncRunner :: SyncRunner,
    bakerBroadcast :: BroadcastMessageType -> BS.ByteString -> IO (),
    bakerMissingBlock :: PeerID -> BlockHash -> BlockHeight -> IO (),
    bakerMissingFinalizationByBlock :: PeerID -> BlockHash -> IO (),
    bakerMissingFinalizationByIndex :: PeerID -> FinalizationIndex -> IO ()
}

-- |Start up an instance of Skov with a baker thread.
startBaker ::
           CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> CString -> Int64 -- ^Serialized baker identity (c string + len)
           -> FunPtr BroadcastCallback -- ^Handler for generated messages
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
                bakerSyncRunner <- makeSyncRunner logM bid genData (initialState 2) bakerHandler
                newStablePtr BakerRunner{..}
            _ -> ioError (userError $ "Error decoding serialized data.")
    where
        logM = toLogMethod lcbk
        bakerBroadcast = callBroadcastCallback bcbk
        bakerMissingBlock = callMissingByBlockDeltaCallback missingBlock
        bakerMissingFinalizationByBlock = callMissingByBlockCallback missingFinBlock
        bakerMissingFinalizationByIndex = callMissingByFinalizationIndexCallback missingFinIx
        bakerHandler (SOMsgNewBlock block) = do
            let blockbs = runPut (put (NormalBlock block))
            logM External LLDebug $ "Sending block [size=" ++ show (BS.length blockbs) ++ "]"
            bakerBroadcast BMTBlock blockbs
        bakerHandler (SOMsgFinalization finMsg) = do
            let finbs = runPut (put finMsg)
            logM External LLDebug $ "Sending finalization message [size=" ++ show (BS.length finbs) ++ "]: " ++ show finMsg
            bakerBroadcast BMTFinalization finbs
        bakerHandler (SOMsgFinalizationRecord finRec) = do
            let msgbs = runPut (put finRec)
            logM External LLDebug $ "Sending finalization record [size=" ++ show (BS.length msgbs) ++ "]: " ++ show finRec
            bakerBroadcast BMTFinalizationRecord msgbs


-- |Stop a baker thread.  The pointer is not valid after this is called, so
-- should not be reused.
stopBaker :: StablePtr BakerRunner -> IO ()
stopBaker bptr = mask_ $ do
    BakerRunner {..} <- deRefStablePtr bptr
    stopSyncRunner bakerSyncRunner
    freeStablePtr bptr

handleSkovFinalizationEvents :: LogMethod IO -> BakerRunner -> PeerID -> [SkovFinalizationEvent] -> IO ()
handleSkovFinalizationEvents logm BakerRunner{..} src = mapM_ handleEvt
    where
        handleEvt (SkovFinalization (BroadcastFinalizationMessage finMsg)) = do
            let finbs = runPut (put finMsg)
            logm External LLDebug $ "Sending finalization message [size=" ++ show (BS.length finbs) ++ "]: " ++ show finMsg
            bakerBroadcast BMTFinalization finbs
        handleEvt (SkovFinalization (BroadcastFinalizationRecord finRec)) = do
            let msgbs = runPut (put finRec)
            logm External LLDebug $ "Sending finalization record [size=" ++ show (BS.length msgbs) ++ "]: " ++ show finRec
            bakerBroadcast BMTFinalizationRecord msgbs
        handleEvt (SkovMissingBlock bh delta) = do
            logm External LLDebug $ "Requesting missing block " ++ show bh ++ "+ delta " ++ show (theBlockHeight delta) ++ " from peer " ++ show src
            bakerMissingBlock src bh delta
        handleEvt (SkovMissingFinalization (Left bh)) = do
            logm External LLDebug $ "Requesting missing finalization record for block " ++ show bh ++ " from peer " ++ show src
            bakerMissingFinalizationByBlock src bh
        handleEvt (SkovMissingFinalization (Right fi)) = do
            logm External LLDebug $ "Requesting missing finalization record at index " ++ show (theFinalizationIndex fi) ++ " from peer " ++ show src
            bakerMissingFinalizationByIndex src fi

-- |Handle receipt of a block.
receiveBlock :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
receiveBlock bptr src cstr l = do
    bkr@BakerRunner {..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
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
                        evts <- syncReceiveBlock bakerSyncRunner block
                        handleSkovFinalizationEvents logm bkr src evts
                        return 0

-- |Handle receipt of a finalization message.
receiveFinalization :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
receiveFinalization bptr src cstr l = do
    bkr@BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLDebug $ "Received finalization message size = " ++ show l ++ ".  Decoding ..."
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get bs of
        Left _ -> do
            logm External LLDebug "Deserialization of finalization message failed."
            return 1
        Right finMsg -> do
            logm External LLDebug "Finalization message deserialized."
            evts <- syncReceiveFinalizationMessage bakerSyncRunner finMsg
            handleSkovFinalizationEvents logm bkr src evts
            return 0

-- |Handle receipt of a finalization record.
receiveFinalizationRecord :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
receiveFinalizationRecord bptr src cstr l = do
    bkr@BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLDebug $ "Received finalization record data size = " ++ show l ++ ". Decoding ..."
    finRecBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get finRecBS of
        Left _ -> do
          logm External LLDebug "Deserialization of finalization record failed."
          return 1
        Right finRec -> do
          logm External LLDebug "Finalization record deserialized."
          evts <- syncReceiveFinalizationRecord bakerSyncRunner finRec
          handleSkovFinalizationEvents logm bkr src evts
          return 0

-- |Print a representation of a block to the standard output.
printBlock :: CString -> Int64 -> IO ()
printBlock cstr l = do
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get blockBS of
        Left _ -> putStrLn "<Bad Block>"
        Right block -> putStrLn $ showsBlock block ""

-- |Handle receipt of a transaction.
receiveTransaction :: StablePtr BakerRunner -> CString -> Int64 -> IO Int64
receiveTransaction bptr tdata len = do
    BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLInfo $ "Received transaction, data size=" ++ show len ++ ". Decoding ..."
    tbs <- BS.packCStringLen (tdata, fromIntegral len)
    case runGet get tbs of
      Left _ -> do logm External LLDebug "Could not decode transaction into header + body."
                   return 1
      Right tr -> do
        logm External LLInfo $ "Transaction decoded. Its header is: " ++ show (trHeader tr)
        evts <- syncReceiveTransaction bakerSyncRunner tr
        unless (null evts) $ logm Skov LLWarning $ "Received transaction triggered events, which are being dropped"
        return 0


-- |Returns a null-terminated string with a JSON representation of the current status of Consensus.
getConsensusStatus :: StablePtr BakerRunner -> IO CString
getConsensusStatus bptr = do
    sfsRef <- bakerSyncRunner <$> deRefStablePtr bptr
    status <- Get.getConsensusStatus sfsRef
    newCString $ LT.unpack $ AET.encodeToLazyText status

-- |Given a null-terminated string that represents a block hash (base 16), returns a null-terminated
-- string containing a JSON representation of the block.
getBlockInfo :: StablePtr BakerRunner -> CString -> IO CString
getBlockInfo bptr blockcstr = do
    sfsRef <- bakerSyncRunner <$> deRefStablePtr bptr
    block <- peekCString blockcstr
    blockInfo <- Get.getBlockInfo sfsRef block
    newCString $ LT.unpack $ AET.encodeToLazyText blockInfo

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
getAncestors :: StablePtr BakerRunner -> CString -> Word64 -> IO CString
getAncestors bptr blockcstr depth = do
    sfsRef <- bakerSyncRunner <$> deRefStablePtr bptr
    block <- peekCString blockcstr
    ancestors <- Get.getAncestors sfsRef block (fromIntegral depth)
    newCString $ LT.unpack $ AET.encodeToLazyText ancestors

-- |Returns a null-terminated string with a JSON representation of the current branches from the
-- last finalized block (inclusive).
getBranches :: StablePtr BakerRunner -> IO CString
getBranches bptr = do
    sfsRef <- bakerSyncRunner <$> deRefStablePtr bptr
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
    BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLInfo "Received account list request."
    alist <- Get.getLastFinalAccountList bakerSyncRunner
    logm External LLInfo $ "Replying with the list: " ++ show alist
    let s = encode alist
    byteStringToCString s

getLastFinalInstances :: StablePtr BakerRunner -> IO CString
getLastFinalInstances bptr = do
    BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLInfo "Received instance list request."
    alist <- Get.getLastFinalInstances bakerSyncRunner
    logm External LLInfo $ "Replying with the list: " ++ (show alist)
    let s = encode alist
    byteStringToCString s
  
getLastFinalAccountInfo :: StablePtr BakerRunner -> CString -> IO CString
getLastFinalAccountInfo bptr cstr = do
    BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLInfo "Received account info request."
    bs <- BS.packCStringLen (cstr, 21)
    case decode bs of
        Left _ -> do
                logm External LLInfo "Could not decode address."
                byteStringToCString BS.empty
        Right acc -> do
                logm External LLInfo $ "Decoded address to: " ++ show acc
                ainfo <- Get.getLastFinalAccountInfo bakerSyncRunner acc
                logm External LLInfo $ "Replying with: " ++ show ainfo
                byteStringToCString (encode ainfo)

getLastFinalInstanceInfo :: StablePtr BakerRunner -> CString -> IO CString
getLastFinalInstanceInfo bptr cstr = do
    BakerRunner{..} <- deRefStablePtr bptr
    let logm = syncLogMethod bakerSyncRunner
    logm External LLInfo "Received account info request."
    bs <- BS.packCStringLen (cstr, 16)
    case decode bs of
        Left _ -> do
                logm External LLInfo "Could not decode address."
                byteStringToCString BS.empty
        Right ii -> do
                logm External LLInfo $ "Decoded address to: " ++ show ii
                iinfo <- Get.getLastFinalContractInfo bakerSyncRunner ii
                logm External LLInfo $ "Replying with: " ++ show ii
                byteStringToCString (encode iinfo)

freeCStr :: CString -> IO ()
freeCStr = free


-- |Get a block for the given block hash.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlock :: StablePtr BakerRunner -> BlockReference -> IO CString
getBlock bptr blockRef = do
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for block: " ++ show bh
        b <- Get.getBlockData bakerSyncRunner bh
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
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for descendent of block " ++ show bh ++ " with delta " ++ show delta
        b <- Get.getBlockDescendant bakerSyncRunner bh (BlockHeight delta)
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
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for finalization record for block: " ++ show bh
        f <- Get.getBlockFinalization bakerSyncRunner bh
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
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        logm External LLInfo $ "Received request for finalization record at index " ++ show finInd
        f <- Get.getIndexedFinalization bakerSyncRunner (fromIntegral finInd)
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
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        logm External LLInfo $ "Received request for finalization messages"
        finPtBS <- BS.packCStringLen (finPtStr, fromIntegral finPtLen)
        case runGet get finPtBS of
            Left _ -> do
                logm External LLDebug "Finalization point deserialization failed"
                return 1
            Right fpt -> do
                finMsgs <- Get.getFinalizationMessages bakerSyncRunner fpt
                forM_ finMsgs $ \finMsg -> do
                    logm External LLDebug $ "Sending finalization catchup, data = " ++ show finMsg
                    BS.useAsCStringLen (runPut $ put finMsg) $ \(cstr, l) -> callFinalizationMessageCallback callback peer cstr (fromIntegral l)
                return 0

-- |Get the current point in the finalization protocol.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
getFinalizationPoint :: StablePtr BakerRunner -> IO CString
getFinalizationPoint bptr = do 
        BakerRunner{..} <- deRefStablePtr bptr
        let logm = syncLogMethod bakerSyncRunner
        logm External LLInfo $ "Received request for finalization point"
        finPt <- Get.getFinalizationPoint bakerSyncRunner
        logm External LLDebug $ "Replying with finalization point = " ++ show finPt
        byteStringToCString $ P.runPut $ put finPt

foreign export ccall makeGenesisData :: Timestamp -> Word64 -> FunPtr GenesisDataCallback -> FunPtr BakerIdentityCallback -> IO ()
foreign export ccall startBaker :: CString -> Int64 -> CString -> Int64 -> FunPtr BroadcastCallback -> FunPtr LogCallback -> FunPtr MissingByBlockDeltaCallback -> FunPtr MissingByBlockCallback -> FunPtr MissingByFinalizationIndexCallback -> IO (StablePtr BakerRunner)
foreign export ccall stopBaker :: StablePtr BakerRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalization :: StablePtr BakerRunner -> PeerID -> CString -> Int64 -> IO Int64
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
