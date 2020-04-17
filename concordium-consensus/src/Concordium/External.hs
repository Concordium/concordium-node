{-# LANGUAGE
    ForeignFunctionInterface,
    ScopedTypeVariables,
    OverloadedStrings,
    RankNTypes,
    GADTs
    #-}
module Concordium.External where

import Foreign
import Foreign.C

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Serialize
import Data.Serialize.Put as P
import qualified Data.Aeson as AE
import Data.Foldable(forM_)
import Text.Read(readMaybe)
import Control.Exception
import Control.Monad.State.Class(MonadState)
import System.FilePath ((</>))

import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text as AET
import Data.Aeson(Value(Null))

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Data.FixedByteString as FBS

import Concordium.Types
import Concordium.ID.Types
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.BlockMonads
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.Birk.Bake as Baker

import Concordium.Runner
import Concordium.Skov hiding (receiveTransaction, getBirkParameters, getCatchUpStatus, MessageType)
import qualified Concordium.Skov as Skov
import Concordium.Afgjort.Finalize (FinalizationInstance(..))
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.TimerMonad (ThreadTimer)
-- import Concordium.Skov.CatchUp (CatchUpStatus,cusIsResponse)

import qualified Concordium.Getters as Get

-- |A 'PeerID' identifies peer at the p2p layer.
type PeerID = Word64

-- |A 'BlockReference' is a pointer to a block hash as a sequence of 32 bytes.
type BlockReference = Ptr Word8

jsonValueToCString :: Value -> IO CString
jsonValueToCString = newCString . LT.unpack . AET.encodeToLazyText

-- |Use a 'BlockHash' as a 'BlockReference'.  The 'BlockReference' may not
-- be valid after the function has returned.
withBlockReference :: BlockHash -> (BlockReference -> IO a) -> IO a
withBlockReference (Hash.Hash fbs) = FBS.withPtrReadOnly fbs

-- |Use a 'TransactionHash' as a 'Ptr Word8'. The pointer may not be valid after
-- the function has returned.
withTxReference :: TransactionHash -> (Ptr Word8 -> IO a) -> IO a
withTxReference (Hash.Hash fbs) = FBS.withPtrReadOnly fbs

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
callGenesisDataCallback cb bs = BS.useAsCStringLen bs $
        \(cdata, clen) -> invokeGenesisDataCallback cb cdata (fromIntegral clen)

-- |Callback for handling a generated baker identity.
-- The first argument is the identity of the baker (TODO: this should really by a 'Word64').
-- The second argument is the pointer to the data, and the third is the length of the data.
type BakerIdentityCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBakerIdentityCallback :: FunPtr BakerIdentityCallback -> BakerIdentityCallback
callBakerIdentityCallback :: FunPtr BakerIdentityCallback -> Word64 -> BS.ByteString -> IO ()
callBakerIdentityCallback cb bix bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBakerIdentityCallback cb (fromIntegral bix) cdata (fromIntegral clen)

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


type LogTransferCallback = Word8 -- ^Type of transfer (see documentation of 'toLogTransferMethod' below)
                           -> Ptr Word8 -- ^Pointer to block hash (32 bytes)
                           -> Slot -- ^Slot number of the block (unsigned 64 bit int)
                           -> Ptr Word8 -- ^Pointer to transaction hash (32 bytes) or NULL for some types.
                           -> Amount -- ^Amount (unsigned 64 bit integer)
                           -> CSize -- ^Size of the rest of the data
                           -> Ptr Word8 -- ^Type dependent serialized data.
                           -> IO ()

foreign import ccall "dynamic" callLogTransferCallback :: FunPtr LogTransferCallback -> LogTransferCallback

-- |Wrap a log callback as a log method, only logging events with loglevel <= given log level.
toLogMethod :: Word8 -> FunPtr LogCallback -> LogMethod IO
toLogMethod maxLogLevel logCallbackPtr = le
    where
        logCallback = callLogCallback logCallbackPtr
        le src lvl = if logLevelId lvl <= maxLogLevel then -- only log if log level less than maximum requested
                       \msg -> BS.useAsCString (BS.pack msg) $
                               logCallback (logSourceId src) (logLevelId lvl)
                     else \_ -> return ()

unsafeWithBSLen :: BS.ByteString -> (CSize -> Ptr Word8 -> IO ()) -> IO ()
unsafeWithBSLen bs f = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> f (fromIntegral len) (castPtr ptr)

-- |Create a method for logging transfers.
-- The output format is the following.
-- |--------+---------------------------------+--------------------------------------------------------|
-- |   Type | Serialized data                 | Comment                                                |
-- | ====== | ====================            | ===========                                            |
-- |      0 | source account, target account  | Direct transfer                                        |
-- |      1 | source account, target contract | Transfer from account to contract                      |
-- |      2 | source contract, target account | Transfer from contract to account                      |
-- |      3 | source account, target baker id | Execution cost of transaction                          |
-- |      4 | baker id, baker account         | Total block reward, transaction hash is a NUll pointer |
-- |      5 | source contract, target contract| Transfer from contract to contract                     |
-- |      6 | from acc, to acc, RegId         | Credential deployed, amount field is a dummy value     |
-- |--------+---------------------------------+--------------------------------------------------------|

-- * Account address serialiation is 21 bytes in length
-- * Contract address serialization is 16 bytes, consisting of two 64-bit unsigned integers in big-endian format
-- * Baker Id serialization is a 64 bit unsigned integer in big-endian format.

toLogTransferMethod :: FunPtr LogTransferCallback -> BS.LogTransferMethod IO
toLogTransferMethod logtCallBackPtr = logTransfer
    where logit = callLogTransferCallback logtCallBackPtr
          logTransfer bh slot reason =
            withBlockReference bh $ \block ->
              case reason of
                BS.DirectTransfer{..} ->
                  withTxReference trdtId $ \txRef ->
                    let rest = runPut (put trdtSource <> put trdtTarget)
                    in unsafeWithBSLen rest $ logit 0 block slot txRef trdtAmount
                BS.AccountToContractTransfer{..} ->
                  withTxReference tractId $ \txRef ->
                    let rest = runPut (put tractSource <> put tractTarget)
                    in unsafeWithBSLen rest $ logit 1 block slot txRef tractAmount
                BS.ContractToAccountTransfer{..} ->
                  withTxReference trcatId $ \txRef ->
                    let rest = runPut (put trcatSource <> put trcatTarget)
                    in unsafeWithBSLen rest $ logit 2 block slot txRef trcatAmount
                BS.ExecutionCost{..} ->
                  withTxReference trecId $ \txRef ->
                    let rest = runPut (put trecSource <> put trecBaker)
                    in unsafeWithBSLen rest $ logit 3 block slot txRef trecAmount
                BS.BakingRewardTransfer{..} ->
                  let rest = runPut (put trbrBaker <> put trbrAccount)
                  in unsafeWithBSLen rest $ logit 4 block slot nullPtr trbrAmount
                BS.ContractToContractTransfer{..} ->
                  withTxReference trcctId $ \txRef ->
                    let rest = runPut (put trcctSource <> put trcctTarget)
                    in unsafeWithBSLen rest $ logit 5 block slot txRef trcctAmount
                BS.CredentialDeployment{..} ->
                  withTxReference trcdId $ \txRef ->
                    let rest = runPut (put trcdAccount <> put trcdCredentialRegId)
                    in unsafeWithBSLen rest $ logit 6 block slot txRef 0

-- |Callback for broadcasting a message to the network.
-- The first argument indicates the message type.
-- The second argument is a pointer to the data to broadcast.
-- The third argument is the length of the data in bytes.
type BroadcastCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastCallback
data MessageType
    = MTBlock
    | MTFinalization
    | MTFinalizationRecord
    | MTCatchUpStatus
callBroadcastCallback :: FunPtr BroadcastCallback -> MessageType -> BS.ByteString -> IO ()
callBroadcastCallback cbk mt bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBroadcastCallback cbk mti cdata (fromIntegral clen)
    where
        mti = case mt of
            MTBlock -> 0
            MTFinalization -> 1
            MTFinalizationRecord -> 2
            MTCatchUpStatus -> 3

broadcastCallback :: (BlockData (TS.BlockPointerType (SkovT (SkovHandlers ThreadTimer (SkovConfig gs finconf hconf) LogIO)
                                                        (SkovConfig gs finconf hconf)
                                                        (LoggerT IO))))
                     => LogMethod IO -> FunPtr BroadcastCallback -> SimpleOutMessage (SkovConfig gs finconf hconf) -> IO ()
broadcastCallback logM bcbk = handleB
    where
        handleB (SOMsgNewBlock block) = do
            -- we assume that genesis block (the only block that doesn't have signature) will never be sent to the network
            let blockbs = runPut $ putBlock block
            logM External LLDebug $ "Broadcasting block [size=" ++ show (BS.length blockbs) ++ "]"
            callBroadcastCallback bcbk MTBlock blockbs
        handleB (SOMsgFinalization finMsg) = do
            let finbs = encode finMsg
            logM External LLDebug $ "Broadcasting finalization message [size=" ++ show (BS.length finbs) ++ "]: " ++ show finMsg
            callBroadcastCallback bcbk MTFinalization finbs
        handleB (SOMsgFinalizationRecord finRec) = do
            let msgbs = encode finRec
            logM External LLDebug $ "Broadcasting finalization record [size=" ++ show (BS.length msgbs) ++ "]: " ++ show finRec
            callBroadcastCallback bcbk MTFinalizationRecord msgbs

-- |Callback for direct-sending a catch-up status message to all (non-pending) peers.
-- The first argument is a pointer to the data, which must be a catch-up
-- status message. The second argument is the length of the data in bytes.
type CatchUpStatusCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> CatchUpStatusCallback
callCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> BS.ByteString -> IO ()
callCatchUpStatusCallback cbk bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeCatchUpStatusCallback cbk cdata (fromIntegral clen)


genesisState :: GenesisData -> Basic.BlockState
genesisState genData = Basic.initialState
                       (genesisBirkParameters genData)
                       (genesisCryptographicParameters genData)
                       (genesisAccounts genData ++ genesisControlAccounts genData)
                       (genesisIdentityProviders genData)
                       (genesisMintPerSlot genData)

type TreeConfig = DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> TreeConfig
makeGlobalStateConfig rt genData = DTDBConfig rt genData (genesisState genData)

type TreeConfigWithLog = DiskTreeDiskBlockWithLogConfig
makeGlobalStateConfigWithLog :: RuntimeParameters -> GenesisData -> BS.ByteString -> TreeConfigWithLog
makeGlobalStateConfigWithLog rt genData = DTDBWLConfig rt genData (genesisState genData)


type ActiveConfig gs = SkovConfig gs (BufferedFinalization ThreadTimer) HookLogHandler
type PassiveConfig gs = SkovConfig gs (NoFinalization ()) HookLogHandler

-- |A 'ConsensusRunner' encapsulates an instance of the consensus, and possibly a baker thread.
data ConsensusRunner = BakerRunner {
        bakerSyncRunner :: SyncRunner (ActiveConfig TreeConfig)
    }
    | PassiveRunner {
        passiveSyncRunner :: SyncPassiveRunner (PassiveConfig TreeConfig)
    }
    | BakerRunnerWithLog {
        bakerSyncRunnerWithLog :: SyncRunner (ActiveConfig TreeConfigWithLog)
    }
    | PassiveRunnerWithLog {
        passiveSyncRunnerWithLog :: SyncPassiveRunner (PassiveConfig TreeConfigWithLog)
    }

consensusLogMethod :: ConsensusRunner -> LogMethod IO
consensusLogMethod BakerRunner{bakerSyncRunner=SyncRunner{syncLogMethod=logM}} = logM
consensusLogMethod PassiveRunner{passiveSyncRunner=SyncPassiveRunner{syncPLogMethod=logM}} = logM
consensusLogMethod BakerRunnerWithLog{bakerSyncRunnerWithLog=SyncRunner{syncLogMethod=logM}} = logM
consensusLogMethod PassiveRunnerWithLog{passiveSyncRunnerWithLog=SyncPassiveRunner{syncPLogMethod=logM}} = logM

runWithConsensus :: ConsensusRunner -> (forall h f gs.
                                        (TS.TreeStateMonad (SkovT h (SkovConfig gs f HookLogHandler) LogIO),
                                        TS.PendingBlockType
                                         (SkovT h (SkovConfig gs f HookLogHandler) (LoggerT IO)) ~ PendingBlock
                                        ) => SkovT h (SkovConfig gs f HookLogHandler) LogIO a) -> IO a
runWithConsensus BakerRunner{..} = runSkovTransaction bakerSyncRunner
runWithConsensus PassiveRunner{..} = runSkovPassive passiveSyncRunner
runWithConsensus BakerRunnerWithLog{..} = runSkovTransaction bakerSyncRunnerWithLog
runWithConsensus PassiveRunnerWithLog{..} = runSkovPassive passiveSyncRunnerWithLog

-- |Default value for early block threshold.
-- Set to 30 seconds.
defaultEarlyBlockThreshold :: Timestamp
defaultEarlyBlockThreshold = 30

-- |Start up an instance of Skov without starting the baker thread.
-- If an error occurs starting Skov, the error will be logged and
-- a null pointer will be returned.
startConsensus ::
           Word64 -- ^Maximum block size.
           -> Word64 -- ^Insertions before purging of transactions
           -> Word64 -- ^Time in seconds during which a transaction can't be purged
           -> CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> CString -> Int64 -- ^Serialized baker identity (c string + len)
           -> FunPtr BroadcastCallback -- ^Handler for generated messages
           -> FunPtr CatchUpStatusCallback -- ^Handler for sending catch-up status to peers
           -> Word8 -- ^Maximum log level (inclusive) (0 to disable logging).
           -> FunPtr LogCallback -- ^Handler for log events
           -> CString -> Int64 -- ^FilePath for the AppData directory
           -> CString -> Int64 -- ^Database connection string. If length is 0 don't do logging.
           -> IO (StablePtr ConsensusRunner)
startConsensus maxBlock insertionsBeforePurge transactionsKeepAlive gdataC gdataLenC bidC bidLenC bcbk cucbk maxLogLevel lcbk appDataC appDataLenC connStringPtr connStringLen = do
        gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        bdata <- BS.packCStringLen (bidC, fromIntegral bidLenC)
        appData <- peekCStringLen (appDataC, fromIntegral appDataLenC)
        case (decode gdata, AE.eitherDecodeStrict bdata) of
            (Right genData, Right bid) -> do
              if connStringLen /= 0 then do -- enable logging of transactions
                connString <- BS.packCStringLen (connStringPtr, fromIntegral connStringLen)
                let
                    gsconfig = makeGlobalStateConfigWithLog
                        (RuntimeParameters (fromIntegral maxBlock) (appData </> "treestate") (appData </> "blockstate") defaultEarlyBlockThreshold (fromIntegral insertionsBeforePurge) transactionsKeepAlive)
                        genData
                        connString
                    finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)) genData
                    hconfig = HookLogHandler Nothing -- logT
                    config = SkovConfig gsconfig finconfig hconfig
                    bakerBroadcast = broadcastCallback logM bcbk
                bakerSyncRunnerWithLog <- makeSyncRunner logM bid config bakerBroadcast catchUpCallback
                newStablePtr BakerRunnerWithLog{..}
              else do
                let
                    gsconfig = makeGlobalStateConfig
                        (RuntimeParameters (fromIntegral maxBlock) (appData </> "treestate") (appData </> "blockstate") defaultEarlyBlockThreshold (fromIntegral insertionsBeforePurge) transactionsKeepAlive)
                        genData
                    finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)) genData
                    hconfig = HookLogHandler Nothing -- logT
                    config = SkovConfig gsconfig finconfig hconfig
                    bakerBroadcast = broadcastCallback logM bcbk
                bakerSyncRunner <- makeSyncRunner logM bid config bakerBroadcast catchUpCallback
                newStablePtr BakerRunner{..}
            (Left err, _) -> do
                logM External LLError $ "Failed to decode genesis data: " ++ err
                return $ castPtrToStablePtr nullPtr
            (_, Left err) -> do
                logM External LLError $ "Failed to decode baker identity data: " ++ err
                return $ castPtrToStablePtr nullPtr
    where
        logM = toLogMethod maxLogLevel lcbk
        -- logT = if enableTransferLogging /= 0 then Just (toLogTransferMethod ltcbk) else Nothing
        catchUpCallback = callCatchUpStatusCallback cucbk . encode


-- |Start consensus without a baker identity.
-- If an error occurs starting Skov, the error will be logged and
-- a null pointer will be returned.
startConsensusPassive ::
           Word64 -- ^Maximum block size.
           -> Word64 -- ^Insertions before purging of transactions
           -> Word64 -- ^Time in seconds during which a transaction can't be purged
           -> CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> FunPtr CatchUpStatusCallback -- ^Handler for sending catch-up status to peers
           -> Word8 -- ^Maximum log level (inclusive) (0 to disable logging).
           -> FunPtr LogCallback -- ^Handler for log events
           -> CString -> Int64 -- ^FilePath for the AppData directory
           -> CString -> Int64 -- ^Connection string to access the database. If length is 0 don't do logging.
            -> IO (StablePtr ConsensusRunner)
startConsensusPassive maxBlock insertionsBeforePurge transactionsKeepAlive gdataC gdataLenC cucbk maxLogLevel lcbk appDataC appDataLenC connStringPtr connStringLen = do
        gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        appData <- peekCStringLen (appDataC, fromIntegral appDataLenC)
        case decode gdata of
            Right genData -> do
              if connStringLen /= 0 then do
                connString <- BS.packCStringLen (connStringPtr, fromIntegral connStringLen)
                let
                    gsconfig = makeGlobalStateConfigWithLog
                        (RuntimeParameters (fromIntegral maxBlock) (appData </> "treestate") (appData </> "blockstate") defaultEarlyBlockThreshold (fromIntegral insertionsBeforePurge) transactionsKeepAlive)
                        genData
                        connString
                    finconfig = NoFinalization genData
                    hconfig = HookLogHandler Nothing
                    config = SkovConfig gsconfig finconfig hconfig
                passiveSyncRunnerWithLog <- makeSyncPassiveRunner logM config catchUpCallback
                newStablePtr PassiveRunnerWithLog{..}
              else do
                let
                    gsconfig = makeGlobalStateConfig
                        (RuntimeParameters (fromIntegral maxBlock) (appData </> "treestate") (appData </> "blockstate") defaultEarlyBlockThreshold (fromIntegral insertionsBeforePurge) transactionsKeepAlive)
                        genData
                    finconfig = NoFinalization genData
                    hconfig = HookLogHandler Nothing
                    config = SkovConfig gsconfig finconfig hconfig
                passiveSyncRunner <- makeSyncPassiveRunner logM config catchUpCallback
                newStablePtr PassiveRunner{..}
            Left err -> do
                logM External LLError $ "Failed to decode genesis data: " ++ err
                return $ castPtrToStablePtr nullPtr
    where
        logM = toLogMethod maxLogLevel lcbk
        catchUpCallback = callCatchUpStatusCallback cucbk . encode

-- |Shuts down consensus, stopping any baker thread if necessary.
-- The pointer is not valid after this function returns.
stopConsensus :: StablePtr ConsensusRunner -> IO ()
stopConsensus cptr = mask_ $ do
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> shutdownSyncRunner bakerSyncRunner
        PassiveRunner{..} -> shutdownSyncPassiveRunner passiveSyncRunner
        BakerRunnerWithLog{..} -> shutdownSyncRunner bakerSyncRunnerWithLog
        PassiveRunnerWithLog{..} -> shutdownSyncPassiveRunner passiveSyncRunnerWithLog
    freeStablePtr cptr

-- |Start the baker thread.  Calling this more than once
-- should not start additional baker threads.
startBaker :: StablePtr ConsensusRunner -> IO ()
startBaker cptr = mask_ $
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> startSyncRunner bakerSyncRunner
        BakerRunnerWithLog{..} -> startSyncRunner bakerSyncRunnerWithLog
        c -> consensusLogMethod c External LLError "Attempted to start baker thread, but consensus was started without baker credentials"

-- |Stop a baker thread.
stopBaker :: StablePtr ConsensusRunner -> IO ()
stopBaker cptr = mask_ $
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> stopSyncRunner bakerSyncRunner
        BakerRunnerWithLog{..} -> stopSyncRunner bakerSyncRunnerWithLog
        c -> consensusLogMethod c External LLError "Attempted to stop baker thread, but consensus was started without baker credentials"

{- | Result values for receive functions.

+=======+====================================+========================================================================================+==========+
| Value |                Name                |                                      Description                                       | Forward? |
+=======+====================================+========================================================================================+==========+
|     0 | ResultSuccess                      | Message received, validated and processed                                              | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     1 | ResultSerializationFail            | Message deserialization failed                                                         | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     2 | ResultInvalid                      | The message was determined to be invalid                                               | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     3 | ResultPendingBlock                 | The message was received, but is awaiting a block to complete processing               | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     4 | ResultPendingFinalization          | The message was received, but is awaiting a finalization record to complete processing | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     5 | ResultAsync                        | The message was received, but is being processed asynchronously                        | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     6 | ResultDuplicate                    | The message duplicates a previously received message                                   | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     7 | ResultStale                        | The message may have been valid in the past, but is no longer relevant                 | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     8 | ResultIncorrectFinalizationSession | The message refers to a different/unknown finalization session                         | No(?)    |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     9 | ResultUnverifiable                 | The message could not be verified in the current state (initiate catch-up with peer)   | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|    10 | ResultContinueCatchUp              | The peer should be marked pending catch-up if it is currently up-to-date               | N/A      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|    11 | ResultEarlyBlock                   | The block has a slot number exceeding our current + the early block threshold          | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-}
type ReceiveResult = Int64

toReceiveResult :: UpdateResult -> ReceiveResult
toReceiveResult ResultSuccess = 0
toReceiveResult ResultSerializationFail = 1
toReceiveResult ResultInvalid = 2
toReceiveResult ResultPendingBlock = 3
toReceiveResult ResultPendingFinalization = 4
toReceiveResult ResultAsync = 5
toReceiveResult ResultDuplicate = 6
toReceiveResult ResultStale = 7
toReceiveResult ResultIncorrectFinalizationSession = 8
toReceiveResult ResultUnverifiable = 9
toReceiveResult ResultContinueCatchUp = 10
toReceiveResult ResultEarlyBlock = 11


-- |Handle receipt of a block.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultAsync@, @ResultDuplicate@,
-- and @ResultStale@.
-- 'receiveBlock' may invoke the callbacks for new finalization messages.
receiveBlock :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveBlock bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received block data size = " ++ show l ++ ". Decoding ..."
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    now <- currentTime
    toReceiveResult <$>
        case (deserializePendingBlock blockBS now) of
            Left err -> do
                logm External LLDebug err
                return ResultSerializationFail
            Right block -> case c of
                            BakerRunner{..} -> syncReceiveBlock bakerSyncRunner block
                            PassiveRunner{..} -> syncPassiveReceiveBlock passiveSyncRunner block
                            BakerRunnerWithLog{..} -> syncReceiveBlock bakerSyncRunnerWithLog block
                            PassiveRunnerWithLog{..} -> syncPassiveReceiveBlock passiveSyncRunnerWithLog block


-- |Handle receipt of a finalization message.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@, @ResultIncorrectFinalizationSession@ and
-- @ResultUnverifiable@.
-- 'receiveFinalization' may invoke the callbacks for new finalization messages.
receiveFinalization :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveFinalization bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received finalization message size = " ++ show l ++ ".  Decoding ..."
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    toReceiveResult <$> case runGet get bs of
        Left _ -> do
            logm External LLDebug "Deserialization of finalization message failed."
            return ResultSerializationFail
        Right finMsg -> do
            logm External LLDebug "Finalization message deserialized."
            case c of
                BakerRunner{..} -> syncReceiveFinalizationMessage bakerSyncRunner finMsg
                PassiveRunner{..} -> syncPassiveReceiveFinalizationMessage passiveSyncRunner finMsg
                BakerRunnerWithLog{..} -> syncReceiveFinalizationMessage bakerSyncRunnerWithLog finMsg
                PassiveRunnerWithLog{..} -> syncPassiveReceiveFinalizationMessage passiveSyncRunnerWithLog finMsg

-- |Handle receipt of a finalization record.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultDuplicate@, and @ResultStale@.
-- (Currently, @ResultDuplicate@ cannot happen, although it may be supported in future.)
-- 'receiveFinalizationRecord' may invoke the callbacks for new finalization messages.
receiveFinalizationRecord :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveFinalizationRecord bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received finalization record data size = " ++ show l ++ ". Decoding ..."
    finRecBS <- BS.packCStringLen (cstr, fromIntegral l)
    toReceiveResult <$> case runGet get finRecBS of
        Left _ -> do
          logm External LLDebug "Deserialization of finalization record failed."
          return ResultSerializationFail
        Right finRec -> do
            logm External LLDebug "Finalization record deserialized."
            case c of
                BakerRunner{..} -> syncReceiveFinalizationRecord bakerSyncRunner finRec
                PassiveRunner{..} -> syncPassiveReceiveFinalizationRecord passiveSyncRunner finRec
                BakerRunnerWithLog{..} -> syncReceiveFinalizationRecord bakerSyncRunnerWithLog finRec
                PassiveRunnerWithLog{..} -> syncPassiveReceiveFinalizationRecord passiveSyncRunnerWithLog finRec

-- |Handle receipt of a transaction.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultDuplicate@, @ResultStale@, @ResultInvalid@.
receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveTransaction bptr tdata len = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received transaction, data size=" ++ show len ++ ". Decoding ..."
    tbs <- BS.packCStringLen (tdata, fromIntegral len)
    now <- getTransactionTime
    toReceiveResult <$> case runGet (getBlockItem now) tbs of
        Left _ -> do
            logm External LLDebug $ "Could not decode transaction."
            return ResultSerializationFail
        Right tr -> do
            logm External LLDebug $ "Transaction decoded. Sending to consensus."
            case wmdData tr of
              NormalTransaction _ -> logm External LLTrace $ "Received normal transaction."
              CredentialDeployment _ -> logm External LLTrace $ "Received credential."
            case c of
                BakerRunner{..} -> syncReceiveTransaction bakerSyncRunner tr
                PassiveRunner{..} -> syncPassiveReceiveTransaction passiveSyncRunner tr
                BakerRunnerWithLog{..} -> syncReceiveTransaction bakerSyncRunnerWithLog tr
                PassiveRunnerWithLog{..} -> syncPassiveReceiveTransaction passiveSyncRunnerWithLog tr

runConsensusQuery :: ConsensusRunner -> (forall z m s. (Get.SkovStateQueryable z m, BlockPointerMonad m, TS.TreeStateMonad m, MonadState s m, LoggerMonad m) => z -> a) -> a
runConsensusQuery BakerRunner{..} f = f bakerSyncRunner
runConsensusQuery PassiveRunner{..} f = f passiveSyncRunner
runConsensusQuery BakerRunnerWithLog{..} f = f bakerSyncRunnerWithLog
runConsensusQuery PassiveRunnerWithLog{..} f = f passiveSyncRunnerWithLog


-- |Returns a null-terminated string with a JSON representation of the current status of Consensus.
getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
getConsensusStatus cptr = do
    c <- deRefStablePtr cptr
    status <- runConsensusQuery c Get.getConsensusStatus
    jsonValueToCString status

-- |Given a null-terminated string that represents a block hash (base 16), returns a null-terminated
-- string containing a JSON representation of the block.
getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockInfo cptr blockcstr = do
    c <- deRefStablePtr cptr
    block <- peekCString blockcstr
    blockInfo <- runConsensusQuery c Get.getBlockInfo block
    jsonValueToCString blockInfo

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
getAncestors cptr blockcstr depth = do
    c <- deRefStablePtr cptr
    block <- peekCString blockcstr
    ancestors <- runConsensusQuery c Get.getAncestors block (fromIntegral depth :: BlockHeight)
    jsonValueToCString ancestors

-- |Returns a null-terminated string with a JSON representation of the current branches from the
-- last finalized block (inclusive).
getBranches :: StablePtr ConsensusRunner -> IO CString
getBranches cptr = do
    c <- deRefStablePtr cptr
    rbranches <- runConsensusQuery c Get.getBranches
    jsonValueToCString rbranches



byteStringToCString :: BS.ByteString -> IO CString
byteStringToCString bs = do
  let bsp = BS.concat [P.runPut (P.putWord32be (fromIntegral (BS.length bs))), bs]
  -- This use of unsafe is fine because bsp is a non-null string.
  BS.unsafeUseAsCStringLen bsp $ \(cstr, len) -> do dest <- mallocBytes len
                                                    copyBytes dest cstr len
                                                    return dest

withBlockHash :: CString -> (String -> IO ()) -> (BlockHash -> IO CString) -> IO CString
withBlockHash blockcstr logm f =
  readMaybe <$> peekCString blockcstr >>=
    \case Nothing -> do
            logm "Block hash invalid. Returning error value."
            newCString "\"Invalid block hash.\""
          Just hash -> f hash

withTransactionHash :: CString -> (String -> IO ()) -> (TransactionHash -> IO CString) -> IO CString
withTransactionHash trcstr logm f =
  readMaybe <$> peekCString trcstr >>=
    \case Nothing -> do
            logm "Transaction hash invalid. Returning error value."
            newCString "\"Invalid transaction hash.\""
          Just hash -> f hash

-- |Get the list of account addresses in the given block. The block must be
-- given as a null-terminated base16 encoding of the block hash. The return
-- value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
getAccountList :: StablePtr ConsensusRunner -> CString -> IO CString
getAccountList cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received account list request."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      alist <- runConsensusQuery c (Get.getAccountList hash)
      logm External LLTrace $ "Replying with the list: " ++ show alist
      jsonValueToCString alist

-- |Get the list of contract instances (their addresses) in the given block. The
-- block must be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
getInstances :: StablePtr ConsensusRunner -> CString -> IO CString
getInstances cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received instance list request."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      istances <- runConsensusQuery c (Get.getInstances hash)
      logm External LLTrace $ "Replying with the list: " ++ show istances
      jsonValueToCString istances

withAccountAddress :: CString -> (String -> IO ()) -> (AccountAddress -> IO CString) -> IO CString
withAccountAddress cstr logm k = do
  bs <- BS.packCString cstr
  case addressFromBytes bs of
      Left err -> do
        logm $ "Could not decode address: " ++ err
        jsonValueToCString Null
      Right acc -> k acc

-- |Get account information for the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the account
-- address (second CString) must be given as a null-terminated string in Base58
-- encoding (same format as returned by 'getAccountList'). The return value is a
-- null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getAccountInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getAccountInfo cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received account info request."
    withAccountAddress cstr (logm External LLDebug) $ \acc -> do
        logm External LLDebug $ "Decoded address to: " ++ show acc
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          ainfo <- runConsensusQuery c (Get.getAccountInfo hash) acc
          logm External LLTrace $ "Replying with: " ++ show ainfo
          jsonValueToCString ainfo

-- |Get the status of the rewards parameters for the given block. The block must
-- be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated, JSON encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getRewardStatus :: StablePtr ConsensusRunner -> CString -> IO CString
getRewardStatus cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for bank status."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      reward <- runConsensusQuery c (Get.getRewardStatus hash)
      logm External LLTrace $ "Replying with: " ++ show reward
      jsonValueToCString reward


-- |Get the list of modules in the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list.
-- The returned string should be freed by calling 'freeCStr'.
getModuleList :: StablePtr ConsensusRunner -> CString -> IO CString
getModuleList cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for list of modules."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      mods <- runConsensusQuery c (Get.getModuleList hash)
      logm External LLTrace $ "Replying with" ++ show mods
      jsonValueToCString mods

-- |Get birk parameters for the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getBirkParameters :: StablePtr ConsensusRunner -> CString -> IO CString
getBirkParameters cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request Birk parameters."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      bps <- runConsensusQuery c (Get.getBlockBirkParameters hash)
      logm External LLTrace $ "Replying with" ++ show bps
      jsonValueToCString bps


-- |Check whether we are a baker from the perspective of the best block.
-- Returns 0 if we are not added as a baker.
-- Returns 1 if we are added as a baker, but not part of the baking committee yet.
-- Returns 2 if we are part of the baking committee.
checkIfWeAreBaker :: StablePtr ConsensusRunner -> IO Word8
checkIfWeAreBaker cptr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLTrace "Checking whether we are a baker."
    case c of
      PassiveRunner _ -> do
        logm External LLTrace "Passive consensus, not a baker."
        return 0
      PassiveRunnerWithLog _ -> do
        logm External LLTrace "Passive consensus, not a baker."
        return 0
      BakerRunner s -> do
        logm External LLTrace "Active consensus, querying best block."
        let bid = syncBakerIdentity s
        let signKey = Baker.bakerSignPublicKey bid
        r <- runConsensusQuery c (Get.checkBakerExistsBestBlock signKey)
        logm External LLTrace $ "Replying with " ++ show r
        return r
      BakerRunnerWithLog s -> do
        logm External LLTrace "Active consensus, querying best block."
        let bid = syncBakerIdentity s
        let signKey = Baker.bakerSignPublicKey bid
        r <- runConsensusQuery c (Get.checkBakerExistsBestBlock signKey)
        logm External LLTrace $ "Replying with " ++ show r
        return r

-- |Check if we are members of the finalization committee.
-- Returns 0 for 'False' and 1 for 'True'.
checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8
checkIfWeAreFinalizer cptr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLTrace "Checking whether we are a finalizer."
    case c of
      PassiveRunner _ -> do
        logm External LLTrace "Passive consensus, not a finalizer."
        return 0
      BakerRunner s -> do
        logm External LLTrace "Active consensus, querying best block."
        r <- Get.checkIsCurrentFinalizer s
        logm External LLTrace $ "Replying with " ++ show r
        if r then return 1 else return 0
      PassiveRunnerWithLog _ -> do
        logm External LLTrace "Passive consensus, not a finalizer."
        return 0
      BakerRunnerWithLog s -> do
        logm External LLTrace "Active consensus, querying best block."
        r <- Get.checkIsCurrentFinalizer s
        logm External LLTrace $ "Replying with " ++ show r
        if r then return 1 else return 0


-- |Get instance information the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the address
-- (second CString) must be given as a null-terminated JSON-encoded value.
-- The return value is a null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getInstanceInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getInstanceInfo cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received account info request."
    bs <- BS.packCString cstr
    case AE.decodeStrict bs :: Maybe ContractAddress of
      Nothing -> do
        logm External LLDebug "Could not decode address."
        jsonValueToCString Null
      Just ii -> do
        logm External LLDebug $ "Decoded address to: " ++ show ii
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          iinfo <- runConsensusQuery c (Get.getContractInfo hash) ii
          logm External LLTrace $ "Replying with: " ++ show iinfo
          jsonValueToCString iinfo


-- |NB: The return value is __NOT__ JSON encoded but rather it is a binary
-- serialization. The first 4 bytes are the length of the rest of the string, and
-- the string is __NOT__ null terminated and can contain null characters.
-- The returned string should be freed by calling 'freeCStr'.
getModuleSource :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getModuleSource cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for a module."
    bs <- peekCString cstr -- null terminated
    case readMaybe bs of
      Nothing -> do
        logm External LLTrace "Cannot decode module reference."
        byteStringToCString BS.empty
      Just mrefhash -> do
        let mref = ModuleRef mrefhash
        logm External LLDebug $ "Decoded module hash to : " ++ show mref -- base 16
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          mmodul <- runConsensusQuery c (Get.getModuleSource hash) mref
          case mmodul :: Maybe (Core.Module Core.UA) of
            Nothing -> do
              logm External LLTrace "Module not available."
              byteStringToCString BS.empty
            Just modul ->
              let reply = P.runPut (Core.putModule modul)
              in do
                logm External LLTrace $ "Replying with data size = " ++ show (BS.length reply)
                byteStringToCString reply

-- |Get the status of a transaction. The input is a base16-encoded null-terminated string
-- denoting a transaction hash. The return value is a NUL-terminated JSON string encoding a
-- JSON value.
getTransactionStatus :: StablePtr ConsensusRunner -> CString -> IO CString
getTransactionStatus cptr trcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received transaction status request."
    withTransactionHash trcstr (logm External LLDebug) $ \hash -> do
      status <- runConsensusQuery c (Get.getTransactionStatus hash)
      logm External LLTrace $ "Replying with: " ++ show status
      jsonValueToCString status

-- |Get the status of a transaction. The first input is a base16-encoded null-terminated string
-- denoting a transaction hash, the second input is the hash of the block.
-- The return value is a NUL-terminated string encoding a JSON value.
-- The arguments are
--
--   * pointer to the consensus runner
--   * NUL-terminated C string with a base16 encoded transaction hash
--   * NUL-terminated C string with base16 encoded block hash
getTransactionStatusInBlock :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getTransactionStatusInBlock cptr trcstr bhcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received transaction status request."
    withTransactionHash trcstr (logm External LLDebug) $ \txHash ->
      withBlockHash bhcstr (logm External LLDebug) $ \blockHash -> do
        status <- runConsensusQuery c (Get.getTransactionStatusInBlock txHash blockHash)
        logm External LLTrace $ "Replying with: " ++ show status
        jsonValueToCString status


-- |Get the list of non-finalized transactions for a given account.
-- The arguments are
--
--   * pointer to the consensus runner
--   * NUL-terminated C string with account address.
getAccountNonFinalizedTransactions :: StablePtr ConsensusRunner -> CString -> IO CString
getAccountNonFinalizedTransactions cptr addrcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received account non-finalized transactions request."
    withAccountAddress addrcstr (logm External LLDebug) $ \addr -> do
        status <- runConsensusQuery c (Get.getAccountNonFinalizedTransactions addr)
        logm External LLTrace $ "Replying with: " ++ show status
        jsonValueToCString (AE.toJSON status)

-- |Get the best guess for the next available account nonce.
-- The arguments are
--
--   * pointer to the consensus runner
--   * NUL-terminated C string with account address.
getNextAccountNonce :: StablePtr ConsensusRunner -> CString -> IO CString
getNextAccountNonce cptr addrcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received next account nonce request."
    withAccountAddress addrcstr (logm External LLDebug) $ \addr -> do
        status <- runConsensusQuery c (Get.getNextAccountNonce addr)
        logm External LLTrace $ "Replying with: " ++ show status
        jsonValueToCString status

-- |Get the list of transactions in a block with short summaries of their effects.
-- Returns a NUL-termianated string encoding a JSON value.
getBlockSummary :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockSummary cptr bhcstr = do
  c <- deRefStablePtr cptr
  let logm = consensusLogMethod c
  logm External LLDebug "Received block summary request."
  withBlockHash bhcstr (logm External LLDebug) $ \blockHash -> do
    summary <- runConsensusQuery c (Get.getBlockSummary blockHash)
    logm External LLTrace $ "Replying with: " ++ show summary
    jsonValueToCString summary

freeCStr :: CString -> IO ()
freeCStr = free

-- |Get a catch-up status message for requesting catch-up with peers.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
getCatchUpStatus :: StablePtr ConsensusRunner -> IO CString
getCatchUpStatus cptr = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        logm External LLDebug $ "Received request for catch-up status"
        cus <- runWithConsensus c (Skov.getCatchUpStatus True)
        logm External LLTrace $ "Replying with catch-up status = " ++ show cus
        byteStringToCString $ encode (cus :: CatchUpStatus)

-- |Callback for sending a message to a peer.
-- The first argument is the peer to send to.
-- The second argument indicates the message type.
-- The third argument is a pointer to the data to broadcast.
-- The fourth argument is the length of the data in bytes.
type DirectMessageCallback = PeerID -> Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeDirectMessageCallback :: FunPtr DirectMessageCallback -> DirectMessageCallback
callDirectMessageCallback :: FunPtr DirectMessageCallback -> PeerID -> MessageType -> BS.ByteString -> IO ()
callDirectMessageCallback cbk peer mt bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeDirectMessageCallback cbk peer mti cdata (fromIntegral clen)
    where
        mti = case mt of
            MTBlock -> 0
            MTFinalization -> 1
            MTFinalizationRecord -> 2
            MTCatchUpStatus -> 3

-- |Handle receiving a catch-up status message.
-- If the message is a request, then the supplied callback will be used to
-- send the requested data for the peer.
-- The response code can be:
-- * @ResultSerializationFail@
-- * @ResultInvalid@ -- the catch-up message is inconsistent with the skov
-- * @ResultPendingBlock@ -- the sender has some data I am missing, and should be marked pending
-- * @ResultSuccess@ -- I do not require additional data from the sender, so mark it as up-to-date
-- * @ResultContinueCatchUp@ -- The sender should be marked pending if it is currently up-to-date (no change otherwise)
receiveCatchUpStatus ::
    StablePtr ConsensusRunner           -- ^Consensus pointer
    -> PeerID                           -- ^Identifier of peer (passed to callback)
    -> CString                          -- ^Serialised catch-up message
    -> Int64                            -- ^Length of message
    -> Word64                           -- ^Limit to number of responses (0 = unlimited)
    -> FunPtr DirectMessageCallback     -- ^Callback to receive messages
    -> IO ReceiveResult
receiveCatchUpStatus cptr src cstr len limit cbk = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    bs <- BS.packCStringLen (cstr, fromIntegral len)
    toReceiveResult <$> case decode bs :: Either String CatchUpStatus of
        Left _ -> do
            logm External LLDebug "Deserialization of catch-up status message failed."
            return ResultSerializationFail
        Right cus -> do
            logm External LLDebug $ "Catch-up status message deserialized: " ++ show cus
            let
                toMsg (MessageBlock, mbs) = (MTBlock, mbs)
                toMsg (MessageFinalizationRecord, mbs) = (MTFinalizationRecord, mbs)
            (response, result) <- case c of
                BakerRunner{..} -> syncReceiveCatchUp bakerSyncRunner cus
                PassiveRunner{..} -> syncPassiveReceiveCatchUp passiveSyncRunner cus
                BakerRunnerWithLog{..} -> syncReceiveCatchUp bakerSyncRunnerWithLog cus
                PassiveRunnerWithLog{..} -> syncPassiveReceiveCatchUp passiveSyncRunnerWithLog cus
            forM_ response $ \(frbs, rcus) -> do
                        let
                            limFrbs = if limit == 0 then frbs else take (fromIntegral limit) frbs
                            sendMsg = callDirectMessageCallback cbk src
                        logm Skov LLTrace $ "Sending " ++ show (length limFrbs) ++ " blocks/finalization records"
                        mapM_ (uncurry sendMsg . toMsg) limFrbs
                        logm Skov LLDebug $ "Catch-up response status message: " ++ show rcus
                        sendMsg MTCatchUpStatus $ encode rcus
            return $! result

foreign export ccall startConsensus :: Word64 -> Word64 -> Word64 -> CString -> Int64 -> CString -> Int64 -> FunPtr BroadcastCallback -> FunPtr CatchUpStatusCallback -> Word8 -> FunPtr LogCallback -> CString -> Int64 -> CString -> Int64 -> IO (StablePtr ConsensusRunner)
foreign export ccall startConsensusPassive :: Word64 -> Word64 -> Word64 -> CString -> Int64 -> FunPtr CatchUpStatusCallback -> Word8 -> FunPtr LogCallback -> CString -> Int64 ->CString -> Int64 -> IO (StablePtr ConsensusRunner)
foreign export ccall stopConsensus :: StablePtr ConsensusRunner -> IO ()
foreign export ccall startBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall stopBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalization :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalizationRecord :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64

foreign export ccall getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
foreign export ccall getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
foreign export ccall getBranches :: StablePtr ConsensusRunner -> IO CString

foreign export ccall getCatchUpStatus :: StablePtr ConsensusRunner -> IO CString
foreign export ccall receiveCatchUpStatus :: StablePtr ConsensusRunner -> PeerID -> CString -> Int64 -> Word64 -> FunPtr DirectMessageCallback -> IO ReceiveResult

-- report global state information will be removed in the future when global
-- state is handled better
foreign export ccall getAccountList :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getInstances :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAccountInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getInstanceInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getRewardStatus :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getBirkParameters :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getModuleList :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getModuleSource :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getTransactionStatus :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getTransactionStatusInBlock :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getAccountNonFinalizedTransactions :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getBlockSummary :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getNextAccountNonce :: StablePtr ConsensusRunner -> CString -> IO CString

-- baker status checking
foreign export ccall checkIfWeAreBaker :: StablePtr ConsensusRunner -> IO Word8
foreign export ccall checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8

-- maintenance
foreign export ccall freeCStr :: CString -> IO ()
