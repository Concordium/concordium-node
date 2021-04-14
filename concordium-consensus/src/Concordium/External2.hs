{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.External2 where

import Control.Exception
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import qualified Data.Serialize as S
import Data.Word
import Foreign
import Foreign.C

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Logger
import Concordium.Types
import qualified Data.FixedByteString as FBS

import Concordium.Afgjort.Finalize.Types (FinalizationInstance (FinalizationInstance))
import Concordium.Birk.Bake
import Concordium.Constants (defaultEarlyBlockThreshold, defaultMaxBakingDelay)
import Concordium.GlobalState
import Concordium.GlobalState.Persistent.TreeState (InitException (..))
import Concordium.ID.Types
import Concordium.MultiVersion (
    Callbacks (..),
    CatchUpConfiguration (..),
    DiskStateConfig (..),
    MVR (..),
    MultiVersionConfiguration (..),
    MultiVersionRunner (..),
    TransactionDBConfig (..),
    makeMultiVersionRunner,
 )
import qualified Concordium.MultiVersion as MV
import qualified Concordium.Queries.MultiVersion as Q
import Concordium.Queries.Types
import Concordium.Scheduler.Types
import Concordium.Skov (
    BufferedFinalization (..),
    MessageType (..),
    NoFinalization (..),
    UpdateResult (..),
 )
import Concordium.TimerMonad (ThreadTimer)
import Control.Monad
import Text.Read (readMaybe)

-- |A 'PeerID' identifies peer at the p2p layer.
type PeerID = Word64

-- * Callbacks

-- ** Logging

-- | External function that logs in Rust a message using standard Rust log output
--
-- The first argument represents the Identifier which shows in which module the message has been
-- emitted.
-- The current mapping is as follows:
--
-- +----------+-----------+
-- |Identifier|Module     |
-- +==========+===========+
-- |0         |Runner     |
-- +----------+-----------+
-- |1         |Afgjort    |
-- +----------+-----------+
-- |2         |Birk       |
-- +----------+-----------+
-- |3         |Crypto     |
-- +----------+-----------+
-- |4         |Kontrol    |
-- +----------+-----------+
-- |5         |Skov       |
-- +----------+-----------+
-- |6         |Baker      |
-- +----------+-----------+
-- |7         |External   |
-- +----------+-----------+
-- |8         |GlobalState|
-- +----------+-----------+
-- |9         |BlockState |
-- +----------+-----------+
-- |10        |TreeState  |
-- +----------+-----------+
-- |11        |LMDB       |
-- +----------+-----------+
-- |12        |Scheduler  |
-- +----------+-----------+
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
-- The third argument is the log message that is emitted.
type LogCallback = Word8 -> Word8 -> CString -> IO ()

-- |FFI wrapper for calling a 'LogCallback' function.
foreign import ccall "dynamic" callLogCallback :: FunPtr LogCallback -> LogCallback

-- |Wrap a log callback as a log method, only logging events with loglevel <= given log level.
toLogMethod :: Word8 -> FunPtr LogCallback -> LogMethod IO
toLogMethod maxLogLevel logCallbackPtr = le
  where
    logCallback = callLogCallback logCallbackPtr
    le src lvl =
        if logLevelId lvl <= maxLogLevel -- only log if log level less than maximum requested
            then \msg ->
                BS.useAsCString (BS.pack msg) $
                    logCallback (logSourceId src) (logLevelId lvl)
            else \_ -> return ()

-- ** Broadcast

-- |Callback for broadcasting a message to the network.
-- The first argument indicates the message type.
-- The second argument is the genesis index.
-- The third argument is a pointer to the data to broadcast.
-- The fourth argument is the length of the data in bytes.
type BroadcastCallback = Int64 -> Word32 -> CString -> Int64 -> IO ()

-- |FFI wrapper for invoking a 'BroadcastCallback' function.
foreign import ccall "dynamic" invokeBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastCallback

-- |Helper for invoking a 'BroadcastCallback' function.
callBroadcastCallback :: FunPtr BroadcastCallback -> MessageType -> GenesisIndex -> BS.ByteString -> IO ()
callBroadcastCallback cbk mt gi bs = BS.useAsCStringLen bs $ \(cdata, clen) ->
    invokeBroadcastCallback cbk mti gi cdata (fromIntegral clen)
  where
    mti = case mt of
        MessageBlock -> 0
        MessageFinalization -> 1
        MessageFinalizationRecord -> 2
        MessageCatchUpStatus -> 3

-- ** Direct-to-peer message

-- |Callback for sending a message to a peer.
-- The first argument is the peer to send to.
-- The second argument indicates the message type.
-- The third argument is a pointer to the data to broadcast.
-- The fourth argument is the length of the data in bytes.
type DirectMessageCallback = PeerID -> Int64 -> Word32 -> CString -> Int64 -> IO ()

-- |FFI wrapper for invoking a 'DirectMessageCallback' function.
foreign import ccall "dynamic" invokeDirectMessageCallback :: FunPtr DirectMessageCallback -> DirectMessageCallback

-- |Helper for invoking a 'DirectMessageCallback' function.
callDirectMessageCallback :: FunPtr DirectMessageCallback -> PeerID -> MessageType -> GenesisIndex -> BS.ByteString -> IO ()
callDirectMessageCallback cbk peer mt genIndex bs = BS.useAsCStringLen bs $ \(cdata, clen) ->
    invokeDirectMessageCallback cbk peer mti genIndex cdata (fromIntegral clen)
  where
    mti = case mt of
        MessageBlock -> 0
        MessageFinalization -> 1
        MessageFinalizationRecord -> 2
        MessageCatchUpStatus -> 3

-- ** Catch-up status

-- |Callback for direct-sending a catch-up status message to all (non-pending) peers.
-- The first argument is the genesis index.
-- The first argument is a pointer to the data, which must be a catch-up
-- status message. The second argument is the length of the data in bytes.
type CatchUpStatusCallback = Word32 -> CString -> Int64 -> IO ()

-- |FFI wrapper for invoking a 'CatchUpStatusCallback' function.
foreign import ccall "dynamic" invokeCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> CatchUpStatusCallback

-- |Helper for invoking a 'CatchUpStatusCallback' function.
callCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> GenesisIndex -> BS.ByteString -> IO ()
callCatchUpStatusCallback cbk gi bs = BS.useAsCStringLen bs $ \(cdata, clen) ->
    invokeCatchUpStatusCallback cbk gi cdata (fromIntegral clen)

-- ** Regenesis

-- |Callback to signal that a new genesis block has occurred.
-- The argument is the block hash as a 32-byte string.
type RegenesisCallback = Ptr RegenesisArc -> Ptr Word8 -> IO ()

-- |FFI wrapper for invoking a 'RegenesisCallback' function.
foreign import ccall "dynamic" invokeRegenesisCallback :: FunPtr RegenesisCallback -> RegenesisCallback

-- |Helper for invoking a 'RegenesisCallback' function.
callRegenesisCallback :: FunPtr RegenesisCallback -> RegenesisRef -> BlockHash -> IO ()
callRegenesisCallback cb rgRef (BlockHash (SHA256.Hash bh)) = withForeignPtr rgRef $ \rg ->
    FBS.withPtrReadOnly bh $ \ptr ->
        invokeRegenesisCallback cb rg ptr

-- |Abstract type representing the rust Arc object used for tracking genesis blocks.
-- A pointer of this type is passed to consensus at start up and must be passed to each call of
-- the regenesis callback.
data RegenesisArc

-- |A reference that must be passed when calling the regenesis callback.
-- This is a 'ForeignPtr', so a finalizer that disposes of the pointer is attached.
type RegenesisRef = ForeignPtr RegenesisArc

-- |A function pointer used for freeing the regenesis reference.
type RegenesisFree = FinalizerPtr RegenesisArc

-- |Construct a 'RegenesisRef' from a finalizer and a raw pointer.
makeRegenesisRef :: RegenesisFree -> Ptr RegenesisArc -> IO RegenesisRef
makeRegenesisRef = newForeignPtr

-- * Consensus operations

data ConsensusRunner = forall gsconf finconf. ConsensusRunner (MultiVersionRunner gsconf finconf)

-- |Result of starting consensus
data StartResult
    = StartSuccess
    | StartGenesisFailure
    | StartBakerIdentityFailure
    | StartIOException
    | StartInitException InitException

-- |Convert a 'StartResult' to an 'Int64'.
toStartResult :: StartResult -> Int64
toStartResult =
    \case
        StartSuccess -> 0
        StartGenesisFailure -> 1
        StartBakerIdentityFailure -> 2
        StartIOException -> 3
        StartInitException ie ->
            case ie of
                BlockStatePathDir -> 4
                BlockStatePermissionError -> 5
                TreeStatePermissionError -> 6
                DatabaseOpeningError _ -> 7
                GenesisBlockNotInDataBaseError -> 8
                GenesisBlockIncorrect _ -> 9
                DatabaseInvariantViolation _ -> 10
                IncorrectDatabaseVersion _ -> 11

-- |Catch exceptions which may occur at start up and return an appropriate exit code.
handleStartExceptions :: LogMethod IO -> IO StartResult -> IO Int64
handleStartExceptions logM c =
    toStartResult
        <$> c
        `catches` [ Handler handleIOError,
                    Handler handleInitException,
                    Handler handleGlobalStateInitException
                  ]
  where
    handleIOError (ex :: IOError) = StartIOException <$ logM External LLError (displayException ex)
    handleInitException ex = StartInitException ex <$ logM External LLError (displayException ex)
    handleGlobalStateInitException (InvalidGenesisData _) = return StartGenesisFailure

-- |Start up an instance of Skov without starting the baker thread.
-- If an error occurs starting Skov, the error will be logged and
-- a null pointer will be returned.
startConsensus ::
    -- |Maximum block size.
    Word64 ->
    -- |Insertions before purging of transactions
    Word64 ->
    -- |Time in seconds during which a transaction can't be purged
    Word64 ->
    -- |Number of seconds between transaction table purging runs
    Word64 ->
    -- |Serialized genesis data (c string + len)
    CString ->
    Int64 ->
    -- |Serialized baker identity (c string + len)
    CString ->
    Int64 ->
    -- |Handler for generated messages
    FunPtr BroadcastCallback ->
    -- |Handler for sending catch-up status to peers
    FunPtr CatchUpStatusCallback ->
    -- |Regenesis object
    Ptr RegenesisArc ->
    -- |Finalizer for the regenesis object
    RegenesisFree ->
    -- |Handler for notifying the node of new regenesis blocks
    FunPtr RegenesisCallback ->
    -- |Maximum log level (inclusive) (0 to disable logging).
    Word8 ->
    -- |Handler for log events
    FunPtr LogCallback ->
    -- |FilePath for the AppData directory
    CString ->
    -- |Length of AppData path
    Int64 ->
    -- |Database connection string. If length is 0 don't do logging.
    CString ->
    -- |Length of database connection string.
    Int64 ->
    -- |Pointer to receive the pointer to the 'ConsensusRunner'.
    Ptr (StablePtr ConsensusRunner) ->
    IO Int64
startConsensus
    maxBlock
    insertionsBeforePurge
    transactionsKeepAlive
    transactionsPurgingDelay
    gdataC
    gdataLenC
    bidC
    bidLenC
    bcbk
    cucbk
    regenesisPtr
    regenesisFree
    regenesisCB
    maxLogLevel
    lcbk
    appDataC
    appDataLenC
    connStringPtr
    connStringLen
    runnerPtrPtr = handleStartExceptions logM $
        decodeGenesis $ \genesisData -> decodeBakerIdentity $ \bakerIdentity -> do
            -- Get the data directory
            appDataPath <- peekCStringLen (appDataC, fromIntegral appDataLenC)
            let mvcStateConfig = DiskStateConfig appDataPath
            let mvcFinalizationConfig =
                    BufferedFinalization
                        ( FinalizationInstance
                            (bakerSignKey bakerIdentity)
                            (bakerElectionKey bakerIdentity)
                            (bakerAggregationKey bakerIdentity)
                        )
            regenesisRef <- makeRegenesisRef regenesisFree regenesisPtr
            -- Callbacks
            let callbacks =
                    Callbacks
                        { broadcastBlock = callBroadcastCallback bcbk MessageBlock,
                          broadcastFinalizationMessage = callBroadcastCallback bcbk MessageFinalization,
                          broadcastFinalizationRecord = callBroadcastCallback bcbk MessageFinalizationRecord,
                          notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                          notifyRegenesis = callRegenesisCallback regenesisCB regenesisRef
                        }
            runner <-
                if connStringLen /= 0
                    then do
                        mvcTXLogConfig <-
                            TransactionDBConfig
                                <$> BS.packCStringLen (connStringPtr, fromIntegral connStringLen)
                        let config ::
                                MultiVersionConfiguration
                                    DiskTreeDiskBlockWithLogConfig
                                    (BufferedFinalization ThreadTimer)
                            config = MultiVersionConfiguration{..}
                        ConsensusRunner
                            <$> makeMultiVersionRunner config callbacks (Just bakerIdentity) logM genesisData
                    else do
                        let config ::
                                MultiVersionConfiguration
                                    DiskTreeDiskBlockConfig
                                    (BufferedFinalization ThreadTimer)
                            config = MultiVersionConfiguration{mvcTXLogConfig = (), ..}
                        ConsensusRunner
                            <$> makeMultiVersionRunner config callbacks (Just bakerIdentity) logM genesisData
            poke runnerPtrPtr =<< newStablePtr runner
            return StartSuccess
      where
        -- Decode genesis data
        decodeGenesis cont = do
            genesisBS <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
            case S.runGet getPVGenesisData genesisBS of
                Left err -> do
                    logM External LLError $ "Failed to decode genesis data: " ++ err
                    return StartGenesisFailure
                Right genData -> cont genData
        -- Decode the baker identity
        decodeBakerIdentity cont = do
            bakerInfoBS <- BS.packCStringLen (bidC, fromIntegral bidLenC)
            case AE.eitherDecodeStrict bakerInfoBS of
                Left err -> do
                    logM External LLError $ "Failed to decode baker identity data: " ++ err
                    return StartBakerIdentityFailure
                Right bakerIdentity -> cont (bakerIdentity :: BakerIdentity)
        -- Log method
        logM = toLogMethod maxLogLevel lcbk
        -- Runtime parameters
        mvcRuntimeParameters =
            RuntimeParameters
                { rpBlockSize = fromIntegral maxBlock,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
                  rpMaxBakingDelay = defaultMaxBakingDelay,
                  rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
                  rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
                  rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay
                }

-- |Start up an instance of Skov without starting the baker thread.
-- If an error occurs starting Skov, the error will be logged and
-- a null pointer will be returned.
startConsensusPassive ::
    -- |Maximum block size.
    Word64 ->
    -- |Insertions before purging of transactions
    Word64 ->
    -- |Time in seconds during which a transaction can't be purged
    Word64 ->
    -- |Number of seconds between transaction table purging runs
    Word64 ->
    -- |Serialized genesis data (c string + len)
    CString ->
    Int64 ->
    -- |Handler for sending catch-up status to peers
    FunPtr CatchUpStatusCallback ->
    -- |Regenesis object
    Ptr RegenesisArc ->
    -- |Finalizer for the regenesis object
    RegenesisFree ->
    -- |Handler for notifying the node of new regenesis blocks
    FunPtr RegenesisCallback ->
    -- |Maximum log level (inclusive) (0 to disable logging).
    Word8 ->
    -- |Handler for log events
    FunPtr LogCallback ->
    -- |FilePath for the AppData directory
    CString ->
    -- |Length of AppData path
    Int64 ->
    -- |Database connection string. If length is 0 don't do logging.
    CString ->
    -- |Length of database connection string.
    Int64 ->
    -- |Pointer to receive the pointer to the 'ConsensusRunner'.
    Ptr (StablePtr ConsensusRunner) ->
    IO Int64
startConsensusPassive
    maxBlock
    insertionsBeforePurge
    transactionsKeepAlive
    transactionsPurgingDelay
    gdataC
    gdataLenC
    cucbk
    regenesisPtr
    regenesisFree
    regenesisCB
    maxLogLevel
    lcbk
    appDataC
    appDataLenC
    connStringPtr
    connStringLen
    runnerPtrPtr = handleStartExceptions logM $
        decodeGenesis $ \genesisData -> do
            -- Get the data directory
            appDataPath <- peekCStringLen (appDataC, fromIntegral appDataLenC)
            let mvcStateConfig = DiskStateConfig appDataPath
            let mvcFinalizationConfig = NoFinalization
            -- Callbacks
            regenesisRef <- makeRegenesisRef regenesisFree regenesisPtr
            let callbacks =
                    Callbacks
                        { broadcastBlock = \_ _ -> return (),
                        broadcastFinalizationMessage = \_ _ -> return (),
                        broadcastFinalizationRecord = \_ _ -> return (),
                        notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                        notifyRegenesis = callRegenesisCallback regenesisCB regenesisRef
                        }
            runner <-
                if connStringLen /= 0
                    then do
                        mvcTXLogConfig <-
                            TransactionDBConfig
                                <$> BS.packCStringLen (connStringPtr, fromIntegral connStringLen)
                        let config ::
                                MultiVersionConfiguration
                                    DiskTreeDiskBlockWithLogConfig
                                    (NoFinalization ThreadTimer)
                            config = MultiVersionConfiguration{..}
                        ConsensusRunner
                            <$> makeMultiVersionRunner config callbacks Nothing logM genesisData
                    else do
                        let config ::
                                MultiVersionConfiguration
                                    DiskTreeDiskBlockConfig
                                    (NoFinalization ThreadTimer)
                            config = MultiVersionConfiguration{mvcTXLogConfig = (), ..}
                        ConsensusRunner
                            <$> makeMultiVersionRunner config callbacks Nothing logM genesisData
            poke runnerPtrPtr =<< newStablePtr runner
            return StartSuccess
      where
        -- Decode genesis data
        decodeGenesis cont = do
            genesisBS <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
            case S.runGet getPVGenesisData genesisBS of
                Left err -> do
                    logM External LLError $ "Failed to decode genesis data: " ++ err
                    return StartGenesisFailure
                Right genData -> cont genData
        -- Log method
        logM = toLogMethod maxLogLevel lcbk
        -- Runtime parameters
        mvcRuntimeParameters =
            RuntimeParameters
                { rpBlockSize = fromIntegral maxBlock,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
                  rpMaxBakingDelay = defaultMaxBakingDelay,
                  rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
                  rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
                  rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay
                }

-- |Shut down consensus, stopping any baker thread if necessary.
-- The pointer is not valid after this function returns.
stopConsensus :: StablePtr ConsensusRunner -> IO ()
stopConsensus cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.shutdownMultiVersionRunner mvr
    freeStablePtr cptr

-- |Start the baker thread.  Calling this mare than once does not start additional baker threads.
startBaker :: StablePtr ConsensusRunner -> IO ()
startBaker cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.startBaker mvr

-- |Stop a baker thread.  The baker thread may be restarted by calling 'startBaker'.
-- This does not otherwise affect the consensus.
stopBaker :: StablePtr ConsensusRunner -> IO ()
stopBaker cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.stopBaker mvr

-- * Receive functions

-- | Result values for receive functions.
--
-- +=======+====================================+========================================================================================+==========+
-- | Value |                Name                |                                      Description                                       | Forward? |
-- +=======+====================================+========================================================================================+==========+
-- |     0 | ResultSuccess                      | Message received, validated and processed                                              | Yes      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     1 | ResultSerializationFail            | Message deserialization failed                                                         | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     2 | ResultInvalid                      | The message was determined to be invalid                                               | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     3 | ResultPendingBlock                 | The message was received, but is awaiting a block to complete processing               | Yes      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     4 | ResultPendingFinalization          | The message was received, but is awaiting a finalization record to complete processing | Yes      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     5 | ResultAsync                        | The message was received, but is being processed asynchronously                        | Yes      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     6 | ResultDuplicate                    | The message duplicates a previously received message                                   | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     7 | ResultStale                        | The message may have been valid in the past, but is no longer relevant                 | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     8 | ResultIncorrectFinalizationSession | The message refers to a different/unknown finalization session                         | No(?)    |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |     9 | ResultUnverifiable                 | The message could not be verified in the current state (initiate catch-up with peer)   | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |    10 | ResultContinueCatchUp              | The peer should be marked pending catch-up if it is currently up-to-date               | N/A      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |    11 | ResultEarlyBlock                   | The block has a slot number exceeding our current + the early block threshold          | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |    12 | ResultMissingImportFile            | The file provided for importing doesn't exist                                          | N/A      |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |    13 | ResultConsensusShutDown            | Consensus has been shut down and the message was ignored                               | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-- |    13 | ResultInvalidGenesisIndex          | The message is for an unknown genesis index                                            | No       |
-- +-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
type ReceiveResult = Int64

-- |Convert an 'UpdateResult' to the corresponding 'ReceiveResult' value.
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
toReceiveResult ResultMissingImportFile = 12
toReceiveResult ResultConsensusShutDown = 13
toReceiveResult ResultInvalidGenesisIndex = 14

-- |Handle receipt of a block.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultAsync@, @ResultDuplicate@,
-- @ResultStale@, @ResultConsensusShutDown@, and @ResultInvalidGenesisIndex@.
-- 'receiveBlock' may invoke the callbacks for new finalization messages.
receiveBlock :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO ReceiveResult
receiveBlock bptr genIndex msg msgLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLDebug $ "Received block data, size = " ++ show msgLen ++ "."
    blockBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    toReceiveResult <$> runMVR (MV.receiveBlock genIndex blockBS) mvr

-- |Handle receipt of a finalization message.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@, @ResultIncorrectFinalizationSession@,
-- @ResultUnverifiable@, @ResultConsensusShutDown@, and @ResultInvalidGenesisIndex@.
-- 'receiveFinalization' may invoke the callbacks for new finalization messages.
receiveFinalizationMessage ::
    StablePtr ConsensusRunner ->
    GenesisIndex ->
    CString ->
    Int64 ->
    IO ReceiveResult
receiveFinalizationMessage bptr genIndex msg msgLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLDebug $ "Received finalization message, size = " ++ show msgLen ++ "."
    finMsgBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    toReceiveResult <$> runMVR (MV.receiveFinalizationMessage genIndex finMsgBS) mvr

-- |Handle receipt of a finalization record.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@,
-- @ResultConsensusShutDown@ and @ResultInvalidGenesisIndex@.
-- 'receiveFinalizationRecord' may invoke the callbacks for new finalization messages.
receiveFinalizationRecord ::
    StablePtr ConsensusRunner ->
    GenesisIndex ->
    CString ->
    Int64 ->
    IO ReceiveResult
receiveFinalizationRecord bptr genIndex msg msgLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLDebug $ "Received finalization record, size = " ++ show msgLen ++ "."
    finRecBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    toReceiveResult <$> runMVR (MV.receiveFinalizationRecord genIndex finRecBS) mvr

-- |Handle receipt of a transaction.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultDuplicate@,
-- @ResultStale@, @ResultInvalid@, and @ResultConsensusShutDown@.
receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveTransaction bptr transactionData transactionLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLDebug $ "Received transaction, size = " ++ show transactionLen ++ "."
    transactionBS <- BS.packCStringLen (transactionData, fromIntegral transactionLen)
    toReceiveResult <$> runMVR (MV.receiveTransaction transactionBS) mvr

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
    -- |Consensus pointer
    StablePtr ConsensusRunner ->
    -- |Identifier of peer (passed to callback)
    PeerID ->
    -- |Genesis index
    GenesisIndex ->
    -- |Serialised catch-up message
    CString ->
    -- |Length of message
    Int64 ->
    -- |Limit to number of responses. Limit <= 0 means no messages will be sent.
    Int64 ->
    -- |Callback to receive messages
    FunPtr DirectMessageCallback ->
    IO ReceiveResult
receiveCatchUpStatus cptr src genIndex cstr len limit cbk =
    toReceiveResult <$> do
        let catchUpMessageLimit = fromIntegral limit
        (ConsensusRunner mvr) <- deRefStablePtr cptr
        if catchUpMessageLimit <= 0
            then do
                mvLog mvr External LLWarning "Requesting catchup with limit <= 0."
                return ResultSuccess
            else do
                bs <- BS.packCStringLen (cstr, fromIntegral len)
                let catchUpCallback mt = callDirectMessageCallback cbk src mt genIndex
                runMVR (MV.receiveCatchUpStatus genIndex bs CatchUpConfiguration{..}) mvr

-- |Get a catch-up status message for requesting catch-up with peers.
-- The genesis index and string pointer are loaded into the given pointers.
-- The return value is the length of the string.
-- The string should be freed by calling 'freeCStr'.
getCatchUpStatus ::
    -- |Consensus pointer
    StablePtr ConsensusRunner ->
    -- |Pointer to receive the genesis index
    Ptr GenesisIndex ->
    -- |Pointer to receive the string pointer
    Ptr CString ->
    IO Int64
getCatchUpStatus cptr genIndexPtr resPtr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    (genIndex, resBS) <- runMVR MV.getCatchUpRequest mvr
    poke genIndexPtr genIndex
    poke resPtr =<< toCString resBS
    return (LBS.length resBS)

-- |Import a file consisting of a set of blocks and finalization records for the purposes of
-- out-of-band catch-up.
importBlocks ::
    -- |Consensus runner
    StablePtr ConsensusRunner ->
    -- |File path to import blocks from
    CString ->
    -- |Length of filename
    Int64 ->
    IO Int64
importBlocks cptr fname fnameLen =
    toReceiveResult <$> do
        (ConsensusRunner mvr) <- deRefStablePtr cptr
        theFile <- peekCStringLen (fname, fromIntegral fnameLen)
        runMVR (MV.importBlocks theFile) mvr

-- * Queries

-- |Converts a lazy 'LBS.ByteString' to a null-terminated 'CString'.
-- The string must be freed after use by calling 'free'.
toCString :: LBS.ByteString -> IO CString
toCString lbs = do
    let len = LBS.length lbs
    buf <- mallocBytes (fromIntegral len + 1)
    let copyChunk px bs = BS.unsafeUseAsCStringLen bs $ \(bsp, bspLen) -> do
            copyBytes px bsp bspLen
            return $ plusPtr px bspLen
    end <- foldM copyChunk buf (LBS.toChunks lbs)
    poke end (0 :: CChar)
    return buf

-- |Encode a value as JSON in a CString. The allocated string must be explicitly freed to avoid
-- memory leaks.
jsonCString :: AE.ToJSON a => a -> IO CString
jsonCString = toCString . AE.encode

-- |Converts a 'BS.ByteString' to a 'CString' that encodes the length of the
-- string in big-endian in the first four bytes (not including the length).
-- This string should be freed after use by calling 'free'.
byteStringToCString :: BS.ByteString -> IO CString
byteStringToCString bs = do
    let bsp = BS.concat [S.runPut (S.putWord32be (fromIntegral (BS.length bs))), bs]
    -- This use of unsafe is fine because bsp is a non-null string.
    BS.unsafeUseAsCStringLen bsp $ \(cstr, len) -> do
        dest <- mallocBytes len
        copyBytes dest cstr len
        return dest

-- |Free a 'CString'. This should be called to dispose of any 'CString' values that are returned by
-- queries.
freeCStr :: CString -> IO ()
freeCStr = free

-- |Convenience wrapper for queries that return JSON values.
jsonQuery :: AE.ToJSON a => StablePtr ConsensusRunner -> (forall gsconf finconf. MVR gsconf finconf a) -> IO CString
jsonQuery cptr a = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    res <- runMVR a mvr
    jsonCString res

-- |Decode a block hash from a null-terminated base-16 string.
decodeBlockHash :: CString -> IO (Maybe BlockHash)
decodeBlockHash blockcstr = readMaybe <$> peekCString blockcstr

-- |Decode an account address from a null-terminated base-58 string.
decodeAccountAddress :: CString -> IO (Either String AccountAddress)
decodeAccountAddress acctstr = addressFromBytes <$> BS.packCString acctstr

-- |Decode an instance address from a null-terminated JSON-encoded string.
decodeInstanceAddress :: CString -> IO (Maybe ContractAddress)
decodeInstanceAddress inststr = AE.decodeStrict <$> BS.packCString inststr

-- |Decode a module reference from a null-terminated base-16 string.
decodeModuleRef :: CString -> IO (Maybe ModuleRef)
decodeModuleRef modstr = readMaybe <$> peekCString modstr

-- |Decode a transaction hash from a null-terminated base-16 string.
decodeTransactionHash :: CString -> IO (Maybe TransactionHash)
decodeTransactionHash trHashStr = readMaybe <$> peekCString trHashStr

-- ** General queries

-- |Returns a null-terminated string with a JSON representation of the current status of Consensus.
getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
getConsensusStatus cptr = jsonQuery cptr Q.getConsensusStatus

-- ** Queries against latest tree

-- |Returns a null-terminated string with a JSON representation of the current branches from the
-- last finalized block (inclusive).
getBranches :: StablePtr ConsensusRunner -> IO CString
getBranches cptr = jsonQuery cptr Q.getBranches

-- |Get the list of live blocks at a given height.
-- Returns a null-terminated string encoding a JSON list.
getBlocksAtHeight :: StablePtr ConsensusRunner -> Word64 -> IO CString
getBlocksAtHeight cptr height = jsonQuery cptr (Q.getBlocksAtHeight (BlockHeight height))

-- ** Block-indexed queries

-- |Given a null-terminated string that represents a block hash (base 16), returns a null-terminated
-- string containing a JSON representation of the block.
-- If the block hash is invalid or unknown, this returns the JSON null value.
-- For details of the value returned, see 'Concordium.Queries.Types.BlockInfo'.
getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockInfo cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getBlockInfo bh)

-- |Get the list of transactions in a block with short summaries of their effects.
-- Returns a null-terminated string encoding a JSON value.
-- If the block hash is invalid or unknown, this returns the JSON null value.
-- For details of the value returned, see 'Concordium.Queries.Types.BlockSummary'.
getBlockSummary :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockSummary cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getBlockSummary bh)

-- |Get the status of the rewards parameters for the given block. The block must
-- be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated, JSON encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getRewardStatus :: StablePtr ConsensusRunner -> CString -> IO CString
getRewardStatus cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getRewardStatus bh)

-- |Get birk parameters for the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getBirkParameters :: StablePtr ConsensusRunner -> CString -> IO CString
getBirkParameters cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getBlockBirkParameters bh)

-- |Get the cryptographic parameters in a given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded object.
-- The returned string should be freed by calling 'freeCStr'.
getCryptographicParameters :: StablePtr ConsensusRunner -> CString -> IO CString
getCryptographicParameters cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getCryptographicParameters bh)

-- |Get all of the identity providers registered in the system as of a given block.
-- The block must be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list. (Or null for an invalid block.)
-- The returned string should be freed by calling 'freeCStr'.
getAllIdentityProviders :: StablePtr ConsensusRunner -> CString -> IO CString
getAllIdentityProviders cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getAllIdentityProviders bh)

-- |Get all of the identity providers registered in the system as of a given block.
-- The block must be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list. (Or null for an invalid block.)
-- The returned string should be freed by calling 'freeCStr'.
getAllAnonymityRevokers :: StablePtr ConsensusRunner -> CString -> IO CString
getAllAnonymityRevokers cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getAllAnonymityRevokers bh)

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
-- If the block hash is invalid or unknown, this returns the JSON null value.
getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
getAncestors cptr blockcstr depth =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getAncestors bh (BlockHeight depth))

-- |Get the list of account addresses in the given block. The block must be
-- given as a null-terminated base16 encoding of the block hash. The return
-- value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
--
-- Note: the behaviour on an ill-formed block hash has changed slightly, so that
-- JSON null value is returned. Previously, the JSON string
-- "Invalid block hash." was returned.
getAccountList :: StablePtr ConsensusRunner -> CString -> IO CString
getAccountList cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getAccountList bh)

-- |Get the list of contract instances (their addresses) in the given block. The
-- block must be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
--
-- Note: the behaviour on ill-formed block hashes has changed as for 'getAccountList'.
getInstances :: StablePtr ConsensusRunner -> CString -> IO CString
getInstances cptr blockcstr =
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getInstanceList bh)

-- |Get the list of modules in the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list.
-- The returned string should be freed by calling 'freeCStr'.
--
-- Note: the behaviour on ill-formed block hashes has changed as for 'getAccountList'.
getModuleList :: StablePtr ConsensusRunner -> CString -> IO CString
getModuleList cptr blockcstr = do
    decodeBlockHash blockcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getModuleList bh)

-- |Get account information for the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the account
-- address (second CString) must be given as a null-terminated string in Base58
-- encoding (same format as returned by 'getAccountList'). The return value is a
-- null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getAccountInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getAccountInfo cptr blockcstr acctcstr = do
    mblock <- decodeBlockHash blockcstr
    maccount <- decodeAccountAddress acctcstr
    case (mblock, maccount) of
        (Just bh, Right acct) -> jsonQuery cptr (Q.getAccountInfo bh acct)
        _ -> jsonCString AE.Null

-- |Get instance information the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the address
-- (second CString) must be given as a null-terminated JSON-encoded value
-- (an object with numeric fields "index" and "subindex").
-- The return value is a null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getInstanceInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getInstanceInfo cptr blockcstr instcstr = do
    mblock <- decodeBlockHash blockcstr
    minst <- decodeInstanceAddress instcstr
    case (mblock, minst) of
        (Just bh, Just inst) -> jsonQuery cptr (Q.getInstanceInfo bh inst)
        _ -> jsonCString AE.Null

-- |Get the source code of a module as deployed on the chain at a particular block.
-- The block must be given as a null-terminated base16 encoding of the block hash.
-- The module is referenced by a null-terminated base16 encoding of the module hash.
-- The return value is __NOT__ JSON encoded but rather it is a binary
-- serialization. The first 4 bytes are the length (big-endian) of the rest of the string, and
-- the string is __NOT__ null terminated and can contain null characters.
-- The returned string should be freed by calling 'freeCStr'.
-- If the module is not found, the length field of the string will be 0.
getModuleSource :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getModuleSource cptr blockcstr modcstr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    mblock <- decodeBlockHash blockcstr
    mmod <- decodeModuleRef modcstr
    case (mblock, mmod) of
        (Just bh, Just modref) -> do
            msrc <- runMVR (Q.getModuleSource bh modref) mvr
            byteStringToCString $ maybe BS.empty S.encode msrc
        _ -> jsonCString AE.Null

-- ** Transaction-indexed queries

-- |Get the status of a transaction. The input is a base16-encoded null-terminated string
-- denoting a transaction hash. The return value is a null-terminated JSON string encoding a
-- JSON value.
getTransactionStatus :: StablePtr ConsensusRunner -> CString -> IO CString
getTransactionStatus cptr trcstr =
    decodeTransactionHash trcstr >>= \case
        Nothing -> jsonCString AE.Null
        Just tr -> jsonQuery cptr (Q.getTransactionStatus tr)

-- |Get the status of a transaction. The first input is a base16-encoded null-terminated string
-- denoting a transaction hash, the second input is the hash of the block.
-- The return value is a null-terminated string encoding a JSON value.
-- The arguments are
--
--   * pointer to the consensus runner
--   * null-terminated C string with a base16 encoded transaction hash
--   * null-terminated C string with base16 encoded block hash
getTransactionStatusInBlock :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getTransactionStatusInBlock cptr trcstr bhcstr = do
    mtr <- decodeTransactionHash trcstr
    mblock <- decodeBlockHash bhcstr
    case (mtr, mblock) of
        (Just tr, Just bh) -> jsonQuery cptr (Q.getTransactionStatusInBlock tr bh)
        _ -> jsonCString AE.Null

-- ** Account-indexed queries

-- |Get the list of non-finalized transactions for a given account.
-- The arguments are
--
--   * pointer to the consensus runner
--   * null-terminated C string with account address.
getAccountNonFinalizedTransactions :: StablePtr ConsensusRunner -> CString -> IO CString
getAccountNonFinalizedTransactions cptr addrcstr =
    decodeAccountAddress addrcstr >>= \case
        Left _ -> jsonCString AE.Null
        Right acct -> jsonQuery cptr (Q.getAccountNonFinalizedTransactions acct)

-- |Get the best guess for the next available account nonce.
-- The arguments are
--
--   * pointer to the consensus runner
--   * null-terminated C string with account address.
getNextAccountNonce :: StablePtr ConsensusRunner -> CString -> IO CString
getNextAccountNonce cptr addrcstr =
    decodeAccountAddress addrcstr >>= \case
        Left _ -> jsonCString AE.Null
        Right acct -> jsonQuery cptr (Q.getNextAccountNonce acct)

-- ** Baker/finalizer status queries

-- |Check if we are members of the finalization committee.
-- Returns 0 for 'False' and 1 for 'True'.
checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8
checkIfWeAreFinalizer cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    res <- runMVR Q.checkIsCurrentFinalizer mvr
    return $! if res then 1 else 0

-- |Check whether we are a baker from the perspective of the best block.
-- Returns -1 if we are not added as a baker.
-- Returns -2 if we are added as a baker, but not part of the baking committee yet.
-- Returns -3 if we have keys that do not match the baker's public keys on the chain.
-- Returns >= 0 if we are part of the baking committee. The return value is the
-- baker id as appearing in blocks.
bakerIdBestBlock :: StablePtr ConsensusRunner -> IO Int64
bakerIdBestBlock cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    res <- runMVR Q.getBakerStatusBestBlock mvr
    return $! case res of
        NoBaker -> -1
        InactiveBaker _ -> -2
        BadKeys _ -> -3
        ActiveBaker bid -> fromIntegral bid

-- FFI exports

foreign export ccall
    startConsensus ::
        -- |Maximum block size.
        Word64 ->
        -- |Insertions before purging of transactions
        Word64 ->
        -- |Time in seconds during which a transaction can't be purged
        Word64 ->
        -- |Number of seconds between transaction table purging runs
        Word64 ->
        -- |Serialized genesis data (c string + len)
        CString ->
        Int64 ->
        -- |Serialized baker identity (c string + len)
        CString ->
        Int64 ->
        -- |Handler for generated messages
        FunPtr BroadcastCallback ->
        -- |Handler for sending catch-up status to peers
        FunPtr CatchUpStatusCallback ->
        -- |Regenesis object
        Ptr RegenesisArc ->
        -- |Finalizer for the regenesis object
        RegenesisFree ->
        -- |Handler for notifying the node of new regenesis blocks
        FunPtr RegenesisCallback ->
        -- |Maximum log level (inclusive) (0 to disable logging).
        Word8 ->
        -- |Handler for log events
        FunPtr LogCallback ->
        -- |FilePath for the AppData directory
        CString ->
        -- |Length of AppData path
        Int64 ->
        -- |Database connection string. If length is 0 don't do logging.
        CString ->
        -- |Length of database connection string.
        Int64 ->
        -- |Pointer to receive the pointer to the 'ConsensusRunner'.
        Ptr (StablePtr ConsensusRunner) ->
        IO Int64
foreign export ccall
    startConsensusPassive ::
        -- |Maximum block size.
        Word64 ->
        -- |Insertions before purging of transactions
        Word64 ->
        -- |Time in seconds during which a transaction can't be purged
        Word64 ->
        -- |Number of seconds between transaction table purging runs
        Word64 ->
        -- |Serialized genesis data (c string + len)
        CString ->
        Int64 ->
        -- |Handler for sending catch-up status to peers
        FunPtr CatchUpStatusCallback ->
        -- |Regenesis object
        Ptr RegenesisArc ->
        -- |Finalizer for the regenesis object
        RegenesisFree ->
        -- |Handler for notifying the node of new regenesis blocks
        FunPtr RegenesisCallback ->
        -- |Maximum log level (inclusive) (0 to disable logging).
        Word8 ->
        -- |Handler for log events
        FunPtr LogCallback ->
        -- |FilePath for the AppData directory
        CString ->
        -- |Length of AppData path
        Int64 ->
        -- |Database connection string. If length is 0 don't do logging.
        CString ->
        -- |Length of database connection string.
        Int64 ->
        -- |Pointer to receive the pointer to the 'ConsensusRunner'.
        Ptr (StablePtr ConsensusRunner) ->
        IO Int64

foreign export ccall stopConsensus :: StablePtr ConsensusRunner -> IO ()
foreign export ccall startBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall stopBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalizationMessage :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalizationRecord :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO Int64
foreign export ccall receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64

foreign export ccall getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
foreign export ccall getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
foreign export ccall getBranches :: StablePtr ConsensusRunner -> IO CString

foreign export ccall
    getCatchUpStatus ::
        StablePtr ConsensusRunner ->
        Ptr GenesisIndex ->
        Ptr CString ->
        IO Int64
foreign export ccall
    receiveCatchUpStatus ::
        StablePtr ConsensusRunner ->
        PeerID ->
        GenesisIndex ->
        CString ->
        Int64 ->
        Int64 ->
        FunPtr DirectMessageCallback ->
        IO ReceiveResult

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
foreign export ccall getBlocksAtHeight :: StablePtr ConsensusRunner -> Word64 -> IO CString
foreign export ccall getAllIdentityProviders :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAllAnonymityRevokers :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getCryptographicParameters :: StablePtr ConsensusRunner -> CString -> IO CString

-- baker status checking
foreign export ccall bakerIdBestBlock :: StablePtr ConsensusRunner -> IO Int64
foreign export ccall checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8

-- maintenance
foreign export ccall freeCStr :: CString -> IO ()

foreign export ccall importBlocks :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
