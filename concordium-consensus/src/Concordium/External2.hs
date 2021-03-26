{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.External2 where

import Control.Exception
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import Data.Int
import Data.Word
import Foreign
import Foreign.C

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Logger
import Concordium.Types
import qualified Data.FixedByteString as FBS

import Concordium.Birk.Bake
import Concordium.GlobalState
import Concordium.GlobalState.Persistent.TreeState (InitException (..))
import Concordium.MultiVersion
import Concordium.Scheduler.Types
import Concordium.Skov (MessageType (..), BufferedFinalization (BufferedFinalization))
import Data.Serialize (runGet)
import Concordium.TimerMonad (ThreadTimer)

-- * Callbacks

-- ** Logging

-- | External function that logs in Rust a message using standard Rust log output
--
-- The first argument represents the Identifier which shows in which module the message has been emited.
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
callBroadcastCallback cbk mt gi bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBroadcastCallback cbk mti gi cdata (fromIntegral clen)
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
callCatchUpStatusCallback cbk gi bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeCatchUpStatusCallback cbk gi cdata (fromIntegral clen)

-- ** Regenesis

-- |Callback to signal that a new genesis block has occurred.
-- The argument is the block hash as a 32-byte string.
type RegenesisCallback = Ptr Word8 -> IO ()

-- |FFI wrapper for invoking a 'RegenesisCallback' function.
foreign import ccall "dynamic" invokeRegenesisCallback :: FunPtr RegenesisCallback -> RegenesisCallback

-- |Helper for invoking a 'RegenesisCallback' function.
callRegenesisCallback :: FunPtr RegenesisCallback -> BlockHash -> IO ()
callRegenesisCallback cb (BlockHash (SHA256.Hash bh)) = FBS.withPtrReadOnly bh $ \ptr ->
    invokeRegenesisCallback cb ptr

-- * Consensus operations

data ConsensusRunner = forall gsconf finconf. ConsensusRunner (MultiVersionRunner gsconf finconf)

-- |Result of starting consensus
data StartResult
    = StartSuccess
    | StartGenesisFailure
    | StartBakerIdentityFailure
    | StartIOException
    | StartInitException InitException

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
    CString ->
    -- |Serialized genesis data (c string + len)
    Int64 ->
    CString ->
    -- |Serialized baker identity (c string + len)
    Int64 ->
    -- |Handler for generated messages
    FunPtr BroadcastCallback ->
    -- |Handler for sending catch-up status to peers
    FunPtr CatchUpStatusCallback ->
    -- |Rust arc for calling the regenesis callback
    Ptr () ->
    -- | free function for the regenesis arc
    FunPtr (Ptr () -> IO ()) ->
    -- |Handler for notifying the node of new regenesis blocks
    FunPtr RegenesisCallback ->
    -- |Maximum log level (inclusive) (0 to disable logging).
    Word8 ->
    -- |Handler for log events
    FunPtr LogCallback ->
    CString ->
    -- |FilePath for the AppData directory
    Int64 ->
    CString ->
    -- |Database connection string. If length is 0 don't do logging.
    Int64 ->
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
    regenesisArcPtr
    freeRegenesisArc
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
            mvr <-
                if connStringLen /= 0
                    then do
                        let config :: MultiVersionConfiguration DiskTreeDiskBlockWithLogConfig (BufferedFinalization ThreadTimer)
                            config = undefined
                        makeMultiVersionRunner config callbacks (Just bakerIdentity) logM genesisData
                    else undefined
            return StartSuccess
      where
        -- Decode genesis data
        decodeGenesis cont = do
            genesisBS <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
            case runGet getPVGenesisData genesisBS of
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
        -- Callbacks
        callbacks =
            Callbacks
                { broadcastBlock = callBroadcastCallback bcbk MessageBlock,
                  broadcastFinalizationMessage = callBroadcastCallback bcbk MessageFinalization,
                  broadcastFinalizationRecord = callBroadcastCallback bcbk MessageFinalizationRecord,
                  notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                  notifyRegenesis = callRegenesisCallback regenesisCB
                }

{-
        genesisBS <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        case runGet getVersionedGenesisData genesisBS of
            Left err -> do
                logM External LLError $ "Failed to decode genesis data: " ++ err
                return (toStartResult StartGenesisFailure)
           Right genData
        bakerInfoBS <- BS.packCStringLen (bidC, fromIntegral bidLenC)
        appData <- peekCStringLen (appDataC, fromIntegral appDataLenC)
-
        let runtimeParams = RuntimeParameters {
              rpBlockSize = fromIntegral maxBlock,
              rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
              rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
              rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
              rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay
            }
        case (runGet getVersionedGenesisData gdata, AE.eitherDecodeStrict bdata) of
            (Right genData, Right bid) -> do
              let
                  finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid))
                  hconfig = LogUpdateHandler
              if connStringLen /= 0 then do -- enable logging of transactions
                connString <- BS.packCStringLen (connStringPtr, fromIntegral connStringLen)
                let
                    gsconfig = makeGlobalStateConfigWithLog
                        runtimeParams
                        appData
                        genData
                        connString
                    config = SkovConfig gsconfig finconfig hconfig
                    bakerBroadcast = broadcastCallback logM bcbk
                regenesisArc <- newForeignPtr freeRegenesisArc regenesisArcPtr
                let regenesisCallback b = withForeignPtr regenesisArc $ \arc -> callRegenesisCallback regenesisCB arc (Hash.hashToByteString (blockHash b))
                bakerSyncRunnerWithLog <-
                  makeSyncRunner logM bid config bakerBroadcast catchUpCallback regenesisCallback
                poke runnerPtrPtr =<< newStablePtr BakerRunnerWithLog{..}
                return (toStartResult StartSuccess)
              else do
                let
                    gsconfig = makeGlobalStateConfig
                        runtimeParams
                        appData
                        genData
                    config = SkovConfig gsconfig finconfig hconfig
                    bakerBroadcast = broadcastCallback logM bcbk
                regenesisArc <- newForeignPtr freeRegenesisArc regenesisArcPtr
                let regenesisCallback b = withForeignPtr regenesisArc $ \arc -> callRegenesisCallback regenesisCB arc (Hash.hashToByteString (blockHash b))
                bakerSyncRunner <- makeSyncRunner logM bid config bakerBroadcast catchUpCallback regenesisCallback
                poke runnerPtrPtr =<< newStablePtr BakerRunner{..}
                return (toStartResult StartSuccess)
            (Left err, _) -> do
                logM External LLError $ "Failed to decode genesis data: " ++ err
                return (toStartResult StartGenesisFailure)
            (_, Left err) -> do
                logM External LLError $ "Failed to decode baker identity data: " ++ err
                return (toStartResult StartBakerIdentityFailure)

    where
        logM = toLogMethod maxLogLevel lcbk
        -- logT = if enableTransferLogging /= 0 then Just (toLogTransferMethod ltcbk) else Nothing
        catchUpCallback = callCatchUpStatusCallback cucbk . runPut . putVersionedCatchUpStatus

-}