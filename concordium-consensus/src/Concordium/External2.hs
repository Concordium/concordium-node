{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}
module Concordium.External2 where

import Control.Exception
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import Data.Serialize (runGet)
import Data.Word
import Foreign
import Foreign.C

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Logger
import Concordium.Types
import qualified Data.FixedByteString as FBS

import Concordium.Afgjort.Finalize.Types (FinalizationInstance (FinalizationInstance))
import Concordium.Birk.Bake
import Concordium.Constants (defaultEarlyBlockThreshold)
import Concordium.GlobalState
import Concordium.GlobalState.Persistent.TreeState (InitException (..))
import Concordium.MultiVersion as MV
import qualified Concordium.Queries.MultiVersion as Q
import Concordium.Scheduler.Types
import Concordium.Skov
import Concordium.TimerMonad (ThreadTimer)
import Control.Monad
import Text.Read (readMaybe)

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
        -- Runtime parameters
        mvcRuntimeParameters =
            RuntimeParameters
                { rpBlockSize = fromIntegral maxBlock,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
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
            case runGet getPVGenesisData genesisBS of
                Left err -> do
                    logM External LLError $ "Failed to decode genesis data: " ++ err
                    return StartGenesisFailure
                Right genData -> cont genData
        -- Log method
        logM = toLogMethod maxLogLevel lcbk
        -- Callbacks
        callbacks =
            Callbacks
                { broadcastBlock = \_ _ -> return (),
                  broadcastFinalizationMessage = \_ _ -> return (),
                  broadcastFinalizationRecord = \_ _ -> return (),
                  notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                  notifyRegenesis = callRegenesisCallback regenesisCB
                }
        -- Runtime parameters
        mvcRuntimeParameters =
            RuntimeParameters
                { rpBlockSize = fromIntegral maxBlock,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
                  rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
                  rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
                  rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay
                }

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

-- * Queries

-- |Converts a lazy 'LBS.ByteString' to a null-terminated 'CString'.
-- The string must be freed after use by calling 'free'.
toCString :: LBS.ByteString -> IO CString
toCString lbs = do
        let len = LBS.length lbs
        buf <- mallocBytes (fromIntegral len+1)
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
getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockInfo cptr blockcstr = do
    block <- peekCString blockcstr
    case readMaybe block of
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getBlockInfo bh)

-- |Get the list of transactions in a block with short summaries of their effects.
-- Returns a null-terminated string encoding a JSON value.
getBlockSummary :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockSummary cptr blockcstr = do
    block <- peekCString blockcstr
    case readMaybe block of
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getBlockSummary bh)

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
getAncestors cptr blockcstr depth = do
    block <- peekCString blockcstr
    case readMaybe block of
        Nothing -> jsonCString AE.Null
        Just bh -> jsonQuery cptr (Q.getAncestors bh (BlockHeight depth))

{-

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

-- |Get the cryptographic parameters in a given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded object.
-- The returned string should be freed by calling 'freeCStr'.
getCryptographicParameters :: StablePtr ConsensusRunner -> CString -> IO CString
getCryptographicParameters cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for cryptographic parameters."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      params <- runConsensusQuery c (Get.getCryptographicParameters hash)
      logm External LLTrace $ "Replying."
      jsonValueToCString (AE.toJSON params)

-- |Check whether we are a baker from the perspective of the best block.
-- Returns -1 if we are not added as a baker.
-- Returns -2 if we are added as a baker, but not part of the baking committee yet.
-- Returns -3 if we have keys that do not match the baker's public keys on the chain.
-- Returns >= 0 if we are part of the baking committee. The return value is the
-- baker id as appearing in blocks.
bakerIdBestBlock :: StablePtr ConsensusRunner -> IO Int64
bakerIdBestBlock cptr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLTrace "Checking whether we are a baker."
    let passive = do
          logm External LLTrace "Passive consensus, not a baker."
          return (-1)
    let active :: SyncRunner (ActiveConfig a) -> IO Int64
        active s = do
          let bid = syncBakerIdentity s
          status <- runConsensusQuery c (Get.bakerStatusBestBlock bid)
          let r = case status of
                  Get.ActiveBaker -> fromIntegral $ bakerId bid
                  Get.InactiveBaker -> (-2)
                  Get.NoBaker -> (-1)
                  Get.BadKeys -> (-3)
          logm External LLTrace $ "Replying with " ++ show r ++ " (" ++ show status ++ ")"
          return r
    case c of
      PassiveRunner _ -> passive
      PassiveRunnerWithLog _ -> passive
      BakerRunner s -> active s
      BakerRunnerWithLog s -> active s

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
          case mmodul of
            Nothing -> do
              logm External LLTrace "Module not available."
              byteStringToCString BS.empty
            Just modul ->
              let reply = encode modul
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



getAllIdentityProviders :: StablePtr ConsensusRunner -> CString -> IO CString
getAllIdentityProviders cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for identity providers."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      ips <- runConsensusQuery c (Get.getAllIdentityProviders hash)
      logm External LLTrace $ "Replying with: " ++ show ips
      jsonValueToCString ips

getAllAnonymityRevokers :: StablePtr ConsensusRunner -> CString -> IO CString
getAllAnonymityRevokers cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLDebug "Received request for anonymity revokers."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      ars <- runConsensusQuery c (Get.getAllAnonymityRevokers hash)
      logm External LLTrace $ "Replying with: " ++ show ars
      jsonValueToCString ars
-}