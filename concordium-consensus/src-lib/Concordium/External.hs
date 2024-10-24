{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.External (ConsensusRunner (..)) where

import Control.Exception
import Control.Monad
import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import qualified Data.ProtoLens as Proto
import qualified Data.ProtoLens.Combinators as Proto
import qualified Data.Serialize as S
import Data.Word
import Foreign
import Foreign.C
import Lens.Micro.Platform
import qualified Proto.V2.Concordium.Types as Proto
import qualified Proto.V2.Concordium.Types_Fields as ProtoFields
import System.Directory
import System.FilePath

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Logger
import Concordium.Types
import Concordium.Types.Block (AbsoluteBlockHeight)
import qualified Data.FixedByteString as FBS

import Concordium.Afgjort.Finalize.Types (FinalizationInstance (FinalizationInstance))
import Concordium.Birk.Bake
import Concordium.Constants.Time (defaultEarlyBlockThreshold, defaultMaxBakingDelay)
import Concordium.GlobalState
import Concordium.GlobalState.Persistent.LMDB (addDatabaseVersion)
import Concordium.GlobalState.Persistent.TreeState (InitException (..))
import Concordium.MultiVersion (
    Callbacks (..),
    CatchUpConfiguration (..),
    MVR (..),
    MultiVersionConfiguration (..),
    MultiVersionRunner (..),
    makeMultiVersionRunner,
 )
import qualified Concordium.MultiVersion as MV
import Concordium.Queries (BakerStatus (..))
import qualified Concordium.Queries as Q
import Concordium.Scheduler.Types
import Concordium.Skov (
    BufferedFinalization (..),
    MessageType (..),
    NoFinalization (..),
    UpdateResult (..),
 )
import Concordium.TimerMonad (ThreadTimer)

-- | A 'PeerID' identifies peer at the p2p layer.
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

-- | FFI wrapper for calling a 'LogCallback' function.
foreign import ccall "dynamic" callLogCallback :: FunPtr LogCallback -> LogCallback

-- | Wrap a log callback as a log method, only logging events with loglevel <= given log level.
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

-- | Callback for broadcasting a message to the network.
--  The first argument indicates the message type.
--  The second argument is the genesis index.
--  The third argument is a pointer to the data to broadcast.
--  The fourth argument is the length of the data in bytes.
type BroadcastCallback = Int64 -> GenesisIndex -> CString -> Int64 -> IO ()

-- | FFI wrapper for invoking a 'BroadcastCallback' function.
foreign import ccall "dynamic" invokeBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastCallback

-- | Helper for invoking a 'BroadcastCallback' function.
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

-- | Callback for sending a message to a peer.
--  The first argument is the peer to send to.
--  The second argument indicates the message type.
--  The third argument is a pointer to the data to broadcast.
--  The fourth argument is the length of the data in bytes.
type DirectMessageCallback = PeerID -> Int64 -> GenesisIndex -> CString -> Int64 -> IO ()

-- | FFI wrapper for invoking a 'DirectMessageCallback' function.
foreign import ccall "dynamic" invokeDirectMessageCallback :: FunPtr DirectMessageCallback -> DirectMessageCallback

-- | Helper for invoking a 'DirectMessageCallback' function.
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

-- | Callback for direct-sending a catch-up status message to all (non-pending) peers.
--  The first argument is the genesis index.
--  The first argument is a pointer to the data, which must be a catch-up
--  status message. The second argument is the length of the data in bytes.
type CatchUpStatusCallback = GenesisIndex -> CString -> Int64 -> IO ()

-- | FFI wrapper for invoking a 'CatchUpStatusCallback' function.
foreign import ccall "dynamic" invokeCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> CatchUpStatusCallback

-- | Helper for invoking a 'CatchUpStatusCallback' function.
callCatchUpStatusCallback :: FunPtr CatchUpStatusCallback -> GenesisIndex -> BS.ByteString -> IO ()
callCatchUpStatusCallback cbk gi bs = BS.useAsCStringLen bs $ \(cdata, clen) ->
    invokeCatchUpStatusCallback cbk gi cdata (fromIntegral clen)

-- ** Regenesis

-- | Callback to signal that a new genesis block has occurred.
--  The argument is the block hash as a 32-byte string.
type RegenesisCallback = Ptr RegenesisArc -> Ptr Word8 -> IO ()

-- | FFI wrapper for invoking a 'RegenesisCallback' function.
foreign import ccall "dynamic" invokeRegenesisCallback :: FunPtr RegenesisCallback -> RegenesisCallback

-- | Helper for invoking a 'RegenesisCallback' function.
callRegenesisCallback :: FunPtr RegenesisCallback -> RegenesisRef -> Maybe BlockHash -> IO ()
callRegenesisCallback cb rgRef (Just (BlockHash (SHA256.Hash bh))) = withForeignPtr rgRef $ \rg ->
    FBS.withPtrReadOnly bh $ \ptr ->
        invokeRegenesisCallback cb rg ptr
callRegenesisCallback cb rgRef Nothing = withForeignPtr rgRef $ \rg ->
    invokeRegenesisCallback cb rg nullPtr

-- | Abstract type representing the rust Arc object used for tracking genesis blocks.
--  A pointer of this type is passed to consensus at start up and must be passed to each call of
--  the regenesis callback.
data RegenesisArc

-- | A reference that must be passed when calling the regenesis callback.
--  This is a 'ForeignPtr', so a finalizer that disposes of the pointer is attached.
type RegenesisRef = ForeignPtr RegenesisArc

-- | A function pointer used for freeing the regenesis reference.
type RegenesisFree = FinalizerPtr RegenesisArc

-- | Construct a 'RegenesisRef' from a finalizer and a raw pointer.
makeRegenesisRef :: RegenesisFree -> Ptr RegenesisArc -> IO RegenesisRef
makeRegenesisRef = newForeignPtr

-- * Consensus operations

-- | A 'ConsensusRunner' is a 'MultiVersionRunner' with an existentially quantified global state
--  and finalization configuration.  A 'StablePtr' to a consensus runner is used as the reference
--  to the consensus that is passed over the FFI.
--
--  The use of the existential type is convenient, since it avoids or defers case analysis, while
--  allowing for multiple possible configurations.
data ConsensusRunner = forall finconf. ConsensusRunner (MultiVersionRunner finconf)

-- | Result of starting consensus
data StartResult
    = StartSuccess
    | StartGenesisFailure
    | StartBakerIdentityFailure
    | StartIOException
    | StartInitException InitException

-- | Convert a 'StartResult' to an 'Int64'.
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
                AccountMapPermissionError -> 12

-- | Catch exceptions which may occur at start up and return an appropriate exit code.
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

-- | Migrate a legacy global state, if necessary.
migrateGlobalState :: FilePath -> LogMethod IO -> IO ()
migrateGlobalState dbPath logM = do
    blockStateExists <- doesPathExist $ dbPath </> "blockstate-0" <.> "dat"
    treeStateExists <- doesPathExist $ dbPath </> "treestate-0"
    -- Only attempt migration when neither state exists
    unless (blockStateExists || treeStateExists) $ do
        oldBlockStateExists <- doesFileExist $ dbPath </> "blockstate" <.> "dat"
        oldTreeStateExists <- doesDirectoryExist $ dbPath </> "treestate"
        case (oldBlockStateExists, oldTreeStateExists) of
            (True, True) -> do
                logM GlobalState LLInfo "Migrating global state from legacy version."
                renameFile (dbPath </> "blockstate" <.> "dat") (dbPath </> "blockstate-0" <.> "dat")
                renameDirectory (dbPath </> "treestate") (dbPath </> "treestate-0")
                runLoggerT (addDatabaseVersion (dbPath </> "treestate-0")) logM
                logM GlobalState LLInfo "Migration complete."
            (True, False) -> logM GlobalState LLWarning "Cannot migrate legacy database as 'treestate' is absent."
            (False, True) -> logM GlobalState LLWarning "Cannot migrate legacy database as 'blockstate.dat' is absent."
            _ -> return ()

-- | The opaque type that represents a foreign (i.e., living in Rust) object. The
--  purpose of this context is to enable consensus to signal important events to
--  the Rust code. In particular it is currently used to inform the GRPC2 server
--  that a new block has arrived, or that a new block is finalized.
data NotifyContext

-- | Type of the callback used to send notifications.
type NotifyCallback =
    -- | Handle to the context.
    Ptr NotifyContext ->
    -- | The type of event. Only 0 and 1 are given meaning.
    --
    --    - 0 for block arrived
    --    - 1 for block finalized
    Word8 ->
    -- | Pointer to the beginning of the data to send.
    Ptr Word8 ->
    -- | Size of the data to send.
    Word64 ->
    -- | Absolute block height of either the arrived block or finalized depending on the type of event.
    Word64 ->
    -- | Byte where a value of 1 indicates the block arrived/finalized was baked by the baker ID
    --  setup for this node.
    Word8 ->
    IO ()

foreign import ccall "dynamic" callNotifyCallback :: FunPtr NotifyCallback -> NotifyCallback

-- | Serialize the provided arguments (block hash, absolute block height) into an appropriate Proto
--  message, and invoke the provided FFI callback with the additional arguments providing the rust layer:
--  block height and whether it was baked by the node.
mkNotifyBlockArrived :: (Word8 -> Ptr Word8 -> Word64 -> Word64 -> Word8 -> IO ()) -> BlockHash -> AbsoluteBlockHeight -> Bool -> IO ()
mkNotifyBlockArrived f = \bh height isHomeBaked -> do
    let msg :: Proto.ArrivedBlockInfo = Proto.make $ do
            ProtoFields.hash . ProtoFields.value .= S.encode bh
            ProtoFields.height . ProtoFields.value .= fromIntegral height
    let isHomeBakedByte = if isHomeBaked then 1 else 0
    BS.unsafeUseAsCStringLen (Proto.encodeMessage msg) $ \(cPtr, len) -> do
        f 0 (castPtr cPtr) (fromIntegral len) (fromIntegral height) isHomeBakedByte

-- | Serialize the provided arguments (block hash, block height) into an appropriate Proto message,
--  and invoke the provided FFI callback with the additional arguments providing the rust layer:
--  block height, whether it was baked by the same baker ID as the one setup for the node.
mkNotifyBlockFinalized :: (Word8 -> Ptr Word8 -> Word64 -> Word64 -> Word8 -> IO ()) -> BlockHash -> AbsoluteBlockHeight -> Bool -> IO ()
mkNotifyBlockFinalized f = \bh height isHomeBaked -> do
    let msg :: Proto.FinalizedBlockInfo = Proto.make $ do
            ProtoFields.hash . ProtoFields.value .= S.encode bh
            ProtoFields.height . ProtoFields.value .= fromIntegral height
    let isHomeBakedByte = if isHomeBaked then 1 else 0
    BS.unsafeUseAsCStringLen (Proto.encodeMessage msg) $ \(cPtr, len) -> do
        f 1 (castPtr cPtr) (fromIntegral len) (fromIntegral height) isHomeBakedByte

-- | Context for when signalling a unsupported protocol update is pending.
data NotifyUnsupportedUpdatesContext

-- | The callback used to signal a unsupported protocol update is pending.
type NotifyUnsupportedUpdatesCallback =
    -- | Handle to the context.
    Ptr NotifyUnsupportedUpdatesContext ->
    -- | Effective timestamp of unsupported protocol update as milliseconds since unix epoch.
    Word64 ->
    IO ()

foreign import ccall "dynamic"
    callNotifyUnsupportedUpdates ::
        FunPtr NotifyUnsupportedUpdatesCallback ->
        NotifyUnsupportedUpdatesCallback

-- | Call FFI callback with effective timestamp of unsupported protocol update, as milliseconds since
--  unix epoch.
mkNotifyUnsupportedUpdates :: (Word64 -> IO ()) -> Timestamp -> IO ()
mkNotifyUnsupportedUpdates f unsupportedUpdateEffectiveTime =
    f (tsMillis unsupportedUpdateEffectiveTime)

-- | Start up an instance of Skov without starting the baker thread.
--  If an error occurs starting Skov, the error will be logged and
--  a null pointer will be returned.
startConsensus ::
    -- | Maximum block size.
    Word64 ->
    -- | Block construction timeout in milliseconds
    Word64 ->
    -- | Insertions before purging of transactions
    Word64 ->
    -- | Time in seconds during which a transaction can't be purged
    Word64 ->
    -- | Number of seconds between transaction table purging runs
    Word64 ->
    -- | Accounts table cache size
    Word32 ->
    -- | Modules table cache size
    Word32 ->
    -- | Serialized genesis data (c string + len)
    CString ->
    Int64 ->
    -- | Serialized baker identity (c string + len)
    CString ->
    Int64 ->
    -- | Context for notifying upon new block arrival, and new finalized blocks.
    Ptr NotifyContext ->
    -- | The callback used to invoke upon new block arrival, and new finalized blocks.
    FunPtr NotifyCallback ->
    -- | Context for when signalling a unsupported protocol update is pending.
    Ptr NotifyUnsupportedUpdatesContext ->
    -- | The callback used to signal a unsupported protocol update is pending.
    FunPtr NotifyUnsupportedUpdatesCallback ->
    -- | Handler for generated messages
    FunPtr BroadcastCallback ->
    -- | Handler for sending catch-up status to peers
    FunPtr CatchUpStatusCallback ->
    -- | Regenesis object
    Ptr RegenesisArc ->
    -- | Finalizer for the regenesis object
    RegenesisFree ->
    -- | Handler for notifying the node of new regenesis blocks
    FunPtr RegenesisCallback ->
    -- | Maximum log level (inclusive) (0 to disable logging).
    Word8 ->
    -- | Handler for log events
    FunPtr LogCallback ->
    -- | FilePath for the AppData directory
    CString ->
    -- | Length of AppData path
    Int64 ->
    -- | Pointer to receive the pointer to the 'ConsensusRunner'.
    Ptr (StablePtr ConsensusRunner) ->
    IO Int64
startConsensus
    maxBlock
    blockConstructionTimeout
    insertionsBeforePurge
    transactionsKeepAlive
    transactionsPurgingDelay
    accountsCacheSize
    modulesCacheSize
    gdataC
    gdataLenC
    bidC
    bidLenC
    notifyContext
    notifyCbk
    unsupportedUpdateContext
    unsupportedUpdateCallback
    bcbk
    cucbk
    regenesisPtr
    regenesisFree
    regenesisCB
    maxLogLevel
    lcbk
    appDataC
    appDataLenC
    runnerPtrPtr = handleStartExceptions logM $
        packGenesis $ \genesisBS -> decodeBakerIdentity $ \bakerIdentity -> do
            -- Get the data directory
            appDataPath <- peekCStringLen (appDataC, fromIntegral appDataLenC)
            -- Do globalstate migration if necessary
            migrateGlobalState appDataPath logM
            mvcStateConfig <- MV.makeDiskStateConfig appDataPath
            let mvcFinalizationConfig =
                    BufferedFinalization
                        ( FinalizationInstance
                            (bakerSignKey bakerIdentity)
                            (bakerElectionKey bakerIdentity)
                            (bakerAggregationKey bakerIdentity)
                        )
            regenesisRef <- makeRegenesisRef regenesisFree regenesisPtr
            -- Callbacks
            let notifyCallback = callNotifyCallback notifyCbk
            let callbacks =
                    Callbacks
                        { broadcastBlock = callBroadcastCallback bcbk MessageBlock,
                          broadcastFinalizationMessage = callBroadcastCallback bcbk MessageFinalization,
                          broadcastFinalizationRecord = callBroadcastCallback bcbk MessageFinalizationRecord,
                          notifyBlockArrived =
                            if notifyContext /= nullPtr
                                then Just $ mkNotifyBlockArrived (notifyCallback notifyContext)
                                else Nothing,
                          notifyBlockFinalized =
                            if notifyContext /= nullPtr
                                then Just $ mkNotifyBlockFinalized (notifyCallback notifyContext)
                                else Nothing,
                          notifyUnsupportedProtocolUpdate =
                            if unsupportedUpdateContext /= nullPtr
                                then
                                    Just $
                                        mkNotifyUnsupportedUpdates $
                                            callNotifyUnsupportedUpdates unsupportedUpdateCallback unsupportedUpdateContext
                                else Nothing,
                          notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                          notifyRegenesis = callRegenesisCallback regenesisCB regenesisRef
                        }
            runner <- do
                let config ::
                        MultiVersionConfiguration
                            (BufferedFinalization ThreadTimer)
                    config = MultiVersionConfiguration{..}
                ConsensusRunner
                    <$> makeMultiVersionRunner config callbacks (Just bakerIdentity) logM (Left genesisBS)
            poke runnerPtrPtr =<< newStablePtr runner
            return StartSuccess
      where
        -- Pack the genesis string as a byte string.
        packGenesis cont = do
            cont =<< BS.packCStringLen (gdataC, fromIntegral gdataLenC)

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
                  rpBlockTimeout = fromIntegral blockConstructionTimeout,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
                  rpMaxBakingDelay = defaultMaxBakingDelay,
                  rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
                  rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
                  rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay,
                  rpAccountsCacheSize = fromIntegral accountsCacheSize,
                  rpModulesCacheSize = fromIntegral modulesCacheSize
                }

-- | Start up an instance of Skov without starting the baker thread.
--  If an error occurs starting Skov, the error will be logged and
--  a null pointer will be returned.
startConsensusPassive ::
    -- | Maximum block size.
    Word64 ->
    -- | Block construction timeout in milliseconds
    Word64 ->
    -- | Insertions before purging of transactions
    Word64 ->
    -- | Time in seconds during which a transaction can't be purged
    Word64 ->
    -- | Number of seconds between transaction table purging runs
    Word64 ->
    -- | Accounts table cache size
    Word32 ->
    -- | Modules table cache size
    Word32 ->
    -- | Serialized genesis data (c string + len)
    CString ->
    Int64 ->
    -- | Context for notifying upon new block arrival, and new finalized blocks.
    Ptr NotifyContext ->
    -- | The callback used to invoke upon new block arrival, and new finalized blocks.
    FunPtr NotifyCallback ->
    -- | Context for when signalling a unsupported protocol update is pending.
    Ptr NotifyUnsupportedUpdatesContext ->
    -- | The callback used to signal a unsupported protocol update is pending.
    FunPtr NotifyUnsupportedUpdatesCallback ->
    -- | Handler for sending catch-up status to peers
    FunPtr CatchUpStatusCallback ->
    -- | Regenesis object
    Ptr RegenesisArc ->
    -- | Finalizer for the regenesis object
    RegenesisFree ->
    -- | Handler for notifying the node of new regenesis blocks
    FunPtr RegenesisCallback ->
    -- | Maximum log level (inclusive) (0 to disable logging).
    Word8 ->
    -- | Handler for log events
    FunPtr LogCallback ->
    -- | FilePath for the AppData directory
    CString ->
    -- | Length of AppData path
    Int64 ->
    -- | Pointer to receive the pointer to the 'ConsensusRunner'.
    Ptr (StablePtr ConsensusRunner) ->
    IO Int64
startConsensusPassive
    maxBlock
    blockConstructionTimeout
    insertionsBeforePurge
    transactionsKeepAlive
    transactionsPurgingDelay
    accountsCacheSize
    modulesCacheSize
    gdataC
    gdataLenC
    notifyContext
    notifycbk
    unsupportedUpdateContext
    unsupportedUpdateCallback
    cucbk
    regenesisPtr
    regenesisFree
    regenesisCB
    maxLogLevel
    lcbk
    appDataC
    appDataLenC
    runnerPtrPtr = handleStartExceptions logM $
        packGenesis $ \genesisBS -> do
            -- Get the data directory
            appDataPath <- peekCStringLen (appDataC, fromIntegral appDataLenC)
            -- Do globalstate migration if necessary
            migrateGlobalState appDataPath logM
            mvcStateConfig <- MV.makeDiskStateConfig appDataPath
            let mvcFinalizationConfig = NoFinalization
            -- Callbacks
            regenesisRef <- makeRegenesisRef regenesisFree regenesisPtr
            let notifyCallback = callNotifyCallback notifycbk
            let callbacks =
                    Callbacks
                        { broadcastBlock = \_ _ -> return (),
                          broadcastFinalizationMessage = \_ _ -> return (),
                          broadcastFinalizationRecord = \_ _ -> return (),
                          notifyCatchUpStatus = callCatchUpStatusCallback cucbk,
                          notifyBlockArrived =
                            if notifyContext /= nullPtr
                                then Just $ mkNotifyBlockArrived (notifyCallback notifyContext)
                                else Nothing,
                          notifyBlockFinalized =
                            if notifyContext /= nullPtr
                                then Just $ mkNotifyBlockFinalized (notifyCallback notifyContext)
                                else Nothing,
                          notifyUnsupportedProtocolUpdate =
                            if unsupportedUpdateContext /= nullPtr
                                then
                                    Just $
                                        mkNotifyUnsupportedUpdates $
                                            callNotifyUnsupportedUpdates unsupportedUpdateCallback unsupportedUpdateContext
                                else Nothing,
                          notifyRegenesis = callRegenesisCallback regenesisCB regenesisRef
                        }
            runner <- do
                let config ::
                        MultiVersionConfiguration
                            (NoFinalization ThreadTimer)
                    config = MultiVersionConfiguration{..}
                ConsensusRunner
                    <$> makeMultiVersionRunner config callbacks Nothing logM (Left genesisBS)
            poke runnerPtrPtr =<< newStablePtr runner
            return StartSuccess
      where
        -- Pack the genesis string as a byte string.
        packGenesis cont = do
            cont =<< BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        -- Log method
        logM = toLogMethod maxLogLevel lcbk
        -- Runtime parameters
        mvcRuntimeParameters =
            RuntimeParameters
                { rpBlockSize = fromIntegral maxBlock,
                  rpBlockTimeout = fromIntegral blockConstructionTimeout,
                  rpEarlyBlockThreshold = defaultEarlyBlockThreshold,
                  rpMaxBakingDelay = defaultMaxBakingDelay,
                  rpInsertionsBeforeTransactionPurge = fromIntegral insertionsBeforePurge,
                  rpTransactionsKeepAliveTime = TransactionTime transactionsKeepAlive,
                  rpTransactionsPurgingDelay = fromIntegral transactionsPurgingDelay,
                  rpAccountsCacheSize = fromIntegral accountsCacheSize,
                  rpModulesCacheSize = fromIntegral modulesCacheSize
                }

-- | Shut down consensus, stopping any baker thread if necessary.
--  The pointer is not valid after this function returns.
stopConsensus :: StablePtr ConsensusRunner -> IO ()
stopConsensus cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.shutdownMultiVersionRunner mvr
    freeStablePtr cptr

-- | Start the baker thread.  Calling this mare than once does not start additional baker threads.
startBaker :: StablePtr ConsensusRunner -> IO ()
startBaker cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.startBaker mvr

-- | Stop a baker thread.  The baker thread may be restarted by calling 'startBaker'.
--  This does not otherwise affect the consensus.
stopBaker :: StablePtr ConsensusRunner -> IO ()
stopBaker cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.stopBaker mvr

-- * Receive functions

-- | Result values for receive functions.
--
-- +=======+=============================================+===============================================================================================+==========+
-- | Value |                Name                         |                                              Description                                      | Forward? |
-- +=======+=============================================+===============================================================================================+==========+
-- |     0 | ResultSuccess                               | Message received, validated and processed                                                     | Yes      |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     1 | ResultSerializationFail                     | Message deserialization failed                                                                | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     2 | ResultInvalid                               | The message was determined to be invalid                                                      | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     3 | ResultPendingBlock                          | The message was received, but is awaiting a block to complete processing                      | No for blocks, yes for other messages|
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     4 | ResultPendingFinalization                   | The message was received, but is awaiting a finalization record to complete processing        | Yes      |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     5 | ResultAsync                                 | The message was received, but is being processed asynchronously                               | Yes      |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     6 | ResultDuplicate                             | The message duplicates a previously received message                                          | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     7 | ResultStale                                 | The message may have been valid in the past, but is no longer relevant                        | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     8 | ResultIncorrectFinalizationSession          | The message refers to a different/unknown finalization session                                | No(?)    |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |     9 | ResultUnverifiable                          | The message could not be verified in the current state (initiate catch-up with peer)          | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    10 | ResultContinueCatchUp                       | The peer should be marked pending catch-up if it is currently up-to-date                      | N/A      |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    11 | ResultEarlyBlock                            | The block has a slot number exceeding our current + the early block threshold                 | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    12 | ResultMissingImportFile                     | The file provided for importing doesn't exist                                                 | N/A      |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    13 | ResultConsensusShutDown                     | Consensus has been shut down and the message was ignored                                      | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    14 | ResultExpiryTooLate                         | The transaction expiry time is too far in the future                                          | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    15 | ResultVerificationFailed                    | The transaction signature verification failed                                                 | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    16 | ResultNonexistingSenderAccount              | The transaction's sender account does not exist according to the focus block                  | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    17 | ResultDuplicateNonce                        | The sequence number for this account or update type was already used                          | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    18 | ResultNonceTooLarge                         | The transaction seq. number is larger than the next one for this account/update type          | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    19 | ResultTooLowEnergy                          | The stated transaction energy is lower than the minimum amount necessary to execute it        | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    20 | ResultInvalidGenesisIndex                   | The message is for an unknown genesis index                                                   | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    21 | ResultDuplicateAccountRegistrationID        | The 'CredentialDeployment' contained a duplicate registration id                              | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    22 | ResultCredentialDeploymentInvalidSignatures | The CredentialDeployment contained invalid identity provider signatures                       | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    23 | ResultCredentialDeploymentInvalidIP         | The CredentialDeployment contained an invalid Identity Provider                               | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    24 | ResultCredentialDeploymentInvalidAR         | The CredentialDeployment contained an invalid Anonymity Revoker                               | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    25 | ResultCredentialDeploymentExpired           | The CredentialDeployment contained an expired 'validTo'                                       | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    26 | ResultChainUpdateInvalidEffectiveTime       | The ChainUpdate contained an invalid effective time                                           | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    27 | ChainUpdateSequenceNumberTooOld             | The ChainUpdate contained an old nonce                                                        | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    28 | ResultChainUpdateInvalidSignatures          | The ChainUpdate contained an invalid signature                                                | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    29 | ResultEnergyExceeded                        | The stated energy of the transaction exceeds the maximum allowed                              | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    30 | ResultInsufficientFunds                     | The sender did not have enough funds to cover the costs.                                      | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    31 | ResultDoubleSign                            | The consensus message is a result of malignant double signing.                                | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
-- |    32 | ResultConsensusFailure                      | The consensus has thrown an exception and entered an unrecoverable state.                     | No       |
-- +-------+---------------------------------------------+-----------------------------------------------------------------------------------------------+----------+
type ReceiveResult = Int64

-- | Convert an 'UpdateResult' to the corresponding 'ReceiveResult' value.
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
toReceiveResult ResultExpiryTooLate = 14
toReceiveResult ResultVerificationFailed = 15
toReceiveResult ResultNonexistingSenderAccount = 16
toReceiveResult ResultDuplicateNonce = 17
toReceiveResult ResultNonceTooLarge = 18
toReceiveResult ResultTooLowEnergy = 19
toReceiveResult ResultInvalidGenesisIndex = 20
toReceiveResult ResultDuplicateAccountRegistrationID = 21
toReceiveResult ResultCredentialDeploymentInvalidSignatures = 22
toReceiveResult ResultCredentialDeploymentInvalidIP = 23
toReceiveResult ResultCredentialDeploymentInvalidAR = 24
toReceiveResult ResultCredentialDeploymentExpired = 25
toReceiveResult ResultChainUpdateInvalidEffectiveTime = 26
toReceiveResult ResultChainUpdateSequenceNumberTooOld = 27
toReceiveResult ResultChainUpdateInvalidSignatures = 28
toReceiveResult ResultEnergyExceeded = 29
toReceiveResult ResultInsufficientFunds = 30
toReceiveResult ResultDoubleSign = 31
toReceiveResult ResultConsensusFailure = 32

-- | Handle receipt of a block.
--  The possible return codes are @ResultSuccess@, @ResultSerializationFail@,
--  @ResultInvalid@, @ResultPendingBlock@, @ResultDuplicate@, @ResultStale@,
--  @ResultConsensusShutDown@, @ResultEarlyBlock@, @ResultInvalidGenesisIndex@, and
--  @ResultDoubleSign@. Additionally @ResultConsensusFailure@ is returned if an exception occurs.
--  'receiveBlock' may invoke the callbacks for new finalization messages.
--  If the block was successfully verified i.e. baker signature, finalization proofs etc. then
--  the continuation for executing the block will be written to the 'Ptr' provided.
receiveBlock ::
    -- | Pointer to the multi version runner.
    StablePtr ConsensusRunner ->
    -- | The genesis index.
    GenesisIndex ->
    -- | The message.
    CString ->
    -- | The length of the message.
    Word64 ->
    -- | If the block was received successfully i.e. 'receiveBlock' yields a
    --  'ResultSuccess' then a continuation for executing the block is written to this ptr.
    --  IMPORTANT! If the continuation is present then it must also be called in order
    --  to avoid a memory leak.
    --  The 'StablePtr' is freed in 'executeBlock'.
    Ptr (StablePtr MV.ExecuteBlock) ->
    IO ReceiveResult
receiveBlock bptr genIndex msg msgLen ptrPtrExecuteBlock = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLTrace $ "Received block data, size = " ++ show msgLen ++ "."
    blockBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    (receiveResult, mExecuteBlock) <- runMVR (MV.receiveBlock genIndex blockBS) mvr
    case mExecuteBlock of
        Nothing -> return $ toReceiveResult receiveResult
        Just eb -> do
            poke ptrPtrExecuteBlock =<< newStablePtr eb
            return $ toReceiveResult receiveResult

-- | Execute a block that has been received and successfully verified.
--  The 'MV.ExecuteBlock' continuation is obtained via first calling 'receiveBlock' which in return
--  will construct a pointer to the continuation.
--  The 'StablePtr' is freed here and so this function should only be called once for each 'MV.ExecuteBlock'.
--  The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@
--  and @ResultConsensusShutDown@.
--  Additionally @ResultConsensusFailure@ is returned if an exception occurs.
executeBlock :: StablePtr ConsensusRunner -> StablePtr MV.ExecuteBlock -> IO ReceiveResult
executeBlock ptrConsensus ptrCont = do
    (ConsensusRunner mvr) <- deRefStablePtr ptrConsensus
    executableBlock <- deRefStablePtr ptrCont
    freeStablePtr ptrCont
    mvLog mvr External LLTrace "Executing block."
    res <- runMVR (MV.executeBlock executableBlock) mvr
    return $ toReceiveResult res

-- | Handle receipt of a finalization message.
--  The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
--  @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@, @ResultIncorrectFinalizationSession@,
--  @ResultUnverifiable@, @ResultConsensusShutDown@, @ResultInvalidGenesisIndex@, and @ResultDoubleSign@.
--  Additionally @ResultConsensusFailure@ is returned if an exception occurs.
--  'receiveFinalization' may invoke the callbacks for new finalization messages.
receiveFinalizationMessage ::
    StablePtr ConsensusRunner ->
    GenesisIndex ->
    CString ->
    Int64 ->
    IO ReceiveResult
receiveFinalizationMessage bptr genIndex msg msgLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLTrace $ "Received finalization message, size = " ++ show msgLen ++ "."
    finMsgBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    toReceiveResult <$> runMVR (MV.receiveFinalizationMessage genIndex finMsgBS) mvr

-- | Handle receipt of a finalization record.
--  The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
--  @ResultPendingBlock@, @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@,
--  @ResultConsensusShutDown@ and @ResultInvalidGenesisIndex@.
--  Additionally @ResultConsensusFailure@ is returned if an exception occurs.
--  'receiveFinalizationRecord' may invoke the callbacks for new finalization messages.
receiveFinalizationRecord ::
    StablePtr ConsensusRunner ->
    GenesisIndex ->
    CString ->
    Int64 ->
    IO ReceiveResult
receiveFinalizationRecord bptr genIndex msg msgLen = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLTrace $ "Received finalization record, size = " ++ show msgLen ++ "."
    finRecBS <- BS.packCStringLen (msg, fromIntegral msgLen)
    toReceiveResult <$> runMVR (MV.receiveFinalization genIndex finRecBS) mvr

-- | Handle receipt of a transaction.
--  The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultDuplicate@,
--  @ResultStale@, @ResultInvalid@, @ResultConsensusShutDown@, @ResultExpiryTooLate@, @ResultVerificationFailed@,
--  @ResultNonexistingSenderAccount@, @ResultDuplicateNonce@, @ResultNonceTooLarge@, @ResultTooLowEnergy@,
--  @ResultDuplicateAccountRegistrationID@,
--  @ResultCredentialDeploymentInvalidSignatures@,
--  @ResultCredentialDeploymentInvalidIP@, @ResultCredentialDeploymentInvalidAR@,
--  @ResultCredentialDeploymentExpired@, @ResultChainUpdateInvalidSequenceNumber@,
--  @ResultChainUpdateInvalidEffectiveTime@, @ResultChainUpdateInvalidSignatures@,
--  @ResultEnergyExceeded@.
--  Additionally @ResultConsensusFailure@ is returned if an exception occurs.
receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> Ptr Word8 -> IO ReceiveResult
receiveTransaction bptr transactionData transactionLen outPtr = do
    (ConsensusRunner mvr) <- deRefStablePtr bptr
    mvLog mvr External LLTrace $ "Received transaction, size = " ++ show transactionLen ++ "."
    transactionBS <- BS.packCStringLen (transactionData, fromIntegral transactionLen)
    (mh, ur) <- runMVR (MV.receiveTransaction transactionBS) mvr
    case mh of
        Nothing -> return (toReceiveResult ur)
        Just (TransactionHashV0 (SHA256.Hash h)) -> do
            FBS.withPtrReadOnly h $ \p -> copyBytes outPtr p 32
            return (toReceiveResult ur)

-- | Handle receiving a catch-up status message.
--  If the message is a request, then the supplied callback will be used to
--  send the requested data for the peer.
--  The response code can be:
--  * @ResultSerializationFail@
--  * @ResultInvalid@ -- the catch-up message is inconsistent with the skov
--  * @ResultPendingBlock@ -- the sender has some data I am missing, and should be marked pending
--  * @ResultSuccess@ -- I do not require additional data from the sender, so mark it as up-to-date
--  * @ResultContinueCatchUp@ -- The sender should be marked pending if it is currently up-to-date (no change otherwise)
--  * @ResultConsensusFailure@ -- an internal exception occurred
receiveCatchUpStatus ::
    -- | Consensus pointer
    StablePtr ConsensusRunner ->
    -- | Identifier of peer (passed to callback)
    PeerID ->
    -- | Genesis index
    GenesisIndex ->
    -- | Serialised catch-up message
    CString ->
    -- | Length of message
    Int64 ->
    -- | Limit to number of responses. Limit <= 0 means no messages will be sent.
    Int64 ->
    -- | Callback to receive messages
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

-- | Get a catch-up status message for requesting catch-up with peers.
--  The genesis index and string pointer are loaded into the given pointers.
--  The return value is the length of the string.
--  The string should be freed by calling 'freeCStr'.
getCatchUpStatus ::
    -- | Consensus pointer
    StablePtr ConsensusRunner ->
    -- | Pointer to receive the genesis index
    Ptr GenesisIndex ->
    -- | Pointer to receive the string pointer
    Ptr CString ->
    IO Int64
getCatchUpStatus cptr genIndexPtr resPtr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    (genIndex, resBS) <- runMVR MV.getCatchUpRequest mvr
    poke genIndexPtr genIndex
    poke resPtr =<< toCString resBS
    return (LBS.length resBS)

-- | Import a file consisting of a set of blocks and finalization records for the purposes of
--  out-of-band catch-up.
--  @ResultConsensusFailure@ is returned if an exception occurs.
importBlocks ::
    -- | Consensus runner
    StablePtr ConsensusRunner ->
    -- | File path to import blocks from
    CString ->
    -- | Length of filename
    Int64 ->
    IO Int64
importBlocks cptr fname fnameLen =
    toReceiveResult <$> do
        (ConsensusRunner mvr) <- deRefStablePtr cptr
        theFile <- peekCStringLen (fname, fromIntegral fnameLen)
        runMVR (MV.importBlocks theFile) mvr

-- | Stops importing blocks from a file.
stopImportingBlocks ::
    -- | Consensus runner
    StablePtr ConsensusRunner ->
    IO ()
stopImportingBlocks cptr = mask_ $ do
    ConsensusRunner mvr <- deRefStablePtr cptr
    MV.stopImportingBlocks mvr

-- * Queries

-- | Converts a lazy 'LBS.ByteString' to a null-terminated 'CString'.
--  The string must be freed after use by calling 'free'.
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

-- | Free a 'CString'. This should be called to dispose of any 'CString' values that are returned by
--  queries.
freeCStr :: CString -> IO ()
freeCStr = free

-- | Retrieve the last finalized block height relative to the most recent genesis index. Used for
-- resuming out-of-band catchup.
getLastFinalizedBlockHeight ::
    StablePtr ConsensusRunner ->
    IO Word64
getLastFinalizedBlockHeight cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    theBlockHeight <$> runMVR Q.getLastFinalizedBlockHeight mvr

getNumberOfNonFinalizedTransactions :: StablePtr ConsensusRunner -> IO Word64
getNumberOfNonFinalizedTransactions cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    fromIntegral <$> runMVR Q.getNumberOfNonFinalizedTransactions mvr

-- ** Baker/finalizer status queries

-- | Check if we are members of the finalization committee.
--  Returns 0 for 'False' and 1 for 'True'.
checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8
checkIfWeAreFinalizer cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    res <- runMVR Q.checkIsCurrentFinalizer mvr
    return $! if res then 1 else 0

-- | Check if consensus is running.
--  Returns 0 for 'False' and 1 for 'True'.
checkIfRunning :: StablePtr ConsensusRunner -> IO Word8
checkIfRunning cptr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    res <- runMVR Q.checkIsShutDown mvr
    return $! if res then 0 else 1

-- | Check whether we are a baker from the perspective of the best block.
--  bakerIdPtr expects to receive the baker ID (optional).
--  hasBakerIdPtr expects to receive either 0 (representing false) or 1 (representing true) if a baker ID is not found or found respectively.
--  bakerLotteryPowerPtr expects to receive the lottery power when member of the baking committee.
--  Returns 1 if we are not added as a baker.
--  Returns 2 if we are added as a baker, but not part of the baking committee yet.
--  Returns 3 if we have keys that do not match the baker's public keys on the chain.
--  Returns 0 if we are part of the baking committee.
bakerStatusBestBlock :: StablePtr ConsensusRunner -> Ptr Word64 -> Ptr Word8 -> Ptr Double -> IO Word8
bakerStatusBestBlock cptr bakerIdPtr hasBakerIdPtr bakerLotteryPowerPtr = do
    (ConsensusRunner mvr) <- deRefStablePtr cptr
    (bs, mBid, lotteryPowerMaybe) <- runMVR Q.getBakerStatusBestBlock mvr
    case lotteryPowerMaybe of
        Just lotteryPower -> do
            poke bakerLotteryPowerPtr lotteryPower
        Nothing -> return ()
    case mBid of
        Nothing -> poke hasBakerIdPtr 0 >> (return $! getBakerStatusCode bs)
        Just bid -> do
            poke hasBakerIdPtr 1
            poke bakerIdPtr $ fromIntegral bid
            return $! getBakerStatusCode bs
  where
    getBakerStatusCode :: BakerStatus -> Word8
    getBakerStatusCode bs = case bs of
        ActiveInComittee -> 0
        NotInCommittee -> 1
        AddedButNotActiveInCommittee -> 2
        AddedButWrongKeys -> 3

-- FFI exports
-- Note: Exports must be listed in the lib.def file in order for the symbols to be correctly
-- exposed from the library on Windows.

foreign export ccall
    startConsensus ::
        -- | Maximum block size.
        Word64 ->
        -- | Block construction timeout in milliseconds
        Word64 ->
        -- | Insertions before purging of transactions
        Word64 ->
        -- | Time in seconds during which a transaction can't be purged
        Word64 ->
        -- | Number of seconds between transaction table purging runs
        Word64 ->
        -- | Accounts table cache size
        Word32 ->
        -- | Modules table cache size
        Word32 ->
        -- | Serialized genesis data (c string + len)
        CString ->
        Int64 ->
        -- | Serialized baker identity (c string + len)
        CString ->
        Int64 ->
        Ptr NotifyContext ->
        FunPtr NotifyCallback ->
        -- | Context for when signalling a unsupported protocol update is pending.
        Ptr NotifyUnsupportedUpdatesContext ->
        -- | The callback used to signal a unsupported protocol update is pending.
        FunPtr NotifyUnsupportedUpdatesCallback ->
        -- | Handler for generated messages
        FunPtr BroadcastCallback ->
        -- | Handler for sending catch-up status to peers
        FunPtr CatchUpStatusCallback ->
        -- | Regenesis object
        Ptr RegenesisArc ->
        -- | Finalizer for the regenesis object
        RegenesisFree ->
        -- | Handler for notifying the node of new regenesis blocks
        FunPtr RegenesisCallback ->
        -- | Maximum log level (inclusive) (0 to disable logging).
        Word8 ->
        -- | Handler for log events
        FunPtr LogCallback ->
        -- | FilePath for the AppData directory
        CString ->
        -- | Length of AppData path
        Int64 ->
        -- | Pointer to receive the pointer to the 'ConsensusRunner'.
        Ptr (StablePtr ConsensusRunner) ->
        IO Int64
foreign export ccall
    startConsensusPassive ::
        -- | Maximum block size.
        Word64 ->
        -- | Block construction timeout in milliseconds
        Word64 ->
        -- | Insertions before purging of transactions
        Word64 ->
        -- | Time in seconds during which a transaction can't be purged
        Word64 ->
        -- | Number of seconds between transaction table purging runs
        Word64 ->
        -- | Accounts table cache size
        Word32 ->
        -- | Modules table cache size
        Word32 ->
        -- | Serialized genesis data (c string + len)
        CString ->
        Int64 ->
        Ptr NotifyContext ->
        FunPtr NotifyCallback ->
        -- | Context for when signalling a unsupported protocol update is pending.
        Ptr NotifyUnsupportedUpdatesContext ->
        -- | The callback used to signal a unsupported protocol update is pending.
        FunPtr NotifyUnsupportedUpdatesCallback ->
        -- | Handler for sending catch-up status to peers
        FunPtr CatchUpStatusCallback ->
        -- | Regenesis object
        Ptr RegenesisArc ->
        -- | Finalizer for the regenesis object
        RegenesisFree ->
        -- | Handler for notifying the node of new regenesis blocks
        FunPtr RegenesisCallback ->
        -- | Maximum log level (inclusive) (0 to disable logging).
        Word8 ->
        -- | Handler for log events
        FunPtr LogCallback ->
        -- | FilePath for the AppData directory
        CString ->
        -- | Length of AppData path
        Int64 ->
        -- | Pointer to receive the pointer to the 'ConsensusRunner'.
        Ptr (StablePtr ConsensusRunner) ->
        IO Int64

foreign export ccall stopConsensus :: StablePtr ConsensusRunner -> IO ()
foreign export ccall startBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall stopBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Word64 -> Ptr (StablePtr MV.ExecuteBlock) -> IO Int64
foreign export ccall executeBlock :: StablePtr ConsensusRunner -> StablePtr MV.ExecuteBlock -> IO Int64
foreign export ccall receiveFinalizationMessage :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalizationRecord :: StablePtr ConsensusRunner -> GenesisIndex -> CString -> Int64 -> IO Int64
foreign export ccall receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> Ptr Word8 -> IO Int64

foreign export ccall getNumberOfNonFinalizedTransactions :: StablePtr ConsensusRunner -> IO Word64

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

foreign export ccall getLastFinalizedBlockHeight :: StablePtr ConsensusRunner -> IO Word64

-- baker status checking
foreign export ccall bakerStatusBestBlock :: StablePtr ConsensusRunner -> Ptr Word64 -> Ptr Word8 -> Ptr Double -> IO Word8
foreign export ccall checkIfWeAreFinalizer :: StablePtr ConsensusRunner -> IO Word8
foreign export ccall checkIfRunning :: StablePtr ConsensusRunner -> IO Word8

-- maintenance
foreign export ccall freeCStr :: CString -> IO ()

foreign export ccall importBlocks :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall stopImportingBlocks :: StablePtr ConsensusRunner -> IO ()
