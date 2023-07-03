{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- This flag is here as otherwise ghc reports that
-- 'migrateSkovFromConsensusV0' has redundant constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |This module provides a monad transformer 'SkovV1T' that implements various typeclasses that
-- are required by the consensus version 1 implementation.
-- It also provides functionality for initialising the state for the consensus.
module Concordium.KonsensusV1.SkovMonad where

import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Reader hiding (ask)
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils

import Concordium.GlobalState (GlobalStateInitException (..))
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState

import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.GlobalState.Persistent.TreeState (checkExistingDatabase)
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.LMDB
import Concordium.KonsensusV1.TreeState.StartUp
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.TimerMonad

-- * 'SkovV1T'

-- |The inner type of the 'SkovV1T' monad.
type InnerSkovV1T pv m = RWST (SkovV1Context pv m) () (SkovV1State pv) m

-- |A type-alias that is used for deriving block state monad implementations for 'SkovV1T'.
-- @PersistentBlockStateMonadHelper pv m a@ is representationally equivalent to @SkovV1T pv m a@.
type PersistentBlockStateMonadHelper pv m =
    PersistentBlockStateMonad pv (SkovV1Context pv m) (InnerSkovV1T pv m)

-- |A monad transformer that provides functionality used by the consensus version 1 implementation.
-- This provides the following instances (with suitable conditions on @pv@ and @m@):
--
--   * Monadic behaviour lifted from the underlying monad @m@: 'Functor', 'Applicative', 'Monad',
--     'MonadIO', 'MonadLogger', 'TimeMonad', 'MonadThrow'.
--
--   * @MonadReader (SkovV1Context pv m)@, where the 'SkovV1Context' implements 'HasBlobStore',
--     @'Cache.HasCache' ('AccountCache' (AccountVersionFor pv))@,
--     @'Cache.HasCache' 'Modules.ModuleCache'@, 'HasHandlerContext', 'HasBakerContext', and
--     'HasDatabaseHandlers'.
--
--   * @MonadState ('SkovData' pv)@.
--
--   * Persistent block-state: 'MonadProtocolVersion', 'BlockStateTypes', 'ContractStateOperations',
--     'ModuleQuery', 'AccountOperations', 'BlockStateQuery', 'BlockStateOperations',
--     'BlockStateStorage'.
--
--   * LMDB-backed persistent tree state storage: 'LowLevel.MonadTreeStateStore'.
--
--   * Handlers for broadcasting messages ('MonadBroadcast') and handlers for consensus events
--     'MonadConsensusEvent'. These are implemented by callbacks provided by the 'SkovV1Context'.
--
--   * Timer events ('TimerMonad') implemented by 'ThreadTimer's.
--
--   * Timeout handling ('MonadTimeout'). This is implemented by a resettable timer that invokes
--     'uponTimeoutEvent' on a timeout.
--
--   * Block pointer type ('GlobalStateTypes') and operations ('BlockPointerMonad').
--
--   * AlreadyNotified: Handle that must be called just before notifying the user of an
--     unsupported protocol update.
--     The handle makes sure we do not keep informing the user of the unsupported protocol
--     by returning 'False' the first time it's called and 'True' for subsequent invocations.
newtype SkovV1T pv m a = SkovV1T
    { runSkovT' :: InnerSkovV1T pv m a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadIO,
          MonadLogger,
          TimeMonad,
          MonadReader (SkovV1Context pv m),
          MonadThrow
        )
    deriving
        (BlockStateTypes, ContractStateOperations, ModuleQuery)
        via (PersistentBlockStateMonadHelper pv m)

-- |Run a 'SkovV1T' operation, given the context and state, returning the updated state.
runSkovT :: Monad m => SkovV1T pv m a -> SkovV1Context pv m -> SkovV1State pv -> m (a, SkovV1State pv)
runSkovT comp ctx st = do
    (ret, st', _) <- runRWST (runSkovT' comp) ctx st
    return (ret, st')

-- |Run a 'SkovV1T' operation, given the context and state, discarding the updated state.
-- This can be used for queries, but should generally not be used when the state is updated as
-- changes to the 'SkovData' will be discarded, but changes to the disk-backed databases will
-- persist.
evalSkovT :: Monad m => SkovV1T pv m a -> SkovV1Context pv m -> SkovV1State pv -> m a
evalSkovT comp ctx st = do
    (ret, _) <- evalRWST (runSkovT' comp) ctx st
    return ret

-- |The state used by the 'SkovV1T' monad.
data SkovV1State pv = SkovV1State
    { -- |The 'SkovData', which encapsulates the (in-memory) mutable state of the skov.
      _v1sSkovData :: SkovData pv,
      -- |The timer used for triggering timeout events.
      _v1sTimer :: Maybe ThreadTimer,
      -- |A flag that indicates whether we have already
      -- informed about an unsupported protocol update.
      _notifiedUnsupportedProtocol :: !Bool
    }

-- |Context of callback handlers for the consensus.
data HandlerContext (pv :: ProtocolVersion) m = HandlerContext
    { -- |Handler to broadcast a timeout message.
      _sendTimeoutHandler :: TimeoutMessage -> m (),
      -- |Handler to broadcast a quorum message.
      _sendQuorumHandler :: QuorumMessage -> m (),
      -- |Handler to broadcast a block.
      _sendBlockHandler :: SignedBlock -> m (),
      -- |An event handler called when a block becomes live.
      _onBlockHandler :: BlockPointer pv -> m (),
      -- |An event handler called per finalization. It is called with the
      -- finalization entry, and the list of all blocks finalized by the entry
      -- in increasing order of block height. It returns a `SkovV1T pv m ()` in order to have
      -- access to the state, in particular whether consensus has shutdown.
      _onFinalizeHandler :: FinalizationEntry -> [BlockPointer pv] -> SkovV1T pv m (),
      -- |An event handler called when a pending block becomes live. This is intended to trigger
      -- sending a catch-up status message to peers, as pending blocks are not relayed when they
      -- are first received.
      _onPendingLiveHandler :: m ()
    }

-- |Context used by the 'SkovV1T' monad.
data SkovV1Context (pv :: ProtocolVersion) m = SkovV1Context
    { -- |The baker context (i.e. baker keys if any).
      _vcBakerContext :: !BakerContext,
      -- |Blob store and caches used by the block state storage.
      _vcPersistentBlockStateContext :: !(PersistentBlockStateContext pv),
      -- |In-memory low-level tree state database.
      _vcDisk :: !(DatabaseHandlers pv),
      -- |Handler functions.
      _vcHandlers :: !(HandlerContext pv m),
      -- |A function for unlifting @'SkovV1T' pv m@ into the 'IO' monad.
      -- This is used for implementing asynchronous behaviour, specifically timer events.
      _skovV1TUnliftIO :: forall a. SkovV1T pv m a -> IO a
    }

instance HasBlobStore (SkovV1Context pv m) where
    blobStore = blobStore . _vcPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _vcPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _vcPersistentBlockStateContext

instance AccountVersionFor pv ~ av => Cache.HasCache (AccountCache av) (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

instance Cache.HasCache Modules.ModuleCache (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

-- Note, these template haskell splices go here because of staging restrictions.
-- '_skovV1TUnliftIO' creates a cyclic dependency between 'SkovV1Context' and 'SkovV1T'.
-- The above instances are required by deriving via instances attached to the 'SkovV1T' definition.
makeLenses ''SkovV1Context
makeLenses ''SkovV1State
makeClassy ''HandlerContext

instance HasHandlerContext (SkovV1Context pv m) pv m where
    handlerContext = vcHandlers

instance HasBakerContext (SkovV1Context pv m) where
    bakerContext = vcBakerContext

instance HasDatabaseHandlers (SkovV1Context pv m) pv where
    databaseHandlers = vcDisk

instance (MonadTrans (SkovV1T pv)) where
    lift = SkovV1T . lift

-- |A class used for implementing whether the user
-- has already been informed about an unsupported protocol.
--
-- It is required since SkovV1State is considered an implementation detail
-- of SkovV1T.
--
-- Note that the 'alreadyNotified' function must be called only just
-- before notifying the user.
class Monad m => AlreadyNotified m where
    -- Set the flag that the user has already been notified, and return the old value.
    alreadyNotified :: m Bool

instance (IsProtocolVersion pv, Monad m) => AlreadyNotified (SkovV1T pv m) where
    alreadyNotified = SkovV1T $ do
        notifiedUnsupportedProtocol <<.= True

instance Monad m => MonadState (SkovData pv) (SkovV1T pv m) where
    state = SkovV1T . state . v1sSkovData
    get = SkovV1T (use v1sSkovData)
    put = SkovV1T . (v1sSkovData .=)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        IsProtocolVersion pv => MonadProtocolVersion (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m) => AccountOperations (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m) => BlockStateQuery (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m) => BlockStateOperations (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m) => BlockStateStorage (SkovV1T pv m)

deriving via
    (DiskLLDBM pv (InnerSkovV1T pv m))
    instance
        ( IsProtocolVersion pv,
          MonadIO m,
          MonadCatch m,
          MonadLogger m
        ) =>
        LowLevel.MonadTreeStateStore (SkovV1T pv m)

instance Monad m => MonadBroadcast (SkovV1T pv m) where
    sendTimeoutMessage tm = do
        handler <- view sendTimeoutHandler
        lift $ handler tm
    sendQuorumMessage qm = do
        handler <- view sendQuorumHandler
        lift $ handler qm
    sendBlock sb = do
        handler <- view sendBlockHandler
        lift $ handler sb

instance Monad m => MonadConsensusEvent (SkovV1T pv m) where
    onBlock bp = do
        handler <- view onBlockHandler
        lift $ handler bp
    onFinalize fe bp = do
        handler <- view onFinalizeHandler
        handler fe bp
    onPendingLive = do
        handler <- view onPendingLiveHandler
        lift handler

instance (MonadIO m, MonadLogger m) => TimerMonad (SkovV1T pv m) where
    type Timer (SkovV1T pv m) = ThreadTimer
    onTimeout timeout a = do
        ctx <- ask
        liftIO $
            makeThreadTimer timeout $ do
                let handler (SomeException e) =
                        _skovV1TUnliftIO ctx $
                            logEvent Konsensus LLError $
                                "Error in timer thread: " ++ show e
                void (_skovV1TUnliftIO ctx a) `catchAll` handler
    cancelTimer = liftIO . cancelThreadTimer

instance
    ( MonadIO m,
      MonadLogger m,
      MonadCatch m,
      IsProtocolVersion pv,
      IsConsensusV1 pv,
      TimeMonad m
    ) =>
    MonadTimeout (SkovV1T pv m)
    where
    resetTimer dur = do
        mTimer <- SkovV1T $ use v1sTimer
        mapM_ cancelTimer mTimer
        newTimer <- onTimeout (DelayFor $ durationToNominalDiffTime dur) uponTimeoutEvent
        SkovV1T $ v1sTimer ?=! newTimer
        logEvent Runner LLTrace $ "Timeout reset for " ++ show (durationToNominalDiffTime dur)

instance GlobalStateTypes (SkovV1T pv m) where
    type BlockPointerType (SkovV1T pv m) = BlockPointer pv

instance (IsProtocolVersion pv, MonadIO m, MonadCatch m, MonadLogger m) => BlockPointerMonad (SkovV1T pv m) where
    blockState = return . bpState
    bpParent = parentOf
    bpLastFinalized = lastFinalizedOf

-- * Initialisation

-- |Global state configuration used for initialising the skov state.
data GlobalStateConfig = GlobalStateConfig
    { -- |Runtime parameters.
      gscRuntimeParameters :: !RuntimeParameters,
      -- |Path to the tree state directory.
      gscTreeStateDirectory :: !FilePath,
      -- |Path to the block state file.
      gscBlockStateFile :: !FilePath
    }

-- |Context used by the 'InitMonad'.
data InitContext pv = InitContext
    { -- |Blob store and caches used by the block state storage.
      _icPersistentBlockStateContext :: !(PersistentBlockStateContext pv),
      -- |In-memory low-level tree state database.
      _icDatabaseHandlers :: !(DatabaseHandlers pv)
    }

makeLenses ''InitContext

instance HasBlobStore (InitContext pv) where
    blobStore = blobStore . _icPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _icPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _icPersistentBlockStateContext

instance AccountVersionFor pv ~ av => Cache.HasCache (AccountCache av) (InitContext pv) where
    projectCache = Cache.projectCache . _icPersistentBlockStateContext

instance Cache.HasCache Modules.ModuleCache (InitContext pv) where
    projectCache = Cache.projectCache . _icPersistentBlockStateContext

instance HasDatabaseHandlers (InitContext pv) pv where
    databaseHandlers = icDatabaseHandlers

-- |Inner type of 'InitMonad'.
type InnerInitMonad pv = ReaderT (InitContext pv) LogIO

-- |A monad for initialising the consensus state. Unlike 'SkovV1T', it is not a monad transformer
-- and it does not have a state component. This is necessary to avoid the bootstrapping problem
-- when we initialise the state. It implements a number of the same interfaces:
--
--   * Monadic behaviour: 'Functor', 'Applicative', 'Monad', 'MonadIO', 'MonadLogger', 'TimeMonad',
--     'MonadThrow', 'MonadCatch'.
--
--   * @MonadReader (InitContext pv)@, where the 'InitContext' implements 'HasBlobStore',
--     @'Cache.HasCache' ('AccountCache' (AccountVersionFor pv))@,
--     @'Cache.HasCache' 'Modules.ModuleCache'@, and 'HasDatabaseHandlers'.
--
--   * Blob store: 'MonadBlobStore'.
--
--   * Persistent block-state: 'MonadProtocolVersion', 'BlockStateTypes', 'ContractStateOperations',
--     'ModuleQuery', 'AccountOperations', 'BlockStateQuery', 'BlockStateOperations',
--     'BlockStateStorage'.
--
--   * LMDB-backed persistent tree state storage: 'LowLevel.MonadTreeStateStore'.
newtype InitMonad pv a = InitMonad {runInitMonad' :: InnerInitMonad pv a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadIO,
          MonadLogger,
          TimeMonad,
          MonadReader (InitContext pv),
          MonadThrow,
          MonadCatch
        )
    deriving
        ( BlockStateTypes,
          ContractStateOperations,
          ModuleQuery,
          MonadBlobStore,
          Cache.MonadCache Modules.ModuleCache
        )
        via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))

deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (av ~ AccountVersionFor pv) => Cache.MonadCache (AccountCache av) (InitMonad pv)
deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (IsProtocolVersion pv) => MonadProtocolVersion (InitMonad pv)
deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (IsProtocolVersion pv) => AccountOperations (InitMonad pv)
deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (IsProtocolVersion pv) => BlockStateQuery (InitMonad pv)
deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (IsProtocolVersion pv) => BlockStateOperations (InitMonad pv)
deriving via
    (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))
    instance
        (IsProtocolVersion pv) => BlockStateStorage (InitMonad pv)
deriving via
    (DiskLLDBM pv (InitMonad pv))
    instance
        ( IsProtocolVersion pv
        ) =>
        LowLevel.MonadTreeStateStore (InitMonad pv)

-- |Run an 'InitMonad' in a 'LogIO' context, given the 'InitContext'.
runInitMonad :: InitMonad pv a -> InitContext pv -> LogIO a
runInitMonad = runReaderT . runInitMonad'

-- |The result of successfully loading an existing SkovV1 state.
data ExistingSkov pv m = ExistingSkov
    { -- |The context.
      esContext :: !(SkovV1Context pv m),
      -- |The state.
      esState :: !(SkovV1State pv),
      -- |The hash of the current genesis block.
      esGenesisHash :: !BlockHash,
      -- |The (relative) height of the last finalized block.
      esLastFinalizedHeight :: !BlockHeight,
      -- |The next protocol version if a protocol update has occurred.
      esNextProtocolVersion :: !(Maybe SomeProtocolVersion)
    }

-- |Load an existing SkovV1 state.
-- Returns 'Nothing' if there is no database to load.
-- May throw a 'TreeStateInvariantViolation' if a database invariant violation occurs when
-- attempting to load the state.
initialiseExistingSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    BakerContext ->
    HandlerContext pv m ->
    (forall a. SkovV1T pv m a -> IO a) ->
    GlobalStateConfig ->
    LogIO (Maybe (ExistingSkov pv m))
initialiseExistingSkovV1 bakerCtx handlerCtx unliftSkov GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Attempting to use existing global state."
    existingDB <- checkExistingDatabase gscTreeStateDirectory gscBlockStateFile
    if existingDB
        then do
            pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize gscRuntimeParameters)
            pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize gscRuntimeParameters)
            pbscBlobStore <- liftIO $ loadBlobStore gscBlockStateFile
            let pbsc = PersistentBlockStateContext{..}
            let initWithLLDB lldb = do
                    let initContext = InitContext pbsc lldb
                    initialSkovData <- runInitMonad (loadSkovData gscRuntimeParameters) initContext
                    let !es =
                            ExistingSkov
                                { esContext =
                                    SkovV1Context
                                        { _vcBakerContext = bakerCtx,
                                          _vcPersistentBlockStateContext = pbsc,
                                          _vcDisk = lldb,
                                          _vcHandlers = handlerCtx,
                                          _skovV1TUnliftIO = unliftSkov
                                        },
                                  esState =
                                    SkovV1State
                                        { _v1sSkovData = initialSkovData,
                                          _v1sTimer = Nothing,
                                          _notifiedUnsupportedProtocol = False
                                        },
                                  esGenesisHash = initialSkovData ^. currentGenesisHash,
                                  esLastFinalizedHeight =
                                    blockHeight (initialSkovData ^. lastFinalized),
                                  -- FIXME: Support protocol updates. Issue #825
                                  esNextProtocolVersion = Nothing
                                }
                    return $ Just es
            let initWithBlockState = do
                    (lldb :: DatabaseHandlers pv) <- liftIO $ openDatabase gscTreeStateDirectory
                    initWithLLDB lldb `onException` liftIO (closeDatabase lldb)
            initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)
        else do
            logEvent Skov LLDebug "No existing global state."
            return Nothing

-- |Construct a new SkovV1 state based on the genesis data, initialising the disk storage.
initialiseNewSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    GenesisData pv ->
    BakerContext ->
    HandlerContext pv m ->
    (forall a. SkovV1T pv m a -> IO a) ->
    GlobalStateConfig ->
    LogIO (SkovV1Context pv m, SkovV1State pv)
initialiseNewSkovV1 genData bakerCtx handlerCtx unliftSkov gsConfig@GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Creating new global state."
    pbsc@PersistentBlockStateContext{..} <- newPersistentBlockStateContext gsConfig
    let
        initGS :: InitMonad pv (SkovData pv)
        initGS = do
            logEvent GlobalState LLTrace "Creating persistent global state"
            result <- genesisState genData
            (pbs, genTT) <- case result of
                Left err -> throwM (InvalidGenesisData err)
                Right genState -> return genState
            logEvent GlobalState LLTrace "Writing persistent global state"
            stateRef <- saveBlockState pbs
            logEvent GlobalState LLTrace "Creating persistent global state context"
            let genHash = genesisBlockHash genData
            let genMeta =
                    GenesisMetadata
                        { gmStateHash = getHash pbs,
                          gmParameters = genesisCoreParametersV1 genData,
                          gmFirstGenesisHash = genHash,
                          gmCurrentGenesisHash = genHash
                        }
            chainParams <- getChainParameters pbs
            let genTimeoutDuration =
                    chainParams ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
            genEpochBakers <- genesisEpochBakers pbs
            let !initSkovData =
                    mkInitialSkovData gscRuntimeParameters genMeta pbs genTimeoutDuration genEpochBakers
                        & transactionTable .~ genTT
            let storedGenesis =
                    LowLevel.StoredBlock
                        { stbStatePointer = stateRef,
                          stbInfo = blockMetadata (initSkovData ^. lastFinalized),
                          stbBlock = GenesisBlock genMeta
                        }
            runDiskLLDBM $ initialiseLowLevelDB storedGenesis (initSkovData ^. persistentRoundStatus)
            return initSkovData
    let initWithBlockState = do
            logEvent Skov LLTrace $ "Opening tree state: " ++ gscTreeStateDirectory
            (lldb :: DatabaseHandlers pv) <- liftIO $ openDatabase gscTreeStateDirectory
            logEvent Skov LLTrace "Opened tree state."
            let context = InitContext pbsc lldb
            !initSkovData <- runInitMonad initGS context `onException` liftIO (closeDatabase lldb)
            return
                ( SkovV1Context
                    { _vcBakerContext = bakerCtx,
                      _vcPersistentBlockStateContext = pbsc,
                      _vcDisk = lldb,
                      _vcHandlers = handlerCtx,
                      _skovV1TUnliftIO = unliftSkov
                    },
                  SkovV1State
                    { _v1sSkovData = initSkovData,
                      _v1sTimer = Nothing,
                      _notifiedUnsupportedProtocol = False
                    }
                )
    initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)

-- |Activate the 'SkovV1State' so that it is prepared for active consensus operation.
-- Activation should be performed after initialisation unless the consensus is already shut down
-- (i.e. a protocol update has taken effect to a new consensus).
--
-- This ensures that the last finalized block state is cached and that the transaction table
-- correctly reflects the state of accounts.
activateSkovV1State ::
    (IsProtocolVersion pv) =>
    SkovV1Context pv m ->
    SkovV1State pv ->
    LogIO (SkovV1State pv)
activateSkovV1State ctx uninitState =
    runBlockState $ do
        logEvent GlobalState LLTrace "Caching last finalized block and initializing transaction table"
        let bps = bpState $ _lastFinalized $ _v1sSkovData uninitState
        !tt <- cacheStateAndGetTransactionTable bps
        logEvent GlobalState LLTrace "Done caching last finalized block"
        return $! uninitState & v1sSkovData . transactionTable .~ tt
  where
    runBlockState a = runReaderT (runPersistentBlockStateMonad a) (_vcPersistentBlockStateContext ctx)

-- |Gracefully close the disk storage used by the skov.
shutdownSkovV1 :: SkovV1Context pv m -> LogIO ()
shutdownSkovV1 SkovV1Context{..} = liftIO $ do
    closeBlobStore (pbscBlobStore _vcPersistentBlockStateContext)
    closeDatabase _vcDisk

-- |Migrate a 'ConsensusV0' state to a 'ConsensusV1' state.
-- This function should be used when a protocol update occurs.
--
-- This function performs the migration from the provided @HashedPersistentBlockState lastpv@ and
-- creates a new @HashedPersistentBlockState pv@.
--
-- After the migration is carried out then the new block state is flushed to disk
-- and the new @SkovData pv@ is created.
--
-- The function returns a pair of ('SkovV1Context', 'SkovV1State') suitable for starting the
-- protocol.
--
-- Note that this function does not free any resources with respect to the
-- skov for the @lastpv@. This is the responsibility of the caller.
migrateSkovFromConsensusV0 ::
    forall lastpv pv m.
    ( IsConsensusV0 lastpv,
      IsConsensusV1 pv,
      IsProtocolVersion pv,
      IsProtocolVersion lastpv
    ) =>
    -- |The genesis for the protocol after the protocol update.
    Regenesis pv ->
    -- |The migration.
    StateMigrationParameters lastpv pv ->
    -- |The configuration for the new consensus instance.
    GlobalStateConfig ->
    -- |The old block state context.
    PersistentBlockStateContext lastpv ->
    -- |The old block state
    HashedPersistentBlockState lastpv ->
    -- |The baker context
    BakerContext ->
    -- |The handler context
    HandlerContext pv m ->
    -- |The function for unlifting a 'SkovV1T' into 'IO'.
    -- See documentation for 'SkovV1Context'.
    (forall a. SkovV1T pv m a -> IO a) ->
    -- |Return back the 'SkovV1Context' and the migrated 'SkovV1State'
    LogIO (SkovV1Context pv m, SkovV1State pv)
migrateSkovFromConsensusV0 regenesis migration gsConfig@GlobalStateConfig{..} oldPbsc oldBlockState bakerCtx handlerCtx unliftSkov = do
    pbsc@PersistentBlockStateContext{..} <- newPersistentBlockStateContext gsConfig
    logEvent GlobalState LLDebug "Migrating existing global state."
    newInitialBlockState <- flip runBlobStoreT oldPbsc . flip runBlobStoreT pbsc $ do
        newState <- migratePersistentBlockState migration $ hpbsPointers oldBlockState
        hashBlockState newState
    let
        initGS :: InitMonad pv (SkovData pv)
        initGS = do
            stateRef <- saveBlockState newInitialBlockState
            chainParams <- getChainParameters newInitialBlockState
            genEpochBakers <- genesisEpochBakers newInitialBlockState
            let genMeta = regenesisMetadata (getHash newInitialBlockState) regenesis
            let genTimeoutDuration =
                    chainParams ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
            let !initSkovData =
                    mkInitialSkovData gscRuntimeParameters genMeta newInitialBlockState genTimeoutDuration genEpochBakers
            let storedGenesis =
                    LowLevel.StoredBlock
                        { stbStatePointer = stateRef,
                          stbInfo = blockMetadata (initSkovData ^. lastFinalized),
                          stbBlock = GenesisBlock genMeta
                        }
            runDiskLLDBM $ initialiseLowLevelDB storedGenesis (initSkovData ^. persistentRoundStatus)
            return initSkovData
    let initWithBlockState = do
            (lldb :: DatabaseHandlers pv) <- liftIO $ openDatabase gscTreeStateDirectory
            let context = InitContext pbsc lldb
            logEvent GlobalState LLDebug "Initializing new tree state."
            !initSkovData <- runInitMonad initGS context `onException` liftIO (closeDatabase lldb)
            return
                ( SkovV1Context
                    { _vcBakerContext = bakerCtx,
                      _vcPersistentBlockStateContext = pbsc,
                      _vcDisk = lldb,
                      _vcHandlers = handlerCtx,
                      _skovV1TUnliftIO = unliftSkov
                    },
                  SkovV1State
                    { _v1sSkovData = initSkovData,
                      _v1sTimer = Nothing,
                      _notifiedUnsupportedProtocol = False
                    }
                )
    initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)

-- |Make a new 'PersistentBlockStateContext' based on the
-- 'GlobalStateConfig' passed into this function.
-- This function creates the block state file i.e. the blob store,
-- the account cache and the module cache.
newPersistentBlockStateContext ::
    (IsProtocolVersion pv, MonadIO m) =>
    -- |The global state config to use
    -- for constructing the persistent block state context.
    GlobalStateConfig ->
    -- |The the persistent block state context.
    m (PersistentBlockStateContext pv)
newPersistentBlockStateContext GlobalStateConfig{..} = liftIO $ do
    pbscBlobStore <- createBlobStore gscBlockStateFile
    pbscAccountCache <- newAccountCache $ rpAccountsCacheSize gscRuntimeParameters
    pbscModuleCache <- Modules.newModuleCache $ rpModulesCacheSize gscRuntimeParameters
    return PersistentBlockStateContext{..}
