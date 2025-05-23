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
-- 'migrateSkovV1' has redundant constraints.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides a monad transformer 'SkovV1T' that implements various typeclasses that
--  are required by the consensus version 1 implementation.
--  It also provides functionality for initialising the state for the consensus.
module Concordium.KonsensusV1.SkovMonad where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Reader hiding (ask)
import qualified Data.Sequence as Seq
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Updates
import Concordium.Utils

import Concordium.GlobalState (GlobalStateInitException (..))
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.AccountMap.ModuleMap (MonadModuleMapStore)
import qualified Concordium.GlobalState.BlockState as PBS
import Concordium.GlobalState.Parameters hiding (getChainParameters)
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.GlobalState.Persistent.TreeState (checkExistingDatabase)
import Concordium.GlobalState.TransactionTable
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

-- | A 'HandlerEvent' is used to execute a handler after the current update has been processed.
data HandlerEvent pv
    = -- | Trigger the event handler for a block arriving.
      OnBlock !(BlockPointer pv)
    | -- | Trigger the event handler for a finalization.
      OnFinalize !(FinalizationEntry pv) ![BlockPointer pv]

-- | The inner type of the 'SkovV1T' monad.
type InnerSkovV1T pv m = RWST (SkovV1Context pv m) (Seq.Seq (HandlerEvent pv)) (SkovV1State pv) m

-- | A type-alias that is used for deriving block state monad implementations for 'SkovV1T'.
--  @PersistentBlockStateMonadHelper pv m a@ is representationally equivalent to @SkovV1T pv m a@.
type PersistentBlockStateMonadHelper pv m =
    PersistentBlockStateMonad pv (SkovV1Context pv m) (InnerSkovV1T pv m)

-- | A monad transformer that provides functionality used by the consensus version 1 implementation.
--  This provides the following instances (with suitable conditions on @pv@ and @m@):
--
--    * Monadic behaviour lifted from the underlying monad @m@: 'Functor', 'Applicative', 'Monad',
--      'MonadIO', 'MonadLogger', 'TimeMonad', 'MonadThrow'.
--
--    * @MonadReader (SkovV1Context pv m)@, where the 'SkovV1Context' implements 'HasBlobStore',
--      @'Cache.HasCache' ('AccountCache' (AccountVersionFor pv))@,
--      @'Cache.HasCache' 'Modules.ModuleCache'@, 'HasHandlerContext', 'HasBakerContext',
--      'HasDatabaseHandlers' and 'LMDBAccountMap.HasDatabaseHandlers.
--
--    * @MonadState ('SkovData' pv)@.
--
--    * Persistent block-state: 'MonadProtocolVersion', 'BlockStateTypes', 'ContractStateOperations',
--      'ModuleQuery', 'AccountOperations', 'BlockStateQuery', 'BlockStateOperations',
--      'BlockStateStorage'.
--
--    * LMDB-backed persistent tree state storage: 'LowLevel.MonadTreeStateStore'.
--
--    * Handlers for broadcasting messages ('MonadBroadcast') and handlers for consensus events
--      'MonadConsensusEvent'. These are implemented by callbacks provided by the 'SkovV1Context'.
--
--    * Timer events ('TimerMonad') implemented by 'ThreadTimer's.
--
--    * Timeout handling ('MonadTimeout'). This is implemented by a resettable timer that invokes
--      'uponTimeoutEvent' on a timeout.
--
--    * Block pointer type ('GlobalStateTypes') and operations ('BlockPointerMonad').
--
--    * AlreadyNotified: Handle that must be called just before notifying the user of an
--      unsupported protocol update.
--      The handle makes sure we do not keep informing the user of the unsupported protocol
--      by returning 'False' the first time it's called and 'True' for subsequent invocations.
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
          MonadThrow,
          MonadCatch
        )
    deriving
        (BlockStateTypes, ContractStateOperations, ModuleQuery)
        via (PersistentBlockStateMonadHelper pv m)

-- | Run a 'SkovV1T' operation, given the context and state, returning the updated state and any
--  handler events that were generated.
runSkovT ::
    (Monad m) =>
    SkovV1T pv m a ->
    SkovV1Context pv m ->
    SkovV1State pv ->
    m (a, SkovV1State pv, Seq.Seq (HandlerEvent pv))
runSkovT comp ctx st = do
    (ret, st', evs) <- runRWST (runSkovT' comp) ctx st
    return (ret, st', evs)

-- | Run a 'SkovV1T' operation, given the context and state, discarding the updated state and any
--  handler events that were generated. This can be used for queries, but should generally not be
--  used when the state is updated as changes to the 'SkovData' will be discarded, but changes to
--  the disk-backed databases will persist. It is also expected that no handler events should be
--  generated.
evalSkovT :: (Monad m) => SkovV1T pv m a -> SkovV1Context pv m -> SkovV1State pv -> m a
evalSkovT comp ctx st = do
    (ret, _) <- evalRWST (runSkovT' comp) ctx st
    return ret

-- | The state used by the 'SkovV1T' monad.
data SkovV1State pv = SkovV1State
    { -- | The 'SkovData', which encapsulates the (in-memory) mutable state of the skov.
      _v1sSkovData :: SkovData pv,
      -- | The timer used for triggering timeout events.
      _v1sTimer :: Maybe ThreadTimer,
      -- | If we have already notified about an upcoming protocol update, this indicates the latest
      -- one, and whether it is scheduled for the end of the present epoch.
      _notifiedProtocolUpdate :: !(Maybe (ProtocolUpdate, Bool))
    }

-- | Context of callback handlers for the consensus.
data HandlerContext (pv :: ProtocolVersion) m = HandlerContext
    { -- | Handler to broadcast a timeout message.
      _sendTimeoutHandler :: TimeoutMessage -> m (),
      -- | Handler to broadcast a quorum message.
      _sendQuorumHandler :: QuorumMessage -> m (),
      -- | Handler to broadcast a block.
      _sendBlockHandler :: SignedBlock pv -> m ()
    }

-- | Context used by the 'SkovV1T' monad.
data SkovV1Context (pv :: ProtocolVersion) m = SkovV1Context
    { -- | The baker context (i.e. baker keys if any).
      _vcBakerContext :: !BakerContext,
      -- | Blob store and caches used by the block state storage.
      _vcPersistentBlockStateContext :: !(PersistentBlockStateContext pv),
      -- | low-level tree state database.
      _vcDisk :: !(DatabaseHandlers pv),
      -- | Handler functions.
      _vcHandlers :: !(HandlerContext pv m),
      -- | A function for unlifting @'SkovV1T' pv m@ into the 'IO' monad.
      --  This is used for implementing asynchronous behaviour, specifically timer events.
      _skovV1TUnliftIO :: forall a. SkovV1T pv m a -> IO a
    }

instance HasBlobStore (SkovV1Context pv m) where
    blobStore = blobStore . _vcPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _vcPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _vcPersistentBlockStateContext

instance (AccountVersionFor pv ~ av) => Cache.HasCache (AccountCache av) (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

instance Cache.HasCache Modules.ModuleCache (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

instance LMDBAccountMap.HasDatabaseHandlers (SkovV1Context pv m) where
    databaseHandlers = lens _vcPersistentBlockStateContext (\s v -> s{_vcPersistentBlockStateContext = v}) . LMDBAccountMap.databaseHandlers

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

-- | A class used for implementing whether a protocol update has already been logged.
--
--  It is required since SkovV1State is considered an implementation detail
--  of SkovV1T.
--
--  Note that the 'alreadyNotified' function must be called only just
--  before notifying the user.
class (Monad m) => AlreadyNotified m where
    -- | Record that we have notified about a protocol update, returning 'True' if we
    -- already notified about this update. The second argument indicates if the update
    -- is scheduled for the end of the present epoch.
    alreadyNotified :: ProtocolUpdate -> Bool -> m Bool

instance (IsProtocolVersion pv, Monad m) => AlreadyNotified (SkovV1T pv m) where
    alreadyNotified pu b = SkovV1T $ do
        mOldPU <- notifiedProtocolUpdate <<.= Just (pu, b)
        return $ mOldPU == Just (pu, b)

instance (Monad m) => MonadState (SkovData pv) (SkovV1T pv m) where
    state = SkovV1T . state . v1sSkovData
    get = SkovV1T (use v1sSkovData)
    put = SkovV1T . (v1sSkovData .=)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadLogger m) => MonadProtocolVersion (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m, MonadLogger m) => AccountOperations (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m, MonadLogger m) => BlockStateQuery (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m, MonadLogger m) => BlockStateOperations (SkovV1T pv m)

deriving via
    (PersistentBlockStateMonadHelper pv m)
    instance
        (IsProtocolVersion pv, MonadIO m, MonadLogger m) => BlockStateStorage (SkovV1T pv m)

deriving via
    (DiskLLDBM pv (InnerSkovV1T pv m))
    instance
        ( IsProtocolVersion pv,
          MonadIO m,
          MonadCatch m,
          MonadLogger m
        ) =>
        LowLevel.MonadTreeStateStore (SkovV1T pv m)

deriving via
    (LMDBAccountMap.AccountMapStoreMonad (InnerSkovV1T pv m))
    instance
        ( IsProtocolVersion pv,
          MonadIO m,
          MonadCatch m,
          MonadLogger m
        ) =>
        LMDBAccountMap.MonadAccountMapStore (SkovV1T pv m)

instance (Monad m) => MonadBroadcast (SkovV1T pv m) where
    sendTimeoutMessage tm = do
        handler <- view sendTimeoutHandler
        lift $ handler tm
    sendQuorumMessage qm = do
        handler <- view sendQuorumHandler
        lift $ handler qm
    sendBlock sb = do
        handler <- view sendBlockHandler
        lift $ handler sb

instance (Monad m) => MonadConsensusEvent (SkovV1T pv m) where
    onBlock bp = SkovV1T $ tell $ Seq.singleton $ OnBlock bp
    onFinalize fe bp = SkovV1T $ tell $ Seq.singleton $ OnFinalize fe bp

instance (MonadIO m, MonadLogger m, MonadCatch m) => TimerMonad (SkovV1T pv m) where
    type Timer (SkovV1T pv m) = ThreadTimer
    onTimeout timeout a = do
        ctx <- ask
        liftIO $
            makeThreadTimer timeout $ do
                let handler ex@(SomeException e) = do
                        logEvent Konsensus LLError $
                            "Error in timer thread: " ++ displayException e
                        throwM ex
                _skovV1TUnliftIO ctx (void a `catch` handler)
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

-- | Global state configuration used for initialising the skov state.
data GlobalStateConfig = GlobalStateConfig
    { -- | Runtime parameters.
      gscRuntimeParameters :: !RuntimeParameters,
      -- | Path to the tree state directory.
      gscTreeStateDirectory :: !FilePath,
      -- | Path to the block state file.
      gscBlockStateFile :: !FilePath,
      -- | Account map.
      gscAccountMap :: !LMDBAccountMap.DatabaseHandlers
    }

-- | Context used by the 'InitMonad'.
data InitContext pv = InitContext
    { -- | Blob store and caches used by the block state storage.
      _icPersistentBlockStateContext :: !(PersistentBlockStateContext pv),
      -- | low-level tree state database.
      _icDatabaseHandlers :: !(DatabaseHandlers pv)
    }

makeLenses ''InitContext

instance HasBlobStore (InitContext pv) where
    blobStore = blobStore . _icPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _icPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _icPersistentBlockStateContext

instance (AccountVersionFor pv ~ av) => Cache.HasCache (AccountCache av) (InitContext pv) where
    projectCache = Cache.projectCache . _icPersistentBlockStateContext

instance Cache.HasCache Modules.ModuleCache (InitContext pv) where
    projectCache = Cache.projectCache . _icPersistentBlockStateContext

instance HasDatabaseHandlers (InitContext pv) pv where
    databaseHandlers = icDatabaseHandlers

instance LMDBAccountMap.HasDatabaseHandlers (InitContext pv) where
    databaseHandlers = icPersistentBlockStateContext . LMDBAccountMap.databaseHandlers

-- | Inner type of 'InitMonad'.
type InnerInitMonad pv = ReaderT (InitContext pv) LogIO

-- | A monad for initialising the consensus state. Unlike 'SkovV1T', it is not a monad transformer
--  and it does not have a state component. This is necessary to avoid the bootstrapping problem
--  when we initialise the state. It implements a number of the same interfaces:
--
--    * Monadic behaviour: 'Functor', 'Applicative', 'Monad', 'MonadIO', 'MonadLogger', 'TimeMonad',
--      'MonadThrow', 'MonadCatch'.
--
--    * @MonadReader (InitContext pv)@, where the 'InitContext' implements 'HasBlobStore',
--      @'Cache.HasCache' ('AccountCache' (AccountVersionFor pv))@,
--      @'Cache.HasCache' 'Modules.ModuleCache'@, and 'HasDatabaseHandlers'.
--
--    * Blob store: 'MonadBlobStore'.
--
--    * Persistent block-state: 'MonadProtocolVersion', 'BlockStateTypes', 'ContractStateOperations',
--      'ModuleQuery', 'AccountOperations', 'BlockStateQuery', 'BlockStateOperations',
--      'BlockStateStorage'.
--
--    * LMDB-backed persistent tree state storage: 'LowLevel.MonadTreeStateStore'.
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
          Cache.MonadCache Modules.ModuleCache,
          LMDBAccountMap.MonadAccountMapStore,
          MonadModuleMapStore
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

-- | Run an 'InitMonad' in a 'LogIO' context, given the 'InitContext'.
runInitMonad :: InitMonad pv a -> InitContext pv -> LogIO a
runInitMonad = runReaderT . runInitMonad'

-- | The result of successfully loading an existing SkovV1 state.
data ExistingSkov pv m = ExistingSkov
    { -- | The context.
      esContext :: !(SkovV1Context pv m),
      -- | The state.
      esState :: !(SkovV1State pv),
      -- | The hash of the current genesis block.
      esGenesisHash :: !BlockHash,
      -- | The effective protocol update if one has occurred, together with the relative
      --  block height of the terminal block.
      esProtocolUpdate :: !(Maybe (ProtocolUpdate, BlockHeight))
    }

-- | Internal type used for deriving 'HasDatabaseHandlers' and 'LMDBAccountMap.HasDatabaseHandlers'
--  used for computations where both lmdb databases are required.
data LMDBDatabases pv = LMDBDatabases
    { -- | the skov lmdb database
      _lmdbSkov :: !(DatabaseHandlers pv),
      -- | the account map lmdb database
      _lmdbAccountMap :: !LMDBAccountMap.DatabaseHandlers
    }

makeLenses ''LMDBDatabases

instance HasDatabaseHandlers (LMDBDatabases pv) pv where
    databaseHandlers = lmdbSkov

instance LMDBAccountMap.HasDatabaseHandlers (LMDBDatabases pv) where
    databaseHandlers = lmdbAccountMap

-- | Load an existing SkovV1 state.
--  Returns 'Nothing' if there is no database to load.
--  May throw a 'TreeStateInvariantViolation' if a database invariant violation occurs when
--  attempting to load the state.
initialiseExistingSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    GenesisBlockHeightInfo ->
    BakerContext ->
    HandlerContext pv m ->
    (forall a. SkovV1T pv m a -> IO a) ->
    GlobalStateConfig ->
    LogIO (Maybe (ExistingSkov pv m))
initialiseExistingSkovV1 genesisBlockHeightInfo bakerCtx handlerCtx unliftSkov gsc@GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Attempting to use existing global state."
    existingDB <- checkExistingDatabase gscTreeStateDirectory gscBlockStateFile
    if existingDB
        then do
            pbsc <- newPersistentBlockStateContext False gsc
            let initWithLLDB skovLldb = do
                    checkDatabaseVersion skovLldb
                    let checkBlockState bs = runReaderT (PBS.runPersistentBlockStateMonad (isValidBlobRef bs)) pbsc
                    RollbackResult{..} <-
                        flip runReaderT (LMDBDatabases skovLldb $ pbscAccountMap pbsc) $
                            runDiskLLDBM (rollBackBlocksUntil checkBlockState)
                    when (rbrCount > 0) $ do
                        logEvent Skov LLWarning $
                            "Could not load state for "
                                ++ show rbrCount
                                ++ " blocks. Truncating block state database."
                        liftIO $ truncateBlobStore (bscBlobStore . PBS.pbscBlobStore $ pbsc) rbrBestState
                    let initContext = InitContext pbsc skovLldb
                    (initialSkovData, effectiveProtocolUpdate) <-
                        runInitMonad
                            (loadSkovData genesisBlockHeightInfo gscRuntimeParameters (rbrCount > 0))
                            initContext
                    let !es =
                            ExistingSkov
                                { esContext =
                                    SkovV1Context
                                        { _vcBakerContext = bakerCtx,
                                          _vcPersistentBlockStateContext = pbsc,
                                          _vcDisk = skovLldb,
                                          _vcHandlers = handlerCtx,
                                          _skovV1TUnliftIO = unliftSkov
                                        },
                                  esState =
                                    SkovV1State
                                        { _v1sSkovData = initialSkovData,
                                          _v1sTimer = Nothing,
                                          _notifiedProtocolUpdate = Nothing
                                        },
                                  esGenesisHash = initialSkovData ^. currentGenesisHash,
                                  esProtocolUpdate = effectiveProtocolUpdate
                                }
                    return $ Just es
            let initWithBlockState = do
                    (lldb :: DatabaseHandlers pv) <- liftIO $ openDatabase gscTreeStateDirectory
                    initWithLLDB lldb `onException` liftIO (closeDatabase lldb)
            initWithBlockState `onException` liftIO (closeBlobStore $ pbscBlobStore pbsc)
        else do
            logEvent Skov LLDebug "No existing global state."
            return Nothing

-- | Construct a new SkovV1 state based on the genesis data, initialising the disk storage.
initialiseNewSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    GenesisData pv ->
    GenesisBlockHeightInfo ->
    BakerContext ->
    HandlerContext pv m ->
    (forall a. SkovV1T pv m a -> IO a) ->
    GlobalStateConfig ->
    LogIO (SkovV1Context pv m, SkovV1State pv)
initialiseNewSkovV1 genData genesisBlockHeightInfo bakerCtx handlerCtx unliftSkov gsConfig@GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Creating new global state."
    pbsc@PersistentBlockStateContext{..} <- newPersistentBlockStateContext True gsConfig
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
            saveGlobalMaps pbs
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
                    mkInitialSkovData gscRuntimeParameters genMeta genesisBlockHeightInfo pbs genTimeoutDuration genEpochBakers genTT emptyPendingTransactionTable
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
                      _notifiedProtocolUpdate = Nothing
                    }
                )
    initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)

-- | Activate the 'SkovV1State' so that it is prepared for active consensus operation.
--  Activation should be performed after initialisation unless the consensus is already shut down
--  (i.e. a protocol update has taken effect to a new consensus).
--
--  This ensures that the last finalized block state is cached and that the transaction table
--  correctly reflects the state of accounts. It also loads the certified blocks from disk.
activateSkovV1State ::
    ( MonadLogger m,
      MonadState (SkovData (MPV m)) m,
      MonadThrow m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      LowLevel.MonadTreeStateStore m,
      BlockStateStorage m,
      TimeMonad m,
      MonadIO m
    ) =>
    m ()
activateSkovV1State = do
    logEvent GlobalState LLTrace "Caching last finalized block and initializing transaction table"
    bps <- use $ lastFinalized . to bpState
    !tt <- cacheBlockStateAndGetTransactionTable bps
    transactionTable .= tt
    logEvent GlobalState LLDebug "Initializing LMDB account map and module map"
    void $ PBS.tryPopulateGlobalMaps bps
    logEvent GlobalState LLDebug "Finished initializing LMDB account map and module map"
    logEvent GlobalState LLTrace "Loading certified blocks"
    loadCertifiedBlocks
    logEvent GlobalState LLTrace "Done activating global state"

-- | Gracefully close the disk storage used by the skov.
shutdownSkovV1 :: SkovV1Context pv m -> LogIO ()
shutdownSkovV1 SkovV1Context{..} = liftIO $ do
    closeBlobStore (pbscBlobStore _vcPersistentBlockStateContext)
    closeDatabase _vcDisk

-- | Migrate an existing state to a 'ConsensusV1' state.
--  This function should be used when a protocol update occurs.
--
--  This function performs the migration from the provided @HashedPersistentBlockState lastpv@ and
--  creates a new @HashedPersistentBlockState pv@.
--
--  After the migration is carried out then the new block state is flushed to disk
--  and the new @SkovData pv@ is created.
--
--  The function returns a pair of ('SkovV1Context', 'SkovV1State') suitable for starting the
--  protocol.
--
--  Note that this function does not free any resources with respect to the
--  skov for the @lastpv@. This is the responsibility of the caller.
migrateSkovV1 ::
    forall lastpv pv m.
    ( IsConsensusV1 pv,
      IsProtocolVersion pv,
      IsProtocolVersion lastpv
    ) =>
    -- | Block height information for the genesis block.
    GenesisBlockHeightInfo ->
    -- | The genesis for the protocol after the protocol update.
    Regenesis pv ->
    -- | The migration.
    StateMigrationParameters lastpv pv ->
    -- | The configuration for the new consensus instance.
    GlobalStateConfig ->
    -- | The old block state context.
    PersistentBlockStateContext lastpv ->
    -- | The old block state
    HashedPersistentBlockState lastpv ->
    -- | The baker context
    BakerContext ->
    -- | The handler context
    HandlerContext pv m ->
    -- | The function for unlifting a 'SkovV1T' into 'IO'.
    --  See documentation for 'SkovV1Context'.
    (forall a. SkovV1T pv m a -> IO a) ->
    -- | Transaction table to migrate
    TransactionTable ->
    -- | Pending transaction table to migrate
    PendingTransactionTable ->
    -- | Return back the 'SkovV1Context' and the migrated 'SkovV1State'
    LogIO (SkovV1Context pv m, SkovV1State pv)
migrateSkovV1 genesisBlockHeightInfo regenesis migration gsConfig@GlobalStateConfig{..} oldPbsc oldBlockState bakerCtx handlerCtx unliftSkov migrateTT migratePTT = do
    pbsc@PersistentBlockStateContext{..} <- newPersistentBlockStateContext True gsConfig
    logEvent GlobalState LLDebug "Migrating existing global state."
    let newInitialBlockState :: InitMonad pv (HashedPersistentBlockState pv)
        newInitialBlockState = flip runBlobStoreT oldPbsc . flip runBlobStoreT pbsc $ do
            newState <- migratePersistentBlockState migration $ hpbsPointers oldBlockState
            hashBlockState newState
    let
        initGS :: InitMonad pv (SkovData pv)
        initGS = do
            newState <- newInitialBlockState
            stateRef <- saveBlockState newState
            chainParams <- getChainParameters newState
            genEpochBakers <- genesisEpochBakers newState
            let genMeta = regenesisMetadata (getHash newState) regenesis
            let genTimeoutDuration =
                    chainParams ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
            let !initSkovData =
                    mkInitialSkovData gscRuntimeParameters genMeta genesisBlockHeightInfo newState genTimeoutDuration genEpochBakers migrateTT migratePTT
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
                      _notifiedProtocolUpdate = Nothing
                    }
                )
    initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)

-- | Make a new 'PersistentBlockStateContext' based on the
--  'GlobalStateConfig' passed into this function.
--  This function creates the block state file (the blob store) if @True@ is passed in,
--  otherwise it tries to reuse an existing blob store.
--  New account cache and the module cache are created.
newPersistentBlockStateContext ::
    (IsProtocolVersion pv, MonadIO m) =>
    -- | Whether a new blobstore should be created or a current one should be reused.
    Bool ->
    -- | The global state config to use
    --  for constructing the persistent block state context.
    GlobalStateConfig ->
    -- | The the persistent block state context.
    m (PersistentBlockStateContext pv)
newPersistentBlockStateContext initialize GlobalStateConfig{..} = liftIO $ do
    pbscBlobStore <- if initialize then createBlobStore gscBlockStateFile else loadBlobStore gscBlockStateFile
    pbscAccountCache <- newAccountCache $ rpAccountsCacheSize gscRuntimeParameters
    pbscModuleCache <- Modules.newModuleCache $ rpModulesCacheSize gscRuntimeParameters
    let pbscAccountMap = gscAccountMap
    return PersistentBlockStateContext{..}
