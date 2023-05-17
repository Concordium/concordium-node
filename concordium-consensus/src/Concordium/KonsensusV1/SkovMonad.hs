{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.SkovMonad where

import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Reader hiding (ask)
import Lens.Micro.Platform

import Concordium.GlobalState (GlobalStateInitException (..))
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
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
import Concordium.Scheduler.Types hiding (getChainParameters)
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Types.HashableTo

type PersistentBlockStateMonadHelper pv m =
    PersistentBlockStateMonad
        pv
        (SkovV1Context pv m)
        (RWST (SkovV1Context pv m) () (SkovV1State pv) m)

newtype SkovV1T pv m a = SkovV1T
    { runSkovT' :: RWST (SkovV1Context pv m) () (SkovV1State pv) m a
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
    deriving (BlockStateTypes, ContractStateOperations, ModuleQuery) via (PersistentBlockStateMonadHelper pv m)

runSkovT :: Monad m => SkovV1T pv m a -> SkovV1Context pv m -> SkovV1State pv -> m (a, SkovV1State pv)
runSkovT comp ctx st = do
    (ret, st', _) <- runRWST (runSkovT' comp) ctx st
    return (ret, st')

evalSkovT :: Monad m => SkovV1T pv m a -> SkovV1Context pv m -> SkovV1State pv -> m a
evalSkovT comp ctx st = do
    (ret, _) <- evalRWST (runSkovT' comp) ctx st
    return ret

data SkovV1State pv = SkovV1State
    { _v1sSkovData :: SkovData pv,
      _v1sTimer :: Maybe ThreadTimer
    }

data HandlerContext (pv :: ProtocolVersion) m = HandlerContext
    { _sendTimeoutHandler :: TimeoutMessage -> m (),
      _sendQuorumHandler :: QuorumMessage -> m (),
      _sendBlockHandler :: SignedBlock -> m (),
      _onBlockHandler :: BlockPointer pv -> m (),
      -- |An event handler called per finalization. It is called with the
      -- finalization entry, and the list of all blocks finalized by the entry
      -- in increasing order of block height.
      _onFinalizeHandler :: FinalizationEntry -> [BlockPointer pv] -> m (),
      _onPendingLiveHandler :: m ()
    }

data SkovV1Context (pv :: ProtocolVersion) m = SkovV1Context
    { -- |The baker context (i.e. baker keys if any).
      _vcBakerContext :: !BakerContext,
      -- |Blob store and caches used by the block state storage.
      _vcPersistentBlockStateContext :: !(PersistentBlockStateContext pv),
      -- |In-memory low-level tree state database.
      _vcDisk :: !(DatabaseHandlers pv),
      _vcHandlers :: !(HandlerContext pv m),
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
    (DiskLLDBM pv (RWST (SkovV1Context pv m) () (SkovV1State pv) m))
    instance
        ( IsProtocolVersion pv,
          MonadIO m,
          MonadCatch m,
          MonadLogger m
        ) =>
        LowLevel.MonadTreeStateStore (SkovV1T pv m)

instance Monad m => MonadMulticast (SkovV1T pv m) where
    sendTimeoutMessage tm = do
        handler <- view sendTimeoutHandler
        lift $ handler tm
    sendQuorumMessage qm = do
        handler <- view sendQuorumHandler
        lift $ handler qm
    sendBlock sb = do
        handler <- view sendBlockHandler
        lift $ handler sb

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
                return ()
    cancelTimer = liftIO . cancelThreadTimer

instance Monad m => MonadConsensusEvent (SkovV1T pv m) where
    onBlock bp = do
        handler <- view onBlockHandler
        lift $ handler bp
    onFinalize fe bp = do
        handler <- view onFinalizeHandler
        lift $ handler fe bp
    onPendingLive = do
        handler <- view onPendingLiveHandler
        lift handler

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
        SkovV1T $ v1sTimer ?= newTimer
        logEvent Runner LLTrace $ "Timeout reset for " ++ show (durationToNominalDiffTime dur)

instance GlobalStateTypes (SkovV1T pv m) where
    type BlockPointerType (SkovV1T pv m) = BlockPointer pv

instance (IsProtocolVersion pv, MonadIO m, MonadCatch m, MonadLogger m) => BlockPointerMonad (SkovV1T pv m) where
    blockState = return . bpState
    bpParent = parentOf
    bpLastFinalized = lastFinalizedOf

data GlobalStateConfig = GlobalStateConfig
    { gscRuntimeParameters :: !RuntimeParameters,
      gscTreeStateDirectory :: !FilePath,
      gscBlockStateFile :: !FilePath
    }

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

type InnerInitMonad pv = ReaderT (InitContext pv) LogIO

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
        (BlockStateTypes, ContractStateOperations, ModuleQuery, MonadBlobStore, Cache.MonadCache Modules.ModuleCache)
        via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv))

deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (av ~ AccountVersionFor pv) => Cache.MonadCache (AccountCache av) (InitMonad pv)
deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (IsProtocolVersion pv) => MonadProtocolVersion (InitMonad pv)
deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (IsProtocolVersion pv) => AccountOperations (InitMonad pv)
deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (IsProtocolVersion pv) => BlockStateQuery (InitMonad pv)
deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (IsProtocolVersion pv) => BlockStateOperations (InitMonad pv)
deriving via (PersistentBlockStateMonad pv (InitContext pv) (InnerInitMonad pv)) instance (IsProtocolVersion pv) => BlockStateStorage (InitMonad pv)
deriving via
    (DiskLLDBM pv (InitMonad pv))
    instance
        ( IsProtocolVersion pv
        ) =>
        LowLevel.MonadTreeStateStore (InitMonad pv)

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
                                          _v1sTimer = Nothing
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

initialiseNewSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    GenesisData pv ->
    BakerContext ->
    HandlerContext pv m ->
    (forall a. SkovV1T pv m a -> IO a) ->
    GlobalStateConfig ->
    LogIO (SkovV1Context pv m, SkovV1State pv)
initialiseNewSkovV1 genData bakerCtx handlerCtx unliftSkov GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Creating new global state."
    pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize gscRuntimeParameters)
    pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize gscRuntimeParameters)
    pbscBlobStore <- liftIO $ createBlobStore gscBlockStateFile
    let pbsc = PersistentBlockStateContext{..}
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
            let genTimeoutDuration = chainParams ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
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
                      _v1sTimer = Nothing
                    }
                )
    initWithBlockState `onException` liftIO (closeBlobStore pbscBlobStore)

activateSkovV1State :: (IsProtocolVersion pv) => SkovV1Context pv m -> SkovV1State pv -> LogIO (SkovV1State pv)
activateSkovV1State ctx uninitState =
    runBlockState $ do
        logEvent GlobalState LLTrace "Caching last finalized block and initializing transaction table"
        let bps = bpState $ _lastFinalized $ _v1sSkovData uninitState
        !tt <- cacheStateAndGetTransactionTable bps
        logEvent GlobalState LLTrace "Done caching last finalized block"
        return $! uninitState & v1sSkovData . transactionTable .~ tt
  where
    runBlockState a = runReaderT (runPersistentBlockStateMonad a) (_vcPersistentBlockStateContext ctx)

shutdownSkovV1 :: SkovV1Context pv m -> LogIO ()
shutdownSkovV1 SkovV1Context{..} = liftIO $ do
    closeBlobStore (pbscBlobStore _vcPersistentBlockStateContext)
    closeDatabase _vcDisk
