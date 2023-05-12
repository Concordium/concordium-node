{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.KonsensusV1.SkovMonad where

import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.TreeState (checkExistingDatabase)
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.LMDB
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.Logger
import Concordium.Scheduler.Types
import Concordium.TimeMonad
import Concordium.TimerMonad

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
      _onFinalizeHandler :: FinalizationEntry -> BlockPointer pv -> m (),
      _onPendingLiveHandler :: m ()
    }

data SkovV1Context (pv :: ProtocolVersion) m = SkovV1Context
    { -- |The baker context (i.e. baker keys if any).
      _vcBakerContext :: !BakerContext,
      -- |Blob store and caches used by the block state storage.
      _vcPersistentBlockStateContext :: PersistentBlockStateContext pv,
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

instance MonadIO m => TimerMonad (SkovV1T pv m) where
    type Timer (SkovV1T pv m) = ThreadTimer
    onTimeout timeout a = do
        unlifter <- view skovV1TUnliftIO
        liftIO $
            makeThreadTimer timeout $
                void $
                    unlifter a
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
      IsConsensusV1 pv
    ) =>
    MonadTimeout (SkovV1T pv m)
    where
    resetTimer dur = do
        mTimer <- SkovV1T $ use v1sTimer
        mapM_ cancelTimer mTimer
        newTimer <- onTimeout (DelayFor $ durationToNominalDiffTime dur) uponTimeoutEvent
        SkovV1T $ v1sTimer ?= newTimer

instance GlobalStateTypes (SkovV1T pv m) where
    type BlockPointerType (SkovV1T pv m) = BlockPointer pv

instance (IsProtocolVersion pv, MonadIO m, MonadCatch m, MonadLogger m) => BlockPointerMonad (SkovV1T pv m) where
    blockState = return . bpState
    bpParent = parentOf
    bpLastFinalized = lastFinalizedOf

-- data SkovConfig (pv :: ProtocolVersion) finconfig handlerconfig = SkovConfig !GlobalStateConfig !finconfig !handlerconfig

data GlobalStateConfig = GlobalStateConfig
    { dtdbRuntimeParameters :: !RuntimeParameters,
      dtdbTreeStateDirectory :: !FilePath,
      dtdbBlockStateFile :: !FilePath
    }

-- initialiseExistingGlobalStateV1 :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> GlobalStateConfig -> LogIO (Maybe (GSContext pv, GSState pv))
-- initialiseExistingGlobalStateV1 _ GlobalStateConfig{..} = do
--     -- check if all the necessary database files exist
--     existingDB <- checkExistingDatabase dtdbTreeStateDirectory dtdbBlockStateFile
--     if existingDB
--         then do
--             logm <- ask
--             liftIO $ do
--                 pbscAccountCache <- newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
--                 pbscModuleCache <- Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
--                 pbscBlobStore <- loadBlobStore dtdbBlockStateFile
--                 let pbsc = PersistentBlockStateContext{..}
--                 skovData <-
--                     runLoggerT (loadSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory pbsc) logm
--                         `onException` closeBlobStore pbscBlobStore
--                 return (Just (pbsc, skovData))
--         else return Nothing

initialiseExistingSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv, Monad m) =>
    BakerContext ->
    HandlerContext pv m ->
    GlobalStateConfig ->
    LogIO
        (Maybe (SkovV1Context pv m, SkovV1State pv))
initialiseExistingSkovV1 bakerContext handlerContext GlobalStateConfig{..} = do
    logEvent Skov LLDebug "Attempting to use existing global state."
    existingDB <- checkExistingDatabase dtdbTreeStateDirectory dtdbBlockStateFile
    if existingDB
        then do
            logm <- ask
            liftIO $ do
                pbscAccountCache <- newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
                pbscModuleCache <- Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
                pbscBlobStore <- loadBlobStore dtdbBlockStateFile
                let pbsc = PersistentBlockStateContext{..}
                skovData <-
                    runLoggerT (loadSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory pbsc) logm
                        `onException` closeBlobStore pbscBlobStore
                return (Just (pbsc, skovData))
        else do
            logEvent Skov LLDebug "No existing global state."
            return Nothing

-- initialiseExistingGlobalState (protocolVersion @pv) gsc >>= \case
--     Nothing -> do
--         logEvent Skov LLDebug "No existing global state."
--         return Nothing
--     Just (c, s) -> do
--         (finctx, finst) <- evalGlobalStateM @pv (initialiseFinalization finconf) c s
--         logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
--         logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
--         let (hctx, hst) = initialiseHandler hconf
--         return (Just (SkovV1Context c finctx hctx, SkovV1State s finst hst))

initialiseNewSkovV1 ::
    forall pv m.
    (IsProtocolVersion pv, IsConsensusV1 pv) =>
    GenesisData pv ->
    -- SkovConfig pv finconfig handlerconfig ->
    LogIO (SkovV1Context pv m, SkovV1State pv)
initialiseNewSkovV1 genData = undefined
