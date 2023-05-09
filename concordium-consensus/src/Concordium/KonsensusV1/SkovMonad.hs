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
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Module
import qualified Concordium.GlobalState.Persistent.Cache as Cache
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

data SkovV1Context (pv :: ProtocolVersion) m = SkovV1Context
    { -- |The baker context (i.e. baker keys if any).
      _vcBakerContext :: !BakerContext,
      -- |Blob store and caches used by the block state storage.
      _vcPersistentBlockStateContext :: PersistentBlockStateContext pv,
      -- |In-memory low-level tree state database.
      _vcDisk :: !(DatabaseHandlers pv),
      _sendTimeoutHandler :: TimeoutMessage -> m (),
      _sendQuorumHandler :: QuorumMessage -> m (),
      _sendBlockHandler :: SignedBlock -> m (),
      _onBlockHandler :: BlockPointer pv -> m (),
      _onFinalizeHandler :: FinalizationEntry -> BlockPointer pv -> m (),
      _onPendingLiveHandler :: m (),
      _skovV1TUnliftIO :: forall a. SkovV1T pv m a -> IO a
    }

instance HasBlobStore (SkovV1Context pv m) where
    blobStore = blobStore . _vcPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _vcPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _vcPersistentBlockStateContext

instance AccountVersionFor pv ~ av => Cache.HasCache (AccountCache av) (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

instance Cache.HasCache Module.ModuleCache (SkovV1Context pv m) where
    projectCache = Cache.projectCache . _vcPersistentBlockStateContext

makeLenses ''SkovV1Context
makeLenses ''SkovV1State

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
