{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DerivingStrategies, DerivingVia, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE LambdaCase, RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Concordium.Skov.MonadImplementations where

import Control.Monad
import Control.Monad.Trans.State.Strict hiding (gets)
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Lens.Micro.Platform

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Rust.TreeState as Rust
import qualified Concordium.GlobalState.Rust.Block as Rust
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.Skov.Monad
import Concordium.Skov.Query
import Concordium.Skov.Update
import Concordium.Skov.Hooks
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer

-- |This wrapper endows a monad that implements 'TreeStateMonad' with
-- an instance of 'SkovQueryMonad'.
newtype TSSkovWrapper m a = TSSkovWrapper {runTSSkovWrapper :: m a}
    deriving (Functor, Applicative, Monad, BlockStateOperations, BlockStateQuery, TreeStateMonad, TimeMonad, LoggerMonad)
type instance BlockPointer (TSSkovWrapper m) = BlockPointer m
type instance UpdatableBlockState (TSSkovWrapper m) = UpdatableBlockState m
type instance PendingBlock (TSSkovWrapper m) = PendingBlock m

instance (TreeStateMonad m) => SkovQueryMonad (TSSkovWrapper m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    {-# INLINE isFinalized #-}
    isFinalized = doIsFinalized
    {-# INLINE lastFinalizedBlock #-}
    lastFinalizedBlock = fst <$> getLastFinalized
    {-# INLINE getBirkParameters #-}
    getBirkParameters = doGetBirkParameters
    {-# INLINE getGenesisData #-}
    getGenesisData = Concordium.GlobalState.TreeState.getGenesisData
    {-# INLINE genesisBlock #-}
    genesisBlock = getGenesisBlockPointer
    {-# INLINE getCurrentHeight #-}
    getCurrentHeight = doGetCurrentHeight
    {-# INLINE branchesFromTop #-}
    branchesFromTop = doBranchesFromTop
    {-# INLINE getBlocksAtHeight #-}
    getBlocksAtHeight = doGetBlocksAtHeight

newtype TSSkovUpdateWrapper r w s m a = TSSkovUpdateWrapper {runTSSkovUpdateWrapper :: m a}
    deriving (Functor, Applicative, Monad, BlockStateOperations,
            BlockStateQuery, TreeStateMonad, TimeMonad, LoggerMonad,
            MonadReader r, MonadWriter w, MonadState s, MonadIO, OnSkov)
    deriving SkovQueryMonad via (TSSkovWrapper m)
type instance BlockPointer (TSSkovUpdateWrapper r w s m) = BlockPointer m
type instance UpdatableBlockState (TSSkovUpdateWrapper r w s m) = UpdatableBlockState m
type instance PendingBlock (TSSkovUpdateWrapper r w s m) = PendingBlock m

instance (TimeMonad m, LoggerMonad m, TreeStateMonad m, MonadReader r m, MonadIO m,
        MonadState s m, MonadWriter w m, MissingEvent w, OnSkov m)
            => SkovMonad (TSSkovUpdateWrapper r w s m) where
    storeBlock = doStoreBlock
    storeBakedBlock = doStoreBakedBlock
    receiveTransaction tr = doReceiveTransaction tr 0
    finalizeBlock = doFinalizeBlock

-- |The 'SkovQueryM' wraps 'StateT' to provide an instance of 'SkovQueryMonad'
-- when the state implements 'SkovLenses'.
newtype SkovQueryM s m a = SkovQueryM {runSkovQueryM :: StateT s m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState s)
    deriving BlockStateQuery via (Rust.SkovTreeState s (StateT s m))
    deriving BlockStateOperations via (Rust.SkovTreeState s (StateT s m))
    deriving TreeStateMonad via (Rust.SkovTreeState s (StateT s m))
    deriving SkovQueryMonad via (TSSkovWrapper (Rust.SkovTreeState s (StateT s m)))
-- UndecidableInstances is required to allow these type instance declarations.
type instance BlockPointer (SkovQueryM s m) = BlockPointer (Rust.SkovTreeState s (StateT s m))
type instance UpdatableBlockState (SkovQueryM s m) = UpdatableBlockState (Rust.SkovTreeState s (StateT s m))
type instance PendingBlock (SkovQueryM s m) = PendingBlock (Rust.SkovTreeState s (StateT s m))



-- |Evaluate an action in the 'SkovQueryM'.  This is intended for
-- running queries against the state (i.e. with no updating side-effects).
evalSkovQueryM :: (Monad m) => SkovQueryM s m a -> s -> m a
evalSkovQueryM (SkovQueryM a) st = evalStateT a st


-- * Without transaction hooks

-- |Skov state with passive finalizion.
-- This keeps finalization messages, but does not process them.
data SkovPassiveState = SkovPassiveState {
    _spsSkov :: !Rust.SkovData,
    _spsFinalization :: !PassiveFinalizationState
}
makeLenses ''SkovPassiveState

instance Rust.SkovLenses SkovPassiveState where
    skov = spsSkov
instance PassiveFinalizationStateLenses SkovPassiveState where
    pfinState = spsFinalization
instance FinalizationQuery SkovPassiveState where
    getPendingFinalizationMessages = getPendingFinalizationMessages . _spsFinalization
    getCurrentFinalizationPoint = getCurrentFinalizationPoint . _spsFinalization

initialSkovPassiveState :: GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> SkovPassiveState
initialSkovPassiveState gen initBS gsptr = SkovPassiveState{..}
    where
        _spsSkov = Rust.initialSkovData gen initBS gsptr
        _spsFinalization = initialPassiveFinalizationState (bpHash (Rust._skovGenesisBlockPointer _spsSkov))

newtype SkovPassiveM m a = SkovPassiveM {unSkovPassiveM :: RWST () (Endo [SkovMissingEvent]) SkovPassiveState m a}
    deriving (Functor, Applicative, Monad, MonadReader (), TimeMonad, LoggerMonad, MonadState SkovPassiveState, MonadWriter (Endo [SkovMissingEvent]), MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Rust.SkovTreeState SkovPassiveState (SkovPassiveM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper () (Endo [SkovMissingEvent]) SkovPassiveState (SkovPassiveM m))
type instance UpdatableBlockState (SkovPassiveM m) = Basic.BlockState
type instance BlockPointer (SkovPassiveM m) = Rust.BlockPointer
type instance PendingBlock (SkovPassiveM m) = Rust.PendingBlock

instance Monad m => OnSkov (SkovPassiveM m) where
    {-# INLINE onBlock #-}
    onBlock _ = return ()
    {-# INLINE onFinalize #-}
    onFinalize fr _ = spsFinalization %= execState (passiveNotifyBlockFinalized fr)

evalSkovPassiveM :: (Monad m) => SkovPassiveM m a -> GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> m a
evalSkovPassiveM (SkovPassiveM a) gd bs0 gsptr = fst <$> evalRWST a () (initialSkovPassiveState gd bs0 gsptr)

runSkovPassiveM :: SkovPassiveM m a -> SkovPassiveState -> m (a, SkovPassiveState, Endo [SkovMissingEvent])
runSkovPassiveM (SkovPassiveM a) s = runRWST a () s


-- |Skov state with active finalization.
data SkovActiveState = SkovActiveState {
    _sasSkov :: !Rust.SkovData,
    _sasFinalization :: !FinalizationState
}
makeLenses ''SkovActiveState

instance Rust.SkovLenses SkovActiveState where
    skov = sasSkov
instance FinalizationStateLenses SkovActiveState where
    finState = sasFinalization
deriving via (FinalizationStateQuery SkovActiveState) instance FinalizationQuery SkovActiveState

initialSkovActiveState :: FinalizationInstance -> GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> SkovActiveState
initialSkovActiveState finInst gen initBS gsptr = SkovActiveState{..}
    where
        _sasSkov = Rust.initialSkovData gen initBS gsptr
        _sasFinalization = initialFinalizationState finInst (bpHash (Rust._skovGenesisBlockPointer _sasSkov)) (genesisFinalizationParameters gen)

newtype SkovActiveM m a = SkovActiveM {unSkovActiveM :: RWST FinalizationInstance (Endo [SkovFinalizationEvent]) SkovActiveState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovActiveState, MonadReader FinalizationInstance, MonadWriter (Endo [SkovFinalizationEvent]), MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Rust.SkovTreeState SkovActiveState (SkovActiveM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper FinalizationInstance (Endo [SkovFinalizationEvent]) SkovActiveState (SkovActiveM m) )
type instance UpdatableBlockState (SkovActiveM m) = Basic.BlockState
type instance BlockPointer (SkovActiveM m) = Rust.BlockPointer
type instance PendingBlock (SkovActiveM m) = Rust.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovActiveM m) where
    {-# INLINE onBlock #-}
    onBlock = notifyBlockArrival
    {-# INLINE onFinalize #-}
    onFinalize = notifyBlockFinalized
instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovActiveState (SkovActiveM m) where
    broadcastFinalizationMessage = tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    requestMissingFinalization = notifyMissingFinalization . Right
    requestMissingBlock bh = notifyMissingBlock bh 0
    requestMissingBlockDescendant = notifyMissingBlock
    getFinalizationInstance = ask

runSkovActiveM :: SkovActiveM m a -> FinalizationInstance -> SkovActiveState -> m (a, SkovActiveState, Endo [SkovFinalizationEvent])
runSkovActiveM (SkovActiveM a) fi fs = runRWST a fi fs

-- |Skov state with buffered finalization.
data SkovBufferedState = SkovBufferedState {
    _sbsSkov :: !Rust.SkovData,
    _sbsFinalization :: !FinalizationState,
    _sbsBuffer :: !FinalizationBuffer
}
makeLenses ''SkovBufferedState

instance Rust.SkovLenses SkovBufferedState where
    skov = sbsSkov
instance FinalizationStateLenses SkovBufferedState where
    finState = sbsFinalization
instance FinalizationBufferLenses SkovBufferedState where
    finBuffer = sbsBuffer
deriving via (FinalizationStateQuery SkovBufferedState) instance FinalizationQuery SkovBufferedState

initialSkovBufferedState :: FinalizationInstance -> GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> SkovBufferedState
initialSkovBufferedState finInst gen initBS gsptr = SkovBufferedState{..}
    where
        _sbsSkov = Rust.initialSkovData gen initBS gsptr
        _sbsFinalization = initialFinalizationState finInst (bpHash (Rust._skovGenesisBlockPointer _sbsSkov)) (genesisFinalizationParameters gen)
        _sbsBuffer = emptyFinalizationBuffer

newtype SkovBufferedM m a = SkovBufferedM {unSkovBufferedM :: RWST FinalizationInstance (Endo [BufferedSkovFinalizationEvent]) SkovBufferedState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovBufferedState, MonadReader FinalizationInstance, MonadWriter (Endo [BufferedSkovFinalizationEvent]), MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Rust.SkovTreeState SkovBufferedState (SkovBufferedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper FinalizationInstance (Endo [BufferedSkovFinalizationEvent]) SkovBufferedState (SkovBufferedM m) )
type instance UpdatableBlockState (SkovBufferedM m) = Basic.BlockState
type instance BlockPointer (SkovBufferedM m) = Rust.BlockPointer
type instance PendingBlock (SkovBufferedM m) = Rust.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovBufferedM m) where
    {-# INLINE onBlock #-}
    onBlock = notifyBlockArrival
    {-# INLINE onFinalize #-}
    onFinalize = notifyBlockFinalized
instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovBufferedState (SkovBufferedM m) where
    broadcastFinalizationMessage msg = bufferFinalizationMessage msg >>= \case
            Left n -> tell $ embedNotifyEvent n
            Right msgs -> forM_ msgs $ tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    requestMissingFinalization = notifyMissingFinalization . Right
    requestMissingBlock bh = notifyMissingBlock bh 0
    requestMissingBlockDescendant = notifyMissingBlock
    getFinalizationInstance = ask

runSkovBufferedM :: SkovBufferedM m a -> FinalizationInstance -> SkovBufferedState -> m (a, SkovBufferedState, Endo [BufferedSkovFinalizationEvent])
runSkovBufferedM (SkovBufferedM a) fi fs = runRWST a fi fs


-- * With transaction hooks

-- |Skov state with passive finalizion and transaction hooks.
-- This keeps finalization messages, but does not process them.
data SkovPassiveHookedState = SkovPassiveHookedState {
    _sphsSkov :: !Rust.SkovData,
    _sphsFinalization :: !PassiveFinalizationState,
    _sphsHooks :: !TransactionHooks
}
makeLenses ''SkovPassiveHookedState

instance Rust.SkovLenses SkovPassiveHookedState where
    skov = sphsSkov
instance PassiveFinalizationStateLenses SkovPassiveHookedState where
    pfinState = sphsFinalization
instance TransactionHookLenses SkovPassiveHookedState where
    hooks = sphsHooks
instance FinalizationQuery SkovPassiveHookedState where
    getPendingFinalizationMessages = getPendingFinalizationMessages . _sphsFinalization
    getCurrentFinalizationPoint = getCurrentFinalizationPoint . _sphsFinalization

initialSkovPassiveHookedState :: GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> SkovPassiveHookedState
initialSkovPassiveHookedState gen initBS gsptr = SkovPassiveHookedState{..}
    where
        _sphsSkov = Rust.initialSkovData gen initBS gsptr
        _sphsFinalization = initialPassiveFinalizationState (bpHash (Rust._skovGenesisBlockPointer _sphsSkov))
        _sphsHooks = emptyHooks

newtype SkovPassiveHookedM m a = SkovPassiveHookedM {unSkovPassiveHookedM :: RWST () (Endo [SkovMissingEvent]) SkovPassiveHookedState m a}
    deriving (Functor, Applicative, Monad, MonadReader (), TimeMonad, LoggerMonad, MonadState SkovPassiveHookedState, MonadWriter (Endo [SkovMissingEvent]), MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Rust.SkovTreeState SkovPassiveHookedState (SkovPassiveHookedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper () (Endo [SkovMissingEvent]) SkovPassiveHookedState (SkovPassiveHookedM m))
type instance UpdatableBlockState (SkovPassiveHookedM m) = Basic.BlockState
type instance BlockPointer (SkovPassiveHookedM m) = Rust.BlockPointer
type instance PendingBlock (SkovPassiveHookedM m) = Rust.PendingBlock

instance (TimeMonad m, MonadIO m, LoggerMonad m) => OnSkov (SkovPassiveHookedM m) where
    {-# INLINE onBlock #-}
    onBlock bp = hookOnBlock bp
    {-# INLINE onFinalize #-}
    onFinalize fr bp = do
        sphsFinalization %= execState (passiveNotifyBlockFinalized fr)
        hookOnFinalize fr bp

evalSkovPassiveHookedM :: (Monad m) => SkovPassiveHookedM m a -> GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> m a
evalSkovPassiveHookedM (SkovPassiveHookedM a) gd bs0 gsptr = fst <$> evalRWST a () (initialSkovPassiveHookedState gd bs0 gsptr)

runSkovPassiveHookedM :: SkovPassiveHookedM m a -> SkovPassiveHookedState -> m (a, SkovPassiveHookedState, Endo [SkovMissingEvent])
runSkovPassiveHookedM (SkovPassiveHookedM a) s = runRWST a () s

-- |Skov state with buffered finalization and transaction hooks.
data SkovBufferedHookedState = SkovBufferedHookedState {
    _sbhsSkov :: !Rust.SkovData,
    _sbhsFinalization :: !FinalizationState,
    _sbhsBuffer :: !FinalizationBuffer,
    _sbhsHooks :: !TransactionHooks
}
makeLenses ''SkovBufferedHookedState

instance Rust.SkovLenses SkovBufferedHookedState where
    skov = sbhsSkov
instance FinalizationStateLenses SkovBufferedHookedState where
    finState = sbhsFinalization
instance FinalizationBufferLenses SkovBufferedHookedState where
    finBuffer = sbhsBuffer
instance TransactionHookLenses SkovBufferedHookedState where
    hooks = sbhsHooks
deriving via (FinalizationStateQuery SkovBufferedHookedState) instance FinalizationQuery SkovBufferedHookedState

initialSkovBufferedHookedState :: FinalizationInstance -> GenesisData -> Basic.BlockState -> Rust.GlobalStatePtr -> SkovBufferedHookedState
initialSkovBufferedHookedState finInst gen initBS gsptr = SkovBufferedHookedState{..}
    where
        _sbhsSkov = Rust.initialSkovData gen initBS gsptr
        _sbhsFinalization = initialFinalizationState finInst (bpHash (Rust._skovGenesisBlockPointer _sbhsSkov)) (genesisFinalizationParameters gen)
        _sbhsBuffer = emptyFinalizationBuffer
        _sbhsHooks = emptyHooks

newtype SkovBufferedHookedM m a = SkovBufferedHookedM {unSkovBufferedHookedM :: RWST FinalizationInstance (Endo [BufferedSkovFinalizationEvent]) SkovBufferedHookedState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovBufferedHookedState, MonadReader FinalizationInstance, MonadWriter (Endo [BufferedSkovFinalizationEvent]), MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Rust.SkovTreeState SkovBufferedHookedState (SkovBufferedHookedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper FinalizationInstance (Endo [BufferedSkovFinalizationEvent]) SkovBufferedHookedState (SkovBufferedHookedM m) )
type instance UpdatableBlockState (SkovBufferedHookedM m) = Basic.BlockState
type instance BlockPointer (SkovBufferedHookedM m) = Rust.BlockPointer
type instance PendingBlock (SkovBufferedHookedM m) = Rust.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovBufferedHookedM m) where
    {-# INLINE onBlock #-}
    onBlock bp = do
        notifyBlockArrival bp
        hookOnBlock bp
    {-# INLINE onFinalize #-}
    onFinalize bp fr = do
        notifyBlockFinalized bp fr
        hookOnFinalize bp fr
instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovBufferedHookedState (SkovBufferedHookedM m) where
    broadcastFinalizationMessage msg = bufferFinalizationMessage msg >>= \case
            Left n -> tell $ embedNotifyEvent n
            Right msgs -> forM_ msgs $ tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    requestMissingFinalization = notifyMissingFinalization . Right
    requestMissingBlock bh = notifyMissingBlock bh 0
    requestMissingBlockDescendant = notifyMissingBlock
    getFinalizationInstance = ask

runSkovBufferedHookedM :: SkovBufferedHookedM m a -> FinalizationInstance -> SkovBufferedHookedState -> m (a, SkovBufferedHookedState, Endo [BufferedSkovFinalizationEvent])
runSkovBufferedHookedM (SkovBufferedHookedM a) fi fs = runRWST a fi fs
