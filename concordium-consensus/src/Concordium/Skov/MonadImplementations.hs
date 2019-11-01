{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, DerivingStrategies, DerivingVia, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, StandaloneDeriving, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase, RecordWildCards, TupleSections, ConstraintKinds, FunctionalDependencies, PartialTypeSignatures, QuantifiedConstraints, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, CPP #-}
module Concordium.Skov.MonadImplementations where

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Lens.Micro.Platform
import Data.Time.Clock (NominalDiffTime)
import Data.Semigroup
import Data.Proxy
import Data.Functor.Compose

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState
import Concordium.Types.HashableTo
import Concordium.Types
import Concordium.GlobalState.Basic.Block (Block(GenesisBlock))
import Concordium.Skov.Monad
import Concordium.Skov.Query
import Concordium.Skov.Update
import Concordium.Skov.Hooks
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer

class (
        HasGlobalStateContext (SkovGSContext c) (SkovContext c),
        HasGlobalState (SkovGSState c) (SkovState c)
        ) => SkovConfiguration c where
    data SkovContext c
    data SkovState c
    type SkovGSState c
    type SkovGSContext c
    initialiseSkov :: c -> IO (SkovContext c, SkovState c)
    shutdownSkov :: SkovContext c -> SkovState c -> IO ()


class SkovTimerHandlers h c m | h -> m c where
    type SkovHandlerTimer h
    handleOnTimeout :: h -> Timeout -> SkovT h c m a -> m (SkovHandlerTimer h)
    handleCancelTimer :: h -> SkovHandlerTimer h -> m ()

class SkovFinalizationHandlers h m where
    handleBroadcastFinalizationMessage :: h -> FinalizationPseudoMessage -> m ()
    handleBroadcastFinalizationRecord :: h -> FinalizationRecord -> m ()

data SkovHandlers t c m = SkovHandlers {
    shBroadcastFinalizationMessage :: FinalizationPseudoMessage -> m (),
    shBroadcastFinalizationRecord :: FinalizationRecord -> m (),
    shOnTimeout :: forall a. Timeout -> SkovT (SkovHandlers t c m) c m a -> m t,
    shCancelTimer :: t -> m ()
}

instance SkovFinalizationHandlers (SkovHandlers t c m) m where
    handleBroadcastFinalizationMessage SkovHandlers{..} = shBroadcastFinalizationMessage
    handleBroadcastFinalizationRecord SkovHandlers{..} = shBroadcastFinalizationRecord

instance SkovTimerHandlers (SkovHandlers t c m) c m where
    type SkovHandlerTimer (SkovHandlers t c m) = t
    handleOnTimeout SkovHandlers{..} = shOnTimeout
    handleCancelTimer SkovHandlers{..} = shCancelTimer

newtype SkovT h c m a = SkovT { runSkovT' :: h -> SkovContext c -> StateT (SkovState c) m a }
    deriving (Functor, Applicative, Monad, MonadState (SkovState c), MonadIO, LoggerMonad, TimeMonad)
        via (ReaderT h (ReaderT (SkovContext c) (StateT (SkovState c) m)))

runSkovT :: SkovT h c m a -> h -> SkovContext c -> SkovState c -> m (a, SkovState c)
runSkovT (SkovT a) h c = runStateT (a h c)

evalSkovT :: (Monad m) => SkovT h c m a -> h -> SkovContext c -> SkovState c -> m a
evalSkovT (SkovT a) h c = evalStateT (a h c)

instance (Monad m) => MonadReader (SkovContext c) (SkovT h c m) where
    ask = SkovT (\_ c -> return c)
    local f (SkovT a) = SkovT (\h -> a h . f)
    {-# INLINE ask #-}
    {-# INLINE local #-}

instance MonadTrans (SkovT h c) where
    lift a = SkovT (\_ _ -> lift a)
    {-# INLINE lift #-}

instance (Monad m, SkovTimerHandlers h c m) => TimerMonad (SkovT h c m) where
    type Timer (SkovT h c m) = SkovHandlerTimer h
    onTimeout timeout a = SkovT (\h _ -> lift (handleOnTimeout h timeout a))
    cancelTimer t = SkovT (\h _ -> lift (handleCancelTimer h t))

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance BlockStateTypes (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (Monad m, BlockStateQuery (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateQuery (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (Monad m, BlockStateOperations (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateOperations (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (Monad m, BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateStorage (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (GlobalStateTypes (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => GlobalStateTypes (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (
        Monad m,
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
        ) => TreeStateMonad (SkovT h c m)

instance (
        Monad m,
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
        ) => SkovQueryMonad (SkovT h c m) where
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

instance (
        Monad m,
        TimeMonad m,
        LoggerMonad m,
        OnSkov (SkovT h c m),
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
        ) => SkovMonad (SkovT h c m) where
    {-# INLINE storeBlock #-}
    storeBlock = doStoreBlock
    {-# INLINE storeBakedBlock #-}
    storeBakedBlock = doStoreBakedBlock
    {-# INLINE receiveTransaction #-}
    receiveTransaction tr = doReceiveTransaction tr 0
    {-# INLINE finalizeBlock #-}
    finalizeBlock = doFinalizeBlock

class HasFinalizationInstance f where
    finalizationInstance :: f -> FinalizationInstance
instance HasFinalizationInstance FinalizationInstance where
    finalizationInstance = id
    {-# INLINE finalizationInstance #-}

class FinalizationConfig c where
    type FCContext c
    type FCState c
    initialiseFinalization :: c -> (FCContext c, FCState c)

class (FinalizationConfig c, Monad m) => FinalizationConfigHandlers c m | m -> c where
    finalizationOnBlock :: (BlockPointerData bs bp) => bp -> m ()
    finalizationOnFinalize :: (BlockPointerData bs bp) => FinalizationRecord -> bp -> m ()
    proxyFinalizationMessage :: (FinalizationMessage -> m ()) -> FinalizationMessage -> m ()

instance (
        FinalizationStateLenses (SkovState c) (Timer (SkovT h c m)),
        HasFinalizationInstance (SkovContext c),
        FinalizationConfigHandlers c (SkovT h c m),
        SkovFinalizationHandlers h m,
        SkovTimerHandlers h c m,
        MonadIO m,
        TimeMonad m,
        LoggerMonad m,
        SkovMonad (SkovT h c m))
        => FinalizationMonad (SkovState c) (SkovT h c m) where
    broadcastFinalizationMessage = proxyFinalizationMessage (\msg' -> SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h (FPMMessage msg')))
    broadcastFinalizationPseudoMessage (FPMMessage msg) = broadcastFinalizationMessage msg
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)
    broadcastFinalizationRecord r = SkovT (\h _ -> lift $ handleBroadcastFinalizationRecord h r)
    getFinalizationInstance = asks finalizationInstance
    {-# INLINE broadcastFinalizationMessage #-}
    {-# INLINE broadcastFinalizationPseudoMessage #-}
    {-# INLINE broadcastFinalizationRecord #-}
    {-# INLINE getFinalizationInstance #-}



data SkovConfig gsconfig finconfig handlerconfig = SkovConfig gsconfig !finconfig !handlerconfig


data NoFinalization = NoFinalization

instance FinalizationConfig (SkovConfig gsconf NoFinalization hconf) where
    type FCContext (SkovConfig gsconf NoFinalization hconf) = ()
    type FCState (SkovConfig gsconf NoFinalization hconf) = ()
    initialiseFinalization _ = ((), ())
    {-# INLINE initialiseFinalization #-}

instance (Monad m) => FinalizationConfigHandlers (SkovConfig gsconf NoFinalization hconf) (SkovT h (SkovConfig gsconf NoFinalization hconf) m) where
    finalizationOnBlock = \_ -> return ()
    finalizationOnFinalize = \_ _ -> return ()
    proxyFinalizationMessage = \_ _ -> return ()
    {-# INLINE finalizationOnBlock #-}
    {-# INLINE finalizationOnFinalize #-}
    {-# INLINE proxyFinalizationMessage #-}

data ActiveFinalization (t :: *) = ActiveFinalization !FinalizationInstance !GenesisData

instance FinalizationConfig (SkovConfig gc (ActiveFinalization t) hc) where
    type FCContext (SkovConfig gc (ActiveFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gc (ActiveFinalization t) hc) = FinalizationState t
    initialiseFinalization (SkovConfig _ (ActiveFinalization finInst genData) _)
            = (finInst, initialFinalizationState finInst genHash finParams)
            where
                genHash = getHash (GenesisBlock genData)
                finParams = genesisFinalizationParameters genData
    {-# INLINE initialiseFinalization #-}

instance (
        Monad m,
        FinalizationMonad (SkovState (SkovConfig gc (ActiveFinalization t) hc)) (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m)
        ) => FinalizationConfigHandlers (SkovConfig gc (ActiveFinalization t) hc) (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m) where
    finalizationOnBlock = notifyBlockArrival
    finalizationOnFinalize = notifyBlockFinalized
    proxyFinalizationMessage = id
    {-# INLINE finalizationOnBlock #-}
    {-# INLINE finalizationOnFinalize #-}
    {-# INLINE proxyFinalizationMessage #-}

data BufferedFinalization (t :: *) = BufferedFinalization !FinalizationInstance !GenesisData

data BufferedFinalizationState t = BufferedFinalizationState {
        _bfsFinalization :: !(FinalizationState t),
        _bfsBuffer :: !FinalizationBuffer
    }
makeLenses ''BufferedFinalizationState

instance FinalizationStateLenses (BufferedFinalizationState t) t where
    finState = bfsFinalization
instance FinalizationBufferLenses (BufferedFinalizationState t) where
    finBuffer = bfsBuffer

instance FinalizationConfig (SkovConfig gc (BufferedFinalization t) hc) where
    type FCContext (SkovConfig gc (BufferedFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gc (BufferedFinalization t) hc) = BufferedFinalizationState t
    initialiseFinalization (SkovConfig _ (BufferedFinalization finInst genData) _)
            = (finInst, BufferedFinalizationState (initialFinalizationState finInst genHash finParams) emptyFinalizationBuffer)
            where
                genHash = getHash (GenesisBlock genData)
                finParams = genesisFinalizationParameters genData
    {-# INLINE initialiseFinalization #-}

instance (
        Monad m,
        FinalizationBufferLenses (SkovState (SkovConfig gc (BufferedFinalization t) hc)),
        FinalizationMonad (SkovState (SkovConfig gc (BufferedFinalization t) hc)) (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m)
        ) => FinalizationConfigHandlers (SkovConfig gc (BufferedFinalization t) hc) (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m) where
    finalizationOnBlock = notifyBlockArrival
    finalizationOnFinalize = notifyBlockFinalized
    proxyFinalizationMessage = bufferFinalizationMessage
    {-# INLINE finalizationOnBlock #-}
    {-# INLINE finalizationOnFinalize #-}
    {-# INLINE proxyFinalizationMessage #-}


class HandlerConfig c where
    handlerLogTransfer :: Proxy c -> HCContext c -> Maybe (LogTransferMethod IO)
    type HCContext c
    type HCState c
    initialiseHandler :: c -> (HCContext c, HCState c)

class (Monad m, HandlerConfig c) => HandlerConfigHandlers c m | m -> c where
    handleBlock :: (SkovMonad m, TreeStateMonad m) => BlockPointer m -> m ()
    handleFinalize :: (SkovMonad m, TreeStateMonad m) => FinalizationRecord -> BlockPointer m -> m ()


data NoHandler = NoHandler

instance HandlerConfig (SkovConfig gc fc NoHandler) where
    handlerLogTransfer = \_ _ -> Nothing
    type HCContext (SkovConfig gc fc NoHandler) = ()
    type HCState (SkovConfig gc fc NoHandler) = ()
    initialiseHandler = \_ -> ((),())

instance Monad m => HandlerConfigHandlers (SkovConfig gc fc NoHandler) (SkovT h (SkovConfig gc fc NoHandler) m) where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ -> return ()


data HookLogHandler = HookLogHandler (Maybe (LogTransferMethod IO))

instance HandlerConfig (SkovConfig gc fc HookLogHandler) where
    handlerLogTransfer = \_ -> id
    type HCContext (SkovConfig gc fc HookLogHandler) = Maybe (LogTransferMethod IO)
    type HCState (SkovConfig gc fc HookLogHandler) = TransactionHooks
    initialiseHandler (SkovConfig _ _ (HookLogHandler logH)) = (logH, emptyHooks)

instance Monad m => HandlerConfigHandlers (SkovConfig gc fc HookLogHandler) (SkovT h (SkovConfig gc fc HookLogHandler) m) where
    handleBlock = hookOnBlock
    handleFinalize = hookOnFinalize

instance (GlobalStateConfig gsconf,
        FinalizationConfig (SkovConfig gsconf finconf hconf),
        HandlerConfig (SkovConfig gsconf finconf hconf))
        => SkovConfiguration (SkovConfig gsconf finconf hconf) where
    data SkovContext (SkovConfig gsconf finconf hconf) = SkovContext {
            scGSContext :: GSContext gsconf,
            scFinContext :: FCContext (SkovConfig gsconf finconf hconf),
            scHandlerContext :: HCContext (SkovConfig gsconf finconf hconf)
        }
    data SkovState (SkovConfig gsconf finconf hconf) = SkovState {
            ssGSState :: GSState gsconf,
            ssFinState :: FCState (SkovConfig gsconf finconf hconf),
            ssHandlerState :: HCState (SkovConfig gsconf finconf hconf)
        }
    type SkovGSState (SkovConfig gsconf finconf hconf) = GSState gsconf
    type SkovGSContext (SkovConfig gsconf finconf hconf) = GSContext gsconf
    initialiseSkov conf@(SkovConfig gsc _ _) = do
        (c, s) <- initialiseGlobalState gsc
        let (finctx, finst) = initialiseFinalization conf
        let (hctx, hst) = initialiseHandler conf
        return (SkovContext c finctx hctx, SkovState s finst hst)
    shutdownSkov (SkovContext c _ _) (SkovState s _ _) = shutdownGlobalState (Proxy :: Proxy gsconf) c s

instance (FinalizationStateLenses (FCState (SkovConfig gsconf finconf hconf)) t)
        => FinalizationStateLenses (SkovState (SkovConfig gsconf finconf hconf)) t where
    finState = lens ssFinState (\s fs -> s {ssFinState = fs}) . finState
    {-# INLINE finState #-}

instance (FinalizationBufferLenses (FCState (SkovConfig gsconf finconf hconf)))
        => FinalizationBufferLenses (SkovState (SkovConfig gsconf finconf hconf)) where
    finBuffer = lens ssFinState (\s fs -> s {ssFinState = fs}) . finBuffer
    {-# INLINE finBuffer #-}

instance (HasFinalizationInstance (FCContext (SkovConfig gsconf finconf hconf)))
        => HasFinalizationInstance (SkovContext (SkovConfig gsconf finconf hconf)) where
    finalizationInstance = finalizationInstance . scFinContext

instance TransactionHookLenses (SkovState (SkovConfig gc fc HookLogHandler)) where
    hooks = lens ssHandlerState (\s v -> s {ssHandlerState = v})

instance (c ~ GSContext gsconf) => HasGlobalStateContext c (SkovContext (SkovConfig gsconf finconf hconf)) where
    globalStateContext = lens (scGSContext) (\sc v -> sc{scGSContext = v})
instance (g ~ GSState gsconf) => HasGlobalState g (SkovState (SkovConfig gsconf finconf hconf)) where
    globalState = lens (ssGSState) (\ss v -> ss {ssGSState = v})

instance (MonadIO m,
        FinalizationConfigHandlers (SkovConfig gsconf finconf hconf) (SkovT h (SkovConfig gsconf finconf hconf) m),
        HandlerConfigHandlers (SkovConfig gsconf finconf hconf) (SkovT h (SkovConfig gsconf finconf hconf) m),
        GlobalStateTypes (SkovT h (SkovConfig gsconf finconf hconf) m),
        SkovMonad (SkovT h (SkovConfig gsconf finconf hconf) m),
        TreeStateMonad (SkovT h (SkovConfig gsconf finconf hconf) m))
        => OnSkov (SkovT h (SkovConfig gsconf finconf hconf) m) where
    onBlock bp = finalizationOnBlock bp >> handleBlock bp
    onFinalize fr bp = finalizationOnFinalize fr bp >> handleFinalize fr bp
    logTransfer = fmap liftLM . handlerLogTransfer (Proxy :: Proxy (SkovConfig gsconf finconf hconf)) <$> asks scHandlerContext
        where
            liftLM lm bh slot reason = liftIO $ lm bh slot reason
    {-# INLINE onBlock #-}
    {-# INLINE onFinalize #-}
    {-# INLINE logTransfer #-}

type SkovConfigMonad h c m = (SkovConfiguration c,
        OnSkov (SkovT h c m),
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    )

type SkovQueryConfigMonad c m = 
    (TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT () c m)),
    BlockStateStorage (GlobalStateM0 (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT () c m))
    )

type SkovFinalizationConfigMonad h c m = (
    SkovConfigMonad h c m,
    FinalizationStateLenses (SkovState c) (Timer (SkovT h c m)),
    HasFinalizationInstance (SkovContext c),
    FinalizationConfigHandlers c (SkovT h c m)
    )

{-
type MyConfig = (SkovConfig DiskTreeDiskBlockConfig NoFinalization NoHandler)

foo :: MonadIO m => BlockHash -> SkovT h MyConfig m Bool
foo bh = do
    r <- resolveBlock bh
    case r of
        Nothing -> return False
        _ -> return True
-}

{-

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

newtype TSSkovUpdateWrapper s m a = TSSkovUpdateWrapper {runTSSkovUpdateWrapper :: m a}
    deriving (Functor, Applicative, Monad, BlockStateOperations,
            BlockStateQuery, TreeStateMonad, TimeMonad, LoggerMonad,
            MonadState s, MonadIO)
    deriving SkovQueryMonad via (TSSkovWrapper m)
type instance BlockPointer (TSSkovUpdateWrapper s m) = BlockPointer m
type instance UpdatableBlockState (TSSkovUpdateWrapper s m) = UpdatableBlockState m
type instance PendingBlock (TSSkovUpdateWrapper s m) = PendingBlock m

instance (Monad m, OnSkov m) => OnSkov (TSSkovUpdateWrapper s m) where
  {-# INLINE onBlock #-}
  onBlock bp = TSSkovUpdateWrapper (onBlock bp)
  {-# INLINE onFinalize #-}
  onFinalize fin bp = TSSkovUpdateWrapper (onFinalize fin bp)
  {-# INLINE logTransfer #-}
  logTransfer = TSSkovUpdateWrapper $ do
    logTransfer >>= \case
      Nothing -> return Nothing
      Just lm -> return (Just (\bh slot reason -> TSSkovUpdateWrapper (lm bh slot reason)))

instance (TimeMonad m, LoggerMonad m, TreeStateMonad m, MonadIO m,
        MonadState s m, OnSkov m)
            => SkovMonad (TSSkovUpdateWrapper s m) where
    storeBlock = doStoreBlock
    storeBakedBlock = doStoreBakedBlock
    receiveTransaction tr = doReceiveTransaction tr 0
    finalizeBlock = doFinalizeBlock

-- |The 'SkovQueryM' wraps 'StateT' to provide an instance of 'SkovQueryMonad'
-- when the state implements 'SkovLenses'.
newtype SkovQueryM s m a = SkovQueryM {runSkovQueryM :: StateT s m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState s)
    deriving BlockStateQuery via (Impl.SkovTreeState s (StateT s m))
    deriving BlockStateOperations via (Impl.SkovTreeState s (StateT s m))
    deriving TreeStateMonad via (Impl.SkovTreeState s (StateT s m))
    deriving SkovQueryMonad via (TSSkovWrapper (Impl.SkovTreeState s (StateT s m)))
-- UndecidableInstances is required to allow these type instance declarations.
type instance BlockPointer (SkovQueryM s m) = BlockPointer (Impl.SkovTreeState s (StateT s m))
type instance UpdatableBlockState (SkovQueryM s m) = UpdatableBlockState (Impl.SkovTreeState s (StateT s m))
type instance PendingBlock (SkovQueryM s m) = PendingBlock (Impl.SkovTreeState s (StateT s m))



-- |Evaluate an action in the 'SkovQueryM'.  This is intended for
-- running queries against the state (i.e. with no updating side-effects).
evalSkovQueryM :: (Monad m) => SkovQueryM s m a -> s -> m a
evalSkovQueryM (SkovQueryM a) st = evalStateT a st

-- * Without transaction hooks

-- |Skov state without finalizion.
data SkovPassiveState = SkovPassiveState {
    _spsSkov :: !Impl.SkovData,
    _spsFinalization :: !PassiveFinalizationState
}
makeLenses ''SkovPassiveState

instance Impl.SkovLenses SkovPassiveState where
    skov = spsSkov
instance PassiveFinalizationStateLenses SkovPassiveState where
    pfinState = spsFinalization

#ifdef RUST
initialSkovPassiveState :: RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> IO SkovPassiveState
initialSkovPassiveState rtParams gen initBS gsptr = do
  _spsSkov <- Impl.initialSkovData rtParams gen initBS gsptr
  let _spsFinalization = initialPassiveFinalizationState (bpHash (Impl._skovGenesisBlockPointer _spsSkov))
  return SkovPassiveState{..}
#else
initialSkovPassiveState :: RuntimeParameters -> GenesisData -> Impl.BlockState -> IO SkovPassiveState
initialSkovPassiveState rtParams gen initBS = do
  let _spsSkov = Impl.initialSkovData rtParams gen initBS
      _spsFinalization = initialPassiveFinalizationState (bpHash (Impl._skovGenesisBlockPointer _spsSkov))
  return SkovPassiveState{..}
#endif

newtype SkovPassiveM m a = SkovPassiveM {unSkovPassiveM :: StateT SkovPassiveState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovPassiveState, MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovPassiveState (SkovPassiveM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovPassiveState (SkovPassiveM m))
type instance UpdatableBlockState (SkovPassiveM m) = Impl.BlockState
type instance BlockPointer (SkovPassiveM m) = Impl.BlockPointer
type instance PendingBlock (SkovPassiveM m) = Impl.PendingBlock

instance Monad m => OnSkov (SkovPassiveM m) where
    {-# INLINE onBlock #-}
    onBlock _ = return ()
    {-# INLINE onFinalize #-}
    onFinalize fr _ = spsFinalization %= execState (passiveNotifyBlockFinalized fr)
    {-# INLINE logTransfer #-}
    logTransfer = return Nothing

#ifdef RUST
evalSkovPassiveM :: (MonadIO m) => SkovPassiveM m a -> RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> m a
evalSkovPassiveM (SkovPassiveM a) rtParams gd bs0 gsptr = do
  initialState <- liftIO $ initialSkovPassiveState rtParams gd bs0 gsptr
  evalStateT a initialState
#else
evalSkovPassiveM :: (MonadIO m) => SkovPassiveM m a -> RuntimeParameters -> GenesisData -> Impl.BlockState -> m a
evalSkovPassiveM (SkovPassiveM a) rtParams gd bs0 = do
  initialState <- liftIO $ initialSkovPassiveState rtParams gd bs0
  evalStateT a initialState
#endif


runSkovPassiveM :: SkovPassiveM m a -> SkovPassiveState -> m (a, SkovPassiveState)
runSkovPassiveM (SkovPassiveM a) s = runStateT a s


-- |Skov state with active finalization.
data SkovActiveState = SkovActiveState {
    _sasSkov :: !Impl.SkovData,
    _sasFinalization :: !FinalizationState
}
makeLenses ''SkovActiveState

instance Impl.SkovLenses SkovActiveState where
    skov = sasSkov
instance FinalizationStateLenses SkovActiveState where
    finState = sasFinalization

#ifdef RUST
initialSkovActiveState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> IO SkovActiveState
initialSkovActiveState finInst rtParams gen initBS gsptr = do
  _sasSkov <- Impl.initialSkovData rtParams gen initBS gsptr
  let _sasFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sasSkov)) (genesisFinalizationParameters gen)
  return SkovActiveState{..}
#else
initialSkovActiveState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> IO SkovActiveState
initialSkovActiveState finInst rtParams gen initBS = do
  let _sasSkov = Impl.initialSkovData rtParams gen initBS
      _sasFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sasSkov)) (genesisFinalizationParameters gen)
  return SkovActiveState{..}
#endif

newtype SkovActiveM m a = SkovActiveM {unSkovActiveM :: RWST FinalizationInstance SkovFinalizationEvents SkovActiveState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovActiveState, MonadReader FinalizationInstance, MonadWriter SkovFinalizationEvents, MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovActiveState (SkovActiveM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovActiveState (SkovActiveM m) )
type instance UpdatableBlockState (SkovActiveM m) = Impl.BlockState
type instance BlockPointer (SkovActiveM m) = Impl.BlockPointer
type instance PendingBlock (SkovActiveM m) = Impl.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovActiveM m) where
    {-# INLINE onBlock #-}
    onBlock = notifyBlockArrival
    {-# INLINE onFinalize #-}
    onFinalize = notifyBlockFinalized
    {-# INLINE logTransfer #-}
    logTransfer = return Nothing

instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovActiveState (SkovActiveM m) where
    broadcastFinalizationMessage = tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    getFinalizationInstance = ask
    resetCatchUpTimer = tell . embedCatchUpTimerEvent

runSkovActiveM :: SkovActiveM m a -> FinalizationInstance -> SkovActiveState -> m (a, SkovActiveState, SkovFinalizationEvents)
runSkovActiveM (SkovActiveM a) fi fs = runRWST a fi fs

-- |Skov state with buffered finalization.
data SkovBufferedState = SkovBufferedState {
    _sbsSkov :: !Impl.SkovData,
    _sbsFinalization :: !FinalizationState,
    _sbsBuffer :: !FinalizationBuffer
}
makeLenses ''SkovBufferedState

instance Impl.SkovLenses SkovBufferedState where
    skov = sbsSkov
instance FinalizationStateLenses SkovBufferedState where
    finState = sbsFinalization
instance FinalizationBufferLenses SkovBufferedState where
    finBuffer = sbsBuffer

#ifdef RUST
initialSkovBufferedState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> IO SkovBufferedState
initialSkovBufferedState finInst rtParams gen initBS gsptr = do
  _sbsSkov <- Impl.initialSkovData rtParams gen initBS gsptr
  let _sbsFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sbsSkov)) (genesisFinalizationParameters gen)
  return SkovBufferedState{..}
    where
        _sbsBuffer = emptyFinalizationBuffer
#else
initialSkovBufferedState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> IO SkovBufferedState
initialSkovBufferedState finInst rtParams gen initBS = do
  let _sbsSkov = Impl.initialSkovData rtParams gen initBS
      _sbsFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sbsSkov)) (genesisFinalizationParameters gen)
  return SkovBufferedState{..}
    where
        _sbsBuffer = emptyFinalizationBuffer
#endif

newtype SkovBufferedM m a = SkovBufferedM {unSkovBufferedM :: RWST FinalizationInstance BufferedSkovFinalizationEvents SkovBufferedState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovBufferedState, MonadReader FinalizationInstance, MonadWriter BufferedSkovFinalizationEvents, MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovBufferedState (SkovBufferedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovBufferedState (SkovBufferedM m))
type instance UpdatableBlockState (SkovBufferedM m) = Impl.BlockState
type instance BlockPointer (SkovBufferedM m) = Impl.BlockPointer
type instance PendingBlock (SkovBufferedM m) = Impl.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovBufferedM m) where
    {-# INLINE onBlock #-}
    onBlock = notifyBlockArrival
    {-# INLINE onFinalize #-}
    onFinalize = notifyBlockFinalized
    {-# INLINE logTransfer #-}
    logTransfer = return Nothing

instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovBufferedState (SkovBufferedM m) where
    broadcastFinalizationMessage msg = bufferFinalizationMessage msg >>= \case
            Left n -> tell $ embedNotifyEvent n
            Right msgs -> forM_ msgs $ tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    getFinalizationInstance = ask
    resetCatchUpTimer = tell . embedCatchUpTimerEvent

runSkovBufferedM :: SkovBufferedM m a -> FinalizationInstance -> SkovBufferedState -> m (a, SkovBufferedState, BufferedSkovFinalizationEvents)
runSkovBufferedM (SkovBufferedM a) fi fs = runRWST a fi fs


-- * With transaction hooks

-- |Skov state with passive finalizion and transaction hooks.
-- This keeps finalization messages, but does not process them.
data SkovPassiveHookedState = SkovPassiveHookedState {
    _sphsSkov :: !Impl.SkovData,
    _sphsFinalization :: !PassiveFinalizationState,
    _sphsHooks :: !TransactionHooks
}
makeLenses ''SkovPassiveHookedState

instance Impl.SkovLenses SkovPassiveHookedState where
    skov = sphsSkov
instance PassiveFinalizationStateLenses SkovPassiveHookedState where
    pfinState = sphsFinalization
instance TransactionHookLenses SkovPassiveHookedState where
    hooks = sphsHooks

#ifdef RUST
initialSkovPassiveHookedState :: RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> IO SkovPassiveHookedState
initialSkovPassiveHookedState rtParams gen initBS gsptr = do
  _sphsSkov <- Impl.initialSkovData rtParams gen initBS gsptr
  let _sphsFinalization = initialPassiveFinalizationState (bpHash (Impl._skovGenesisBlockPointer _sphsSkov))
  return SkovPassiveHookedState{..}
  where
        _sphsHooks = emptyHooks
#else
initialSkovPassiveHookedState :: RuntimeParameters -> GenesisData -> Impl.BlockState -> IO SkovPassiveHookedState
initialSkovPassiveHookedState rtParams gen initBS = do
  let _sphsSkov = Impl.initialSkovData rtParams gen initBS
      _sphsFinalization = initialPassiveFinalizationState (bpHash (Impl._skovGenesisBlockPointer _sphsSkov))
  return SkovPassiveHookedState{..}
  where
        _sphsHooks = emptyHooks
#endif

newtype SkovPassiveHookedM m a = SkovPassiveHookedM {unSkovPassiveHookedM :: StateT SkovPassiveHookedState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovPassiveHookedState, MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovPassiveHookedState (SkovPassiveHookedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovPassiveHookedState (SkovPassiveHookedM m))
type instance UpdatableBlockState (SkovPassiveHookedM m) = Impl.BlockState
type instance BlockPointer (SkovPassiveHookedM m) = Impl.BlockPointer
type instance PendingBlock (SkovPassiveHookedM m) = Impl.PendingBlock

instance (TimeMonad m, MonadIO m, LoggerMonad m) => OnSkov (SkovPassiveHookedM m) where
    {-# INLINE onBlock #-}
    onBlock bp = hookOnBlock bp
    {-# INLINE onFinalize #-}
    onFinalize fr bp = do
        sphsFinalization %= execState (passiveNotifyBlockFinalized fr)
        hookOnFinalize fr bp
    {-# INLINE logTransfer #-}
    logTransfer = return Nothing

#ifdef RUST
evalSkovPassiveHookedM :: (MonadIO m) => SkovPassiveHookedM m a -> RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> m a
evalSkovPassiveHookedM (SkovPassiveHookedM a) rtParams gd bs0 gsptr = do
  initialState <- liftIO $ initialSkovPassiveHookedState rtParams gd bs0 gsptr
  evalStateT a initialState
#else
evalSkovPassiveHookedM :: (MonadIO m) => SkovPassiveHookedM m a -> RuntimeParameters -> GenesisData -> Impl.BlockState -> m a
evalSkovPassiveHookedM (SkovPassiveHookedM a) rtParams gd bs0 = do
  initialState <- liftIO $ initialSkovPassiveHookedState rtParams gd bs0
  evalStateT a initialState
#endif
runSkovPassiveHookedM :: SkovPassiveHookedM m a -> SkovPassiveHookedState -> m (a, SkovPassiveHookedState)
runSkovPassiveHookedM (SkovPassiveHookedM a) s = runStateT a s

-- |Skov state with buffered finalization and transaction hooks.
data SkovBufferedHookedState = SkovBufferedHookedState {
    _sbhsSkov :: !Impl.SkovData,
    _sbhsFinalization :: !FinalizationState,
    _sbhsBuffer :: !FinalizationBuffer,
    _sbhsHooks :: !TransactionHooks
}
makeLenses ''SkovBufferedHookedState

instance Impl.SkovLenses SkovBufferedHookedState where
    skov = sbhsSkov
instance FinalizationStateLenses SkovBufferedHookedState where
    finState = sbhsFinalization
instance FinalizationBufferLenses SkovBufferedHookedState where
    finBuffer = sbhsBuffer
instance TransactionHookLenses SkovBufferedHookedState where
    hooks = sbhsHooks

#ifdef RUST
initialSkovBufferedHookedState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> Impl.GlobalStatePtr -> IO SkovBufferedHookedState
initialSkovBufferedHookedState finInst rtParams gen initBS gsptr = do
  _sbhsSkov <- Impl.initialSkovData rtParams gen initBS gsptr
  let _sbhsFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sbhsSkov)) (genesisFinalizationParameters gen)
  return SkovBufferedHookedState{..}
  where
        _sbhsBuffer = emptyFinalizationBuffer
        _sbhsHooks = emptyHooks
#else
initialSkovBufferedHookedState :: FinalizationInstance -> RuntimeParameters -> GenesisData -> Impl.BlockState -> IO SkovBufferedHookedState
initialSkovBufferedHookedState finInst rtParams gen initBS = do
  let _sbhsSkov = Impl.initialSkovData rtParams gen initBS
      _sbhsFinalization = initialFinalizationState finInst (bpHash (Impl._skovGenesisBlockPointer _sbhsSkov)) (genesisFinalizationParameters gen)
  return SkovBufferedHookedState{..}
  where
        _sbhsBuffer = emptyFinalizationBuffer
        _sbhsHooks = emptyHooks
#endif

newtype SkovBufferedHookedM m a = SkovBufferedHookedM {unSkovBufferedHookedM :: RWST FinalizationInstance BufferedSkovFinalizationEvents SkovBufferedHookedState m a}
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovBufferedHookedState, MonadReader FinalizationInstance, MonadWriter BufferedSkovFinalizationEvents, MonadIO)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovBufferedHookedState (SkovBufferedHookedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovBufferedHookedState (SkovBufferedHookedM m) )
type instance UpdatableBlockState (SkovBufferedHookedM m) = Impl.BlockState
type instance BlockPointer (SkovBufferedHookedM m) = Impl.BlockPointer
type instance PendingBlock (SkovBufferedHookedM m) = Impl.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovBufferedHookedM m) where
    {-# INLINE onBlock #-}
    onBlock bp = do
        notifyBlockArrival bp
        hookOnBlock bp
    {-# INLINE onFinalize #-}
    onFinalize bp fr = do
        notifyBlockFinalized bp fr
        hookOnFinalize bp fr
    {-# INLINE logTransfer #-}
    logTransfer = return Nothing

instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovBufferedHookedState (SkovBufferedHookedM m) where
    broadcastFinalizationMessage msg = bufferFinalizationMessage msg >>= \case
            Left n -> tell $ embedNotifyEvent n
            Right msgs -> forM_ msgs $ tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    getFinalizationInstance = ask
    resetCatchUpTimer = tell . embedCatchUpTimerEvent

runSkovBufferedHookedM :: SkovBufferedHookedM m a -> FinalizationInstance -> SkovBufferedHookedState -> m (a, SkovBufferedHookedState, BufferedSkovFinalizationEvents)
runSkovBufferedHookedM (SkovBufferedHookedM a) fi fs = runRWST a fi fs


newtype SkovBufferedHookedLoggedM m a = SkovBufferedHookedLoggedM {
  unSkovBufferedHookedLoggedM :: RWST (FinalizationInstance, Maybe (LogTransferMethod IO)) BufferedSkovFinalizationEvents SkovBufferedHookedState m a
  }
    deriving (Functor, Applicative, Monad, TimeMonad, LoggerMonad, MonadState SkovBufferedHookedState, MonadReader (FinalizationInstance, Maybe (LogTransferMethod IO)), MonadWriter BufferedSkovFinalizationEvents, MonadIO, ATLMonad)
    deriving (BlockStateQuery, BlockStateOperations, TreeStateMonad) via (Impl.SkovTreeState SkovBufferedHookedState (SkovBufferedHookedLoggedM m))
    deriving (SkovQueryMonad, SkovMonad) via (TSSkovUpdateWrapper SkovBufferedHookedState (SkovBufferedHookedLoggedM m) )
type instance UpdatableBlockState (SkovBufferedHookedLoggedM m) = Impl.BlockState
type instance BlockPointer (SkovBufferedHookedLoggedM m) = Impl.BlockPointer
type instance PendingBlock (SkovBufferedHookedLoggedM m) = Impl.PendingBlock
instance (TimeMonad m, LoggerMonad m, MonadIO m) => OnSkov (SkovBufferedHookedLoggedM m) where
    {-# INLINE onBlock #-}
    onBlock bp = do
        notifyBlockArrival bp
        hookOnBlock bp
    {-# INLINE onFinalize #-}
    onFinalize bp fr = do
        notifyBlockFinalized bp fr
        hookOnFinalize bp fr

    {-# INLINE logTransfer #-}
    logTransfer =
      asks snd >>= \case
        Nothing -> return Nothing
        Just lm -> return (Just (\bh slot reason -> liftIO (lm bh slot reason)))


instance (TimeMonad m, LoggerMonad m, MonadIO m)
            => FinalizationMonad SkovBufferedHookedState (SkovBufferedHookedLoggedM m) where
    broadcastFinalizationMessage msg = bufferFinalizationMessage msg >>= \case
            Left n -> tell $ embedNotifyEvent n
            Right msgs -> forM_ msgs $ tell . embedFinalizationEvent . BroadcastFinalizationMessage
    broadcastFinalizationRecord = tell . embedFinalizationEvent . BroadcastFinalizationRecord
    getFinalizationInstance = asks fst
    resetCatchUpTimer = tell . embedCatchUpTimerEvent

runSkovBufferedHookedLoggedM :: SkovBufferedHookedLoggedM m a -> FinalizationInstance -> Maybe (LogTransferMethod IO) -> SkovBufferedHookedState -> m (a, SkovBufferedHookedState, BufferedSkovFinalizationEvents)
runSkovBufferedHookedLoggedM (SkovBufferedHookedLoggedM a) fi tlog fs = do
  runRWST a (fi, tlog) fs
-}