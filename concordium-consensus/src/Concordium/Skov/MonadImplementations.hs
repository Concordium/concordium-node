{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    TypeFamilies,
    DerivingStrategies,
    DerivingVia,
    UndecidableInstances,
    StandaloneDeriving,
    ScopedTypeVariables,
    ConstraintKinds,
    PartialTypeSignatures,
    QuantifiedConstraints,
    RankNTypes,
    TemplateHaskell,
    CPP #-}
module Concordium.Skov.MonadImplementations where

import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Proxy

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState
import Concordium.Types.HashableTo
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
    instance (Monad m, BlockStateQuery (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateQuery (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (Monad m, BlockStateOperations (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateOperations (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (Monad m, BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => BlockStateStorage (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (GlobalStateTypes (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)))
        => GlobalStateTypes (SkovT h c m)

deriving via (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    instance (
        Monad m,
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
        ) => TreeStateMonad (SkovT h c m)

instance (
        Monad m,
        TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)),
        BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
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
        BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
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
        BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m))
    )

type SkovQueryConfigMonad c m = 
    (TreeStateMonad (GlobalStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT () c m)),
    BlockStateStorage (BlockStateM (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT () c m))
    )

type SkovFinalizationConfigMonad h c m = (
    SkovConfigMonad h c m,
    FinalizationStateLenses (SkovState c) (Timer (SkovT h c m)),
    HasFinalizationInstance (SkovContext c),
    FinalizationConfigHandlers c (SkovT h c m)
    )