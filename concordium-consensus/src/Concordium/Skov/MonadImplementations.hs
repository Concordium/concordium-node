{-# LANGUAGE
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
    TypeFamilies
    #-}
module Concordium.Skov.MonadImplementations where

import Data.Kind
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Proxy
import Data.Set(toList)

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Block hiding (PendingBlock)
import qualified Concordium.GlobalState.Block as GB (PendingBlock(..))
import Concordium.GlobalState.Parameters
import Concordium.GlobalState
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.Skov.Monad
import Concordium.Skov.Query
import Concordium.Skov.Update
import Concordium.Skov.CatchUp
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Afgjort.FinalizationQueue
import Concordium.Afgjort.Monad
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Buffer


-- |An instance @c@ of 'SkovConfiguration' defines a configuration for
-- executing the 'SkovT' monad.
class (
        HasGlobalStateContext (SkovGSContext c) (SkovContext c),
        HasGlobalState (SkovGSState c) (SkovState c)
        ) => SkovConfiguration c where
    -- |The type of contexts (i.e. read only data) for the configuration type.
    data SkovContext c
    -- |The type of states for the configuration type.
    data SkovState c
    -- |A type representing the global state part of 'SkovState'.
    -- We require 'HasGlobalState (SkovGSState c) (SkovState c)'.
    type SkovGSState c
    -- |A type representing the global state context part of 'SkovContext'.
    -- We require 'HasGlobalStateContext (SkovGSContext c) (SkovContext c)'.
    type SkovGSContext c
    -- |A type representing the log context of 'SkovContext'. For the moment this is only
    -- (optionally) the index of transactions per account.
    type SkovLogContext c
    -- |Create an initial context and state from a given configuration.
    initialiseSkov :: c -> IO (SkovContext c, SkovState c)
    -- |Free any resources when we are done with the context and state.
    shutdownSkov :: SkovContext c -> SkovState c -> IO ()

-- |An instance of 'SkovTimerHandlers' provides a means for implementing
-- a 'TimerMonad' instance for 'SkovT'.
class SkovTimerHandlers h c m | h -> m c where
    -- |Type to represent a timer
    type SkovHandlerTimer h
    -- |Handler for creating a timer event
    handleOnTimeout :: h -> Timeout -> SkovT h c m a -> m (SkovHandlerTimer h)
    -- |Handler for cancelling a timer
    handleCancelTimer :: h -> SkovHandlerTimer h -> m ()

-- |An instance of 'SkovFinalizationHandlers' provides handlers for broadcasting
-- finalization-related messages.
class SkovFinalizationHandlers h m where
    handleBroadcastFinalizationMessage :: h -> FinalizationPseudoMessage -> m ()
    handleBroadcastFinalizationRecord :: h -> FinalizationRecord -> m ()

-- |An instance of 'SkovPendingLiveHandlers' provides handlers for dealing with
-- pending blocks or records becoming live by notifying peers that this is the
-- case.
class SkovPendingLiveHandlers h m where
    -- |Called to notify that a block or finalization record that was previously
    -- pending is now live. An implementation should cause a catch-up status
    -- message to be sent to each peer within a bounded time (which alerts them
    -- to the newly live block/fin-rec).
    handlePendingLive :: h -> m ()

-- |'SkovHandlers' provides an implementation of 'SkovTimerHandlers' and
-- 'SkovFinalizationHandlers'.
data SkovHandlers t c m = SkovHandlers {
    shBroadcastFinalizationMessage :: FinalizationPseudoMessage -> m (),
    shBroadcastFinalizationRecord :: FinalizationRecord -> m (),
    shOnTimeout :: forall a. Timeout -> SkovT (SkovHandlers t c m) c m a -> m t,
    shCancelTimer :: t -> m (),
    shPendingLive :: m ()
}

instance SkovFinalizationHandlers (SkovHandlers t c m) m where
    handleBroadcastFinalizationMessage SkovHandlers{..} = shBroadcastFinalizationMessage
    handleBroadcastFinalizationRecord SkovHandlers{..} = shBroadcastFinalizationRecord

instance SkovTimerHandlers (SkovHandlers t c m) c m where
    type SkovHandlerTimer (SkovHandlers t c m) = t
    handleOnTimeout SkovHandlers{..} = shOnTimeout
    handleCancelTimer SkovHandlers{..} = shCancelTimer

instance SkovPendingLiveHandlers (SkovHandlers t c m) m where
    handlePendingLive = shPendingLive

data SkovPassiveHandlers (c :: Type) m = SkovPassiveHandlers {
    sphPendingLive :: m ()
}

instance SkovPendingLiveHandlers (SkovPassiveHandlers c m) m where
    handlePendingLive = sphPendingLive

-- This provides an instance of timer handlers that should not be used.
-- TODO: In future, the types in finalization should be refined so that
-- this instance is not needed.
instance SkovTimerHandlers (SkovPassiveHandlers c m) c m where
    type SkovHandlerTimer (SkovPassiveHandlers c m) = ()
    handleOnTimeout _ _ _ = error "Attempted to set a timer, but SkovPassiveHandlers does not support timers."
    handleCancelTimer _ _ = error "Attempted to cancel a timer, but SkovPassiveHandlers does not support timers."

-- |The 'SkovT' monad transformer equips a monad with state, context and handlers for
-- performing Skov operations.
newtype SkovT h c m a = SkovT { runSkovT' :: h -> SkovContext c -> StateT (SkovState c) m a }
    deriving (Functor, Applicative, Monad, MonadState (SkovState c), MonadIO, LoggerMonad, TimeMonad)
        via (ReaderT h (ReaderT (SkovContext c) (StateT (SkovState c) m)))

-- GlobalStateM using config abstractions
type SkovTGSM h c m = GlobalStateM (SkovLogContext c) (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT h c m)
type SkovTTBM h c m = TreeStateBlockStateM (SkovGSState c) (SkovGSContext c) (SkovContext c) (SkovState c) (SkovT h c m)

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
-----------------------------------------------------------------------------
-- Inherit the instances from GlobalStateM

deriving via SkovTGSM h c' m
    instance BlockStateTypes (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              BirkParametersMonad (SkovTGSM h c' m))
             => BirkParametersMonad (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              BlockStateQuery (SkovTGSM h c' m))
             => BlockStateQuery (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              BlockStateOperations (SkovTGSM h c' m))
             => BlockStateOperations (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              BlockStateStorage (SkovTGSM h c' m))
             => BlockStateStorage (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance ATITypes (SkovTGSM h c' m)
             => ATITypes (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m, PerAccountDBOperations (SkovTGSM h c' m))
             => PerAccountDBOperations (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance GlobalStateTypes (SkovTGSM h c' m)
             => GlobalStateTypes (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              BlockPointerMonad (SkovTGSM h c' m))
             => BlockPointerMonad (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              TreeStateMonad (SkovTGSM h c' m))
              => TreeStateMonad (SkovT h c' m)

-----------------------------------------------------------------------------

instance (
        Monad m,
        BlockStateQuery (SkovT h c m),
        TreeStateMonad (SkovT h c m)
        ) => SkovQueryMonad (SkovT h c m) where
    {-# INLINE resolveBlock #-}
    resolveBlock = doResolveBlock
    {-# INLINE isFinalized #-}
    isFinalized = doIsFinalized
    {-# INLINE blockAtFinIndex #-}
    blockAtFinIndex = getFinalizedAtIndex
    {-# INLINE lastFinalizedBlock #-}
    lastFinalizedBlock = fst <$> getLastFinalized
    {-# INLINE nextFinalizationIndex #-}
    nextFinalizationIndex = getNextFinalizationIndex
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
    {-# INLINE queryBlockState #-}
    queryBlockState = blockState
    {-# INLINE blockLastFinalizedIndex #-}
    blockLastFinalizedIndex = doBlockLastFinalizedIndex
    {-# INLINE getCatchUpStatus #-}
    getCatchUpStatus = doGetCatchUpStatus
    {-# INLINE queryTransactionStatus #-}
    queryTransactionStatus = lookupTransaction
    {-# INLINE queryNonFinalizedTransactions #-}
    queryNonFinalizedTransactions addr = do
      txs <- getAccountNonFinalized addr minNonce
      return $! map getHash . concatMap (toList . snd) $ txs

    {-# INLINE queryNextAccountNonce #-}
    queryNextAccountNonce = getNextAccountNonce

instance (
        Monad m,
        TimeMonad m,
        LoggerMonad m,
        OnSkov (SkovT h c m),
        BlockStateStorage (SkovT h c m),
        TreeStateMonad (SkovT h c m),
        FinalizationMonad (SkovT h c m),
        PendingBlockType (SkovT h c m) ~ GB.PendingBlock)
        => SkovMonad (SkovT h c m) where
    {-# INLINE storeBlock #-}
    storeBlock = doStoreBlock
    {-# INLINE storeBakedBlock #-}
    storeBakedBlock = doStoreBakedBlock
    {-# INLINE receiveTransaction #-}
    receiveTransaction tr = doReceiveTransaction tr 0
    {-# INLINE trustedFinalize #-}
    trustedFinalize = doTrustedFinalize
    {-# INLINE handleCatchUpStatus #-}
    handleCatchUpStatus = doHandleCatchUp

class FinalizationConfig c where
    type FCContext c
    type FCState c
    initialiseFinalization :: c -> (FCContext c, FCState c)

data SkovConfig gsconfig finconfig handlerconfig = SkovConfig gsconfig !finconfig !handlerconfig


data NoFinalization (t :: Type) = NoFinalization !GenesisData

instance FinalizationConfig (SkovConfig gsconf (NoFinalization t) hconf) where
    type FCContext (SkovConfig gsconf (NoFinalization t) hconf) = ()
    type FCState (SkovConfig gsconf (NoFinalization t) hconf) = FinalizationState t
    initialiseFinalization (SkovConfig _ (NoFinalization genData) _)
            = ((), initialPassiveFinalizationState genHash finParams genBakers gtu)
        where
            genHash = getHash (GenesisBlock genData)
            finParams = genesisFinalizationParameters genData
            genBakers = genesisBakers genData
            gtu = genesisTotalGTU genData
    {-# INLINE initialiseFinalization #-}

-- This provides an implementation of FinalizationOutputMonad that does nothing.
-- This should be fine, because NoFinalization indicates that no participation in
-- finalization is expected.  However, in future, it would be nice if the type signatures
-- in finalization meant that this instance is not required.
instance (Monad m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (NoFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage _ = return ()

data ActiveFinalization (t :: Type) = ActiveFinalization !FinalizationInstance !GenesisData

instance FinalizationConfig (SkovConfig gc (ActiveFinalization t) hc) where
    type FCContext (SkovConfig gc (ActiveFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gc (ActiveFinalization t) hc) = FinalizationState t
    initialiseFinalization (SkovConfig _ (ActiveFinalization finInst genData) _)
            = (finInst, initialFinalizationState finInst genHash finParams genBakers gtu)
            where
                genHash = getHash (GenesisBlock genData)
                finParams = genesisFinalizationParameters genData
                genBakers = genesisBakers genData
                gtu = genesisTotalGTU genData
    {-# INLINE initialiseFinalization #-}

instance (SkovFinalizationHandlers h m, Monad m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

data BufferedFinalization (t :: Type) = BufferedFinalization !FinalizationInstance !GenesisData

data BufferedFinalizationState t = BufferedFinalizationState {
        _bfsFinalization :: !(FinalizationState t),
        _bfsBuffer :: !FinalizationBuffer
    }
makeLenses ''BufferedFinalizationState

instance FinalizationQueueLenses (BufferedFinalizationState t) where
    finQueue = bfsFinalization . finQueue
instance FinalizationStateLenses (BufferedFinalizationState t) t where
    finState = bfsFinalization
instance FinalizationBufferLenses (BufferedFinalizationState t) where
    finBuffer = bfsBuffer

instance FinalizationConfig (SkovConfig gc (BufferedFinalization t) hc) where
    type FCContext (SkovConfig gc (BufferedFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gc (BufferedFinalization t) hc) = BufferedFinalizationState t
    initialiseFinalization (SkovConfig _ (BufferedFinalization finInst genData) _)
            = (finInst, BufferedFinalizationState (initialFinalizationState finInst genHash finParams genBakers gtu) emptyFinalizationBuffer)
            where
                genHash = getHash $ GenesisBlock genData
                finParams = genesisFinalizationParameters genData
                genBakers = genesisBakers genData
                gtu = genesisTotalGTU genData
    {-# INLINE initialiseFinalization #-}

instance (SkovFinalizationHandlers h m, Monad m, TimeMonad m, LoggerMonad m, SkovTimerHandlers h (SkovConfig gc (BufferedFinalization t) hc) m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m) where
    broadcastFinalizationMessage = bufferFinalizationMessage (\msg' -> SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h (FPMMessage msg')))
    broadcastFinalizationPseudoMessage (FPMMessage msg) = broadcastFinalizationMessage msg
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

class HandlerConfig c where
    type HCContext c
    type HCState c
    initialiseHandler :: c -> (HCContext c, HCState c)

class (Monad m, HandlerConfig c) => HandlerConfigHandlers c m | m -> c where
    handleBlock :: (SkovMonad m, TreeStateMonad m) => BlockPointerType m -> m ()
    handleFinalize :: (SkovMonad m, TreeStateMonad m) => FinalizationRecord -> BlockPointerType m -> m ()


data NoHandler = NoHandler

instance HandlerConfig (SkovConfig gc fc NoHandler) where
    type HCContext (SkovConfig gc fc NoHandler) = ()
    type HCState (SkovConfig gc fc NoHandler) = ()
    initialiseHandler = \_ -> ((),())

instance Monad m => HandlerConfigHandlers (SkovConfig gc fc NoHandler) (SkovT h (SkovConfig gc fc NoHandler) m) where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ -> return ()

instance (
        GlobalStateConfig gsconf,
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
            ssHandlerState :: HCState (SkovConfig gsconf finconf hconf),
            scLogContext :: GSLogContext gsconf
        }
    type SkovGSState (SkovConfig gsconf finconf hconf) = GSState gsconf
    type SkovGSContext (SkovConfig gsconf finconf hconf) = GSContext gsconf

    type SkovLogContext (SkovConfig gsconf finconf hconf) = GSLogContext gsconf

    initialiseSkov conf@(SkovConfig gsc _ _) = do
        (c, s, logCtx) <- initialiseGlobalState gsc
        let (finctx, finst) = initialiseFinalization conf
        let (hctx, hst) = initialiseHandler conf
        return (SkovContext c finctx hctx, SkovState s finst hst logCtx)
    shutdownSkov (SkovContext c _ _) (SkovState s _ _ logCtx) = shutdownGlobalState (Proxy :: Proxy gsconf) c s logCtx

instance (FinalizationQueueLenses (FCState (SkovConfig gsconf finconf hconf)))
        => FinalizationQueueLenses (SkovState (SkovConfig gsconf finconf hconf)) where
    finQueue = lens ssFinState (\s fs -> s {ssFinState = fs}) . finQueue

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

instance GSLogContext gsconf ~ a => HasLogContext a (SkovState (SkovConfig gsconf finconf hconf)) where
  logContext = lens scLogContext (\sc v -> sc{scLogContext = v })

instance (c ~ GSContext gsconf) => HasGlobalStateContext c (SkovContext (SkovConfig gsconf finconf hconf)) where
    globalStateContext = lens (scGSContext) (\sc v -> sc{scGSContext = v})
instance (g ~ GSState gsconf) => HasGlobalState g (SkovState (SkovConfig gsconf finconf hconf)) where
    globalState = lens (ssGSState) (\ss v -> ss {ssGSState = v})

instance (MonadIO m,
        HandlerConfigHandlers (SkovConfig gsconf finconf hconf) (SkovT h (SkovConfig gsconf finconf hconf) m),
        SkovPendingLiveHandlers h m,
        SkovMonad (SkovT h (SkovConfig gsconf finconf hconf) m),
        TreeStateMonad (SkovT h (SkovConfig gsconf finconf hconf) m))
        => OnSkov (SkovT h (SkovConfig gsconf finconf hconf) m) where
    onBlock bp = handleBlock bp
    onFinalize fr bp = handleFinalize fr bp
    onPendingLive = SkovT $ \h _ -> lift $ handlePendingLive h
    {-# INLINE onBlock #-}
    {-# INLINE onFinalize #-}

deriving via (ActiveFinalizationM (SkovContext (SkovConfig gc (NoFinalization t) hc)) (SkovState (SkovConfig gc (NoFinalization t) hc)) (SkovT h (SkovConfig gc (NoFinalization t) hc) m))
    instance (t ~ SkovHandlerTimer h, MonadIO m, SkovMonad (SkovT h (SkovConfig gc (NoFinalization t) hc) m),
        TreeStateMonad (SkovT h (SkovConfig gc (NoFinalization t) hc) m),
        SkovTimerHandlers h (SkovConfig gc (NoFinalization t) hc) m)
        => FinalizationMonad (SkovT h (SkovConfig gc (NoFinalization t) hc) m)

deriving via (ActiveFinalizationM (SkovContext (SkovConfig gc (ActiveFinalization t) hc)) (SkovState (SkovConfig gc (ActiveFinalization t) hc)) (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m))
    instance (t ~ SkovHandlerTimer h, MonadIO m, SkovMonad (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m),
        TreeStateMonad (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m),
        SkovTimerHandlers h (SkovConfig gc (ActiveFinalization t) hc) m, SkovFinalizationHandlers h m)
        => FinalizationMonad (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m)

deriving via (ActiveFinalizationM (SkovContext (SkovConfig gc (BufferedFinalization t) hc)) (SkovState (SkovConfig gc (BufferedFinalization t) hc)) (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m))
    instance (t ~ SkovHandlerTimer h, MonadIO m, TimeMonad m, LoggerMonad m, SkovMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m),
        TreeStateMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m),
        SkovTimerHandlers h (SkovConfig gc (BufferedFinalization t) hc) m, SkovFinalizationHandlers h m)
        => FinalizationMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m)
-- If we require `C (SkovT ...)` for a specific class `C`, the compiler will complain
-- because:
-- 1. the instances for `SkovT` are derived from `GlobalStateM` setting the type
--    variables to the associated types defined in `SkovConfiguration`.
-- 2. The instances for `GlobalStateM` are derived from the matching `BlockStateM` and
--    `TreeStateM` instances
-- 3. these two have instances for specific instantiations of the type variables, such as
--    `PersistentBlockStateContext`.
-- 4. `SkovGSContext` is not limited to the alternatives that provide an implementation.
--
-- Because of this, when requiring `C (SkovT ...)`, if `C` was derived for `GlobalStateM`
-- using the intermediate monad `M`, we will instead require `C (M)` setting the type variables
-- to the names provided by `SkovConfiguration` as this will automatically trigger `C (SkovT ...)`.

type BlockStateConfigM h c m = BlockStateM
               (SkovGSContext c)
               (SkovContext c)
               (SkovGSState c)
               (SkovState c)
               (SkovT h c m)

type TreeStateConfigM h c m = TreeStateM
               (SkovGSState c)
               (BlockStateM
                (SkovGSContext c)
                (SkovContext c)
                (SkovGSState c)
                (SkovState c)
                (SkovT h c m))

type SkovConfigMonad h c m = (SkovConfiguration c,
        OnSkov (SkovT h c m),
        BlockStateStorage (BlockStateConfigM h c m),
        GlobalStateTypes (TreeStateConfigM h c m),
        TreeStateMonad (TreeStateConfigM h c m),
        BlockPointerMonad (TreeStateConfigM h c m),
        PendingBlockType (SkovT h c m) ~ GB.PendingBlock,
        BlockFields ~ BlockFieldType (BlockPointerType (SkovTGSM h c m)),
        PendingBlockType (TreeStateConfigM h c m) ~ PendingBlockType (SkovT h c m),
        BlockPointerType (TreeStateConfigM h c m) ~ BlockPointerType (SkovT h c m))

type SkovQueryConfigMonad c m =
    (PendingBlockType (SkovT () c m) ~ GB.PendingBlock,
     BlockFields ~ BlockFieldType (BlockPointerType (TreeStateConfigM () c m)),
     TreeStateMonad (SkovTGSM () c m),
     BlockPointerMonad (SkovTGSM () c m),
     BlockStateStorage (BlockStateConfigM () c m)
    )
