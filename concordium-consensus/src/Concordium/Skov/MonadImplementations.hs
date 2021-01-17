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
    GeneralizedNewtypeDeriving,
    RankNTypes,
    TypeApplications,
    TemplateHaskell,
    TypeFamilies
    #-}
module Concordium.Skov.MonadImplementations where

import Data.Kind
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.Identity
import Lens.Micro.Platform
import Data.Proxy

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Block hiding (PendingBlock)
import Concordium.GlobalState
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.Skov.Monad
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

-- * Global state runner for querying the tree and block states to establish
-- a finalization state.

type RWSTIO c = RWST (Identity (GSContext c)) () (Identity (GSState c)) LogIO

type BlockStateType c = BlockStateM
                        (GSContext c)
                        (Identity (GSContext c))
                        (GSState c)
                        (Identity (GSState c))
                        (RWSTIO c)

type TreeStateType c = TreeStateBlockStateM
                       (GSState c)
                       (GSContext c)
                       (Identity (GSContext c))
                       (Identity (GSState c))
                       (RWSTIO c)

type GlobalStateQuery c = (BlockStateStorage (BlockStateType c), TreeStateMonad (TreeStateType c))

-- |This is a convenience wrapper that automatically implements SkovQueryMonad via SkovQueryMonadT
-- instance.
type GlobalState c =
  GlobalStateM NoLogContext (GSContext c) (Identity (GSContext c)) (GSState c) (Identity (GSState c)) (RWSTIO c)


getFinalizationState :: forall c timer . GlobalStateQuery c
                     => Proxy c
                     -- ^Dummy type to fix the type variable 'c' which does not appear in the result type
                     -- and is not uniquely determined by GSContext or GSState since they are non-injective
                     -- type families.
                     -> (GSContext c, GSState c)
                     -- ^Global state from which to get the tree information.
                     -> Maybe FinalizationInstance
                     -- ^Just finInst if active finalization, Nothing if keys are not available.
                     -> LogIO (FinalizationState timer)
getFinalizationState Proxy (gsCtx, gsState) mInst = fst <$> evalRWST (runGlobalStateM comp) (Identity gsCtx) (Identity gsState)
  where comp :: GlobalState c (FinalizationState timer)
        comp = recoverFinalizationState mInst

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
    initialiseSkov :: c -> LogIO (SkovContext c, SkovState c)
    -- |Free any resources when we are done with the context and state.
    shutdownSkov :: SkovContext c -> SkovState c -> LogIO ()

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
    deriving (Functor, Applicative, Monad, MonadState (SkovState c), MonadIO, MonadLogger, TimeMonad)
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
    {- - INLINE ask - -}
    {- - INLINE local - -}

instance MonadTrans (SkovT h c) where
    lift a = SkovT (\_ _ -> lift a)
    {- - INLINE lift - -}

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
              BlockStateQuery (SkovTGSM h c' m))
             => BlockStateQuery (SkovT h c' m)

deriving via SkovTGSM h c' m
    instance (Monad m,
              AccountOperations (SkovTGSM h c' m))
             => AccountOperations (SkovT h c' m)

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

deriving via SkovQueryMonadT (SkovT h c m) instance (
        Monad m,
        BlockStateQuery (SkovT h c m),
        TreeStateMonad (SkovT h c m)
        ) => SkovQueryMonad (SkovT h c m)

instance (
        Monad m,
        TimeMonad m,
        MonadLogger m,
        OnSkov (SkovT h c m),
        BlockStateStorage (SkovT h c m),
        TreeStateMonad (SkovT h c m),
        FinalizationMonad (SkovT h c m))
        => SkovMonad (SkovT h c m) where
    {- - INLINE storeBlock - -}
    storeBlock = doStoreBlock
    {- - INLINE receiveTransaction - -}
    receiveTransaction tr = doReceiveTransaction tr 0
    {- - INLINE trustedFinalize - -}
    trustedFinalize = doTrustedFinalize
    {- - INLINE handleCatchUpStatus - -}
    handleCatchUpStatus = doHandleCatchUp

class GlobalStateQuery gsconf => FinalizationConfig gsconf c | c -> gsconf where
    type FCContext c
    type FCState c
    initialiseFinalization :: c -> (GSContext gsconf, GSState gsconf) -> LogIO (FCContext c, FCState c)

data SkovConfig gsconfig finconfig handlerconfig = SkovConfig gsconfig !finconfig !handlerconfig

data NoFinalization (t :: Type) = NoFinalization

instance GlobalStateQuery gsconf => FinalizationConfig gsconf (SkovConfig gsconf (NoFinalization t) hconf) where
    type FCContext (SkovConfig gsconf (NoFinalization t) hconf) = ()
    type FCState (SkovConfig gsconf (NoFinalization t) hconf) = FinalizationState t
    initialiseFinalization (SkovConfig _ NoFinalization _) gs = do
      ((),) <$> getFinalizationState (Proxy @ gsconf) gs Nothing
    {- - INLINE initialiseFinalization - -}

-- This provides an implementation of FinalizationOutputMonad that does nothing.
-- This should be fine, because NoFinalization indicates that no participation in
-- finalization is expected.  However, in future, it would be nice if the type signatures
-- in finalization meant that this instance is not required.
instance (Monad m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (NoFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage _ = return ()

data ActiveFinalization (t :: Type) = ActiveFinalization !FinalizationInstance

instance GlobalStateQuery gsconf => FinalizationConfig gsconf (SkovConfig gsconf (ActiveFinalization t) hc) where
    type FCContext (SkovConfig gsconf (ActiveFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gsconf (ActiveFinalization t) hc) = FinalizationState t
    initialiseFinalization (SkovConfig _ (ActiveFinalization finInst) _) gs = do
      (finInst,) <$> getFinalizationState (Proxy @ gsconf) gs (Just finInst)
    {- - INLINE initialiseFinalization - -}

instance (SkovFinalizationHandlers h m, Monad m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (ActiveFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

data BufferedFinalization (t :: Type) = BufferedFinalization !FinalizationInstance

data BufferedFinalizationState t = BufferedFinalizationState {
        _bfsFinalization :: !(FinalizationState t),
        _bfsBuffer :: !FinalizationBuffer
    }
    deriving(Show)
makeLenses ''BufferedFinalizationState

instance FinalizationQueueLenses (BufferedFinalizationState t) where
    finQueue = bfsFinalization . finQueue
instance FinalizationStateLenses (BufferedFinalizationState t) t where
    finState = bfsFinalization
instance FinalizationBufferLenses (BufferedFinalizationState t) where
    finBuffer = bfsBuffer

instance GlobalStateQuery gsconf => FinalizationConfig gsconf (SkovConfig gsconf (BufferedFinalization t) hc) where
    type FCContext (SkovConfig gsconf (BufferedFinalization t) hc) = FinalizationInstance
    type FCState (SkovConfig gsconf (BufferedFinalization t) hc) = BufferedFinalizationState t
    initialiseFinalization (SkovConfig _ (BufferedFinalization finInst) _) gs = do
      finalizationState <- getFinalizationState (Proxy @ gsconf) gs (Just finInst)
      return (finInst, BufferedFinalizationState finalizationState emptyFinalizationBuffer)
    {- - INLINE initialiseFinalization - -}

instance (SkovFinalizationHandlers h m, Monad m, TimeMonad m, MonadLogger m, SkovTimerHandlers h (SkovConfig gc (BufferedFinalization t) hc) m)
        => FinalizationOutputMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m) where
    broadcastFinalizationMessage = bufferFinalizationMessage (\msg' -> SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h (FPMMessage msg')))
    broadcastFinalizationPseudoMessage (FPMMessage msg) = broadcastFinalizationMessage msg
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

class HandlerConfig c where
    type HCContext c
    type HCState c
    initialiseHandler :: c -> (HCContext c, HCState c)

class (Monad m, HandlerConfig c) => HandlerConfigHandlers c m | m -> c where
    handleBlock :: SkovMonad m => BlockPointerType m -> m ()
    handleFinalize :: SkovMonad m => FinalizationRecord -> BlockPointerType m -> m ()


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
        Show (FCContext (SkovConfig gsconf finconf hconf)), Show (FCState (SkovConfig gsconf finconf hconf)),
        FinalizationConfig gsconf (SkovConfig gsconf finconf hconf),
        HandlerConfig (SkovConfig gsconf finconf hconf))
        => SkovConfiguration (SkovConfig gsconf finconf hconf) where
    data SkovContext (SkovConfig gsconf finconf hconf) = SkovContext {
            scGSContext :: !(GSContext gsconf),
            scFinContext :: !(FCContext (SkovConfig gsconf finconf hconf)),
            scHandlerContext :: !(HCContext (SkovConfig gsconf finconf hconf))
        }
    data SkovState (SkovConfig gsconf finconf hconf) = SkovState {
            ssGSState :: !(GSState gsconf),
            ssFinState :: !(FCState (SkovConfig gsconf finconf hconf)),
            ssHandlerState :: !(HCState (SkovConfig gsconf finconf hconf)),
            scLogContext :: !(GSLogContext gsconf)
        }
    type SkovGSState (SkovConfig gsconf finconf hconf) = GSState gsconf
    type SkovGSContext (SkovConfig gsconf finconf hconf) = GSContext gsconf

    type SkovLogContext (SkovConfig gsconf finconf hconf) = GSLogContext gsconf

    initialiseSkov conf@(SkovConfig gsc _ _) = do
        (c, s, logCtx) <- initialiseGlobalState gsc
        (finctx, finst) <- initialiseFinalization conf (c, s)
        logEvent Baker LLDebug $ "Initializing finalization with context = " ++ show finctx
        logEvent Baker LLDebug $ "Initializing finalization with initial state = " ++ show finst
        let (hctx, hst) = initialiseHandler conf
        return (SkovContext c finctx hctx, SkovState s finst hst logCtx)
    shutdownSkov (SkovContext c _ _) (SkovState s _ _ logCtx) = liftIO $ shutdownGlobalState (Proxy :: Proxy gsconf) c s logCtx

instance (FinalizationQueueLenses (FCState (SkovConfig gsconf finconf hconf)))
        => FinalizationQueueLenses (SkovState (SkovConfig gsconf finconf hconf)) where
    finQueue = lens ssFinState (\s fs -> s {ssFinState = fs}) . finQueue
    {- - INLINE finQueue - -}

instance (FinalizationStateLenses (FCState (SkovConfig gsconf finconf hconf)) t)
        => FinalizationStateLenses (SkovState (SkovConfig gsconf finconf hconf)) t where
    finState = lens ssFinState (\s fs -> s {ssFinState = fs}) . finState
    {- - INLINE finState - -}

instance (FinalizationBufferLenses (FCState (SkovConfig gsconf finconf hconf)))
        => FinalizationBufferLenses (SkovState (SkovConfig gsconf finconf hconf)) where
    finBuffer = lens ssFinState (\s fs -> s {ssFinState = fs}) . finBuffer
    {- - INLINE finBuffer - -}

instance (HasFinalizationInstance (FCContext (SkovConfig gsconf finconf hconf)))
        => HasFinalizationInstance (SkovContext (SkovConfig gsconf finconf hconf)) where
    finalizationInstance = finalizationInstance . scFinContext
    {- - INLINE finalizationInstance - -}

instance GSLogContext gsconf ~ a => HasLogContext a (SkovState (SkovConfig gsconf finconf hconf)) where
  logContext = lens scLogContext (\sc v -> sc{scLogContext = v })
  {- - INLINE logContext - -}

instance (c ~ GSContext gsconf) => HasGlobalStateContext c (SkovContext (SkovConfig gsconf finconf hconf)) where
    globalStateContext = lens (scGSContext) (\sc v -> sc{scGSContext = v})
    {- - INLINE globalStateContext - -}
instance (g ~ GSState gsconf) => HasGlobalState g (SkovState (SkovConfig gsconf finconf hconf)) where
    globalState = lens (ssGSState) (\ss v -> ss {ssGSState = v})
    {- - INLINE globalState - -}

instance (MonadIO m,
          HandlerConfigHandlers (SkovConfig gsconf finconf hconf) (SkovT h (SkovConfig gsconf finconf hconf) m),
          SkovPendingLiveHandlers h m,
          SkovMonad (SkovT h (SkovConfig gsconf finconf hconf) m))
         => OnSkov (SkovT h (SkovConfig gsconf finconf hconf) m) where
    onBlock bp = handleBlock bp
    onFinalize fr bp = handleFinalize fr bp
    onPendingLive = SkovT $ \h _ -> lift $ handlePendingLive h
    {- - INLINE onBlock - -}
    {- - INLINE onFinalize - -}
    {- - INLINE onPendingLive - -}

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
    instance (t ~ SkovHandlerTimer h, MonadIO m, TimeMonad m, MonadLogger m, SkovMonad (SkovT h (SkovConfig gc (BufferedFinalization t) hc) m),
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
        BlockFields ~ BlockFieldType (BlockPointerType (SkovTGSM h c m)),
        BlockPointerType (TreeStateConfigM h c m) ~ BlockPointerType (SkovT h c m))

type SkovQueryConfigMonad c m =
    (BlockFields ~ BlockFieldType (BlockPointerType (TreeStateConfigM () c m)),
     TreeStateMonad (SkovTGSM () c m),
     BlockPointerMonad (SkovTGSM () c m),
     BlockStateStorage (BlockStateConfigM () c m)
    )
