
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Concordium.Skov.MonadImplementations where

import Data.Kind
import Control.Monad.State.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.RWS.Strict
import Control.Monad.Identity
import Lens.Micro.Platform
import Data.Proxy
import qualified Data.Text as Text

import Concordium.Genesis.Data
import Concordium.Types
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus(..))
import Concordium.Types.Updates

import Concordium.GlobalState.Finalization
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.Block hiding (PendingBlock)
import Concordium.GlobalState
import Concordium.Skov.Monad as Skov
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

-- |Base monad that provides: IO, logging, global-state context and global-state state.
type RWSTIO c pv = RWST (Identity (GSContext c pv)) () (Identity (GSState c pv)) LogIO

type BlockStateType pv c = BlockStateM
                        pv
                        (GSContext c pv)
                        (Identity (GSContext c pv))
                        (GSState c pv)
                        (Identity (GSState c pv))
                        (RWSTIO c pv)

type TreeStateType pv c = TreeStateBlockStateM
                       pv
                       (GSState c pv)
                       (GSContext c pv)
                       (Identity (GSContext c pv))
                       (Identity (GSState c pv))
                       (RWSTIO c pv)

type GlobalStateQuery pv c = (BlockStateStorage (BlockStateType pv c), TreeStateMonad (TreeStateType pv c))

-- |This is a convenience wrapper that automatically implements SkovQueryMonad via SkovQueryMonadT
-- instance.
type GlobalState pv c =
  GlobalStateM pv (GSContext c pv) (Identity (GSContext c pv)) (GSState c pv) (Identity (GSState c pv)) (RWSTIO c pv)

evalGlobalState :: Proxy c -> GlobalState pv c a -> GSContext c pv -> GSState c pv -> LogIO a
evalGlobalState _ comp gsCtx gsState = fst <$> evalRWST (runGlobalStateM comp) (Identity gsCtx) (Identity gsState)


getFinalizationState :: forall pv c timer. GlobalStateQuery pv c
                     => Proxy pv
                     -> Proxy c
                     -- ^Dummy type to fix the type variable 'c' which does not appear in the result type
                     -- and is not uniquely determined by GSContext or GSState since they are non-injective
                     -- type families.
                     -> (GSContext c pv, GSState c pv)
                     -- ^Global state from which to get the tree information.
                     -> Maybe FinalizationInstance
                     -- ^Just finInst if active finalization, Nothing if keys are not available.
                     -> LogIO (FinalizationState timer)
getFinalizationState _ _ (gsCtx, gsState) mInst = fst <$> evalRWST (runGlobalStateM comp) (Identity gsCtx) (Identity gsState)
  where comp :: GlobalState pv c (FinalizationState timer)
        comp = recoverFinalizationState mInst

-- * Handler configuration

-- |'HandlerConfig' characterises a type of configuration for handlers that can respond to certain
-- consensus events.
class HandlerConfig handlerconfig where
    -- |The read-only context type associated with handlers of this type of configuration.
    type HCContext handlerconfig
    -- |The state type associated with handlers of this type of configuration.
    type HCState handlerconfig
    -- |Generate an initial context and state from a handler configuration.
    initialiseHandler :: handlerconfig -> (HCContext handlerconfig, HCState handlerconfig)

-- * Finalization configuration

-- |'FinalizationConfig' characterises a type of configuration for how finalization is handled.
class FinalizationConfig finconfig where
    -- |The read-only context type associated with this type of finalization configuration.
    type FCContext finconfig
    -- |The state type associated with this type of finalization configuration.
    type FCState finconfig
    -- |Generate an initial context and state from a finalization configuration.
    initialiseFinalization :: (MonadIO m, SkovQueryMonad m) => finconfig -> m (FCContext finconfig, FCState finconfig)

-- |Type of finalization configuration for no active participation in finalization.
-- The type parameter is the type of timers supported by the 'TimerMonad' in which finalization
-- operations occur.
data NoFinalization (timer :: Type) = NoFinalization

instance FinalizationConfig (NoFinalization t) where
    type FCContext (NoFinalization t) = ()
    type FCState (NoFinalization t) = FinalizationState t
    initialiseFinalization NoFinalization =
      ((),) <$> recoverFinalizationState Nothing

-- This provides an implementation of FinalizationOutputMonad that does nothing.
-- This should be fine, because NoFinalization indicates that no participation in
-- finalization is expected.  However, in future, it would be nice if the type signatures
-- in finalization meant that this instance is not required.
instance (Monad m)
        => FinalizationOutputMonad (SkovT pv h (SkovConfig pv gc (NoFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage _ = return ()

-- |Type of finalization configuration for active participation in finalization with no buffering.
-- The type parameter is the type of timers supported by the 'TimerMonad' in which finalization
-- operations occur.
newtype ActiveFinalization (timer :: Type) = ActiveFinalization FinalizationInstance

instance FinalizationConfig (ActiveFinalization t) where
    type FCContext (ActiveFinalization t) = FinalizationInstance
    type FCState (ActiveFinalization t) = FinalizationState t
    initialiseFinalization (ActiveFinalization finInst) =
      (finInst,) <$> recoverFinalizationState (Just finInst)

instance (SkovFinalizationHandlers h m, Monad m)
        => FinalizationOutputMonad (SkovT pv h (SkovConfig pv gc (ActiveFinalization t) hc) m) where
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

-- |Type of finalization configuration for active participation in finalization with buffering
-- to reduce the number of finalization messages that need to be sent.
-- The type parameter is the type of timers supported by the 'TimerMonad' in which finalization
-- operations occur.
newtype BufferedFinalization (timer :: Type) = BufferedFinalization FinalizationInstance


instance FinalizationConfig (BufferedFinalization t) where
    type FCContext (BufferedFinalization t) = FinalizationInstance
    type FCState (BufferedFinalization t) = BufferedFinalizationState t
    initialiseFinalization (BufferedFinalization finInst) = do
      finalizationState <- recoverFinalizationState (Just finInst)
      return (finInst, BufferedFinalizationState finalizationState emptyFinalizationBuffer)

instance (SkovFinalizationHandlers h m, Monad m, TimeMonad m, MonadLogger m, SkovTimerHandlers pv h (SkovConfig pv gc (BufferedFinalization t) hc) m)
        => FinalizationOutputMonad (SkovT pv h (SkovConfig pv gc (BufferedFinalization t) hc) m) where
    broadcastFinalizationMessage = bufferFinalizationMessage (\msg' -> SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h (FPMMessage msg')))
    broadcastFinalizationPseudoMessage (FPMMessage msg) = broadcastFinalizationMessage msg
    broadcastFinalizationPseudoMessage pmsg = SkovT (\h _ -> lift $ handleBroadcastFinalizationMessage h pmsg)

-- * Skov configuration

-- |Configuration for the Skov.
-- This type has the following parameters:
--
-- * @pv@: the protocol version. Possible values include @'P1@.
-- * @gsconfig@: the global state configuration. This should be an instance of 'GlobalStateConfig',
--   such as @MemoryTreeMemoryBlockConfig pv@ or @DiskTreeDiskBlockConfig pv@.
-- * @finconfig@: the finalization configuration. Currently supported types are @NoFinalization t@,
--   @ActiveFinalization t@ and @BufferedFinalization t@, where @t@ is the type of timers in the supporting monad.
-- * @handlerconfig@ is the type of event handlers. Currently supported types are @NoHandlers@ and @LogUpdateHandlers@.
data SkovConfig (pv :: ProtocolVersion) gsconfig finconfig handlerconfig = SkovConfig !gsconfig !finconfig !handlerconfig

-- |The type of contexts (i.e. read only data) for the skov configuration type.
data family SkovContext c
data instance SkovContext (SkovConfig pv gsconf finconf hconf) = SkovContext {
        scGSContext :: !(GSContext gsconf pv),
        scFinContext :: !(FCContext finconf),
        scHandlerContext :: !(HCContext hconf)
    }

-- |The type of states (i.e. mutable data) for the skov configuration type.
data family SkovState c
data instance SkovState (SkovConfig pv gsconf finconf hconf) = SkovState {
        ssGSState :: !(GSState gsconf pv),
        ssFinState :: !(FCState finconf),
        ssHandlerState :: !(HCState hconf)
    }

type family SkovGSState c
type instance SkovGSState (SkovConfig pv gsconf finconf hconf) = GSState gsconf pv
type family SkovGSContext c
type instance SkovGSContext (SkovConfig pv gsconf finconf hconf) = GSContext gsconf pv

-- |A pair of 'SkovContext' and 'SkovState' for a given 'SkovConfig' determined by the type parameters.
type InitialisedSkov pv gsconfig finconfig handlerconfig = (SkovContext (SkovConfig pv gsconfig finconfig handlerconfig), SkovState (SkovConfig pv gsconfig finconfig handlerconfig))

class SkovConfiguration gsconfig finconfig handlerconfig where
    -- |Create an initial context and state from a given configuration. The
    -- return value is 'Maybe' if an existing state was found for the given
    -- configuration and successfully loaded. In case the state is found, but
    -- could not be loaded, an IO exception will be thrown.
    --
    -- To use the state for an active consensus instance use 'activateSkovState'
    -- after initialising the state.
    initialiseExistingSkov :: IsProtocolVersion pv => SkovConfig pv gsconfig finconfig handlerconfig ->
        LogIO (Maybe (InitialisedSkov pv gsconfig finconfig handlerconfig))
    

    -- |Create an initial context and state from a given configuration assuming
    -- no state exists. If the state exists then an IO exception will be thrown.
    --
    -- It is not necessary to call 'activateSkovState' on the resulting state.
    initialiseNewSkov :: IsProtocolVersion pv
                      => GenesisData pv
                      -> SkovConfig pv gsconfig finconfig handlerconfig
                      -> LogIO (SkovContext (SkovConfig pv gsconfig finconfig handlerconfig), SkovState (SkovConfig pv gsconfig finconfig handlerconfig))

    -- |A helper which attemps to use the existing state if it exists, and
    -- otherwise initialises skov from a new state created from the given genesis.
    initialiseSkov :: IsProtocolVersion pv
                   => GenesisData pv
                   -> SkovConfig pv gsconfig finconfig handlerconfig
                   -> LogIO (SkovContext (SkovConfig pv gsconfig finconfig handlerconfig), SkovState (SkovConfig pv gsconfig finconfig handlerconfig))
    initialiseSkov gd cfg = initialiseExistingSkov cfg >>= \case
        Nothing -> initialiseNewSkov gd cfg
        Just x -> return x

    -- |Activate an initialised skov instance. Activation is necessary before
    -- the state can be used by consensus for anything other than queries.
    activateSkovState :: IsProtocolVersion pv
                      => SkovContext (SkovConfig pv gsconfig finconfig handlerconfig)
                      -> SkovState (SkovConfig pv gsconfig finconfig handlerconfig)
                      -> LogIO (SkovState (SkovConfig pv gsconfig finconfig handlerconfig))

    -- |Free any resources when we are done with the context and state.
    shutdownSkov :: IsProtocolVersion pv => SkovContext (SkovConfig pv gsconfig finconfig handlerconfig) -> SkovState (SkovConfig pv gsconfig finconfig handlerconfig) -> LogIO ()

instance
    ( GlobalStateConfig gsconfig,
      FinalizationConfig finconfig,
      HandlerConfig handlerconfig,
      Show (FCContext finconfig),
      Show (FCState finconfig),
      forall pv c s.
      (IsProtocolVersion pv, c ~ GSContext gsconfig pv, s ~ GSState gsconfig pv) =>
      SkovQueryMonad
        ( GlobalStateM
            pv
            c
            (Identity c)
            s
            (Identity s)
            ( RWST
                (Identity c)
                ()
                (Identity s)
                LogIO
            )
        )
    ) =>
    SkovConfiguration gsconfig finconfig handlerconfig
    where
    initialiseExistingSkov ::
        forall pv.
        IsProtocolVersion pv =>
        SkovConfig pv gsconfig finconfig handlerconfig ->
        LogIO (Maybe (SkovContext (SkovConfig pv gsconfig finconfig handlerconfig), SkovState (SkovConfig pv gsconfig finconfig handlerconfig)))
    initialiseExistingSkov (SkovConfig gsc finconf hconf) = do
        logEvent Skov LLDebug "Attempting to use existing global state."
        initialiseExistingGlobalState (protocolVersion @pv) gsc >>= \case
          Nothing -> do
            logEvent Skov LLDebug "No existing global state."
            return Nothing
          Just (c, s) -> do
            (finctx, finst) <- evalGlobalState @_ @pv (Proxy @gsconfig) (initialiseFinalization finconf) c s
            logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
            logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
            let (hctx, hst) = initialiseHandler hconf
            return (Just (SkovContext c finctx hctx, SkovState s finst hst))

    initialiseNewSkov ::
        forall pv.
        IsProtocolVersion pv =>
        GenesisData pv ->
        SkovConfig pv gsconfig finconfig handlerconfig ->
        LogIO (SkovContext (SkovConfig pv gsconfig finconfig handlerconfig), SkovState (SkovConfig pv gsconfig finconfig handlerconfig))
    initialiseNewSkov genData (SkovConfig gsc finconf hconf) = do
        logEvent Skov LLDebug "Creating new global state."
        (c, s) <- initialiseNewGlobalState genData gsc
        (finctx, finst) <- evalGlobalState @_ @pv (Proxy @gsconfig) (initialiseFinalization finconf) c s
        logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
        logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
        let (hctx, hst) = initialiseHandler hconf
        return (SkovContext c finctx hctx, SkovState s finst hst)

    activateSkovState ::
        forall pv . IsProtocolVersion pv =>
        SkovContext (SkovConfig pv gsconfig finconfig handlerconfig) ->
        SkovState (SkovConfig pv gsconfig finconfig handlerconfig) ->
        LogIO (SkovState (SkovConfig pv gsconfig finconfig handlerconfig))
    activateSkovState skovContext skovState = do
        activatedState <- activateGlobalState (Proxy @gsconfig) (Proxy @pv) (scGSContext skovContext) (ssGSState skovState)
        return skovState { ssGSState = activatedState }
    shutdownSkov :: forall pv. IsProtocolVersion pv => SkovContext (SkovConfig pv gsconfig finconfig handlerconfig) -> SkovState (SkovConfig pv gsconfig finconfig handlerconfig) -> LogIO ()
    shutdownSkov (SkovContext c _ _) (SkovState s _ _) = liftIO $ shutdownGlobalState (protocolVersion @pv) (Proxy :: Proxy gsconfig) c s

-- |An instance of 'SkovTimerHandlers' provides a means for implementing
-- a 'TimerMonad' instance for 'SkovT'.
class SkovTimerHandlers pv h c m | h -> pv m c where
    -- |Type to represent a timer
    type SkovHandlerTimer h
    -- |Handler for creating a timer event
    handleOnTimeout :: h -> Timeout -> SkovT pv h c m a -> m (SkovHandlerTimer h)
    -- |Handler for cancelling a timer
    handleCancelTimer :: h -> SkovHandlerTimer h -> m ()

-- |An instance of 'SkovFinalizationHandlers' provides handlers for broadcasting
-- finalization-related messages.
class SkovFinalizationHandlers h m where
    handleBroadcastFinalizationMessage :: h -> FinalizationPseudoMessage -> m ()

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
data SkovHandlers pv t c m = SkovHandlers {
    shBroadcastFinalizationMessage :: FinalizationPseudoMessage -> m (),
    shOnTimeout :: forall a. Timeout -> SkovT pv (SkovHandlers pv t c m) c m a -> m t,
    shCancelTimer :: t -> m (),
    shPendingLive :: m ()
}

instance SkovFinalizationHandlers (SkovHandlers pv t c m) m where
    handleBroadcastFinalizationMessage SkovHandlers{..} = shBroadcastFinalizationMessage

instance SkovTimerHandlers pv (SkovHandlers pv t c m) c m where
    type SkovHandlerTimer (SkovHandlers pv t c m) = t
    handleOnTimeout SkovHandlers{..} = shOnTimeout
    handleCancelTimer SkovHandlers{..} = shCancelTimer

instance SkovPendingLiveHandlers (SkovHandlers pv t c m) m where
    handlePendingLive = shPendingLive

newtype SkovPassiveHandlers (pv :: ProtocolVersion) (c :: Type) m = SkovPassiveHandlers {
    sphPendingLive :: m ()
}

instance SkovPendingLiveHandlers (SkovPassiveHandlers pv c m) m where
    handlePendingLive = sphPendingLive

-- This provides an instance of timer handlers that should not be used.
-- TODO: In future, the types in finalization should be refined so that
-- this instance is not needed.
instance SkovTimerHandlers pv (SkovPassiveHandlers pv c m) c m where
    type SkovHandlerTimer (SkovPassiveHandlers pv c m) = ()
    handleOnTimeout _ _ _ = error "Attempted to set a timer, but SkovPassiveHandlers does not support timers."
    handleCancelTimer _ _ = error "Attempted to cancel a timer, but SkovPassiveHandlers does not support timers."

-- |The 'SkovT' monad transformer equips a monad with state, context and handlers for
-- performing Skov operations.
-- The type parameters are as follows:
--
-- * @pv@: the protocol version.
-- * @h@: the handler configuration. This should be an instance of 'SkovPendingLiveHandlers', and,
--   if active finalization is required, 'SkovFinalizationHandlers' and 'SkovTimerHandlers'.
--   Typically, this is either @SkovHandlers pv t s m@ or @SkovPassiveHandlers pv c m@.
-- * @c@: the Skov configuration. Typically, this is @SkovConfig pv gc fc hc@.
-- * @m@: the underlying monad. Typically, this should be an instance of 'MonadIO', 'MonadLogger',
--   and 'TimeMonad'.
-- * @a@: the return type.
newtype SkovT (pv :: ProtocolVersion) h c m a = SkovT { runSkovT' :: h -> SkovContext c -> StateT (SkovState c) m a }
    deriving (Functor, Applicative, Monad, MonadState (SkovState c), MonadIO, MonadLogger, TimeMonad)
        via (ReaderT h (ReaderT (SkovContext c) (StateT (SkovState c) m)))

-- GlobalStateM using config abstractions
type SkovTGSM pv h c m = GlobalStateM pv (SkovGSContext c) (SkovContext c) (SkovGSState c) (SkovState c) (SkovT pv h c m)
type SkovTTBM pv h c m = TreeStateBlockStateM pv (SkovGSState c) (SkovGSContext c) (SkovContext c) (SkovState c) (SkovT pv h c m)

runSkovT :: SkovT pv h c m a -> h -> SkovContext c -> SkovState c -> m (a, SkovState c)
runSkovT (SkovT a) h c = runStateT (a h c)

evalSkovT :: (Monad m) => SkovT pv h c m a -> h -> SkovContext c -> SkovState c -> m a
evalSkovT (SkovT a) h c = evalStateT (a h c)

instance (Monad m) => MonadReader (SkovContext c) (SkovT pv h c m) where
    ask = SkovT (\_ c -> return c)
    local f (SkovT a) = SkovT (\h -> a h . f)
    {- - INLINE ask - -}
    {- - INLINE local - -}

instance MonadTrans (SkovT pv h c) where
    lift a = SkovT (\_ _ -> lift a)
    {- - INLINE lift - -}

instance (Monad m, SkovTimerHandlers pv h c m) => TimerMonad (SkovT pv h c m) where
    type Timer (SkovT pv h c m) = SkovHandlerTimer h
    onTimeout timeout a = SkovT (\h _ -> lift (handleOnTimeout h timeout a))
    cancelTimer t = SkovT (\h _ -> lift (handleCancelTimer h t))
-----------------------------------------------------------------------------
-- Inherit the instances from GlobalStateM

deriving via SkovTGSM pv h c' m
    instance BlockStateTypes (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              BlockStateQuery (SkovTGSM pv h c' m))
             => BlockStateQuery (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              AccountOperations (SkovTGSM pv h c' m))
             => AccountOperations (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              ContractStateOperations (SkovTGSM pv h c' m))
             => ContractStateOperations (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              ModuleQuery (SkovTGSM pv h c' m))
             => ModuleQuery (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              BlockStateOperations (SkovTGSM pv h c' m))
             => BlockStateOperations (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              BlockStateStorage (SkovTGSM pv h c' m))
             => BlockStateStorage (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance GlobalStateTypes (SkovTGSM pv h c' m)
             => GlobalStateTypes (SkovT pv h c' m)

instance (IsProtocolVersion pv) => MonadProtocolVersion (SkovT pv h c' m) where
    type MPV (SkovT pv h c' m) = pv

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              BlockPointerMonad (SkovTGSM pv h c' m))
             => BlockPointerMonad (SkovT pv h c' m)

deriving via SkovTGSM pv h c' m
    instance (Monad m,
              IsProtocolVersion pv,
              TreeStateMonad (SkovTGSM pv h c' m))
              => TreeStateMonad (SkovT pv h c' m)

deriving via SkovQueryMonadT (SkovT pv h c m) instance (
        IsProtocolVersion pv,
        Monad m,
        BlockStateQuery (SkovT pv h c m),
        BlockPointerMonad (SkovT pv h c m),
        TreeStateMonad (SkovT pv h c m)
        ) => SkovQueryMonad (SkovT pv h c m)

instance (
        Monad m,
        TimeMonad m,
        MonadLogger m,
        OnSkov (SkovT pv h c m),
        BlockStateStorage (SkovT pv h c m),
        TreeStateMonad (SkovT pv h c m),
        FinalizationMonad (SkovT pv h c m))
        => SkovMonad (SkovT pv h c m) where
    {- - INLINE storeBlock - -}
    storeBlock = doStoreBlock
    {- - INLINE receiveTransaction - -}
    receiveTransaction tr = doReceiveTransaction tr
    {- - INLINE trustedFinalize - -}
    trustedFinalize = doTrustedFinalize
    {- - INLINE handleCatchUpStatus - -}
    handleCatchUpStatus = doHandleCatchUp
    terminateSkov = doTerminateSkov
    purgeTransactions = doPurgeTransactions

class (Monad m, HandlerConfig c) => HandlerConfigHandlers c m | m -> c where
    handleBlock :: BlockPointerType m -> m ()
    handleFinalize :: FinalizationRecord -> BlockPointerType m -> m ()

-- |A handler that does nothing.
data NoHandler = NoHandler

instance HandlerConfig NoHandler where
    type HCContext NoHandler = ()
    type HCState NoHandler = ()
    initialiseHandler = \_ -> ((),())

instance (Monad m) => HandlerConfigHandlers NoHandler (SkovT pv h (SkovConfig pv gc fc NoHandler) m) where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ -> return ()

-- |A handler that checks finalized blocks for protocol updates and:
--  * logs a warning if a protocol update is queued;
--  * logs an error if a protocol update has occurred.
data LogUpdateHandler = LogUpdateHandler

instance HandlerConfig LogUpdateHandler where
    type HCContext LogUpdateHandler = ()
    type HCState LogUpdateHandler = ()
    initialiseHandler = \_ -> ((),())

instance (SkovMonad (SkovT pv h (SkovConfig pv gc fc LogUpdateHandler) m)) => HandlerConfigHandlers LogUpdateHandler (SkovT pv h (SkovConfig pv gc fc LogUpdateHandler) m) where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ ->
        Skov.getProtocolUpdateStatus >>= \case
            ProtocolUpdated pu -> logEvent Kontrol LLError $
                "Consensus has been updated: " ++ showPU pu
            PendingProtocolUpdates [] -> return ()
            PendingProtocolUpdates ((ts, pu):_) -> logEvent Kontrol LLWarning $
                "Consensus is scheduled to update at " ++
                show (timestampToUTCTime $ transactionTimeToTimestamp ts) ++
                ": " ++ showPU pu
        where
            showPU ProtocolUpdate{..} = Text.unpack puMessage ++ "\n["
                ++ Text.unpack puSpecificationURL ++ " (hash " ++ show puSpecificationHash ++ ")]"

instance (FinalizationQueueLenses (FCState finconf))
        => FinalizationQueueLenses (SkovState (SkovConfig pv gsconf finconf hconf)) where
    finQueue = lens ssFinState (\s fs -> s {ssFinState = fs}) . finQueue
    {- - INLINE finQueue - -}

instance (FinalizationStateLenses (FCState finconf) t)
        => FinalizationStateLenses (SkovState (SkovConfig pv gsconf finconf hconf)) t where
    finState = lens ssFinState (\s fs -> s {ssFinState = fs}) . finState
    {- - INLINE finState - -}

instance (FinalizationBufferLenses (FCState finconf))
        => FinalizationBufferLenses (SkovState (SkovConfig pv gsconf finconf hconf)) where
    finBuffer = lens ssFinState (\s fs -> s {ssFinState = fs}) . finBuffer
    {- - INLINE finBuffer - -}

instance (HasFinalizationInstance (FCContext finconf))
        => HasFinalizationInstance (SkovContext (SkovConfig pv gsconf finconf hconf)) where
    finalizationInstance = finalizationInstance . scFinContext
    {- - INLINE finalizationInstance - -}

instance (c ~ GSContext gsconf pv) => HasGlobalStateContext c (SkovContext (SkovConfig pv gsconf finconf hconf)) where
    globalStateContext = lens scGSContext (\sc v -> sc{scGSContext = v})
    {- - INLINE globalStateContext - -}
instance (g ~ GSState gsconf pv) => HasGlobalState g (SkovState (SkovConfig pv gsconf finconf hconf)) where
    globalState = lens ssGSState (\ss v -> ss {ssGSState = v})
    {- - INLINE globalState - -}

instance (MonadIO m,
          HandlerConfigHandlers hconf (SkovT pv h (SkovConfig pv gsconf finconf hconf) m),
          SkovPendingLiveHandlers h m)
         => OnSkov (SkovT pv h (SkovConfig pv gsconf finconf hconf) m) where
    onBlock bp = handleBlock bp
    onFinalize fr bp = handleFinalize fr bp
    onPendingLive = SkovT $ \h _ -> lift $ handlePendingLive h
    {- - INLINE onBlock - -}
    {- - INLINE onFinalize - -}
    {- - INLINE onPendingLive - -}

-- |Synonym for 'ActiveFinalizationM' based on a specific configuration. Arguments:
--
--   * @pv@: protocol version
--   * @gc@: global state configuration type
--   * @fc@: finalization configuration type
--   * @hc@: handler configuration type
--   * @h@: handler type
--   * @m@: base monad
type ActiveFinalizationMWith pv gc fc hc h m =
    ActiveFinalizationM
        pv
        (SkovContext (SkovConfig pv gc fc hc))
        (SkovState (SkovConfig pv gc fc hc))
        (SkovT pv h (SkovConfig pv gc fc hc) m)

deriving via (ActiveFinalizationMWith pv gc (NoFinalization t) hc h m)
    instance (t ~ SkovHandlerTimer h, MonadIO m, SkovMonad (SkovT pv h (SkovConfig pv gc (NoFinalization t) hc) m),
        SkovTimerHandlers pv h (SkovConfig pv gc (NoFinalization t) hc) m)
        => FinalizationMonad (SkovT pv h (SkovConfig pv gc (NoFinalization t) hc) m)

deriving via (ActiveFinalizationMWith pv gc (ActiveFinalization t) hc h m)
    instance (t ~ SkovHandlerTimer h, MonadIO m, SkovMonad (SkovT pv h (SkovConfig pv gc (ActiveFinalization t) hc) m),
        SkovTimerHandlers pv h (SkovConfig pv gc (ActiveFinalization t) hc) m, SkovFinalizationHandlers h m)
        => FinalizationMonad (SkovT pv h (SkovConfig pv gc (ActiveFinalization t) hc) m)

deriving via (ActiveFinalizationMWith pv gc (BufferedFinalization t) hc h m)
    instance (t ~ SkovHandlerTimer h, MonadIO m, TimeMonad m, MonadLogger m, SkovMonad (SkovT pv h (SkovConfig pv gc (BufferedFinalization t) hc) m),
        SkovTimerHandlers pv h (SkovConfig pv gc (BufferedFinalization t) hc) m, SkovFinalizationHandlers h m)
        => FinalizationMonad (SkovT pv h (SkovConfig pv gc (BufferedFinalization t) hc) m)

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

type BlockStateConfigM pv h c m = BlockStateM
               pv
               (SkovGSContext c)
               (SkovContext c)
               (SkovGSState c)
               (SkovState c)
               (SkovT pv h c m)

type TreeStateConfigM pv h c m = TreeStateM
               (SkovGSState c)
               (BlockStateM
                pv
                (SkovGSContext c)
                (SkovContext c)
                (SkovGSState c)
                (SkovState c)
                (SkovT pv h c m))

type SkovConfigMonad (pv :: ProtocolVersion) (h :: Type) (c :: Type) (m :: Type -> Type) =
    ( OnSkov (SkovT pv h c m),
      BlockStateStorage (BlockStateConfigM pv h c m),
      GlobalStateTypes (TreeStateConfigM pv h c m),
      TreeStateMonad (TreeStateConfigM pv h c m),
      BlockPointerMonad (TreeStateConfigM pv h c m),
      BlockFields ~ BlockFieldType (BlockPointerType (SkovTGSM pv h c m)),
      BlockPointerType (TreeStateConfigM pv h c m) ~ BlockPointerType (SkovT pv h c m)
    )

type SkovQueryConfigMonad pv c m =
    ( BlockFields ~ BlockFieldType (BlockPointerType (TreeStateConfigM pv () c m)),
      TreeStateMonad (SkovTGSM pv () c m),
      BlockPointerMonad (SkovTGSM pv () c m),
      BlockStateStorage (BlockStateConfigM pv () c m)
    )
