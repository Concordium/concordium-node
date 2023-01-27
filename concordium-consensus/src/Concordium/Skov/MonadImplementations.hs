{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.Skov.MonadImplementations where

import Control.Monad.RWS.Strict
import Control.Monad.Reader
import Data.Kind
import Data.Proxy
import Lens.Micro.Platform

import Concordium.Genesis.Data
import Concordium.Types

import Concordium.Afgjort.Buffer
import Concordium.Afgjort.FinalizationQueue
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.Monad
import Concordium.GlobalState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.BlockState.Modules
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.LMDB
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState
import Concordium.Logger
import Concordium.Skov.CatchUp
import Concordium.Skov.Monad as Skov
import Concordium.Skov.Update
import Concordium.TimeMonad
import Concordium.TimerMonad

-- |Monad that provides: IO, logging, global-state context, global-state state and SkovQueryMonad via SkovQueryMonadT.
newtype GlobalState pv a = GlobalState
    { runGlobalState ::
        SkovQueryMonadT
            ( PersistentTreeStateMonad
                (GSState pv)
                ( PersistentBlockStateMonad
                    pv
                    (GSContext pv)
                    (RWST (GSContext pv) () (GSState pv) LogIO)
                )
            )
            a
    }
    deriving
        ( Applicative,
          Functor,
          Monad,
          MonadIO,
          BlockStateTypes,
          GlobalStateTypes,
          ContractStateOperations,
          AccountOperations,
          ModuleQuery,
          BlockStateQuery,
          BlockStateOperations,
          BlockStateStorage,
          BlockPointerMonad,
          SkovQueryMonad
        )

instance IsProtocolVersion pv => MonadProtocolVersion (GlobalState pv) where
    type MPV (GlobalState pv) = pv

evalGlobalState :: GlobalState pv a -> GSContext pv -> GSState pv -> LogIO a
evalGlobalState comp gsCtx gsState = fst <$> evalRWST (runPersistentBlockStateMonad . runPersistentTreeStateMonad . runSkovQueryMonad . runGlobalState $ comp) gsCtx gsState

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
instance
    (Monad m) =>
    FinalizationOutputMonad (SkovT pv h (SkovConfig pv (NoFinalization t) hc) m)
    where
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

instance
    (SkovFinalizationHandlers h m, Monad m) =>
    FinalizationOutputMonad (SkovT pv h (SkovConfig pv (ActiveFinalization t) hc) m)
    where
    broadcastFinalizationPseudoMessage pmsg = do
        h <- askHandler
        lift $ handleBroadcastFinalizationMessage h pmsg

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

instance
    (SkovFinalizationHandlers h m, Monad m, TimeMonad m, MonadLogger m, SkovTimerHandlers pv h (SkovConfig pv (BufferedFinalization t) hc) m) =>
    FinalizationOutputMonad (SkovT pv h (SkovConfig pv (BufferedFinalization t) hc) m)
    where
    broadcastFinalizationMessage =
        bufferFinalizationMessage
            ( \msg' -> do
                h <- askHandler
                lift $ handleBroadcastFinalizationMessage h (FPMMessage msg')
            )
    broadcastFinalizationPseudoMessage (FPMMessage msg) = broadcastFinalizationMessage msg
    broadcastFinalizationPseudoMessage pmsg = do
        h <- askHandler
        lift $ handleBroadcastFinalizationMessage h pmsg

-- * Skov configuration

-- |Configuration for the Skov.
-- This type has the following parameters:
--
-- * @pv@: the protocol version. Possible values include @'P1@.
-- * @finconfig@: the finalization configuration. Currently supported types are @NoFinalization t@,
--   @ActiveFinalization t@ and @BufferedFinalization t@, where @t@ is the type of timers in the supporting monad.
-- * @handlerconfig@ is the type of event handlers. Currently supported types are @NoHandlers@ and @LogUpdateHandlers@.
data SkovConfig (pv :: ProtocolVersion) finconfig handlerconfig = SkovConfig GlobalStateConfig !finconfig !handlerconfig

-- |The type of contexts (i.e. read only data) for the skov configuration type.
data family SkovContext c

data instance SkovContext (SkovConfig pv finconf hconf) = SkovContext
    { scGSContext :: !(GSContext pv),
      scFinContext :: !(FCContext finconf),
      scHandlerContext :: !(HCContext hconf)
    }

-- |The type of states (i.e. mutable data) for the skov configuration type.
data family SkovState c

data instance SkovState (SkovConfig pv finconf hconf) = SkovState
    { ssGSState :: !(GSState pv),
      ssFinState :: !(FCState finconf),
      ssHandlerState :: !(HCState hconf)
    }

-- |A pair of 'SkovContext' and 'SkovState' for a given 'SkovConfig' determined by the type parameters.
type InitialisedSkov pv finconfig handlerconfig = (SkovContext (SkovConfig pv finconfig handlerconfig), SkovState (SkovConfig pv finconfig handlerconfig))

class SkovConfiguration finconfig handlerconfig where
    -- |Create an initial context and state from a given configuration. The
    -- return value is 'Maybe' if an existing state was found for the given
    -- configuration and successfully loaded. In case the state is found, but
    -- could not be loaded, an IO exception will be thrown.
    --
    -- To use the state for an active consensus instance use 'activateSkovState'
    -- after initialising the state.
    initialiseExistingSkov ::
        IsProtocolVersion pv =>
        SkovConfig pv finconfig handlerconfig ->
        LogIO (Maybe (InitialisedSkov pv finconfig handlerconfig))

    -- |Create an initial context and state from a given configuration assuming
    -- no state exists. If the state exists then an IO exception will be thrown.
    --
    -- It is not necessary to call 'activateSkovState' on the resulting state.
    initialiseNewSkov ::
        IsProtocolVersion pv =>
        GenesisData pv ->
        SkovConfig pv finconfig handlerconfig ->
        LogIO (SkovContext (SkovConfig pv finconfig handlerconfig), SkovState (SkovConfig pv finconfig handlerconfig))

    -- |Migrate an existing skov instance to a fresh one. This is used on
    -- protocol updates to construct a new instance to be used after the
    -- protocol update. The new instance is constructed by migrating state from
    -- the existing one. The block state and the tree state are migrated, and at
    -- present the new instance is from the time of construction independent of
    -- the old one. Transactions are also migrated from the existing instance to
    -- the new one. See @migrateExistingState@ in @Concordium.GlobalState@ for
    -- additional details.
    migrateExistingSkov ::
        (IsProtocolVersion oldpv, IsProtocolVersion pv) =>
        -- |Context for the existing skov instance.
        SkovContext (SkovConfig oldpv finconfig handlerconfig) ->
        -- |State of the existing skov instance. This must be prepared for
        -- migration. See @rememberFinalState@ and @clearSkovOnProtocolUpdate@, and
        -- @migrateExistingState@ for details on the assumptions on this state.
        SkovState (SkovConfig oldpv finconfig handlerconfig) ->
        -- |Any parameters needed for the migration of the block state.
        StateMigrationParameters oldpv pv ->
        -- |The genesis for the new chain after the protocol update.
        Regenesis pv ->
        -- |Configuration for the new chain after the protocol update.
        SkovConfig pv finconfig handlerconfig ->
        LogIO
            ( SkovContext (SkovConfig pv finconfig handlerconfig),
              SkovState (SkovConfig pv finconfig handlerconfig)
            )

    -- |A helper which attemps to use the existing state if it exists, and
    -- otherwise initialises skov from a new state created from the given genesis.
    initialiseSkov ::
        IsProtocolVersion pv =>
        GenesisData pv ->
        SkovConfig pv finconfig handlerconfig ->
        LogIO (SkovContext (SkovConfig pv finconfig handlerconfig), SkovState (SkovConfig pv finconfig handlerconfig))
    initialiseSkov gd cfg =
        initialiseExistingSkov cfg >>= \case
            Nothing -> initialiseNewSkov gd cfg
            Just x -> return x

    -- |Activate an initialised skov instance. Activation is necessary before
    -- the state can be used by consensus for anything other than queries.
    activateSkovState ::
        IsProtocolVersion pv =>
        SkovContext (SkovConfig pv finconfig handlerconfig) ->
        SkovState (SkovConfig pv finconfig handlerconfig) ->
        LogIO (SkovState (SkovConfig pv finconfig handlerconfig))

    -- |Free any resources when we are done with the context and state.
    shutdownSkov :: IsProtocolVersion pv => SkovContext (SkovConfig pv finconfig handlerconfig) -> SkovState (SkovConfig pv finconfig handlerconfig) -> LogIO ()

instance
    ( FinalizationConfig finconfig,
      HandlerConfig handlerconfig,
      Show (FCContext finconfig),
      Show (FCState finconfig),
      forall pv.
      IsProtocolVersion pv =>
      SkovQueryMonad (GlobalState pv)
    ) =>
    SkovConfiguration finconfig handlerconfig
    where
    initialiseExistingSkov ::
        forall pv.
        IsProtocolVersion pv =>
        SkovConfig pv finconfig handlerconfig ->
        LogIO (Maybe (SkovContext (SkovConfig pv finconfig handlerconfig), SkovState (SkovConfig pv finconfig handlerconfig)))
    initialiseExistingSkov (SkovConfig gsc finconf hconf) = do
        logEvent Skov LLDebug "Attempting to use existing global state."
        initialiseExistingGlobalState (protocolVersion @pv) gsc >>= \case
            Nothing -> do
                logEvent Skov LLDebug "No existing global state."
                return Nothing
            Just (c, s) -> do
                (finctx, finst) <- evalGlobalState @pv (initialiseFinalization finconf) c s
                logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
                logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
                let (hctx, hst) = initialiseHandler hconf
                return (Just (SkovContext c finctx hctx, SkovState s finst hst))

    initialiseNewSkov ::
        forall pv.
        IsProtocolVersion pv =>
        GenesisData pv ->
        SkovConfig pv finconfig handlerconfig ->
        LogIO (SkovContext (SkovConfig pv finconfig handlerconfig), SkovState (SkovConfig pv finconfig handlerconfig))
    initialiseNewSkov genData (SkovConfig gsc finconf hconf) = do
        logEvent Skov LLDebug "Creating new global state."
        (c, s) <- initialiseNewGlobalState genData gsc
        (finctx, finst) <- evalGlobalState @pv (initialiseFinalization finconf) c s
        logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
        logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
        let (hctx, hst) = initialiseHandler hconf
        return (SkovContext c finctx hctx, SkovState s finst hst)

    migrateExistingSkov ::
        forall oldpv pv.
        (IsProtocolVersion oldpv, IsProtocolVersion pv) =>
        SkovContext (SkovConfig oldpv finconfig handlerconfig) ->
        SkovState (SkovConfig oldpv finconfig handlerconfig) ->
        StateMigrationParameters oldpv pv ->
        Regenesis pv ->
        SkovConfig pv finconfig handlerconfig ->
        LogIO
            ( SkovContext (SkovConfig pv finconfig handlerconfig),
              SkovState (SkovConfig pv finconfig handlerconfig)
            )
    migrateExistingSkov oldCtx oldState migration genData (SkovConfig gsc finconf hconf) = do
        logEvent Skov LLDebug "Migrating existing global state."
        (c, s) <- migrateExistingState gsc (scGSContext oldCtx) (ssGSState oldState) migration genData
        (finctx, finst) <- evalGlobalState @pv (initialiseFinalization finconf) c s
        logEvent Skov LLDebug $ "Initializing finalization with context = " ++ show finctx
        logEvent Skov LLDebug $ "Initializing finalization with initial state = " ++ show finst
        let (hctx, hst) = initialiseHandler hconf
        return (SkovContext c finctx hctx, SkovState s finst hst)

    activateSkovState ::
        forall pv.
        IsProtocolVersion pv =>
        SkovContext (SkovConfig pv finconfig handlerconfig) ->
        SkovState (SkovConfig pv finconfig handlerconfig) ->
        LogIO (SkovState (SkovConfig pv finconfig handlerconfig))
    activateSkovState skovContext skovState = do
        activatedState <- activateGlobalState (Proxy @pv) (scGSContext skovContext) (ssGSState skovState)
        return skovState{ssGSState = activatedState}
    shutdownSkov :: forall pv. IsProtocolVersion pv => SkovContext (SkovConfig pv finconfig handlerconfig) -> SkovState (SkovConfig pv finconfig handlerconfig) -> LogIO ()
    shutdownSkov (SkovContext c _ _) (SkovState s _ _) = liftIO $ shutdownGlobalState (protocolVersion @pv) c s

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
data SkovHandlers pv t c m = SkovHandlers
    { shBroadcastFinalizationMessage :: FinalizationPseudoMessage -> m (),
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

newtype SkovPassiveHandlers (pv :: ProtocolVersion) (c :: Type) m = SkovPassiveHandlers
    { sphPendingLive :: m ()
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

-- |Context with handlers and context.
data SkovTInternalContext h c = SkovTInternalContext
    { srHandler :: h,
      srContext :: c
    }

-- |Monad transformer equips a monad with MonadReader (SkovContext c) and MonadState (SkovState c)
-- used internally in SkovT.
newtype SkovTInternal (pv :: ProtocolVersion) h c m a = SkovTInternal
    { runSkovTInternal' :: RWST (SkovTInternalContext h (SkovContext c)) () (SkovState c) m a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (SkovState c),
          MonadIO,
          MonadLogger,
          TimeMonad
        )

runSkovTInternal :: (Monad m) => SkovTInternal pv h c m a -> h -> SkovContext c -> SkovState c -> m (a, SkovState c)
runSkovTInternal comp h context sstate = do
    (b, s, _) <- runRWST (runSkovTInternal' comp) (SkovTInternalContext h context) sstate
    return (b, s)

instance (Monad m) => MonadReader (SkovContext c) (SkovTInternal pv h c m) where
    ask = SkovTInternal $ asks srContext
    local f (SkovTInternal a) = SkovTInternal $ local mapSkovTReadContext a
      where
        mapSkovTReadContext (SkovTInternalContext h context) = SkovTInternalContext h (f context)

instance MonadTrans (SkovTInternal pv h c) where
    lift a = SkovTInternal $ lift a

-- |The 'SkovT' monad transformer equips a monad with state, context and handlers for
-- performing Skov operations.
-- The type parameters are as follows:
--
-- * @pv@: the protocol version.
-- * @h@: the handler configuration. This should be an instance of 'SkovPendingLiveHandlers', and,
--   if active finalization is required, 'SkovFinalizationHandlers' and 'SkovTimerHandlers'.
--   Typically, this is either @SkovHandlers pv t s m@ or @SkovPassiveHandlers pv c m@.
-- * @c@: the Skov configuration. Typically, this is @SkovConfig pv fc hc@.
-- * @m@: the underlying monad. Typically, this should be an instance of 'MonadIO', 'MonadLogger',
--   and 'TimeMonad'.
-- * @a@: the return type.
newtype SkovT pv h c m a = SkovT
    { runSkovT' ::
        PersistentTreeStateMonad
            (SkovState c)
            (PersistentBlockStateMonad pv (SkovContext c) (SkovTInternal pv h c m))
            a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (SkovState c),
          MonadIO,
          MonadLogger,
          TimeMonad,
          BlockStateTypes
        )

runSkovT :: (Monad m) => SkovT pv h c m a -> h -> SkovContext c -> SkovState c -> m (a, SkovState c)
runSkovT comp =
    runSkovTInternal $ runPersistentBlockStateMonad $ runPersistentTreeStateMonad $ runSkovT' comp

evalSkovT :: (Monad m) => SkovT pv h c m a -> h -> SkovContext c -> SkovState c -> m a
evalSkovT comp handler context sstate = fst <$> runSkovT comp handler context sstate

askHandler :: (Monad m) => SkovT pv h c m h
askHandler = SkovT $ lift $ lift $ SkovTInternal $ asks srHandler

instance (Monad m) => MonadReader (SkovContext c) (SkovT pv h c m) where
    ask = SkovT $ lift ask
    local f (SkovT (PersistentTreeStateMonad a)) = SkovT $ lift $ local f a

instance MonadTrans (SkovT pv h c) where
    lift a = SkovT $ lift $ lift $ lift a

instance (Monad m, SkovTimerHandlers pv h c m) => TimerMonad (SkovT pv h c m) where
    type Timer (SkovT pv h c m) = SkovHandlerTimer h
    onTimeout timeout a = do
        h <- askHandler
        lift (handleOnTimeout h timeout a)
    cancelTimer t = do
        h <- askHandler
        lift (handleCancelTimer h t)

instance (IsProtocolVersion pv) => MonadProtocolVersion (SkovT pv h c' m) where
    type MPV (SkovT pv h c' m) = pv

instance (c ~ SkovConfig pv finconfig handlerconfig) => HasBlobStore (SkovContext c) where
    blobStore = blobStore . scGSContext
    blobLoadCallback = blobLoadCallback . scGSContext
    blobStoreCallback = blobStoreCallback . scGSContext

instance (c ~ SkovConfig pv finconfig handlerconfig, AccountVersionFor pv ~ av) => HasCache (AccountCache av) (SkovContext c) where
    projectCache = projectCache . scGSContext

instance (c ~ SkovConfig pv finconfig handlerconfig) => HasCache ModuleCache (SkovContext c) where
    projectCache = projectCache . scGSContext

deriving instance
    ( IsProtocolVersion pv,
      MonadIO m,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    BlockStateQuery (SkovT pv h c m)

deriving instance
    ( MonadIO m,
      IsProtocolVersion pv,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    AccountOperations (SkovT pv h c m)

deriving instance
    ( MonadIO m,
      IsProtocolVersion pv,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    ContractStateOperations (SkovT pv h c m)

deriving instance
    ( MonadIO m,
      IsProtocolVersion pv,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    ModuleQuery (SkovT pv h c m)

deriving instance
    ( IsProtocolVersion pv,
      MonadIO m,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    BlockStateOperations (SkovT pv h c m)

deriving instance
    ( IsProtocolVersion pv,
      MonadIO m,
      c ~ SkovConfig pv finconfig handlerconfig
    ) =>
    BlockStateStorage (SkovT pv h c m)

deriving instance
    (IsProtocolVersion pv) =>
    GlobalStateTypes (SkovT pv h c m)

deriving instance
    ( MonadIO m,
      IsProtocolVersion pv,
      c ~ SkovConfig pv finconfig handlerconfig,
      MonadLogger m
    ) =>
    BlockPointerMonad (SkovT pv h c m)

instance (c ~ SkovConfig pv finconfig handlerconfig, st ~ BlockStatePointer (PersistentBlockState pv)) => HasDatabaseHandlers pv st (SkovState c) where
    dbHandlers = lens ssGSState (\s v -> s{ssGSState = v}) . db

instance (c ~ SkovConfig pv finconfig handlerconfig) => HasSkovPersistentData pv (SkovState c) where
    skovPersistentData = lens ssGSState (\s v -> s{ssGSState = v})

deriving instance
    ( MonadIO m,
      IsProtocolVersion pv,
      c ~ SkovConfig pv finconfig handlerconfig,
      MonadLogger m
    ) =>
    TreeStateMonad (SkovT pv h c m)

deriving via
    SkovQueryMonadT (SkovT pv h c m)
    instance
        ( IsProtocolVersion pv,
          Monad m,
          TimeMonad m,
          BlockStateQuery (SkovT pv h c m),
          BlockPointerMonad (SkovT pv h c m),
          TreeStateMonad (SkovT pv h c m)
        ) =>
        SkovQueryMonad (SkovT pv h c m)

instance
    ( Monad m,
      TimeMonad m,
      MonadLogger m,
      OnSkov (SkovT pv h c m),
      BlockStateStorage (SkovT pv h c m),
      TreeStateMonad (SkovT pv h c m),
      FinalizationMonad (SkovT pv h c m)
    ) =>
    SkovMonad (SkovT pv h c m)
    where
    receiveBlock = doReceiveBlock
    executeBlock = doExecuteBlock
    receiveExecuteBlock = doReceiveExecuteBlock
    receiveTransaction = doReceiveTransaction
    addPreverifiedTransaction = doAddPreverifiedTransaction
    trustedFinalize = doTrustedFinalize
    handleCatchUpStatus = doHandleCatchUp
    clearSkovOnProtocolUpdate = doClearSkov
    terminateSkov = doTerminateSkov
    purgeTransactions = doPurgeTransactions
    rememberFinalState = storeFinalState

class (Monad m, HandlerConfig c) => HandlerConfigHandlers c m | m -> c where
    -- |Called upon a block being added to the tree.
    handleBlock :: BlockPointerType m -> m ()

    -- |An event handler called per finalization. It is called with the
    -- finalization record, the block that the finalization record finalized,
    -- and the remaining blocks that were finalized as a result of this
    -- finalization. These blocks are ordered by decreasing height.
    handleFinalize :: FinalizationRecord -> BlockPointerType m -> [BlockPointerType m] -> m ()

-- |A handler that does nothing.
data NoHandler = NoHandler

instance HandlerConfig NoHandler where
    type HCContext NoHandler = ()
    type HCState NoHandler = ()
    initialiseHandler = \_ -> ((), ())

instance (Monad m) => HandlerConfigHandlers NoHandler (SkovT pv h (SkovConfig pv fc NoHandler) m) where
    handleBlock = \_ -> return ()
    handleFinalize = \_ _ _ -> return ()

instance
    (FinalizationQueueLenses (FCState finconf)) =>
    FinalizationQueueLenses (SkovState (SkovConfig pv finconf hconf))
    where
    finQueue = lens ssFinState (\s fs -> s{ssFinState = fs}) . finQueue

instance
    (FinalizationStateLenses (FCState finconf) t) =>
    FinalizationStateLenses (SkovState (SkovConfig pv finconf hconf)) t
    where
    finState = lens ssFinState (\s fs -> s{ssFinState = fs}) . finState

instance
    (FinalizationBufferLenses (FCState finconf)) =>
    FinalizationBufferLenses (SkovState (SkovConfig pv finconf hconf))
    where
    finBuffer = lens ssFinState (\s fs -> s{ssFinState = fs}) . finBuffer

instance
    (HasFinalizationInstance (FCContext finconf)) =>
    HasFinalizationInstance (SkovContext (SkovConfig pv finconf hconf))
    where
    finalizationInstance = finalizationInstance . scFinContext

instance HasGlobalStateContext (GSContext pv) (SkovContext (SkovConfig pv finconf hconf)) where
    globalStateContext = lens scGSContext (\sc v -> sc{scGSContext = v})

instance HasGlobalState (GSState pv) (SkovState (SkovConfig pv finconf hconf)) where
    globalState = lens ssGSState (\ss v -> ss{ssGSState = v})

instance
    ( MonadIO m,
      c ~ SkovConfig pv finconf hconf,
      HandlerConfigHandlers hconf (SkovT pv h c m),
      SkovPendingLiveHandlers h m
    ) =>
    OnSkov (SkovT pv h c m)
    where
    onBlock bp = handleBlock bp
    onFinalize = handleFinalize
    onPendingLive = do
        h <- askHandler
        lift $ handlePendingLive h

-- |Synonym for 'ActiveFinalizationM' based on a specific configuration. Arguments:
--
--   * @pv@: protocol version
--   * @gc@: global state configuration type
--   * @fc@: finalization configuration type
--   * @hc@: handler configuration type
--   * @h@: handler type
--   * @m@: base monad
type ActiveFinalizationMWith pv fc hc h m =
    ActiveFinalizationM
        pv
        (SkovContext (SkovConfig pv fc hc))
        (SkovState (SkovConfig pv fc hc))
        (SkovT pv h (SkovConfig pv fc hc) m)

deriving via
    (ActiveFinalizationMWith pv (NoFinalization t) hc h m)
    instance
        ( t ~ SkovHandlerTimer h,
          MonadIO m,
          SkovMonad (SkovT pv h (SkovConfig pv (NoFinalization t) hc) m),
          SkovTimerHandlers pv h (SkovConfig pv (NoFinalization t) hc) m
        ) =>
        FinalizationMonad (SkovT pv h (SkovConfig pv (NoFinalization t) hc) m)

deriving via
    (ActiveFinalizationMWith pv (ActiveFinalization t) hc h m)
    instance
        ( t ~ SkovHandlerTimer h,
          MonadIO m,
          SkovMonad (SkovT pv h (SkovConfig pv (ActiveFinalization t) hc) m),
          SkovTimerHandlers pv h (SkovConfig pv (ActiveFinalization t) hc) m,
          SkovFinalizationHandlers h m
        ) =>
        FinalizationMonad (SkovT pv h (SkovConfig pv (ActiveFinalization t) hc) m)

deriving via
    (ActiveFinalizationMWith pv (BufferedFinalization t) hc h m)
    instance
        ( t ~ SkovHandlerTimer h,
          MonadIO m,
          TimeMonad m,
          MonadLogger m,
          SkovMonad (SkovT pv h (SkovConfig pv (BufferedFinalization t) hc) m),
          SkovTimerHandlers pv h (SkovConfig pv (BufferedFinalization t) hc) m,
          SkovFinalizationHandlers h m
        ) =>
        FinalizationMonad (SkovT pv h (SkovConfig pv (BufferedFinalization t) hc) m)
