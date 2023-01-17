{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module lifts the abstractions declared in the globalstate package to an
-- abstracted new type `GlobalStateM` that inherits all the monad behaviors defined
-- in this package.
module Concordium.GlobalState where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader hiding (ask)
import Data.Kind
import Data.Proxy

import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Account (AccountCache, newAccountCache)
import Concordium.GlobalState.Persistent.BlobStore (BlobStoreT (runBlobStoreT), closeBlobStore, createBlobStore, destroyBlobStore, loadBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.Genesis
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState as TS
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types.ProtocolVersion

-- For the avid reader.
-- The strategy followed in this module is the following: First `BlockStateM` and
-- `TreeStateM` derive the behaviors of the BlockState Monads and TreeState Monads
-- respectively. If such behavior is independent of the specific implementation of
-- the monad we use generic implementations. As these are just wrappers that lift
-- behaviors, some of these are generic.
--
-- For example, deriving `MonadIO` when wrapping a monad that is already an instance
-- of `MonadIO` is trivially done in the deriving clause next to the definition of
-- the new type. Deriving a class that is agnostic to the types used (usually because
-- the relations between those types are given by another typeclass) is done in the
-- `Generic implementations` section for each type.
--
-- When the actual typeclass that is being derived depends on the type variables to
-- inherit one behavior or another (for example with the `TreeStateMonad` implementation)
-- we actually need to use two derive instances to generate the different type classes.
-- These derivations are done in the `Specialized implementations` section for each type.
--
-- The general strategy followed for these derivations is:
--
-- 1. derive via a type that is already an instance of the class we want (for example
-- `PureTreeStateMonad` is an instance of `TreeStateMonad`).
-- 2. Use the constraint `Monad m` because the newtype we are mirroring needs this to
-- even exist (`Monad m => MonadIO/MonadState s/MonadReader r/MonadWriter w m` so any
-- of those also work).
-- 3. If we are deriving the class `C` via the type `T`, require the constraint `C T`
-- that we know is true for some instances of the type variables in `T`.
-- 3b. If the class `C` depends in another typeclass `D` that is not implemented in a
-- generic way, we will need to add this constraint to our derivation. This happens
-- with the `BlockStateStorage m` constraint when deriving the `TreeStateMonad`,
-- as `BlockStateStorage` is not mandatory for all the possible instantiations of
-- the type variables in the `T` type.
-- 4. Declare the instance `C V` being V the actual newtype we want to give this
-- behavior.
--
-- This deriving mechanism most often requires the use of UndecidableInstances. Adding
-- derivations should be done with great care as it might lead to situations where the
-- constraints for a specific instance requires that same instance to be fulfilled
-- i.e. `C (T a) => C (T a)` leading to infinite loops in instance resolution (in runtime).
--
-- In the case of the `GlobalStateM` new type we will derive the BlockState monads,
-- generically from `BlockStateM` and the TreeState monads also generically from
-- `TreeStateM bs (BlockStateM ..)`.

-- |A newtype wrapper for providing instances of the block state related monads:
-- 'BlockStateTypes', 'BlockStateQuery', 'AccountOperations', 'BakerQuery', 'BlockStateOperations', 'BirkParametersOperations' and 'BlockStateStorage'.
--
-- For the monad @BlockStateM pv c r g s m@, the underlying monad @m@ should satisfy
-- @MonadReader r m@ and @MonadState s m@.  The types @c@ and @s@ should be components
-- of the context @r@ and state @s@, satisfying @HasGlobalStateContext c r@ and
-- @HasGlobalState g s@ respectively.
--
-- The particular instances for the block state monads are determined by the @c@ and @g@
-- parameters (although currently @g@ is not used).
--
-- * If @c@ is @()@, the block state is a pure, in-memory, Haskell implementation using
--   'Basic.PureBlockStateMonad'.
--
-- * If @c@ is 'PersistentBlockStateContext', the block state is a persistent, Haskell
--   implementation using 'PersistentBlockStateMonad'.
newtype BlockStateM (pv :: ProtocolVersion) (c :: Type) (r :: Type) (g :: Type) (s :: Type) (m :: Type -> Type) (a :: Type) = BlockStateM {runBlockStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance (IsProtocolVersion pv) => MonadProtocolVersion (BlockStateM pv c r g s m) where
    type MPV (BlockStateM pv c r g s m) = pv

-- * Generic implementations

deriving via
    FocusGlobalStateM c g m
    instance
        (HasGlobalStateContext c r, MonadReader r m) =>
        MonadReader c (BlockStateM pv c r g s m)

deriving via
    FocusGlobalStateM c g m
    instance
        (HasGlobalState g s, MonadState s m) =>
        MonadState g (BlockStateM pv c r g s m)

-- * Disk implementations
type PersistentBlockStateM pv (r :: Type) (g :: Type) (s :: Type) (m :: Type -> Type) = BlockStateM pv (PersistentBlockStateContext pv) r g s m

deriving via
    PersistentBlockStateMonad pv (PersistentBlockStateContext pv) m
    instance
        BlockStateTypes (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          BlockStateQuery
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        BlockStateQuery (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          AccountOperations
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        AccountOperations (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          ContractStateOperations
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        ContractStateOperations (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          ModuleQuery
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        ModuleQuery (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          BlockStateOperations
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        BlockStateOperations (PersistentBlockStateM pv r g s m)

deriving via
    PersistentBlockStateMonad
        pv
        (PersistentBlockStateContext pv)
        (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
    instance
        ( MonadIO m,
          BlockStateStorage
            ( PersistentBlockStateMonad
                pv
                (PersistentBlockStateContext pv)
                (FocusGlobalStateM (PersistentBlockStateContext pv) g m)
            )
        ) =>
        BlockStateStorage (PersistentBlockStateM pv r g s m)

instance
    ( MonadIO m,
      c ~ PersistentBlockStateContext pv,
      HasGlobalStateContext c r,
      AccountVersionFor pv ~ av,
      MonadReader r m
    ) =>
    MonadCache (AccountCache av) (PersistentBlockStateM pv r g s m)
    where
    getCache = projectCache <$> ask

instance
    ( MonadIO m,
      c ~ PersistentBlockStateContext pv,
      HasGlobalStateContext c r,
      MonadReader r m
    ) =>
    MonadCache Modules.ModuleCache (PersistentBlockStateM pv r g s m)
    where
    getCache = projectCache <$> ask

-----------------------------------------------------------------------------

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
--
-- For the monad @TreeStateM s m@, the underlying monad @m@ should satisfy
-- @MonadState s m@.
--
-- * If @s@ is 'SkovData pv bs', then the in-memory, Haskell tree state is used.
-- * If @s@ is 'SkovPersistentData pv bs', then the persistent Haskell tree state is used.
newtype TreeStateM (s :: Type) (m :: Type -> Type) (a :: Type) = TreeStateM {runTreeStateM :: m a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState s,
          MonadIO,
          BlockStateTypes,
          BlockStateQuery,
          ModuleQuery,
          AccountOperations,
          BlockStateOperations,
          BlockStateStorage,
          ContractStateOperations
        )

deriving instance MonadProtocolVersion m => MonadProtocolVersion (TreeStateM s m)

-- * Disk implementations
type PersistentTreeStateM pv (bs :: Type) (m :: Type -> Type) = TreeStateM (SkovPersistentData pv bs) m

deriving via
    PersistentTreeStateMonad bs m
    instance
        (IsProtocolVersion pv, pv ~ MPV m) => GlobalStateTypes (PersistentTreeStateM pv bs m)

deriving via
    PersistentTreeStateMonad bs m
    instance
        ( Monad m,
          IsProtocolVersion pv,
          BlockPointerMonad (PersistentTreeStateMonad bs m),
          MPV m ~ pv
        ) =>
        BlockPointerMonad (PersistentTreeStateM pv bs m)

deriving via
    PersistentTreeStateMonad bs m
    instance
        ( MonadProtocolVersion m,
          BlockStateStorage m,
          TreeStateMonad (PersistentTreeStateMonad bs m),
          MPV m ~ pv
        ) =>
        TreeStateMonad (PersistentTreeStateM pv bs m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state monad instances are derived directly from 'TreeStateM'.
-- The arguments c, r, g, s, m, a are as in BlockStateM, whereas the argument @db@
-- is an additional context that manages auxiliary databases not needed by consensus.
-- In particular this means the index of transactions that affect a given account.
newtype GlobalStateM (pv :: ProtocolVersion) (c :: Type) (r :: Type) (g :: Type) (s :: Type) (m :: Type -> Type) (a :: Type) = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO, MonadLogger, TimeMonad)
    deriving (BlockStateTypes) via (BlockStateM pv c r g s m)

instance (IsProtocolVersion pv) => MonadProtocolVersion (GlobalStateM pv c r g s m) where
    type MPV (GlobalStateM pv c r g s m) = pv

-- * Specializations

type TreeStateBlockStateM pv (g :: Type) (c :: Type) (r :: Type) (s :: Type) (m :: Type -> Type) = TreeStateM g (BlockStateM pv c r g s m)

-- * Generic implementations

deriving via
    BlockStateM pv c r g s m
    instance
        ( Monad m,
          BlockStateQuery (BlockStateM pv c r g s m)
        ) =>
        BlockStateQuery (GlobalStateM pv c r g s m)

deriving via
    BlockStateM pv c r g s m
    instance
        ( Monad m,
          AccountOperations (BlockStateM pv c r g s m)
        ) =>
        AccountOperations (GlobalStateM pv c r g s m)

deriving via
    BlockStateM pv c r g s m
    instance
        ( Monad m,
          ContractStateOperations (BlockStateM pv c r g s m)
        ) =>
        ContractStateOperations (GlobalStateM pv c r g s m)

deriving via
    BlockStateM pv c r g s m
    instance
        ( Monad m,
          ModuleQuery (BlockStateM pv c r g s m)
        ) =>
        ModuleQuery (GlobalStateM pv c r g s m)

deriving via
    BlockStateM pv c r g s m
    instance
        ( BlockStateQuery (GlobalStateM pv c r g s m),
          BlockStateOperations (BlockStateM pv c r g s m)
        ) =>
        BlockStateOperations (GlobalStateM pv c r g s m)

deriving via
    BlockStateM pv c r g s m
    instance
        ( BlockStateOperations (GlobalStateM pv c r g s m),
          BlockStateStorage (BlockStateM pv c r g s m)
        ) =>
        BlockStateStorage (GlobalStateM pv c r g s m)

deriving via
    TreeStateBlockStateM pv g c r s m
    instance
        GlobalStateTypes (TreeStateBlockStateM pv g c r s m) =>
        GlobalStateTypes (GlobalStateM pv c r g s m)

deriving via
    TreeStateBlockStateM pv g c r s m
    instance
        ( Monad m,
          BlockPointerMonad (TreeStateBlockStateM pv g c r s m)
        ) =>
        BlockPointerMonad (GlobalStateM pv c r g s m)

deriving via
    TreeStateBlockStateM pv g c r s m
    instance
        ( Monad m,
          BlockStateStorage (BlockStateM pv c r g s m),
          TreeStateMonad (TreeStateBlockStateM pv g c r s m)
        ) =>
        TreeStateMonad (GlobalStateM pv c r g s m)

-----------------------------------------------------------------------------

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig = DTDBConfig
    { dtdbRuntimeParameters :: !RuntimeParameters,
      dtdbTreeStateDirectory :: !FilePath,
      dtdbBlockStateFile :: !FilePath
    }

-- |Exceptions that can occur when initialising the global state.
data GlobalStateInitException
    = -- |Genesis data could not be used to construct initial state.
      InvalidGenesisData !String

instance Show GlobalStateInitException where
    show (InvalidGenesisData desc) =
        "Could not initialise state from genesis data: " ++ desc

instance Exception GlobalStateInitException

-- |This class is implemented by types that determine configurations for the global state.
class GlobalStateConfig (c :: Type) where
    -- |The read-only context type associated with a global state configuration.
    type GSContext c (pv :: ProtocolVersion)

    -- |The (mutable) state type associated with a global state configuration.
    type GSState c (pv :: ProtocolVersion)

    -- |Generate context and state from the initial configuration if the state
    -- exists already. This may have 'IO' side effects to set up any necessary
    -- storage. This may throw a 'GlobalStateInitException'.
    --
    -- The return value is 'Nothing' if the state could not be found. If the
    -- state could be found, but could not be loaded, recovery is attempted,
    -- and if that fails,  an exception is thrown.
    --
    -- Note that even if the state is successfully loaded it is not in a usable
    -- state for an active consensus and must be activated before. Use
    -- 'activateGlobalState' for that.
    initialiseExistingGlobalState :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> c -> LogIO (Maybe (GSContext c pv, GSState c pv))

    -- |Migrate an existing global state. This is only intended to be used on a
    -- protocol update and requires that the initial state for the new protocol
    -- version is prepared (see @TreeState.storeFinalState@). This function will
    -- construct a new active instance of global state by migrating state from
    -- the existing instance. The existing instance is unchanged. It is assumed
    -- that the existing instance is in a good state, i.e., all internal
    -- invariants normally maintained during execution still hold. This function
    -- additionally assumes the focus block is the last finalized block, and
    -- there are no branches. This in particular implies that any transactions
    -- in the transaction table are either finalized, or "received".
    --
    -- As well as block and tree state, this migrates the transaction table with
    -- the auxiliary pending table. At present these are simply carried over,
    -- but future protocol updates might need to do some migration of these as
    -- well.
    migrateExistingState ::
        (IsProtocolVersion pv, IsProtocolVersion oldpv) =>
        -- |The configuration.
        c ->
        -- |Global state context for the state we are migrating from.
        GSContext c oldpv ->
        -- |The state of the chain we are migrating from. See documentation above for assumptions.
        GSState c oldpv ->
        -- |Auxiliary migration data.
        StateMigrationParameters oldpv pv ->
        -- |Regenesis data for the new chain. This is in effect the genesis block of the new chain.
        Regenesis pv ->
        -- |The return value is the context and state for the new chain.
        LogIO (GSContext c pv, GSState c pv)

    -- |Initialise new global state with the given genesis. If the state already
    -- exists this will raise an exception. It is not necessary to call 'activateGlobalState'
    -- on the generated state, as this will establish the necessary invariants.
    initialiseNewGlobalState :: IsProtocolVersion pv => GenesisData pv -> c -> LogIO (GSContext c pv, GSState c pv)

    -- |Either initialise an existing state, or if it does not exist, initialise a new one with the given genesis.
    initialiseGlobalState :: forall pv. IsProtocolVersion pv => GenesisData pv -> c -> LogIO (GSContext c pv, GSState c pv)
    initialiseGlobalState gd cfg =
        initialiseExistingGlobalState (protocolVersion @pv) cfg >>= \case
            Nothing -> initialiseNewGlobalState gd cfg
            Just config -> return config

    -- |Establish all the necessary invariants so that the state can be used by
    -- consensus. This should only be called once per initialised state.
    activateGlobalState :: IsProtocolVersion pv => Proxy c -> Proxy pv -> GSContext c pv -> GSState c pv -> LogIO (GSState c pv)

    -- |Shutdown the global state.
    shutdownGlobalState :: SProtocolVersion pv -> Proxy c -> GSContext c pv -> GSState c pv -> IO ()

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    type GSState DiskTreeDiskBlockConfig pv = SkovPersistentData pv (HashedPersistentBlockState pv)
    type GSContext DiskTreeDiskBlockConfig pv = PersistentBlockStateContext pv

    initialiseExistingGlobalState _ DTDBConfig{..} = do
        -- check if all the necessary database files exist
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
            else return Nothing

    migrateExistingState DTDBConfig{..} oldPbsc oldState migration genData = do
        pbscBlobStore <- liftIO $ createBlobStore dtdbBlockStateFile
        pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
        pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
        let pbsc = PersistentBlockStateContext{..}
        newInitialBlockState <- flip runBlobStoreT oldPbsc . flip runBlobStoreT pbsc $ do
            case _nextGenesisInitialState oldState of
                Nothing -> error "Precondition violation. Migration called in state without initial block state."
                Just initState -> do
                    newState <- migratePersistentBlockState migration (hpbsPointers initState)
                    Concordium.GlobalState.Persistent.BlockState.hashBlockState newState
        let initGS = do
                ser <- saveBlockState newInitialBlockState
                initialSkovPersistentData
                    dtdbRuntimeParameters
                    dtdbTreeStateDirectory
                    (regenesisConfiguration genData)
                    newInitialBlockState
                    ser
                    (_transactionTable oldState)
                    (Just (_pendingTransactions oldState))
        isd <-
            runReaderT (runPersistentBlockStateMonad initGS) pbsc
                `onException` liftIO (destroyBlobStore pbscBlobStore)
        return (pbsc, isd)

    initialiseNewGlobalState genData DTDBConfig{..} = do
        pbscBlobStore <- liftIO $ createBlobStore dtdbBlockStateFile
        pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
        pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
        let pbsc = PersistentBlockStateContext{..}
        let initGS = do
                logEvent GlobalState LLTrace "Creating persistent global state"
                result <- genesisState genData
                (pbs, genTT) <- case result of
                    Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
                    Right genState -> return genState
                logEvent GlobalState LLTrace "Writing persistent global state"
                ser <- saveBlockState pbs
                logEvent GlobalState LLTrace "Creating persistent global state context"
                initialSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory (genesisConfiguration genData) pbs ser genTT Nothing
        isd <-
            runReaderT (runPersistentBlockStateMonad initGS) pbsc
                `onException` liftIO (destroyBlobStore pbscBlobStore)
        return (pbsc, isd)

    activateGlobalState _ _ = activateSkovPersistentData

    shutdownGlobalState _ _ PersistentBlockStateContext{..} st = do
        closeBlobStore pbscBlobStore
        closeSkovPersistentData st
