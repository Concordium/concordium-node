{-# LANGUAGE
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    DerivingVia,
    FlexibleContexts,
    FlexibleInstances,
    DerivingStrategies,
    TypeFamilies,
    RecordWildCards,
    MultiParamTypeClasses,
    QuantifiedConstraints,
    UndecidableInstances,
    RankNTypes,
    ScopedTypeVariables,
    ConstraintKinds,
    PartialTypeSignatures
    #-}
-- |This module lifts the abstractions declared in the globalstate package to an
-- abstracted new type `GlobalStateM` that inherits all the monad behaviors defined
-- in this package.
module Concordium.GlobalState where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader
import Data.IORef (newIORef,writeIORef)
import Data.Proxy
import Data.ByteString.Char8(ByteString)
import Data.Pool(destroyAllResources)

import Concordium.GlobalState.Classes
import Concordium.GlobalState.Types
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore (createTempBlobStore,destroyTempBlobStore, loadBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as Persistent
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.SQL.AccountTransactionIndex

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
-- 'BlockStateTypes', 'BlockStateQuery', 'BlockStateOperations', 'BirkParametersOperations' and 'BlockStateStorage'.
--
-- For the monad @BlockStateM c r g s m@, the underlying monad @m@ should satisfy
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
newtype BlockStateM c r g s m a = BlockStateM (m a)
    deriving (Functor, Applicative, Monad, MonadIO)

-- * Specializations

type MemoryBlockStateM r g s m = BlockStateM () r g s m
type PersistentBlockStateM r g s m = BlockStateM PersistentBlockStateContext r g s m

-- * Generic implementations

deriving via FocusGlobalStateM c g m
    instance (HasGlobalStateContext c r, MonadReader r m)
             => MonadReader c (BlockStateM c r g s m)

deriving via FocusGlobalStateM c g m
    instance (HasGlobalState g s, MonadState s m)
             => MonadState g (BlockStateM c r g s m)

-- * Specific implementations

-- ** Memory implementations
deriving via PureBlockStateMonad m
    instance BlockStateTypes (MemoryBlockStateM r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m)
             => BlockStateQuery (MemoryBlockStateM r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m)
             => BirkParametersOperations (MemoryBlockStateM r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m,
              BlockStateQuery (MemoryBlockStateM r g s m))
             => BlockStateOperations (MemoryBlockStateM r g s m)

deriving via PureBlockStateMonad m
    instance (Monad m,
              BlockStateOperations (MemoryBlockStateM r g s m))
             => BlockStateStorage (MemoryBlockStateM r g s m)

-- ** Disk implementations
deriving via (PersistentBlockStateMonad PersistentBlockStateContext m)
    instance BlockStateTypes (PersistentBlockStateM r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateQuery (PersistentBlockStateMonad
                                PersistentBlockStateContext
                                (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateQuery (PersistentBlockStateM r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BirkParametersOperations (PersistentBlockStateMonad
                                PersistentBlockStateContext
                                (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BirkParametersOperations (PersistentBlockStateM r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateOperations (PersistentBlockStateMonad
                                     PersistentBlockStateContext
                                     (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateOperations (PersistentBlockStateM r g s m)

deriving via (PersistentBlockStateMonad
               PersistentBlockStateContext
               (FocusGlobalStateM PersistentBlockStateContext g m))
    instance (MonadIO m,
              BlockStateStorage (PersistentBlockStateMonad
                                  PersistentBlockStateContext
                                  (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateStorage (PersistentBlockStateM r g s m)

-----------------------------------------------------------------------------

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
--
-- For the monad @TreeStateM s m@, the underlying monad @m@ should satisfy
-- @MonadState s m@.
--
-- * If @s@ is 'SkovData bs', then the in-memory, Haskell tree state is used.
-- * If @s@ is 'SkovPersistentData ati bs', then the persistent Haskell tree state is used.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO,
              BlockStateTypes, BlockStateQuery, BlockStateOperations, BlockStateStorage, BirkParametersOperations)

-- * Specializations
type MemoryTreeStateM bs m = TreeStateM (SkovData bs) m
type PersistentTreeStateM ati m = TreeStateM (SkovPersistentData ati) m

-- * Specialized implementations
-- ** Memory implementations
deriving via PureTreeStateMonad bs m
    instance ATITypes (MemoryTreeStateM bs m)

deriving via PureTreeStateMonad bs m
    instance Monad m => PerAccountDBOperations (MemoryTreeStateM bs m)

deriving via PureTreeStateMonad bs m
    instance GlobalStateTypes (MemoryTreeStateM bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              BlockPointerMonad (PureTreeStateMonad bs m))
              => BlockPointerMonad (MemoryTreeStateM bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              BlockStateStorage m,
              TreeStateMonad (PureTreeStateMonad bs m))
              => TreeStateMonad (MemoryTreeStateM bs m)

-- ** Disk implementations
deriving via PersistentTreeStateMonad ati m
    instance ATITypes (PersistentTreeStateMonad ati m)
             => ATITypes (PersistentTreeStateM ati m)

deriving via PersistentTreeStateMonad ati m
    instance (Monad m, PerAccountDBOperations (PersistentTreeStateMonad ati m))
             => PerAccountDBOperations (PersistentTreeStateM ati m)

deriving via PersistentTreeStateMonad ati m
    instance GlobalStateTypes (PersistentTreeStateM ati m)

deriving via PersistentTreeStateMonad ati m
    instance (Monad m,
              BlockPointerMonad (PersistentTreeStateMonad ati m))
             => BlockPointerMonad (PersistentTreeStateM ati m)

deriving via PersistentTreeStateMonad ati m
    instance (Monad m,
              BlockStateStorage m,
              TreeStateMonad (PersistentTreeStateMonad ati m))
             => TreeStateMonad (PersistentTreeStateM ati m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state monad instances are derived directly from 'TreeStateM'.
-- The arguments c, r, g, s, m, a are as in BlockStateM, whereas the argument @db@
-- is an additional context that manages auxiliary databases not needed by consensus.
-- In particular this means the index of transactions that affect a given account.
newtype GlobalStateM db c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO)
    deriving (BlockStateTypes) via (BlockStateM c r g s m)

-- * Specializations

type TreeStateBlockStateM g c r s m = TreeStateM g (BlockStateM c r g s m)

-- * Generic implementations

deriving via BlockStateM c r g s m
    instance (Monad m, BlockStateQuery (BlockStateM c r g s m))
             => BlockStateQuery (GlobalStateM db c r g s m)

deriving via BlockStateM c r g s m
    instance (Monad m, BirkParametersOperations (BlockStateM c r g s m))
             => BirkParametersOperations (GlobalStateM db c r g s m)

deriving via BlockStateM c r g s m
    instance (BlockStateQuery (GlobalStateM db c r g s m),
              BlockStateOperations (BlockStateM c r g s m))
             => BlockStateOperations (GlobalStateM db c r g s m)

deriving via BlockStateM c r g s m
    instance (BlockStateOperations (GlobalStateM db c r g s m),
              BlockStateStorage (BlockStateM c r g s m))
             => BlockStateStorage (GlobalStateM db c r g s m)

deriving via TreeStateBlockStateM g c r s m
    instance ATITypes (TreeStateBlockStateM g c r s m)
             => ATITypes (GlobalStateM db c r g s m)

deriving via TreeStateBlockStateM g c r s m
    instance (Monad m, PerAccountDBOperations (TreeStateBlockStateM g c r s m))
             => PerAccountDBOperations (GlobalStateM db c r g s m)

deriving via TreeStateBlockStateM g c r s m
    instance GlobalStateTypes (TreeStateBlockStateM g c r s m)
             => GlobalStateTypes (GlobalStateM db c r g s m)

deriving via TreeStateBlockStateM g c r s m
    instance (Monad m,
              BlockPointerMonad (TreeStateBlockStateM g c r s m))
             => BlockPointerMonad (GlobalStateM db c r g s m)

deriving via TreeStateBlockStateM g c r s m
    instance (Monad m,
              BlockStateStorage (BlockStateM c r g s m),
              TreeStateMonad (TreeStateBlockStateM g c r s m))
             => TreeStateMonad (GlobalStateM db c r g s m)

-----------------------------------------------------------------------------

-- |Configuration that uses in-memory, Haskell implementations for both tree state and block state.
data MemoryTreeMemoryBlockConfig = MTMBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig = DTDBConfig RuntimeParameters GenesisData BS.BlockState

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state, as well as an external database for producing
-- an index of transactions affecting a given account.
data DiskTreeDiskBlockWithLogConfig = DTDBWLConfig {
  configRP :: RuntimeParameters,
  configGD :: GenesisData,
  configBS :: BS.BlockState,
  configTxLog :: !ByteString
  }

type family GSContext c where
  GSContext MemoryTreeMemoryBlockConfig = ()
  GSContext DiskTreeDiskBlockConfig = PersistentBlockStateContext
  GSContext DiskTreeDiskBlockWithLogConfig = PersistentBlockStateContext

type family GSState c where
  GSState MemoryTreeMemoryBlockConfig = SkovData BS.BlockState
  GSState DiskTreeDiskBlockConfig = SkovPersistentData ()
  GSState DiskTreeDiskBlockWithLogConfig = SkovPersistentData DiskDump

type family GSLogContext c where
  GSLogContext MemoryTreeMemoryBlockConfig = NoLogContext
  GSLogContext DiskTreeDiskBlockConfig = NoLogContext
  GSLogContext DiskTreeDiskBlockWithLogConfig = PerAccountAffectIndex

-- |This class is implemented by types that determine configurations for the global state.
class GlobalStateConfig c where
    -- |Generate context and state from the initial configuration. This may
    -- have 'IO' side effects to set up any necessary storage.
    initialiseGlobalState :: c -> IO (GSContext c, GSState c, GSLogContext c)
    -- |Shutdown the global state.
    shutdownGlobalState :: Proxy c -> GSContext c -> GSState c -> GSLogContext c -> IO ()

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    initialiseGlobalState (MTMBConfig rtparams gendata bs) = do
      return ((), initialSkovData rtparams gendata bs, NoLogContext)
    shutdownGlobalState _ _ _ _ = return ()

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    initialiseGlobalState (DTDBConfig rtparams gendata bs) = do
      -- check if all the necessary database files exist
      (blockStateFile, existingDB) <- checkExistingDatabase rtparams
      if existingDB then do
        -- the block state file exists, is readable and writable
        -- we ignore the given block state parameter in such a case.
        pbscBlobStore <- loadBlobStore blockStateFile
        -- we start with an empty module cache. It will be repopulated
        -- on block execution, and the cache is only an optimization, it is
        -- fine, if slow, if modules are always loaded from the database.
        pbscModuleCache <- newIORef emptyModuleCache
        let pbsc = PersistentBlockStateContext{..}
        skovData <- loadSkovPersistentData rtparams gendata pbsc ((), NoLogContext)
        return (pbsc, skovData, NoLogContext)
      else do
        pbscBlobStore <- createTempBlobStore blockStateFile
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        isd <- initialSkovPersistentData rtparams gendata pbs ((), NoLogContext) serBS
        return (pbsc, isd, NoLogContext)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ _ = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache

instance GlobalStateConfig DiskTreeDiskBlockWithLogConfig where
    initialiseGlobalState (DTDBWLConfig rtparams gendata bs txLog) = do
      -- check if all the necessary database files exist
      (blockStateFile, existingDB) <- checkExistingDatabase rtparams
      dbHandle <- connectPostgres txLog
      createTable dbHandle
      if existingDB then do
        -- the block state file exists, is readable and writable
        -- we ignore the given block state parameter in such a case.
        pbscBlobStore <- loadBlobStore blockStateFile
        -- we start with an empty module cache. It will be repopulated
        -- on block execution, and the cache is only an optimization, it is
        -- fine, if slow, if modules are always loaded from the database.
        pbscModuleCache <- newIORef emptyModuleCache
        let pbsc = PersistentBlockStateContext{..}
        let ati = defaultValue
        skovData <-
          loadSkovPersistentData rtparams gendata pbsc (ati, PAAIConfig dbHandle)
          `onException` (destroyAllResources dbHandle >> destroyTempBlobStore pbscBlobStore)
        return (pbsc, skovData, PAAIConfig dbHandle)
      else do
        pbscBlobStore <- createTempBlobStore blockStateFile
        pbscModuleCache <- newIORef emptyModuleCache
        pbs <- makePersistent bs
        let pbsc = PersistentBlockStateContext{..}
        serBS <- runReaderT (runPersistentBlockStateMonad (putBlockState pbs)) pbsc
        let ati = defaultValue
        isd <- initialSkovPersistentData rtparams gendata pbs (ati, PAAIConfig dbHandle) serBS
                 `onException` (destroyAllResources dbHandle >> destroyTempBlobStore pbscBlobStore)
        return (pbsc, isd, PAAIConfig dbHandle)
    shutdownGlobalState _ (PersistentBlockStateContext{..}) _ (PAAIConfig dbHandle) = do
        destroyTempBlobStore pbscBlobStore
        writeIORef pbscModuleCache Persistent.emptyModuleCache
        destroyAllResources dbHandle
