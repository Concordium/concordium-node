{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- |This module lifts the abstractions declared in the globalstate package to an
-- abstracted new type `GlobalStateM` that inherits all the monad behaviors defined
-- in this package.
module Concordium.GlobalState where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader hiding (ask)
import Data.Proxy
import Data.ByteString.Char8(ByteString)
import Data.Pool(destroyAllResources)
import Data.Kind

import Concordium.Types.ProtocolVersion

import Concordium.GlobalState.Classes
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.BlobStore (closeBlobStore, createBlobStore, destroyBlobStore, loadBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.SQL.AccountTransactionIndex
import Concordium.Logger
import Concordium.Types.Block (AbsoluteBlockHeight)

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
newtype BlockStateM (pv :: ProtocolVersion) c r g s m a = BlockStateM { runBlockStateM :: m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance (IsProtocolVersion pv) => MonadProtocolVersion (BlockStateM pv c r g s m) where
    type MPV (BlockStateM pv c r g s m) = pv

-- * Specializations

type MemoryBlockStateM pv r g s m = BlockStateM pv () r g s m
type PersistentBlockStateM pv r g s m = BlockStateM pv PersistentBlockStateContext r g s m

-- * Generic implementations

deriving via FocusGlobalStateM c g m
    instance (HasGlobalStateContext c r, MonadReader r m)
             => MonadReader c (BlockStateM pv c r g s m)

deriving via FocusGlobalStateM c g m
    instance (HasGlobalState g s, MonadState s m)
             => MonadState g (BlockStateM pv c r g s m)

-- * Specific implementations

-- ** Memory implementations
deriving via PureBlockStateMonad pv m
    instance BlockStateTypes (MemoryBlockStateM pv r g s m)

deriving via PureBlockStateMonad pv m
    instance (Monad m, IsProtocolVersion pv)
             => BlockStateQuery (MemoryBlockStateM pv r g s m)

deriving via PureBlockStateMonad pv m
    instance (Monad m, IsProtocolVersion pv)
             => AccountOperations (MemoryBlockStateM pv r g s m)

deriving via PureBlockStateMonad pv m
    instance Monad m
             => ContractStateOperations (MemoryBlockStateM pv r g s m)

deriving via PureBlockStateMonad pv m
    instance (Monad m,
              IsProtocolVersion pv,
              BlockStateQuery (MemoryBlockStateM pv r g s m))
             => BlockStateOperations (MemoryBlockStateM pv r g s m)

deriving via PureBlockStateMonad pv m
    instance (MonadIO m,
              IsProtocolVersion pv,
              BlockStateOperations (MemoryBlockStateM pv r g s m))
             => BlockStateStorage (MemoryBlockStateM pv r g s m)

-- ** Disk implementations
deriving via PersistentBlockStateMonad pv PersistentBlockStateContext m
    instance BlockStateTypes (PersistentBlockStateM pv r g s m)

deriving via PersistentBlockStateMonad pv
              PersistentBlockStateContext
              (FocusGlobalStateM PersistentBlockStateContext g m)
    instance (MonadIO m,
              IsProtocolVersion pv,
              BlockStateQuery (PersistentBlockStateMonad pv
                                PersistentBlockStateContext
                                (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateQuery (PersistentBlockStateM pv r g s m)

deriving via PersistentBlockStateMonad pv
              PersistentBlockStateContext
              (FocusGlobalStateM PersistentBlockStateContext g m)
    instance (MonadIO m,
              AccountOperations (PersistentBlockStateMonad pv
                                  PersistentBlockStateContext
                                  (FocusGlobalStateM PersistentBlockStateContext g m)))
             => AccountOperations (PersistentBlockStateM pv r g s m)

deriving via PersistentBlockStateMonad pv
              PersistentBlockStateContext
              (FocusGlobalStateM PersistentBlockStateContext g m)
    instance (MonadIO m,
              IsProtocolVersion pv,
              ContractStateOperations (PersistentBlockStateMonad pv
                                        PersistentBlockStateContext
                                        (FocusGlobalStateM PersistentBlockStateContext g m)))
             => ContractStateOperations (PersistentBlockStateM pv r g s m)


deriving via PersistentBlockStateMonad pv
              PersistentBlockStateContext
              (FocusGlobalStateM PersistentBlockStateContext g m)
    instance (MonadIO m,
              IsProtocolVersion pv,
              BlockStateOperations (PersistentBlockStateMonad pv
                                     PersistentBlockStateContext
                                     (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateOperations (PersistentBlockStateM pv r g s m)

deriving via PersistentBlockStateMonad pv
              PersistentBlockStateContext
              (FocusGlobalStateM PersistentBlockStateContext g m)
    instance (MonadIO m,
              IsProtocolVersion pv,
              BlockStateStorage (PersistentBlockStateMonad pv
                                  PersistentBlockStateContext
                                  (FocusGlobalStateM PersistentBlockStateContext g m)))
             => BlockStateStorage (PersistentBlockStateM pv r g s m)

-----------------------------------------------------------------------------

-- |@TreeStateM s m@ is a newtype wrapper around a monad for
-- implementing tree state monads.  The parameter @s@ should
-- be the state type of the underlying monad @m@.
--
-- For the monad @TreeStateM s m@, the underlying monad @m@ should satisfy
-- @MonadState s m@.
--
-- * If @s@ is 'SkovData pv bs', then the in-memory, Haskell tree state is used.
-- * If @s@ is 'SkovPersistentData pv ati bs', then the persistent Haskell tree state is used.
newtype TreeStateM s m a = TreeStateM {runTreeStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, BlockStateTypes, BlockStateQuery,
            AccountOperations, BlockStateOperations, BlockStateStorage, ContractStateOperations)

deriving instance MonadProtocolVersion m => MonadProtocolVersion (TreeStateM s m)

-- * Specializations
type MemoryTreeStateM pv bs m = TreeStateM (SkovData pv bs) m
type PersistentTreeStateM pv ati bs m = TreeStateM (SkovPersistentData pv ati bs) m

-- * Specialized implementations
-- ** Memory implementations
deriving via PureTreeStateMonad bs m
    instance ATITypes (MemoryTreeStateM pv bs m)

deriving via PureTreeStateMonad bs m
    instance Monad m => PerAccountDBOperations (MemoryTreeStateM pv bs m)

deriving via PureTreeStateMonad bs m
    instance GlobalStateTypes (MemoryTreeStateM pv bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              BlockPointerMonad (PureTreeStateMonad bs m))
             => BlockPointerMonad (MemoryTreeStateM pv bs m)

deriving via PureTreeStateMonad bs m
    instance (Monad m,
              MPV m ~ pv, MonadProtocolVersion m,
              BlockStateStorage m,
              TreeStateMonad (PureTreeStateMonad bs m))
             => TreeStateMonad (MemoryTreeStateM pv bs m)

-- ** Disk implementations
deriving via PersistentTreeStateMonad ati bs m
    instance ATITypes (PersistentTreeStateMonad ati bs m)
             => ATITypes (PersistentTreeStateM pv ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              PerAccountDBOperations (PersistentTreeStateMonad ati bs m))
           => PerAccountDBOperations (PersistentTreeStateM pv ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance GlobalStateTypes (PersistentTreeStateM pv ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              BlockPointerMonad (PersistentTreeStateMonad ati bs m),
              MPV m ~ pv)
             => BlockPointerMonad (PersistentTreeStateM pv ati bs m)

deriving via PersistentTreeStateMonad ati bs m
    instance (Monad m,
              MonadProtocolVersion m,
              BlockStateStorage m,
              TreeStateMonad (PersistentTreeStateMonad ati bs m),
              MPV m ~ pv)
             => TreeStateMonad (PersistentTreeStateM pv ati bs m)

-- |A newtype wrapper for providing instances of global state monad classes.
-- The block state monad instances are derived directly from 'BlockStateM'.
-- The tree state monad instances are derived directly from 'TreeStateM'.
-- The arguments c, r, g, s, m, a are as in BlockStateM, whereas the argument @db@
-- is an additional context that manages auxiliary databases not needed by consensus.
-- In particular this means the index of transactions that affect a given account.
newtype GlobalStateM (pv :: ProtocolVersion) db c r g s m a = GlobalStateM {runGlobalStateM :: m a}
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadIO, MonadLogger)
    deriving (BlockStateTypes) via (BlockStateM pv c r g s m)

instance (IsProtocolVersion pv) => MonadProtocolVersion (GlobalStateM pv db c r g s m) where
    type MPV (GlobalStateM pv db c r g s m) = pv

-- * Specializations

type TreeStateBlockStateM pv g c r s m = TreeStateM g (BlockStateM pv c r g s m)

-- * Generic implementations

deriving via BlockStateM pv c r g s m
    instance (Monad m,
              BlockStateQuery (BlockStateM pv c r g s m))
             => BlockStateQuery (GlobalStateM pv db c r g s m)

deriving via BlockStateM pv c r g s m
    instance (Monad m,
              AccountOperations (BlockStateM pv c r g s m))
             => AccountOperations (GlobalStateM pv db c r g s m)

deriving via BlockStateM pv c r g s m
    instance (Monad m,
              ContractStateOperations (BlockStateM pv c r g s m))
             => ContractStateOperations (GlobalStateM pv db c r g s m)

deriving via BlockStateM pv c r g s m
    instance (BlockStateQuery (GlobalStateM pv db c r g s m),
              BlockStateOperations (BlockStateM pv c r g s m))
             => BlockStateOperations (GlobalStateM pv db c r g s m)

deriving via BlockStateM pv c r g s m
    instance (BlockStateOperations (GlobalStateM pv db c r g s m),
              BlockStateStorage (BlockStateM pv c r g s m))
             => BlockStateStorage (GlobalStateM pv db c r g s m)

deriving via TreeStateBlockStateM pv g c r s m
    instance ATITypes (TreeStateBlockStateM pv g c r s m)
             => ATITypes (GlobalStateM pv db c r g s m)

deriving via TreeStateBlockStateM pv g c r s m
    instance (Monad m,
              PerAccountDBOperations (TreeStateBlockStateM pv g c r s m))
             => PerAccountDBOperations (GlobalStateM pv db c r g s m)

deriving via TreeStateBlockStateM pv g c r s m
    instance GlobalStateTypes (TreeStateBlockStateM pv g c r s m)
             => GlobalStateTypes (GlobalStateM pv db c r g s m)

deriving via TreeStateBlockStateM pv g c r s m
    instance (Monad m,
              BlockPointerMonad (TreeStateBlockStateM pv g c r s m))
             => BlockPointerMonad (GlobalStateM pv db c r g s m)

deriving via TreeStateBlockStateM pv g c r s m
    instance (Monad m,
              BlockStateStorage (BlockStateM pv c r g s m),
              TreeStateMonad (TreeStateBlockStateM pv g c r s m))
             => TreeStateMonad (GlobalStateM pv db c r g s m)

-----------------------------------------------------------------------------

-- |Configuration that uses in-memory, Haskell implementations for both tree state and block state.
data MemoryTreeMemoryBlockConfig pv = MTMBConfig {
    mtmbRuntimeParameters :: !RuntimeParameters,
    mtmbGenesisData :: !(GenesisData pv)
    }

-- |Configuration that uses the in-memory, Haskell implementation of tree state and the
-- persistent Haskell implementation of block state.
data MemoryTreeDiskBlockConfig pv = MTDBConfig {
    mtdbRuntimeParameters :: !RuntimeParameters,
    mtdbBlockStateFile :: !FilePath,
    mtdbGenesisData :: !(GenesisData pv)
    }

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data DiskTreeDiskBlockConfig pv = DTDBConfig {
    dtdbRuntimeParameters :: !RuntimeParameters,
    dtdbTreeStateDirectory :: !FilePath,
    dtdbBlockStateFile :: !FilePath,
    dtdbGenesisData :: !(GenesisData pv)
    }

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state, as well as an external database for producing
-- an index of transactions affecting a given account.
data DiskTreeDiskBlockWithLogConfig pv = DTDBWLConfig {
    dtdbwlRuntimeParameters :: !RuntimeParameters,
    dtdbwlTreeStateDirectory :: !FilePath,
    dtdbwlBlockStateFile :: !FilePath,
    dtdbwlGenesisData :: !(GenesisData pv),
    dtdbwlTxDBConnectionString :: !ByteString,
    dtdbwlGenesisHeight  :: !AbsoluteBlockHeight
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
class GlobalStateConfig (c :: ProtocolVersion -> Type) where
    -- |The read-only context type associated with a global state configuration.
    type GSContext c (pv :: ProtocolVersion)
    -- |The (mutable) state type associated with a global state configuration.
    type GSState c (pv :: ProtocolVersion)
    -- |Context for transaction logging associated with a global state configuration.
    type GSLogContext c (pv :: ProtocolVersion)
    -- |Generate context and state from the initial configuration. This may
    -- have 'IO' side effects to set up any necessary storage.
    -- This may throw a 'GlobalStateInitException'.
    initialiseGlobalState :: IsProtocolVersion pv => c pv -> LogIO (GSContext c pv, GSState c pv, GSLogContext c pv)

    -- |Shutdown the global state.
    shutdownGlobalState :: SProtocolVersion pv -> Proxy c -> GSContext c pv -> GSState c pv -> GSLogContext c pv -> IO ()

instance GlobalStateConfig MemoryTreeMemoryBlockConfig where
    type GSContext MemoryTreeMemoryBlockConfig pv = ()
    type GSState MemoryTreeMemoryBlockConfig pv = SkovData pv (BS.HashedBlockState pv)
    type GSLogContext MemoryTreeMemoryBlockConfig pv = NoLogContext
    initialiseGlobalState (MTMBConfig rtparams gendata) = do
        bs <- case genesisState gendata of
            Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
            Right bs -> return $ BS.hashBlockState bs
        skovData <- runPureBlockStateMonad (initialSkovData rtparams gendata bs)
        return ((), skovData, NoLogContext)
    shutdownGlobalState _ _ _ _ _ = return ()

-- |Configuration that uses the Haskell implementation of tree state and the
-- in-memory, Haskell implementation of the block state.
instance GlobalStateConfig MemoryTreeDiskBlockConfig where
    type GSContext MemoryTreeDiskBlockConfig pv = PersistentBlockStateContext
    type GSLogContext MemoryTreeDiskBlockConfig pv = NoLogContext
    type GSState MemoryTreeDiskBlockConfig pv = SkovData pv (HashedPersistentBlockState pv)
    initialiseGlobalState MTDBConfig{..} = do
        genState <- case genesisState mtdbGenesisData of
            Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
            Right genState -> return genState
        liftIO $ do
            pbscBlobStore <- createBlobStore mtdbBlockStateFile
            let pbsc = PersistentBlockStateContext {..}
            let initState = do
                    pbs <- makePersistent genState
                    _ <- saveBlockState pbs
                    initialSkovData mtdbRuntimeParameters mtdbGenesisData pbs
            skovData <- runReaderT (runPersistentBlockStateMonad initState) pbsc
            return (pbsc, skovData, NoLogContext)
    shutdownGlobalState _ _ PersistentBlockStateContext{..} _ _ = liftIO $ do
        closeBlobStore pbscBlobStore

instance GlobalStateConfig DiskTreeDiskBlockConfig where
    type GSLogContext DiskTreeDiskBlockConfig pv = NoLogContext
    type GSState DiskTreeDiskBlockConfig pv = SkovPersistentData pv () (HashedPersistentBlockState pv)
    type GSContext DiskTreeDiskBlockConfig pv = PersistentBlockStateContext

    initialiseGlobalState DTDBConfig{..} = do
      -- check if all the necessary database files exist
      existingDB <- checkExistingDatabase dtdbTreeStateDirectory dtdbBlockStateFile
      if existingDB then do
        pbscBlobStore <- liftIO $ do
          -- the block state file exists, is readable and writable
          -- we ignore the given block state parameter in such a case.
          loadBlobStore dtdbBlockStateFile
        let pbsc = PersistentBlockStateContext{..}
        logm <- ask
        skovData <- liftIO (runLoggerT (loadSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory dtdbGenesisData pbsc NoLogContext) logm
                    `onException` (closeBlobStore pbscBlobStore))
        return (pbsc, skovData, NoLogContext)
      else do
        pbscBlobStore <- liftIO $ createBlobStore dtdbBlockStateFile
        let pbsc = PersistentBlockStateContext{..}
        let initGS = do
                genState <- case genesisState dtdbGenesisData of
                    Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
                    Right genState -> return genState
                pbs <- makePersistent genState
                ser <- saveBlockState pbs
                initialSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory dtdbGenesisData pbs () NoLogContext ser
        isd <- runReaderT (runPersistentBlockStateMonad initGS) pbsc
                `onException` liftIO (destroyBlobStore pbscBlobStore)
        return (pbsc, isd, NoLogContext)
    shutdownGlobalState _ _ PersistentBlockStateContext{..} st _ = do
        closeBlobStore pbscBlobStore
        closeSkovPersistentData st

instance GlobalStateConfig DiskTreeDiskBlockWithLogConfig where
    type GSState DiskTreeDiskBlockWithLogConfig pv = SkovPersistentData pv SQLTransactionLog (HashedPersistentBlockState pv)
    type GSContext DiskTreeDiskBlockWithLogConfig pv = PersistentBlockStateContext
    type GSLogContext DiskTreeDiskBlockWithLogConfig pv = SQLTransactionLogContext
    initialiseGlobalState DTDBWLConfig{..} = do
        -- check if all the necessary database files exist
      existingDB <- checkExistingDatabase dtdbwlTreeStateDirectory dtdbwlBlockStateFile
      dbHandle <- do
        dbHandle <- liftIO $ connectPostgres dtdbwlTxDBConnectionString
        liftIO (checkTablesExist dbHandle) >>= \case
          Ok -> logEvent GlobalState LLInfo "Using existing PostgreSQL tables for transaction logging."
          NoTables -> do
            logEvent GlobalState LLInfo "No relevant tables found in transaction logging database. Creating them."
            liftIO $ createTables dbHandle
          IncorrectFormat -> logExceptionAndThrow GlobalState (DatabaseOpeningError (userError "The connected SQL database has some of the 'ati', 'cti', or 'summaries', but either not all or they have incorrect columns."))
        return dbHandle
      let transactionLogContext = SQLTransactionLogContext{
            connectionPool = dbHandle,
            genesisAbsoluteHeight = dtdbwlGenesisHeight                
          }
      if existingDB then do
        pbscBlobStore <- liftIO $
          -- the block state file exists, is readable and writable
          -- we ignore the given block state parameter in such a case.
          loadBlobStore dtdbwlBlockStateFile
        let pbsc = PersistentBlockStateContext{..}
        logm <- ask
        skovData <- liftIO (runLoggerT (loadSkovPersistentData dtdbwlRuntimeParameters dtdbwlTreeStateDirectory dtdbwlGenesisData pbsc transactionLogContext) logm
                    `onException` (destroyAllResources dbHandle >> closeBlobStore pbscBlobStore))
        return (pbsc, skovData, transactionLogContext)
      else do
        pbscBlobStore <- liftIO $ createBlobStore dtdbwlBlockStateFile
        let pbsc = PersistentBlockStateContext{..}
        let initGS = do
                genState <- case genesisState dtdbwlGenesisData of
                    Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
                    Right genState -> return genState
                pbs <- makePersistent genState
                ser <- saveBlockState pbs
                let ati = defaultValue
                initialSkovPersistentData dtdbwlRuntimeParameters dtdbwlTreeStateDirectory dtdbwlGenesisData pbs ati transactionLogContext ser
        isd <- runReaderT (runPersistentBlockStateMonad initGS) pbsc
                `onException` liftIO (destroyAllResources dbHandle >> destroyBlobStore pbscBlobStore)
        return (pbsc, isd, transactionLogContext)
    shutdownGlobalState _ _ PersistentBlockStateContext{..} st transactionLogContext = do
        closeBlobStore pbscBlobStore
        destroyAllResources (connectionPool transactionLogContext)
        closeSkovPersistentData st

