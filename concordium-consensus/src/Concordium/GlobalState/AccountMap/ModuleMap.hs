{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module defines types and interfaces for the module map, which maps module references to
--  module indices. In particular, it defines the 'MonadModuleMapStore' class, which provides
--  operations for inserting and looking up modules in the LMDB-persisted module map, representing
--  the finalized modules. Additionally, it defines the 'ModuleDifferenceMapReference' type, which
--  is used to record the mapping from module references to module indices for modules added since
--  the last finalized block.
module Concordium.GlobalState.AccountMap.ModuleMap where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Data.Word

import Concordium.Types
import Concordium.Utils.Serialization.Put

import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap
import Concordium.GlobalState.Classes

-- | Index of the module in the module table. Reflects when the module was added
--  to the table.
type ModuleIndex = Word64

-- | An updatable reference to a module difference map, which is used to record the mapping
--  from module references to module indices for modules added since the last finalized block.
type ModuleDifferenceMapReference = DiffMap.DifferenceMapReference ModuleRef ModuleIndex

-- | Monad for inserting and looking up modules in the finalized module map.
--  This maps module references to their corresponding module indices.
--
--  An implementation should ensure atomicity of operations.
--
--  Invariants:
--      * All modules in the store are finalized.
class (Monad m) => MonadModuleMapStore m where
    -- | Inserts the modules into the underlying store.
    insertModules :: [(ModuleRef, ModuleIndex)] -> m ()

    -- | Looks up the 'ModuleIndex' for the provided 'ModuleRef'.
    --  Returns @Just i@ if the module is present in the module table with index @i@,
    --  and returns @Nothing@ if it is not present.
    lookupModuleIndex :: ModuleRef -> m (Maybe ModuleIndex)

    -- | Return all 'ModuleRef's and their corresponding 'ModuleIndex'es in the
    -- module map where the @ModuleIndex@ is less than or equal to the provided
    -- @ModuleIndex@.
    getAllModules :: ModuleIndex -> m [(ModuleRef, ModuleIndex)]

    -- | Get number of entries in the module map.
    getNumberOfModules :: m Word64

    -- | Clear and set the modules to the ones provided.
    reconstruct :: [(ModuleRef, ModuleIndex)] -> m ()

instance
    (Monad (t m), MonadTrans t, MonadModuleMapStore m) =>
    MonadModuleMapStore (MGSTrans t m)
    where
    insertModules mods = lift $ insertModules mods
    lookupModuleIndex = lift . lookupModuleIndex
    getAllModules = lift . getAllModules
    getNumberOfModules = lift getNumberOfModules
    reconstruct = lift . reconstruct
    {-# INLINE insertModules #-}
    {-# INLINE lookupModuleIndex #-}
    {-# INLINE getAllModules #-}
    {-# INLINE getNumberOfModules #-}
    {-# INLINE reconstruct #-}

deriving via
    (MGSTrans (StateT s) m)
    instance
        (MonadModuleMapStore m) => MonadModuleMapStore (StateT s m)
deriving via
    (MGSTrans (ExceptT e) m)
    instance
        (MonadModuleMapStore m) => MonadModuleMapStore (ExceptT e m)
deriving via
    (MGSTrans (WriterT w) m)
    instance
        (Monoid w, MonadModuleMapStore m) => MonadModuleMapStore (WriterT w m)
deriving via
    (MGSTrans (ReaderT w) m)
    instance
        (MonadModuleMapStore m) => MonadModuleMapStore (ReaderT w m)
deriving via
    (MGSTrans PutT m)
    instance
        (MonadModuleMapStore m) => MonadModuleMapStore (PutT m)
