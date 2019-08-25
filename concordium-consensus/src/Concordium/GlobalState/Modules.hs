{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Modules where

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.Types.HashableTo

import Data.Maybe
import Data.Word
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Serialize

import Data.Void

type ModuleIndex = Word64

-- |Module for storage in block state.
-- TODO: in future, we should probably also store the module source, which can
-- be used to recover the interfaces, and should be what is sent over the
-- network.
data Module = Module {
    moduleInterface :: !(Interface Core.UA),
    moduleValueInterface :: !(UnlinkedValueInterface Void),
    moduleIndex :: !ModuleIndex,
    moduleSource :: Core.Module Core.UA
}

-- |A collection of modules.
data Modules = Modules {
    _modules :: HashMap Core.ModuleRef Module,
    _nextModuleIndex :: !ModuleIndex,
    _runningHash :: !H.Hash
}

instance Show Modules where
    show Modules{..} = "Modules {\n" ++ concatMap f (Map.keys _modules) ++ "}"
        where
            f x = show x ++ "\n"

instance HashableTo H.Hash Modules where
    getHash = _runningHash

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules Map.empty 0 (H.hash "")

-- |Create a collection of modules from a list in reverse order of creation.
fromModuleList :: [(Core.ModuleRef, Interface Core.UA, UnlinkedValueInterface Void, Core.Module Core.UA)] -> Modules
fromModuleList = foldr safePut emptyModules
    where
        safePut (mref, iface, viface, source) m = fromMaybe m $ putInterfaces mref iface viface source m

-- |Get the interfaces for a given module by 'Core.ModuleRef'.
getInterfaces :: Core.ModuleRef -> Modules -> Maybe (Interface Core.UA, UnlinkedValueInterface Void)
getInterfaces mref m = do
        Module {..} <- Map.lookup mref (_modules m)
        return (moduleInterface, moduleValueInterface)
       

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterfaces :: Core.ModuleRef -> Interface Core.UA -> UnlinkedValueInterface Void -> Core.Module Core.UA -> Modules -> Maybe Modules
putInterfaces mref iface viface source m =
  if Map.member mref (_modules m) then Nothing
  else Just (Modules {
                _modules = Map.insert mref (Module iface viface (_nextModuleIndex m) source) (_modules m),
                _nextModuleIndex = 1 + _nextModuleIndex m,
                _runningHash = H.hashLazy $ runPutLazy $ put (_runningHash m) <> put mref
            })


-- |Same as 'putInterfaces', but do not check for existence of a module. Hence
-- the precondition of this method is that a module with the same hash is not in
-- the table already
unsafePutInterfaces :: Core.ModuleRef -> Interface Core.UA -> UnlinkedValueInterface Void -> Core.Module Core.UA -> Modules -> Modules
unsafePutInterfaces mref iface viface source m =
    Modules {
             _modules = Map.insert mref (Module iface viface (_nextModuleIndex m) source) (_modules m),
             _nextModuleIndex = 1 + _nextModuleIndex m,
             _runningHash = H.hashLazy $ runPutLazy $ put (_runningHash m) <> put mref
            }

-- |Get a module.
getModule :: Core.ModuleRef -> Modules -> Maybe Module
getModule ref mods = Map.lookup ref (_modules mods)

moduleList :: Modules -> [Core.ModuleRef]
moduleList mods = Map.keys (_modules mods)
