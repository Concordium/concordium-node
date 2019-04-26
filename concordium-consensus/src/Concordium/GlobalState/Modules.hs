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

type ModuleIndex = Word64

-- |Module for storage in block state.
-- TODO: in future, we should probably also store the module source, which can
-- be used to recover the interfaces, and should be what is sent over the
-- network.
data Module = Module {
    moduleInterface :: !Interface,
    moduleValueInterface :: !ValueInterface,
    moduleIndex :: !ModuleIndex
}

-- |A collection of modules.
data Modules = Modules {
    _modules :: HashMap Core.ModuleRef Module,
    _nextModuleIndex :: !ModuleIndex,
    _runningHash :: !H.Hash
}

instance HashableTo H.Hash Modules where
    getHash = _runningHash

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules Map.empty 0 (H.hash "")

-- |Create a collection of modules from a list in reverse order of creation.
fromModuleList :: [(Core.ModuleRef, Interface, ValueInterface)] -> Modules
fromModuleList = foldr safePut emptyModules
    where
        safePut (mref, iface, viface) m = fromMaybe m $ putInterfaces mref iface viface m

-- |Get the interfaces for a given module by 'Core.ModuleRef'.
getInterfaces :: Core.ModuleRef -> Modules -> Maybe (Interface, ValueInterface)
getInterfaces mref m = do
        Module {..} <- Map.lookup mref (_modules m)
        return (moduleInterface, moduleValueInterface)
       

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterfaces :: Core.ModuleRef -> Interface -> ValueInterface -> Modules -> Maybe Modules
putInterfaces mref iface viface m =
  if Map.member mref (_modules m) then Nothing
  else Just (Modules {
                _modules = Map.insert mref (Module iface viface (_nextModuleIndex m)) (_modules m),
                _nextModuleIndex = 1 + _nextModuleIndex m,
                _runningHash = H.hashLazy $ runPutLazy $ put (_runningHash m) <> put mref
            })
