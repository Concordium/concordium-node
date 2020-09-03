{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Modules where

import Data.Maybe
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Serialize

import Lens.Micro.Platform
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Wasm as Wasm
import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.BlockState

-- |A collection of modules.
data Modules = Modules {
    _modules :: HashMap ModuleRef Module,
    _nextModuleIndex :: !ModuleIndex,
    _runningHash :: !H.Hash
}

makeLenses ''Modules

instance Show Modules where
    show Modules{..} = "Modules {\n" ++ concatMap f (Map.keys _modules) ++ "}"
        where
            f x = show x ++ "\n"

instance HashableTo H.Hash Modules where
    getHash = _runningHash

instance Monad m => MHashableTo m H.Hash Modules where

-- |The empty collection of modules
emptyModules :: Modules
emptyModules = Modules Map.empty 0 (H.hash "")

-- |Create a collection of modules from a list in reverse order of creation.
fromModuleList :: [Wasm.ModuleInterface] -> Modules
fromModuleList = foldr safePut emptyModules
    where
        safePut iface m = fromMaybe m $ putInterfaces iface m

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterfaces :: Wasm.ModuleInterface -> Modules -> Maybe Modules
putInterfaces iface m =
  if Map.member mref (_modules m) then Nothing
  else Just (Modules {
                _modules = Map.insert mref (Module iface (_nextModuleIndex m)) (_modules m),
                _nextModuleIndex = 1 + _nextModuleIndex m,
                _runningHash = H.hashLazy $ runPutLazy $ put (_runningHash m) <> put mref
            })
  where mref = Wasm.miModuleRef iface


-- |Same as 'putInterfaces', but do not check for existence of a module. Hence
-- the precondition of this method is that a module with the same hash is not in
-- the table already
unsafePutInterfaces
    :: Wasm.ModuleInterface
    -> Modules
    -> Modules
unsafePutInterfaces iface m =
    Modules {
             _modules = Map.insert mref (Module iface (_nextModuleIndex m)) (_modules m),
             _nextModuleIndex = 1 + _nextModuleIndex m,
             _runningHash = H.hashLazy $ runPutLazy $ put (_runningHash m) <> put mref
            }
  where mref = Wasm.miModuleRef iface

-- |Get a full module by name.
getModule :: ModuleRef -> Modules -> Maybe Module
getModule ref mods = Map.lookup ref (_modules mods)

-- |Get the list of all currently deployed modules.
-- The order of the list is not specified.
moduleList :: Modules -> [ModuleRef]
moduleList mods = Map.keys (_modules mods)
