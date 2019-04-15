module Concordium.GlobalState.Modules where

import qualified Concordium.GlobalState.Acorn.Core as Core
import Concordium.GlobalState.Acorn.Interfaces

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map

type Module = (Interface, ValueInterface)

newtype Modules = Modules { _modules :: HashMap Core.ModuleRef Module }

emptyModules :: Modules
emptyModules = Modules Map.empty

getInterfaces :: Core.ModuleRef -> Modules -> Maybe (Interface, ValueInterface)
getInterfaces mref (Modules m) = Map.lookup mref m

-- |Try to add interfaces to the module table. If a module with the given
-- reference exists returns @Nothing@.
putInterfaces :: Core.ModuleRef -> Interface -> ValueInterface -> Modules -> Maybe Modules
putInterfaces mref iface viface (Modules m) =
  if Map.member mref m then Nothing
  else Just (Modules (Map.insert mref (iface, viface) m))
