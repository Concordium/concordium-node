module Concordium.GlobalState.Instances where

import Concordium.Types
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map


data Instance = Instance
    {iaddress :: !ContractAddress
    ,ireceiveFun :: !Expr                         -- ^Pointer to its receive function.
    ,iModuleIface :: !(Interface, ValueInterface) -- ^Pointers to the module interfaces of the module this contract belongs to.
    ,imsgTy :: !(Core.Type Core.ModuleRef)        -- ^The type of messages its receive function supports.
    ,imodel :: !Value                     -- ^The current local state of the instance.
    ,iamount :: !Amount                   -- ^And the amount of GTUs it currently owns.
    ,instanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) ImplementsValue)  
    -- ^Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it
    -- here simplifies things.
    } deriving(Show)



newtype Instances = Instances {
  _instances :: HashMap ContractAddress Instance
  }

emptyInstances :: Instances
emptyInstances = Instances Map.empty

getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance m (Instances is) = Map.lookup m is

-- |Insert or replace an instance on a given address.
putInstance :: Instance -> Instances -> Instances
putInstance is (Instances iss) =
  let addr = iaddress is in Instances (Map.insert addr is iss)

-- |Create a new instance. If an instance with the given address already exists
-- do nothing and return @Nothing@.
newInstance :: Instance -> Instances -> Maybe Instances
newInstance is (Instances iss) =
    if addr `Map.member` iss then Nothing
    else Just (Instances (Map.insert addr is iss))
  where addr = iaddress is

-- |FIXME (when we have the ability to remove contracts)
-- Get the first contract available contract address.
firstFreeContract :: Instances -> ContractAddress
firstFreeContract (Instances instances) = 
  ContractAddress (fromIntegral (length instances)) 0 

toList :: Instances -> [(ContractAddress, Instance)]
toList = Map.toList . _instances
