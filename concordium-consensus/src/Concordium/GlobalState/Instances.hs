{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}
module Concordium.GlobalState.Instances where

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Information (InstanceInfo(InstanceInfo))

import Lens.Micro.Platform
import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Serialize

{-
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
-}

-- |The fixed parameters associated with a smart contract instance
data InstanceParameters = InstanceParameters {
    -- |Address of the instance
    instanceAddress :: !ContractAddress,
    -- |The module that the contract is defined in
    instanceContractModule :: !Core.ModuleRef,
    -- |The name of the contract
    instanceContract :: !Core.TyName,
    -- |The contract's receive function
    instanceReceiveFun :: !Expr,
    -- |The interface of 'instanceContractModule'
    instanceModuleInterface :: !Interface,
    -- |The value interface of 'instanceContractModule'
    instanceModuleValueInterface :: !ValueInterface,
    -- |The type of messages the contract receive function supports
    instanceMessageType :: !(Core.Type Core.ModuleRef),
    -- |Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it here
    -- simplifies things.
    instanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) ImplementsValue),
    -- |Hash of the fixed parameters
    instanceParameterHash :: !H.Hash
}

instance Show InstanceParameters where
    show InstanceParameters{..} = show instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceContract

instance HashableTo H.Hash InstanceParameters where
    getHash = instanceParameterHash

data Instance = Instance {
    -- |The fixed parameters of the instance
    instanceParameters :: InstanceParameters,
    -- |The current local state of the instance
    instanceModel :: !Value,
    -- |The current amount of GTU owned by the instance
    instanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    instanceHash :: !H.Hash
}

instance Show Instance where
    show Instance{..} = show instanceParameters ++ " {balance=" ++ show instanceAmount ++ ", model=" ++ show instanceModel ++ "}"

instance HashableTo H.Hash Instance where
    getHash = instanceHash

instanceInfo :: Instance -> InstanceInfo
instanceInfo Instance{..} = InstanceInfo (instanceMessageType instanceParameters) instanceModel instanceAmount

makeInstanceParameterHash :: ContractAddress -> Core.ModuleRef -> Core.TyName -> H.Hash
makeInstanceParameterHash ca modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put modRef
        put conName

makeInstanceHash :: InstanceParameters -> Value -> Amount -> H.Hash
makeInstanceHash params v a = H.hashLazy $ runPutLazy $ do
        put (instanceParameterHash params)
        putStorable v
        put a

makeInstance :: 
    -- |Module of the contract
    Core.ModuleRef
    -- |Contract name
    -> Core.TyName
    -- |The contract value
    -> ContractValue
    -- |Message type
    -> Core.Type Core.ModuleRef
    -- |Module interface
    -> Interface
    -- |Module value interface
    -> ValueInterface
    -- |Initial state
    -> Value
    -- |Initial balance
    -> Amount
    -- |Address for the instance
    -> ContractAddress
    -> Instance
makeInstance instanceContractModule instanceContract conVal instanceMessageType instanceModuleInterface instanceModuleValueInterface instanceModel instanceAmount instanceAddress
        = Instance {..}
    where
        instanceReceiveFun = updateMethod conVal
        instanceImplements = implements conVal
        instanceParameterHash = makeInstanceParameterHash instanceAddress instanceContractModule instanceContract
        instanceParameters = InstanceParameters {..}
        instanceHash = makeInstanceHash instanceParameters instanceModel instanceAmount

iaddress :: Instance -> ContractAddress
iaddress = instanceAddress . instanceParameters

ireceiveFun :: Instance -> Expr
ireceiveFun = instanceReceiveFun . instanceParameters

imsgTy :: Instance -> Core.Type Core.ModuleRef
imsgTy = instanceMessageType . instanceParameters

iModuleIface :: Instance -> (Interface, ValueInterface)
iModuleIface i = (instanceModuleInterface, instanceModuleValueInterface)
    where
        InstanceParameters{..} = instanceParameters i


newtype Instances = Instances {
  _instances :: HashMap ContractAddress Instance
  }

emptyInstances :: Instances
emptyInstances = Instances Map.empty

getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance m (Instances is) = Map.lookup m is

updateInstance :: Amount -> Value -> Instance -> Instance
updateInstance amt val i =  i {
                                instanceModel = val,
                                instanceAmount = amt,
                                instanceHash = makeInstanceHash (instanceParameters i) val amt
                            }

-- |Update the instance at the specified address with a new amount and value.
-- If there is no instance with the given address, this does nothing.
updateInstanceAt :: ContractAddress -> Amount -> Value -> Instances -> Instances
updateInstanceAt ca amt val (Instances iss) = Instances (iss & ix ca %~ updateInstance amt val)

createInstance :: (ContractAddress -> Instance) -> Instances -> (ContractAddress, Instances)
createInstance mkInst (Instances instances) = (ca, Instances (Map.insert ca inst instances))
    where
        ca = ContractAddress (fromIntegral (length instances)) 0
        inst = mkInst ca

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
