{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Concordium.GlobalState.Instances(
    InstanceParameters(..),
    Instance(..),
    instanceInfo,
    makeInstance,
    iaddress,
    ireceiveFun,
    imsgTy,
    iModuleIface,
    Instances,
    emptyInstances,
    getInstance,
    updateInstance,
    updateInstanceAt,
    createInstance,
    deleteInstance,
    foldInstances
) where

import Concordium.Types
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Information (InstanceInfo(InstanceInfo))
import Concordium.GlobalState.Instances.Internal

import Lens.Micro.Platform

-- |Get the 'InstanceInfo' summary of an 'Instance'.
instanceInfo :: Instance -> InstanceInfo
instanceInfo Instance{..} = InstanceInfo (instanceMessageType instanceParameters) instanceModel instanceAmount

makeInstance :: 
    Core.ModuleRef     -- ^Module of the contract
    -> Core.TyName     -- ^Contract name
    -> ContractValue   -- ^The contract value
    -> Core.Type Core.ModuleRef     -- ^Message type
    -> Interface                    -- ^Module interface
    -> ValueInterface               -- ^Module value interface
    -> Value                        -- ^Initial state
    -> Amount                       -- ^Initial balance
    -> ContractAddress              -- ^Address for the instance
    -> Instance
makeInstance instanceContractModule instanceContract conVal instanceMessageType instanceModuleInterface instanceModuleValueInterface instanceModel instanceAmount instanceAddress
        = Instance {..}
    where
        instanceReceiveFun = updateMethod conVal
        instanceImplements = implements conVal
        instanceParameterHash = makeInstanceParameterHash instanceAddress instanceContractModule instanceContract
        instanceParameters = InstanceParameters {..}
        instanceHash = makeInstanceHash instanceParameters instanceModel instanceAmount

-- |The address of a smart contract instance.
iaddress :: Instance -> ContractAddress
iaddress = instanceAddress . instanceParameters

-- |The receive method of a smart contract instance.
ireceiveFun :: Instance -> Expr
ireceiveFun = instanceReceiveFun . instanceParameters

-- |The message type of a smart contract instance.
imsgTy :: Instance -> Core.Type Core.ModuleRef
imsgTy = instanceMessageType . instanceParameters

-- |The module interfaces of a smart contract instance.
iModuleIface :: Instance -> (Interface, ValueInterface)
iModuleIface i = (instanceModuleInterface, instanceModuleValueInterface)
    where
        InstanceParameters{..} = instanceParameters i

-- |The empty set of smart contract instances.
emptyInstances :: Instances
emptyInstances = Instances Empty

-- |Get the smart contract instance at the given address, if it exists.
getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance addr (Instances iss) = iss ^? ix addr

-- |Update a given smart contract instance.
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

-- |Create a new smart contract instance.
createInstance :: (ContractAddress -> Instance) -> Instances -> (ContractAddress, Instances)
createInstance mkInst (Instances iss) = Instances <$> (iss & newContractInstance <<%~ mkInst)

-- |Delete the instance with the given address.  Does nothing
-- if there is no such instance.
deleteInstance :: ContractAddress -> Instances -> Instances
deleteInstance ca (Instances i) = Instances (deleteContractInstanceExact ca i)

-- |A fold over smart contract instances.
foldInstances :: SimpleFold Instances Instance
foldInstances _ is@(Instances Empty) = is <$ mempty
foldInstances f is@(Instances (Tree t)) = is <$ (foldIT . _Right) f t
