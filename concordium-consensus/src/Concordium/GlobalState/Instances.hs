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
    updateInstanceAt',
    createInstance,
    deleteInstance,
    foldInstances,
    instanceCount
) where

import Concordium.Types
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Information (InstanceInfo(InstanceInfo))
import Concordium.GlobalState.Instances.Internal

import Data.Word
import Lens.Micro.Platform

import Data.Void

-- |Get the 'InstanceInfo' summary of an 'Instance'.
instanceInfo :: Instance -> InstanceInfo
instanceInfo Instance{..} = InstanceInfo (instanceMessageType instanceParameters) instanceModel instanceAmount

makeInstance ::
    Core.ModuleRef     -- ^Module of the contract
    -> Core.TyName     -- ^Contract name
    -> LinkedContractValue Void  -- ^The contract value
    -> Core.Type Core.UA Core.ModuleRef     -- ^Message type
    -> Interface Core.UA          -- ^Module interface
    -> UnlinkedValueInterface Void  -- ^Module value interface
    -> Value Void                   -- ^Initial state
    -> Amount                       -- ^Initial balance
    -> AccountAddress               -- ^Owner/creator of the instance.
    -> ContractAddress              -- ^Address for the instance
    -> Instance
makeInstance instanceContractModule instanceContract conVal instanceMessageType instanceModuleInterface instanceModuleValueInterface instanceModel instanceAmount instanceOwner instanceAddress
        = Instance {..}
    where
        instanceReceiveFun = fst (cvReceiveMethod conVal)
        instanceImplements = cvImplements conVal
        instanceParameterHash = makeInstanceParameterHash instanceAddress instanceOwner instanceContractModule instanceContract
        instanceParameters = InstanceParameters {..}
        instanceHash = makeInstanceHash instanceParameters instanceModel instanceAmount

-- |The address of a smart contract instance.
iaddress :: Instance -> ContractAddress
iaddress = instanceAddress . instanceParameters

-- |The receive method of a smart contract instance.
ireceiveFun :: Instance -> LinkedExpr Void
ireceiveFun = instanceReceiveFun . instanceParameters

-- |The message type of a smart contract instance.
imsgTy :: Instance -> Core.Type Core.UA Core.ModuleRef
imsgTy = instanceMessageType . instanceParameters

-- |The module interfaces of a smart contract instance.
iModuleIface :: Instance -> (Interface Core.UA, UnlinkedValueInterface Void)
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
updateInstance :: AmountDelta -> Value Void -> Instance -> Instance
updateInstance delta val i =  i {
                                instanceModel = val,
                                instanceAmount = amnt,
                                instanceHash = makeInstanceHash (instanceParameters i) val amnt
                            }
  where amnt = applyAmountDelta delta (instanceAmount i)

-- |Update a given smart contract instance.
updateInstance' :: Amount -> Value Void -> Instance -> Instance
updateInstance' amnt val i =  i {
                                instanceModel = val,
                                instanceAmount = amnt,
                                instanceHash = makeInstanceHash (instanceParameters i) val amnt
                            }


-- |Update the instance at the specified address with an amount delta and value.
-- If there is no instance with the given address, this does nothing.
updateInstanceAt :: ContractAddress -> AmountDelta -> Value Void -> Instances -> Instances
updateInstanceAt ca amt val (Instances iss) = Instances (iss & ix ca %~ updateInstance amt val)

-- |Update the instance at the specified address with a __new amount__ and value.
-- If there is no instance with the given address, this does nothing.
updateInstanceAt' :: ContractAddress -> Amount -> Value Void -> Instances -> Instances
updateInstanceAt' ca amt val (Instances iss) = Instances (iss & ix ca %~ updateInstance' amt val)

-- |Create a new smart contract instance.
createInstance :: (ContractAddress -> Instance) -> Instances -> (Instance, Instances)
createInstance mkInst (Instances iss) = Instances <$> (iss & newContractInstance <%~ mkInst)

-- |Delete the instance with the given address.  Does nothing
-- if there is no such instance.
deleteInstance :: ContractAddress -> Instances -> Instances
deleteInstance ca (Instances i) = Instances (deleteContractInstanceExact ca i)

-- |A fold over smart contract instances.
foldInstances :: SimpleFold Instances Instance
foldInstances _ is@(Instances Empty) = is <$ mempty
foldInstances f is@(Instances (Tree _ t)) = is <$ (foldIT . _Right) f t

instanceCount :: Instances -> Word64
instanceCount (Instances Empty) = 0
instanceCount (Instances (Tree c _)) = c
