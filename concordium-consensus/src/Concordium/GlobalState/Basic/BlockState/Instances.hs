{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Concordium.GlobalState.Basic.BlockState.Instances(
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
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Basic.BlockState.InstanceTable

import Data.Word
import Lens.Micro.Platform

import Data.Void


-- |The empty set of smart contract instances.
emptyInstances :: Instances
emptyInstances = Instances Empty

-- |Get the smart contract instance at the given address, if it exists.
getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance addr (Instances iss) = iss ^? ix addr

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
