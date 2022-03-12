{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.Basic.BlockState.Instances(
    InstanceParameters(..),
    Instance(..),
    InstanceV(..),
    HasInstanceAddress(..),
    makeInstance,
    Instances,
    emptyInstances,
    getInstance,
    updateInstanceAt,
    updateInstanceAt',
    createInstance,
    deleteInstance,
    foldInstances,
    instanceCount,
    -- * Serialization
    putInstancesV0,
    getInstancesV0
) where

import Concordium.Types
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Basic.BlockState.InstanceTable

import Data.Serialize
import qualified Data.Set as Set
import Data.Word
import Lens.Micro.Platform

-- |The empty set of smart contract instances.
emptyInstances :: Instances
emptyInstances = Instances Empty

-- |Get the smart contract instance at the given address, if it exists.
getInstance :: ContractAddress -> Instances -> Maybe Instance
getInstance addr (Instances iss) = iss ^? ix addr

-- |Update the instance at the specified address with an amount delta and
-- potentially new state. If new state is not provided the state of the instance
-- is not changed. If there is no instance with the given address, this does
-- nothing. If the instance at the given address has a different version than
-- given this function raises an exception.
updateInstanceAt :: forall v .Wasm.IsWasmVersion v => ContractAddress -> AmountDelta -> Maybe (InstanceStateV v) -> Instances -> Instances
updateInstanceAt ca amt val (Instances iss) = Instances (iss & ix ca %~ updateOnlyV)
    where
        -- only update if the instance matches the state version. Otherwise raise an exception.
        updateOnlyV = case Wasm.getWasmVersion @v of
                          Wasm.SV0 -> \case
                            InstanceV0 i -> InstanceV0 $ updateInstanceV amt val i
                            InstanceV1 _ -> error "Expected a V0 instance, but got V1."
                          Wasm.SV1 -> \case
                            InstanceV0 _ -> error "Expected a V1 instance, but got V0"
                            InstanceV1 i -> InstanceV1 $ updateInstanceV amt val i

-- |Update the instance at the specified address with a __new amount__ and
-- potentially new state. If new state is not provided the state of the instance
-- is not changed. If there is no instance with the given address, this does
-- nothing. If the instance at the given address has a different version than
-- given this function raises an exception.
updateInstanceAt' :: forall v . Wasm.IsWasmVersion v => ContractAddress -> Amount -> Maybe (InstanceStateV v) -> Instances -> Instances
updateInstanceAt' ca amt val (Instances iss) = Instances (iss & ix ca %~ updateOnlyV)
    where
        -- only update if the instance matches the state version. Otherwise raise an exception.
        updateOnlyV = case Wasm.getWasmVersion @v of
                          Wasm.SV0 -> \case
                            InstanceV0 i -> InstanceV0 $ updateInstanceV' amt val i
                            InstanceV1 _ -> error "Expected a V0 instance, but got V1."
                          Wasm.SV1 -> \case
                            InstanceV0 _ -> error "Expected a V1 instance, but got V0"
                            InstanceV1 i -> InstanceV1 $ updateInstanceV' amt val i

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

-- |Serialize 'Instances' in V0 format.
putInstancesV0 :: Putter Instances
putInstancesV0 (Instances Empty) = putWord8 0
putInstancesV0 (Instances (Tree _ t)) = do
        mapM_ putOptInstance (t ^.. foldIT)
        putWord8 0
    where
        putOptInstance (Left si) = do
            putWord8 1
            put si
        putOptInstance (Right inst) = do
            case inst of
              InstanceV0 i -> do
                putWord8 2
                putV0InstanceV0 i
              InstanceV1 i -> do
                putWord8 3
                putV1InstanceV0 i

-- |Deserialize 'Instances' in V0 format.
getInstancesV0
    :: (ModuleRef -> Wasm.InitName -> Maybe (Set.Set Wasm.ReceiveName, GSWasm.ModuleInterface))
    -> Get Instances
getInstancesV0 resolve = Instances <$> constructM buildInstance
    where
        buildInstance idx = getWord8 >>= \case
            0 -> return Nothing
            1 -> Just . Left <$> get
            2 -> Just . Right . InstanceV0 <$> getV0InstanceV0 resolve idx
            3 -> Just . Right . InstanceV1 <$> getV1InstanceV0 resolve idx
            _ -> fail "Bad instance list"
