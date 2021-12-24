{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Concordium.GlobalState.Instance where

import Data.Aeson
import Data.Serialize
import qualified Data.Set as Set
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import Data.Word

-- |The fixed parameters associated with a smart contract instance
data InstanceParameters v = InstanceParameters {
    -- |Address of the instance
    _instanceAddress :: !ContractAddress,
    -- |Address of this contract instance owner, i.e., the creator account.
    instanceOwner :: !AccountAddress,
    -- |The name of the init method which created this contract.
    instanceInitName :: !Wasm.InitName,
    -- |The receive functions supported by this instance. Always a subset of
    -- receive methods of the module.
    instanceReceiveFuns :: !(Set.Set Wasm.ReceiveName),
    -- |The interface of 'instanceContractModule'
    instanceModuleInterface :: !(GSWasm.ModuleInterfaceV v),
    -- |Hash of the fixed parameters
    instanceParameterHash :: !H.Hash
}

class HasInstanceParameters a where
  instanceAddress :: a -> ContractAddress

instance HasInstanceParameters (InstanceParameters v) where
  instanceAddress InstanceParameters{..} = _instanceAddress

instance Show (InstanceParameters v) where
    show InstanceParameters{..} = show _instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceInitName
        where instanceContractModule = GSWasm.miModuleRef instanceModuleInterface


instance HashableTo H.Hash (InstanceParameters v) where
    getHash = instanceParameterHash

data InstanceV v = InstanceV {
  -- |The fixed parameters of the instance
  _instanceVParameters :: !(InstanceParameters v),
  -- |The current local state of the instance
  _instanceVModel :: !Wasm.ContractState,
  -- |The current amount of GTU owned by the instance
  _instanceVAmount :: !Amount,
  -- |Hash of the smart contract instance
  _instanceVHash :: H.Hash
  }

class HasInstanceFields a where
  instanceAmount :: a -> Amount
  instanceModel :: a -> Wasm.ContractState
  instanceHash :: a -> H.Hash

instance HasInstanceFields (InstanceV v) where
  {-# INLINE instanceAmount #-}
  instanceAmount = _instanceVAmount
  {-# INLINE instanceModel #-}
  instanceModel = _instanceVModel
  {-# INLINE instanceHash #-}
  instanceHash = _instanceVHash

instance HasInstanceFields Instance where
  instanceAmount (InstanceV0 i) = instanceAmount i
  instanceAmount (InstanceV1 i) = instanceAmount i
  instanceModel (InstanceV0 i) = instanceModel i
  instanceModel (InstanceV1 i) = instanceModel i
  instanceHash (InstanceV0 i) = instanceHash i
  instanceHash (InstanceV1 i) = instanceHash i


instance HasInstanceParameters (InstanceV v) where
  instanceAddress = instanceAddress . _instanceVParameters

instance HasInstanceParameters Instance where
  instanceAddress (InstanceV0 i) = instanceAddress i
  instanceAddress (InstanceV1 i) = instanceAddress i

-- |An instance of a smart contract.
data Instance = InstanceV0 (InstanceV GSWasm.V0)
    | InstanceV1 (InstanceV GSWasm.V1)

instance Show Instance where
    show (InstanceV0 InstanceV{..}) = show  _instanceVParameters ++ " {balance=" ++ show _instanceVAmount ++ ", model=" ++ show _instanceVModel ++ ", hash=" ++ show _instanceVHash ++ "}"
    show (InstanceV1 InstanceV{..}) = show  _instanceVParameters ++ " {balance=" ++ show _instanceVAmount ++ ", model=" ++ show _instanceVModel ++ ", hash=" ++ show _instanceVHash ++ "}"

instance HashableTo H.Hash Instance where
    getHash (InstanceV0 InstanceV{..}) = _instanceVHash
    getHash (InstanceV1 InstanceV{..}) = _instanceVHash

-- |Helper function for JSON encoding an 'Instance'.
instancePairs :: KeyValue kv => Instance -> [kv]
{-# INLINE instancePairs #-}
-- TODO: Get the version from the module instead.
instancePairs (InstanceV0 InstanceV{..}) =
    [ "model" .= _instanceVModel,
      "owner" .= instanceOwner  _instanceVParameters,
      "amount" .= _instanceVAmount,
      "methods" .= instanceReceiveFuns  _instanceVParameters,
      "name" .= instanceInitName  _instanceVParameters,
      "sourceModule" .= GSWasm.miModuleRef (instanceModuleInterface  _instanceVParameters),
      "version" .= (0 :: Word32)
    ]
instancePairs (InstanceV1 InstanceV{..}) =
    [ "model" .= _instanceVModel,
      "owner" .= instanceOwner  _instanceVParameters,
      "amount" .= _instanceVAmount,
      "methods" .= instanceReceiveFuns  _instanceVParameters,
      "name" .= instanceInitName  _instanceVParameters,
      "sourceModule" .= GSWasm.miModuleRef (instanceModuleInterface  _instanceVParameters),
      "version" .= (1 :: Word32)
    ]
    

-- |JSON instance to support consensus queries.
instance ToJSON Instance where
    toJSON inst = object $ instancePairs inst
    toEncoding inst = pairs $ mconcat $ instancePairs inst

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> ModuleRef -> Wasm.InitName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHash' :: H.Hash -> Wasm.ContractState -> Amount -> H.Hash
makeInstanceHash' paramHash conState a = H.hashLazy $ runPutLazy $ do
        put paramHash
        putByteString (H.hashToByteString (getHash conState))
        put a

makeInstanceHash :: InstanceParameters v -> Wasm.ContractState -> Amount -> H.Hash
makeInstanceHash params = makeInstanceHash' (instanceParameterHash params)

makeInstanceV ::
    Wasm.InitName
    -- ^Name of the init method used to initialize the contract.
    -> Set.Set Wasm.ReceiveName
    -- ^Receive functions suitable for this instance.
    -> GSWasm.ModuleInterfaceV v
    -- ^Module interface
    -> Wasm.ContractState
    -- ^Initial state
    -> Amount
    -- ^Initial balance
    -> AccountAddress
    -- ^Owner/creator of the instance.
    -> ContractAddress
    -- ^Address for the instance
    -> InstanceV v
makeInstanceV instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress
        = case GSWasm.miModule instanceModuleInterface of
            GSWasm.InstrumentedWasmModuleV0 {} -> InstanceV{..}
            GSWasm.InstrumentedWasmModuleV1 {} -> InstanceV{..}
    where
        instanceContractModule = GSWasm.miModuleRef instanceModuleInterface
        instanceParameterHash = makeInstanceParameterHash _instanceAddress instanceOwner instanceContractModule instanceInitName
        _instanceVParameters = InstanceParameters {..}
        _instanceVHash = makeInstanceHash  _instanceVParameters _instanceVModel _instanceVAmount

makeInstance ::
    Wasm.InitName
    -- ^Name of the init method used to initialize the contract.
    -> Set.Set Wasm.ReceiveName
    -- ^Receive functions suitable for this instance.
    -> GSWasm.ModuleInterfaceV v
    -- ^Module interface
    -> Wasm.ContractState
    -- ^Initial state
    -> Amount
    -- ^Initial balance
    -> AccountAddress
    -- ^Owner/creator of the instance.
    -> ContractAddress
    -- ^Address for the instance
    -> Instance
makeInstance instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress
        = case GSWasm.miModule instanceModuleInterface of
            GSWasm.InstrumentedWasmModuleV0 {} -> InstanceV0 (makeInstanceV instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress)
            GSWasm.InstrumentedWasmModuleV1 {} -> InstanceV1 (makeInstanceV instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress)

-- |The address of a smart contract instance.
iaddress :: Instance -> ContractAddress
iaddress (InstanceV0 InstanceV{..}) = _instanceAddress  _instanceVParameters
iaddress (InstanceV1 InstanceV{..}) = _instanceAddress  _instanceVParameters

-- |Update a given smart contract instance.
-- FIXME: Updates to the state should be done better in the future, we should not just replace it.
updateInstanceV :: AmountDelta -> Wasm.ContractState -> InstanceV v -> InstanceV v
updateInstanceV delta val i =  updateInstanceV' amnt val i
  where amnt = applyAmountDelta delta (_instanceVAmount i)

updateInstance :: AmountDelta -> Wasm.ContractState -> Instance -> Instance
updateInstance delta val (InstanceV0 i) = InstanceV0 $ updateInstanceV delta val i
updateInstance delta val (InstanceV1 i) = InstanceV1 $ updateInstanceV delta val i


-- |Update a given smart contract instance with exactly the given amount and state.
updateInstanceV' :: Amount -> Wasm.ContractState -> InstanceV v -> InstanceV v
updateInstanceV' amnt val i =  i {
                                _instanceVModel = val,
                                _instanceVAmount = amnt,
                                _instanceVHash = makeInstanceHash ( _instanceVParameters i) val amnt
                            }

updateInstance' :: Amount -> Wasm.ContractState -> Instance -> Instance
updateInstance' amnt val (InstanceV0 i) = InstanceV0 $ updateInstanceV' amnt val i
updateInstance' amnt val (InstanceV1 i) = InstanceV1 $ updateInstanceV' amnt val i


-- |Serialize a V0 smart contract instance in V0 format.
putV0InstanceV0 :: Putter (InstanceV GSWasm.V0)
putV0InstanceV0 InstanceV{ _instanceVParameters = InstanceParameters{..}, ..} = do
        -- InstanceParameters
        -- Only put the Subindex part of the address
        put (contractSubindex _instanceAddress)
        put instanceOwner
        put (GSWasm.miModuleRef instanceModuleInterface)
        put instanceInitName
        -- instanceReceiveFuns, instanceModuleInterface and instanceParameterHash
        -- are not included, since they can be derived from context.
        put _instanceVModel
        put _instanceVAmount

-- |Serialize a V1 smart contract instance in V0 format.
putV1InstanceV0 :: Putter (InstanceV GSWasm.V1)
putV1InstanceV0 InstanceV{ _instanceVParameters = InstanceParameters{..}, ..} = do
        -- InstanceParameters
        -- Only put the Subindex part of the address
        put (contractSubindex _instanceAddress)
        put instanceOwner
        put (GSWasm.miModuleRef instanceModuleInterface)
        put instanceInitName
        -- instanceReceiveFuns, instanceModuleInterface and instanceParameterHash
        -- are not included, since they can be derived from context.
        put _instanceVModel
        put _instanceVAmount


-- |Deserialize a V0 smart contract instance in V0 format.
getV0InstanceV0
    :: (ModuleRef -> Wasm.InitName -> Maybe (Set.Set Wasm.ReceiveName, GSWasm.ModuleInterface))
    -- ^Function for resolving the receive functions and module interface.
    -> ContractIndex
    -- ^Index of the contract
    -> Get (InstanceV GSWasm.V0)
getV0InstanceV0 resolve idx = do
        -- InstanceParameters
        subindex <- get
        let _instanceAddress = ContractAddress idx subindex
        instanceOwner <- get
        instanceContractModule <- get
        instanceInitName <- get
        (instanceReceiveFuns, instanceModuleInterface) <-
            case resolve instanceContractModule instanceInitName of
                Just (r, GSWasm.ModuleInterfaceV0 iface) -> return (r, iface)
                Just (_, GSWasm.ModuleInterfaceV1 _) -> fail "Expected module version 0, but module version 1 encountered."
                Nothing -> fail "Unable to resolve smart contract"
        _instanceVModel <- get
        _instanceVAmount <- get
        return $ makeInstanceV instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress

-- |Deserialize a V1 smart contract instance in V0 format.
getV1InstanceV0
    :: (ModuleRef -> Wasm.InitName -> Maybe (Set.Set Wasm.ReceiveName, GSWasm.ModuleInterface))
    -- ^Function for resolving the receive functions and module interface.
    -> ContractIndex
    -- ^Index of the contract
    -> Get (InstanceV GSWasm.V1)
getV1InstanceV0 resolve idx = do
        -- InstanceParameters
        subindex <- get
        let _instanceAddress = ContractAddress idx subindex
        instanceOwner <- get
        instanceContractModule <- get
        instanceInitName <- get
        (instanceReceiveFuns, instanceModuleInterface) <-
            case resolve instanceContractModule instanceInitName of
                Just (_, GSWasm.ModuleInterfaceV0 _) -> fail "Expected module version 1, but module version 0 encountered."
                Just (r, GSWasm.ModuleInterfaceV1 iface) -> return (r, iface)
                Nothing -> fail "Unable to resolve smart contract"
        _instanceVModel <- get
        _instanceVAmount <- get
        return $ makeInstanceV instanceInitName instanceReceiveFuns instanceModuleInterface _instanceVModel _instanceVAmount instanceOwner _instanceAddress
