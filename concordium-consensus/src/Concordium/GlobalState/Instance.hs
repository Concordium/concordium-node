{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Concordium.GlobalState.Instance where

import Data.Maybe
import Data.Serialize
import qualified Data.Set as Set
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.Crypto.SHA256 as SHA256

data InstanceStateV (v :: Wasm.WasmVersion) where
  InstanceStateV0 :: Wasm.ContractState -> InstanceStateV GSWasm.V0
  InstanceStateV1 :: StateV1.TransientState -> InstanceStateV GSWasm.V1

-- There is no versioning added to this. Contract state is always serialized in
-- the context of an instance, which gives it a version.
instance Serialize (InstanceStateV GSWasm.V0) where
  put (InstanceStateV0 model) = put model
  get = InstanceStateV0 <$> get

instance Serialize (InstanceStateV GSWasm.V1) where
  put (InstanceStateV1 model) = put model
  get = InstanceStateV1 <$> get

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
  _instanceVModel :: !(InstanceStateV v),
  -- |The current amount of GTU owned by the instance
  _instanceVAmount :: !Amount,
  -- |Hash of the smart contract instance
  _instanceVHash :: H.Hash
  }

class HasInstanceFields a where
  instanceAmount :: a -> Amount
  instanceHash :: a -> H.Hash

instance HasInstanceFields (InstanceV v) where
  {-# INLINE instanceAmount #-}
  instanceAmount = _instanceVAmount
  {-# INLINE instanceHash #-}
  instanceHash = _instanceVHash

instance HasInstanceFields Instance where
  instanceAmount (InstanceV0 i) = instanceAmount i
  instanceAmount (InstanceV1 i) = instanceAmount i
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
    show (InstanceV0 InstanceV{..}) = show  _instanceVParameters ++ " {balance=" ++ show _instanceVAmount ++ ", hash = " ++ show _instanceVHash ++ "}"
    show (InstanceV1 InstanceV{..}) = show  _instanceVParameters ++ " {balance=" ++ show _instanceVAmount ++ ", hash=" ++ show _instanceVHash ++ "}"

instance HashableTo H.Hash Instance where
    getHash (InstanceV0 InstanceV{..}) = _instanceVHash
    getHash (InstanceV1 InstanceV{..}) = _instanceVHash

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> ModuleRef -> Wasm.InitName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHashV0' :: H.Hash -> InstanceStateV GSWasm.V0 -> Amount -> H.Hash
makeInstanceHashV0' paramHash (InstanceStateV0 conState) a = H.hashLazy $ runPutLazy $ do
        put paramHash
        putByteString (H.hashToByteString (getHash conState))
        put a

makeInstanceHashV0 :: InstanceParameters v -> InstanceStateV GSWasm.V0 -> Amount -> H.Hash
makeInstanceHashV0 params = makeInstanceHashV0' (instanceParameterHash params)

makeInstanceHashV1' :: H.Hash -> InstanceStateV GSWasm.V1 -> Amount -> H.Hash
makeInstanceHashV1' paramHash (InstanceStateV1 conState) a = H.hashLazy $ runPutLazy $ do
        put Wasm.V1
        put paramHash
        put (getHash conState :: SHA256.Hash)
        put a

makeInstanceHashV1 :: InstanceParameters v -> InstanceStateV GSWasm.V1 -> Amount -> H.Hash
makeInstanceHashV1 params = makeInstanceHashV1' (instanceParameterHash params)

makeInstanceHash :: forall v .Wasm.IsWasmVersion v => InstanceParameters v -> InstanceStateV v -> Amount -> H.Hash
makeInstanceHash params =
    case Wasm.getWasmVersion @v of
        Wasm.SV0 -> makeInstanceHashV0' (instanceParameterHash params)
        Wasm.SV1 -> makeInstanceHashV1' (instanceParameterHash params)

makeInstanceV ::
    Wasm.InitName
    -- ^Name of the init method used to initialize the contract.
    -> Set.Set Wasm.ReceiveName
    -- ^Receive functions suitable for this instance.
    -> GSWasm.ModuleInterfaceV v
    -- ^Module interface
    -> InstanceStateV v
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
            GSWasm.InstrumentedWasmModuleV0 {} -> InstanceV{
              _instanceVHash = makeInstanceHashV0  _instanceVParameters _instanceVModel _instanceVAmount,
              ..
              }
            GSWasm.InstrumentedWasmModuleV1 {} -> InstanceV{
              _instanceVHash = makeInstanceHashV1  _instanceVParameters _instanceVModel _instanceVAmount,
              ..}
    where
        instanceContractModule = GSWasm.miModuleRef instanceModuleInterface
        instanceParameterHash = makeInstanceParameterHash _instanceAddress instanceOwner instanceContractModule instanceInitName
        _instanceVParameters = InstanceParameters {..}

makeInstance ::
    Wasm.InitName
    -- ^Name of the init method used to initialize the contract.
    -> Set.Set Wasm.ReceiveName
    -- ^Receive functions suitable for this instance.
    -> GSWasm.ModuleInterfaceV v
    -- ^Module interface
    -> InstanceStateV v
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
updateInstanceV :: Wasm.IsWasmVersion v => AmountDelta -> Maybe (InstanceStateV v) -> InstanceV v -> InstanceV v
updateInstanceV delta val i = updateInstanceV' amnt val i
  where amnt = applyAmountDelta delta (_instanceVAmount i)

-- |Update a given smart contract instance with exactly the given amount and state.
updateInstanceV' :: forall v . Wasm.IsWasmVersion v => Amount -> Maybe (InstanceStateV v) -> InstanceV v -> InstanceV v
updateInstanceV' amnt val i =  i {
                                _instanceVModel = newVal,
                                _instanceVAmount = amnt,
                                _instanceVHash = makeInstanceHash ( _instanceVParameters i) newVal amnt
                            }
  where newVal = fromMaybe (_instanceVModel i) val

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
