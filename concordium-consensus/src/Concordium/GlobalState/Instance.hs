module Concordium.GlobalState.Instance where

import Data.Serialize
import qualified Data.Set as Set
import qualified Data.ByteString as BS

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Wasm as Wasm

-- |State of a smart contract. In general we don't know anything other than
-- it is a sequence of bytes.
-- FIXME: In the future this should be more structured allowing for more sharing.
newtype ContractState = ContractState {contractState :: BS.ByteString }
    deriving(Eq, Show)

-- The serialize instance uses Word32 for length. This should be reasonable since
-- no instance should ever be able to produce a state bigger than 4GB.
instance Serialize ContractState where
  put ContractState{..} = do
    putWord32be (fromIntegral (BS.length contractState))
    putByteString contractState

  get = do
    len <- fromIntegral <$> getWord32be
    ContractState <$> getByteString len

instance HashableTo H.Hash ContractState where
  getHash cs = H.hash (encode cs)

-- |The fixed parameters associated with a smart contract instance
data InstanceParameters = InstanceParameters {
    -- |Address of the instance
    instanceAddress :: !ContractAddress,
    -- |Address of this contract instance owner, i.e., the creator account.
    instanceOwner :: !AccountAddress,
    -- |The module that the contract is defined in
    instanceContractModule :: !ModuleRef,
    -- |The name of the init method which created this contract.
    instanceInitName :: !Wasm.InitName,
    -- |The receive functions supported by this instance. Always a subset of
    -- receive methods of the module.
    instanceReceiveFuns :: !(Set.Set Wasm.ReceiveName),
    -- |The interface of 'instanceContractModule'
    instanceModuleInterface :: !Wasm.ModuleInterface,
    -- |Hash of the fixed parameters
    instanceParameterHash :: !H.Hash
}

instance Show InstanceParameters where
    show InstanceParameters{..} = show instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceInitName

instance HashableTo H.Hash InstanceParameters where
    getHash = instanceParameterHash

-- |An instance of a smart contract.
data Instance = Instance {
    -- |The fixed parameters of the instance
    instanceParameters :: !InstanceParameters,
    -- |The current local state of the instance
    instanceModel :: !ContractState,
    -- |The current amount of GTU owned by the instance
    instanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    instanceHash :: H.Hash
}

instance Show Instance where
    show Instance{..} = show instanceParameters ++ " {balance=" ++ show instanceAmount ++ ", model=" ++ show instanceModel ++ ", hash=" ++ show instanceHash ++ "}"

instance HashableTo H.Hash Instance where
    getHash = instanceHash

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> ModuleRef -> Wasm.InitName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHash' :: H.Hash -> ContractState -> Amount -> H.Hash
makeInstanceHash' paramHash conState a = H.hashLazy $ runPutLazy $ do
        put paramHash
        putByteString (H.hashToByteString (getHash conState))
        put a

makeInstanceHash :: InstanceParameters -> ContractState -> Amount -> H.Hash
makeInstanceHash params = makeInstanceHash' (instanceParameterHash params)

makeInstance ::
    ModuleRef     -- ^Module of the contract.
    -> Wasm.InitName     -- ^Name of the init method used to initialize the contract.
    -> Set.Set Wasm.ReceiveName -- ^Receive functions suitable for this instance.
    -> Wasm.ModuleInterface        -- ^Module interface
    -> ContractState  -- ^Initial state
    -> Amount       -- ^Initial balance
    -> AccountAddress               -- ^Owner/creator of the instance.
    -> ContractAddress              -- ^Address for the instance
    -> Instance
makeInstance instanceContractModule instanceInitName instanceReceiveFuns instanceModuleInterface instanceModel instanceAmount instanceOwner instanceAddress
        = Instance {..}
    where
        instanceParameterHash = makeInstanceParameterHash instanceAddress instanceOwner instanceContractModule instanceInitName
        instanceParameters = InstanceParameters {..}
        instanceHash = makeInstanceHash instanceParameters instanceModel instanceAmount

-- |The address of a smart contract instance.
iaddress :: Instance -> ContractAddress
iaddress = instanceAddress . instanceParameters

-- |Update a given smart contract instance.
-- FIXME: Updates to the state should be done better in the future, we should not just replace it.
updateInstance :: AmountDelta -> ContractState -> Instance -> Instance
updateInstance delta val i =  updateInstance' amnt val i
  where amnt = applyAmountDelta delta (instanceAmount i)

-- |Update a given smart contract instance with exactly the given amount and state.
updateInstance' :: Amount -> ContractState -> Instance -> Instance
updateInstance' amnt val i =  i {
                                instanceModel = val,
                                instanceAmount = amnt,
                                instanceHash = makeInstanceHash (instanceParameters i) val amnt
                            }
