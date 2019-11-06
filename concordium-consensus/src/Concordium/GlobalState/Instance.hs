{-# LANGUAGE
    RecordWildCards,
    MultiParamTypeClasses #-}
module Concordium.GlobalState.Instance where

import Data.Serialize
import Data.HashMap.Strict(HashMap)
import Data.Void

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Information (InstanceInfo(InstanceInfo))


-- |The fixed parameters associated with a smart contract instance
data InstanceParameters = InstanceParameters {
    -- |Address of the instance
    instanceAddress :: !ContractAddress,
    -- |Address of this contract instance owner, i.e., the creator account.
    instanceOwner :: !AccountAddress,
    -- |The module that the contract is defined in
    instanceContractModule :: !Core.ModuleRef,
    -- |The name of the contract
    instanceContract :: !Core.TyName,
    -- |The contract's receive function
    instanceReceiveFun :: !(LinkedExpr Void),
    -- |The interface of 'instanceContractModule'
    instanceModuleInterface :: !(Interface Core.UA),
    -- |The value interface of 'instanceContractModule'
    instanceModuleValueInterface :: !(UnlinkedValueInterface Void),
    -- |The type of messages the contract receive function supports
    instanceMessageType :: !(Core.Type Core.UA Core.ModuleRef),
    -- |Implementation of the given class sender method. This can also be looked
    -- up through the contract, and we should probably do that, but having it here
    -- simplifies things.
    instanceImplements :: !(HashMap (Core.ModuleRef, Core.TyName) (LinkedImplementsValue Void)),
    -- |Hash of the fixed parameters
    instanceParameterHash :: !H.Hash
}

instance Show InstanceParameters where
    show InstanceParameters{..} = show instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceContract

instance HashableTo H.Hash InstanceParameters where
    getHash = instanceParameterHash

-- |An instance of a smart contract.
data Instance = Instance {
    -- |The fixed parameters of the instance
    instanceParameters :: !InstanceParameters,
    -- |The current local state of the instance
    instanceModel :: !(Value Void),
    -- |The current amount of GTU owned by the instance
    instanceAmount :: !Amount,
    -- |Hash of the smart contract instance
    instanceHash :: H.Hash
}

instance Show Instance where
    show Instance{..} = show instanceParameters ++ " {balance=" ++ show instanceAmount ++ ", model=" ++ show instanceModel ++ "}"

instance HashableTo H.Hash Instance where
    getHash = instanceHash

makeInstanceParameterHash :: ContractAddress -> AccountAddress -> Core.ModuleRef -> Core.TyName -> H.Hash
makeInstanceParameterHash ca aa modRef conName = H.hashLazy $ runPutLazy $ do
        put ca
        put aa
        put modRef
        put conName

makeInstanceHash :: InstanceParameters -> Value Void -> Amount -> H.Hash
makeInstanceHash params v a = H.hashLazy $ runPutLazy $ do
        put (instanceParameterHash params)
        putStorable v
        put a

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
