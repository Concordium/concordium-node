{-# LANGUAGE DeriveFunctor #-}

-- | Basic interface definitions for smart contract instances.
module Concordium.GlobalState.Instance (InstanceParameters (..), HasInstanceAddress (..)) where

import qualified Data.Set as Set

import Concordium.Types
import qualified Concordium.Wasm as Wasm

import qualified Concordium.GlobalState.Wasm as GSWasm

-- | The fixed parameters associated with a smart contract instance, parametrized by the type
--  of the instrumented module.
data InstanceParameters instrumentedModule = InstanceParameters
    { -- | Address of the instance
      _instanceAddress :: !ContractAddress,
      -- | Address of this contract instance owner, i.e., the creator account.
      instanceOwner :: !AccountAddress,
      -- | The name of the init method which created this contract.
      instanceInitName :: !Wasm.InitName,
      -- | The receive functions supported by this instance. Always a subset of
      --  receive methods of the module.
      instanceReceiveFuns :: !(Set.Set Wasm.ReceiveName),
      -- | The interface of 'instanceContractModule'
      instanceModuleInterface :: !(GSWasm.ModuleInterfaceA instrumentedModule)
    }
    deriving (Eq, Functor)

class HasInstanceAddress a where
    instanceAddress :: a -> ContractAddress

instance HasInstanceAddress (InstanceParameters im) where
    instanceAddress InstanceParameters{..} = _instanceAddress

instance Show (InstanceParameters im) where
    show InstanceParameters{..} = show _instanceAddress ++ " :: " ++ show instanceContractModule ++ "." ++ show instanceInitName
      where
        instanceContractModule = GSWasm.miModuleRef instanceModuleInterface
