{-# LANGUAGE RecordWildCards #-}
module Concordium.GlobalState where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Concordium.GlobalState.Acorn.Core as Core
import Concordium.GlobalState.Acorn.Interfaces
import Concordium.GlobalState.Execution
import Concordium.GlobalState.Types
import Concordium.GlobalState.Account
import Concordium.GlobalState.Transactions


data TreeState = TreeState {
    tsTransactions :: TransactionTable
    -- TODO: move Skov data here
}

data BestBlockState


-- | Current global state used by the scheduler.
data GlobalState = GlobalState 
    {instances :: !(HashMap ContractAddress Instance)  -- ^Contract instances.
    ,accounts :: !(HashMap AccountAddress Account)      -- ^FIXME: Unclear what the distinction between accounts/instances is.
    ,modules :: !(HashMap Core.ModuleRef (Interface, ValueInterface)) -- ^Interfaces of deployed modules.
    }

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState {
  instances = HM.empty
  , accounts = HM.empty
  , modules = HM.empty
  }

firstFreeContract :: GlobalState -> ContractAddress
firstFreeContract (GlobalState{..}) = 
  ContractAddress (fromIntegral (length instances)) 0 
