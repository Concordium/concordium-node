module Concordium.GlobalState.BlockState where

import Concordium.GlobalState.Account
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Modules

data BlockState = BlockState {
    blockAccounts :: Accounts,
    blockInstances :: Instances,
    blockModules :: Modules
}

emptyBlockState :: BlockState
emptyBlockState = BlockState {
  blockAccounts = emptyAccounts
  , blockInstances = emptyInstances
  , blockModules = emptyModules
  }

