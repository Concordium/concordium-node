{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.BlockState where

import Concordium.GlobalState.Account
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Modules

import Lens.Micro.Platform

data BlockState = BlockState {
    _blockAccounts :: Accounts,
    _blockInstances :: Instances,
    _blockModules :: Modules
}

makeLenses ''BlockState

emptyBlockState :: BlockState
emptyBlockState = BlockState {
  _blockAccounts = emptyAccounts
  , _blockInstances = emptyInstances
  , _blockModules = emptyModules
  }

