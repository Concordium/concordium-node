
module Concordium.GlobalState.BlockState where

import Concordium.GlobalState.Types
import Concordium.GlobalState.Account


data BlockState = BlockState {
    blockAccounts :: Accounts
}

