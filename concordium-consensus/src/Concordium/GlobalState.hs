module Concordium.GlobalState where

import Concordium.GlobalState.Types
import Concordium.GlobalState.Account
import Concordium.GlobalState.Transactions



data BlockState = BlockState {
    blockAccounts :: Accounts
}


data TreeState = TreeState {
    tsTransactions :: TransactionTable
    -- TODO: move Skov data here
}

data BestBlockState

