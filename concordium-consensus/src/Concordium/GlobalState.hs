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
