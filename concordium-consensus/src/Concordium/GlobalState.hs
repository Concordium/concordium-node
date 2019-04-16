{-# LANGUAGE RecordWildCards #-}
module Concordium.GlobalState where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.Types.Execution
import Concordium.Types
import Concordium.GlobalState.Account
import Concordium.GlobalState.Transactions


data TreeState = TreeState {
    tsTransactions :: TransactionTable
    -- TODO: move Skov data here
}
