{-# LANGUAGE TemplateHaskell, BangPatterns #-}
module Concordium.GlobalState.TransactionTable where

import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Transactions

data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions, together with their current status.
    _ttHashMap :: !(HM.HashMap TransactionHash (BlockItem, TransactionStatus)),
    _ttNonFinalizedTransactions :: !(HM.HashMap AccountAddress AccountNonFinalizedTransactions)
}
makeLenses ''TransactionTable

emptyTransactionTable :: TransactionTable
emptyTransactionTable = TransactionTable {
        _ttHashMap = HM.empty,
        _ttNonFinalizedTransactions = HM.empty
    }
