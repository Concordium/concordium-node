{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Basic.TransactionTable where

import Lens.Micro.Platform
import qualified Data.HashMap.Strict as HM
import Concordium.Types.Transactions
import Concordium.Types

data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions, together with their current status.
    _ttHashMap :: HM.HashMap TransactionHash (Transaction, TransactionStatus),
    _ttNonFinalizedTransactions :: HM.HashMap AccountAddress (AccountNonFinalizedTransactions Transaction)
}
makeLenses ''TransactionTable

emptyTransactionTable :: TransactionTable
emptyTransactionTable = TransactionTable {
        _ttHashMap = HM.empty,
        _ttNonFinalizedTransactions = HM.empty
    }
