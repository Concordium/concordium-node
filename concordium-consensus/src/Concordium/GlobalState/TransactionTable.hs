{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.TransactionTable where

import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Transactions

-- |The transaction table stores transactions and their statuses.
-- In the persistent tree state implementation, finalized transactions are not
-- stored in this table, but can be looked up from a disk-backed database.
-- In the in-memory implementation, finalized transactions are stored in this
-- table.
--
-- A transaction's status indicates which blocks it is included in and the slot
-- number of the highest such block.  A transaction that is not included any block
-- may also have a non-zero highest slot if it is received in a block, but that block
-- is not yet considered arrived.
data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions, together with their current status.
    _ttHashMap :: !(HM.HashMap TransactionHash (BlockItem, TransactionStatus)),
    -- |For each account, the non-finalized transactions for that account, grouped by
    -- nonce.
    _ttNonFinalizedTransactions :: !(HM.HashMap AccountAddress AccountNonFinalizedTransactions)
}
makeLenses ''TransactionTable

emptyTransactionTable :: TransactionTable
emptyTransactionTable = TransactionTable {
        _ttHashMap = HM.empty,
        _ttNonFinalizedTransactions = HM.empty
    }
