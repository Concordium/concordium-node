module Concordium.GlobalState.Persistent.TransactionTable where

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.PersistentTransactions
import qualified Data.HashMap.Strict as HM

-- | The PersistentTransactionTable only holds the NonFinalizedTransactions collection
-- as the real transaction table will be on the disk
type PersistentTransactionTable = HM.HashMap AccountAddress (AccountNonFinalizedTransactions PersistentTransaction)
