module Concordium.Payload.Monad where
-- TODO : Probably remove this module
{-
import qualified Data.Map.Strict as Map

import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Payload.Transaction

class SkovMonad m => PayloadMonad m where
    -- |Get the transactions for a block.  Can return 'Nothing' if any of the following hold:
    -- 1. the block is not in the tree;
    -- 2. more than one transaction with the same nonce occurs in the chain;
    -- 3. the transactions of any block in the chain cannot be deserialized.
    getTransactionsAtBlock :: BlockPointer -> m (Maybe (Map.Map TransactionNonce Transaction))
    getPendingTransactionsAtBlock :: BlockPointer -> m (Maybe (Map.Map TransactionNonce Transaction))
    addPendingTransaction :: Transaction -> m ()
-}