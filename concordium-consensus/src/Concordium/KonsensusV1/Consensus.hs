module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.Vector as Vector

import Concordium.Types
import Concordium.Types.Transactions

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.TransactionVerification as TVer
import Concordium.GlobalState.TransactionTable

class MonadMulticast m where
    sendMessage :: BS.ByteString -> m ()

-- uponTimeoutEvent :: ()
-- uponTimeoutEvent = ()

-- |Result of attempting to put a
-- 'BlockItem' into tree state.
data PutBlockItemResult
    -- |The transaction was accepted into
    -- the tree state.
    = Accepted
    -- |The transaction was rejected. See
    -- the 'TVer.VerificationResult' for why it
    -- was rejected.
    | Rejected !TVer.VerificationResult
    -- |The transaction was already present
    -- in the tree state.
    | Duplicate
    -- |The transaction nonce was obsolete,
    -- i.e. inferior to the next available nonce.
    | OldNonce

-- |Attempt to put the 'BlockItem' into the tree state.
-- If the the 'BlockItem' was successfully added then it will be
-- in 'Received' state where the associated 'CommitPoint' will be set to zero.
-- Return the resulting 'PutBlockItemResult'.
putBlockItem :: (MonadState (SkovData pv) m) => BlockItem -> m PutBlockItemResult
putBlockItem = undefined

-- |Attempt to put the 'BlockItem's of a 'BakedBlock' into the tree state.
-- Return 'True' of the transactions were added otherwise 'False'.
--
-- Pre-condition: The provided block items must be part of a 'BakedBlock'.
--
-- Post-condition: The transactions are only added to the tree state if they could
-- *all* be deemed verifiable i.e. the verification of each transaction either yields a
-- 'TVer.OkResult' or a 'TVer.MaybeOkResult'.
putBlockItems ::  (MonadState (SkovData pv) m) => CommitPoint -> Vector.Vector BlockItem -> m Bool
putBlockItems = undefined
