{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Concordium.GlobalState.TransactionTable where

import Data.Coerce
import Data.Word

import Concordium.KonsensusV1.Types
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Utils
import Control.Exception
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

-- |A commit point is a specific point within an 'Epoch'.
-- For ConsensusV0 it is a 'Slot'.
-- For ConsensusV1 it is a 'Round'.
type CommitPoint = Word64

class IsCommitPoint a where
    commitPoint :: a -> CommitPoint

    -- |Default implementation for commit points @o@ which is
    -- 'Coercible' with 'Word64'.
    default commitPoint :: Coercible a Word64 => a -> CommitPoint
    commitPoint = coerce

-- |'Slot' is using the default implementation
-- as 'Slot' is just a wrapper around 'Word64'.
instance IsCommitPoint Slot

-- |'Round' is using the default implementation
-- as 'Round' is just a wrapper around 'Word64'
instance IsCommitPoint Round

-- * Transaction status

-- |The status of a transaction that has been verified and is not yet finalized.
-- A 'Received' transaction is not yet in any live blocks (but could be in a pending block).
-- A 'Committed' transaction is known to be in a live block, and records the index that it occurs
-- in any such block.
data LiveTransactionStatus
    = -- |Transaction is received, but no outcomes from any blocks are known
      -- although the transaction might be known to be in some (pending) blocks. The 'CommitPoint'
      -- is the largest commit point of a block the transaction is in.
      -- A transaction verification result is attached to the transaction which is used by
      -- the 'Scheduler' to verify the transaction and possibly short-circuit some of the
      -- verification required before executing the transaction.
      Received
        { _tsCommitPoint :: !CommitPoint,
          _tsVerRes :: !TVer.VerificationResult
        }
    | -- |Transaction is committed in a number of blocks. '_tsCommitPoint' is the maximal 'CommitPoint'.
      -- 'tsResults' is always a non-empty map and global state must maintain the invariant
      -- that if a block hash @bh@ is in the 'tsResults' map then
      --
      -- * @bh@ is a live block
      -- * we have blockState for the block available
      -- * if @tsResults(bh) = i@ then the transaction is the relevant transaction is the i-th transaction in the block
      --   (where we start counting from 0)
      -- A transaction verification result is attached to the transaction which is used by
      -- the 'Scheduler' to verify the transaction and possibly short-circuit some of the verification required
      -- before executing the transaction.
      Committed
        { _tsCommitPoint :: !CommitPoint,
          _tsVerRes :: !TVer.VerificationResult,
          tsResults :: !(HM.HashMap BlockHash TransactionIndex)
        }
    deriving (Eq, Show)

makeLenses ''LiveTransactionStatus

-- |Add a transaction result. This function assumes the transaction is not finalized yet.
-- If the transaction is already finalized the function will return the original status.
{-# SPECIALIZE addResult :: BlockHash -> Round -> TransactionIndex -> LiveTransactionStatus -> LiveTransactionStatus #-}
{-# SPECIALIZE addResult :: BlockHash -> Slot -> TransactionIndex -> LiveTransactionStatus -> LiveTransactionStatus #-}
addResult :: IsCommitPoint a => BlockHash -> a -> TransactionIndex -> LiveTransactionStatus -> LiveTransactionStatus
addResult bh cp vr = \case
    Committed{_tsCommitPoint = currentCommitPoint, tsResults = currentResults, ..} ->
        Committed
            { _tsCommitPoint = max (commitPoint cp) currentCommitPoint,
              tsResults = HM.insert bh vr currentResults,
              _tsVerRes = _tsVerRes
            }
    Received{_tsCommitPoint = currentCommitPoint, ..} ->
        Committed
            { _tsCommitPoint = max (commitPoint cp) currentCommitPoint,
              tsResults = HM.singleton bh vr,
              _tsVerRes = _tsVerRes
            }

-- |Remove a transaction result for a given block. This can happen when a block
-- is removed from the block tree because it is not a successor of the last
-- finalized block.
-- This function will only have effect if the transaction status is 'Committed' and
-- the given block hash is in the table of outcomes.
markDeadResult :: BlockHash -> LiveTransactionStatus -> LiveTransactionStatus
markDeadResult bh Committed{..} =
    let newResults = HM.delete bh tsResults
    in  if HM.null newResults then Received{..} else Committed{tsResults = newResults, ..}
markDeadResult _ ts = ts

{-# SPECIALIZE updateCommitPoint :: Round -> LiveTransactionStatus -> LiveTransactionStatus #-}
{-# SPECIALIZE updateCommitPoint :: Slot -> LiveTransactionStatus -> LiveTransactionStatus #-}
updateCommitPoint :: IsCommitPoint a => a -> LiveTransactionStatus -> LiveTransactionStatus
updateCommitPoint s ts = ts{_tsCommitPoint = commitPoint s}

{-# INLINE getTransactionIndex #-}

-- |Get the outcome of the transaction in a particular block, and whether it is finalized.
getTransactionIndex :: BlockHash -> LiveTransactionStatus -> Maybe (Bool, TransactionIndex)
getTransactionIndex bh = \case
    Committed{..} -> (False,) <$> HM.lookup bh tsResults
    _ -> Nothing

-- * Transaction table

-- |The non-finalized transactions for a particular account.
data AccountNonFinalizedTransactions = AccountNonFinalizedTransactions
    { -- |Non-finalized transactions (for an account) and their verification results indexed by nonce.
      _anftMap :: !(Map.Map Nonce (Map.Map Transaction TVer.VerificationResult)),
      -- |The next available nonce at the last finalized block.
      -- 'anftMap' should only contain nonces that are at least 'anftNextNonce'.
      _anftNextNonce :: !Nonce
    }
    deriving (Eq, Show)

makeLenses ''AccountNonFinalizedTransactions

-- |Empty (no pending transactions) account non-finalized table starting at the
-- minimal nonce.
emptyANFT :: AccountNonFinalizedTransactions
emptyANFT = emptyANFTWithNonce minNonce

-- |An account non-finalized table with no pending transactions and given
-- starting nonce.
emptyANFTWithNonce :: Nonce -> AccountNonFinalizedTransactions
emptyANFTWithNonce = AccountNonFinalizedTransactions Map.empty

-- |The non-finalized chain updates of a particular type.
data NonFinalizedChainUpdates = NonFinalizedChainUpdates
    { _nfcuMap :: !(Map.Map UpdateSequenceNumber (Map.Map (WithMetadata UpdateInstruction) TVer.VerificationResult)),
      _nfcuNextSequenceNumber :: !UpdateSequenceNumber
    }
    deriving (Eq, Show)

makeLenses ''NonFinalizedChainUpdates

emptyNFCU :: NonFinalizedChainUpdates
emptyNFCU = emptyNFCUWithSequenceNumber minUpdateSequenceNumber

emptyNFCUWithSequenceNumber :: UpdateSequenceNumber -> NonFinalizedChainUpdates
emptyNFCUWithSequenceNumber = NonFinalizedChainUpdates Map.empty

-- * Account address equivalence

-- $equivalence
-- The non-finalized transactions in the transaction table and the pending table
-- maintain indices by account address equivalence classes. The reason for this
-- is that really the mappings should be per account. Since multiple account
-- addresses refer to the same account we need to identify them.
-- AccountAddressEq is a best-effort attempt at that. If two addresses refer to
-- the same account then they must agree on the first 29 bytes, and the AccountAddressEq
-- identifies any addresses that match on the 29 byte prefix.
--
-- There is a caveat, in protocol versions 1 and 2 addresses are in 1-1
-- correspondence with accounts. This means that technically AccountAddressEq
-- could identify too many addresses, leading to inability of some accounts to
-- send transactions in certain circumstances.
-- This would happen in particular because when receiving transactions we
-- compare the transaction nonce against the last finalized nonce so we might
-- reject a transaction directly when receiving it due to accidental address
-- identification. Similarly we might reject a valid block since we deem a nonce
-- to be duplicate due to accidental address identification, and we reject a
-- block with transactions with obsolete nonces outright. Once transactions are
-- in the transaction table there are no soundness issues anymore since the
-- scheduler resolves addresses to accounts and compares nonces of those
-- accounts, however it might still happen that when baking a transaction would
-- not be selected since we might skip it if we think it has a duplicate nonce due
-- to accidental address identification.
-- This is an extremely unlikely scenario and will only occur in case of a
-- SHA256 collision on the first 29 bytes (but not all remaining 3 bytes).

-- |The transaction table stores transactions and their statuses.
-- Finalized transactions are not stored in this table, but can be looked up from a disk-backed
-- database.
--
-- A transaction's status indicates which blocks it is included in and the commit point
-- of the highest such block. A transaction that is not included in any blocks
-- may also have a non-zero highest commit point if it is received in a block, but that block
-- is not yet considered arrived (e.g. it is pending its parent).
--
-- Generally, '_ttNonFinalizedTransactions' should have an entry for every account,
-- with the exception of where the entry would be 'emptyANFT'. Similarly with
-- '_ttNonFinalizedChainUpdates' and 'emptyNFCU'.  In particular, there should be
-- an entry if the next nonce/sequence number is not the minimum value.
data TransactionTable = TransactionTable
    { -- |Map from transaction hashes to transactions, together with their current status.
      _ttHashMap :: !(HM.HashMap TransactionHash (BlockItem, LiveTransactionStatus)),
      -- |For each account, the non-finalized transactions for that account,
      -- grouped by nonce. See $equivalence for reasons why AccountAddressEq is used.
      _ttNonFinalizedTransactions :: !(HM.HashMap AccountAddressEq AccountNonFinalizedTransactions),
      -- |For each update types, the non-finalized update instructions, grouped by
      -- sequence number.
      _ttNonFinalizedChainUpdates :: !(Map.Map UpdateType NonFinalizedChainUpdates)
    }
    deriving (Eq, Show)

makeLenses ''TransactionTable

-- |Get the number of non-finalized transactions stored in the transaction table.
getNumberOfNonFinalizedTransactions :: TransactionTable -> Int
getNumberOfNonFinalizedTransactions table = HM.size (table ^. ttHashMap)

-- |Get the verification result for a non finalized transaction given by its hash.
getNonFinalizedVerificationResult :: WithMetadata a -> TransactionTable -> Maybe TVer.VerificationResult
getNonFinalizedVerificationResult bi table =
    case snd <$> table ^. ttHashMap . at' (wmdHash bi) of
        Just status ->
            case status of
                Received _ verRes -> Just verRes
                Committed _ verRes _ -> Just verRes
        Nothing -> Nothing

emptyTransactionTable :: TransactionTable
emptyTransactionTable =
    TransactionTable
        { _ttHashMap = HM.empty,
          _ttNonFinalizedTransactions = HM.empty,
          _ttNonFinalizedChainUpdates = Map.empty
        }

-- |A transaction table with no transactions, but with the initial next sequence numbers
-- set for the accounts and update types.
emptyTransactionTableWithSequenceNumbers :: [(AccountAddress, Nonce)] -> Map.Map UpdateType UpdateSequenceNumber -> TransactionTable
emptyTransactionTableWithSequenceNumbers accs upds =
    TransactionTable
        { _ttHashMap = HM.empty,
          _ttNonFinalizedTransactions = HM.fromList . map (\(k, n) -> (accountAddressEmbed k, emptyANFTWithNonce n)) . filter (\(_, n) -> n /= minNonce) $ accs,
          _ttNonFinalizedChainUpdates = emptyNFCUWithSequenceNumber <$> Map.filter (/= minUpdateSequenceNumber) upds
        }

-- |Add a transaction to a transaction table if its nonce/sequence number is at least the next
-- non-finalized nonce/sequence number.  A return value of 'True' indicates that the transaction
-- was added.  The caller should check that the transaction is not already present.
addTransaction :: BlockItem -> CommitPoint -> TVer.VerificationResult -> TransactionTable -> (Bool, TransactionTable)
addTransaction blockItem@WithMetadata{..} cp !verRes tt0 =
    case wmdData of
        NormalTransaction tr
            | tt0 ^. senderANFT . anftNextNonce <= nonce ->
                (True, tt1 & senderANFT . anftMap . at' nonce . non Map.empty . at' wmdtr ?~ verRes)
          where
            sender = accountAddressEmbed (transactionSender tr)
            senderANFT :: Lens' TransactionTable AccountNonFinalizedTransactions
            senderANFT = ttNonFinalizedTransactions . at' sender . non emptyANFT
            nonce = transactionNonce tr
            wmdtr = WithMetadata{wmdData = tr, ..}
        CredentialDeployment{} -> (True, tt1)
        ChainUpdate cu
            | tt0 ^. utNFCU . nfcuNextSequenceNumber <= sn ->
                (True, tt1 & utNFCU . nfcuMap . at' sn . non Map.empty . at' wmdcu ?~ verRes)
          where
            uty = updateType (uiPayload cu)
            sn = updateSeqNumber (uiHeader cu)
            utNFCU :: Lens' TransactionTable NonFinalizedChainUpdates
            utNFCU = ttNonFinalizedChainUpdates . at' uty . non emptyNFCU
            wmdcu = WithMetadata{wmdData = cu, ..}
        _ -> (False, tt0)
  where
    tt1 = tt0 & ttHashMap . at' wmdHash ?~ (blockItem, Received cp verRes)

-- * Pending transaction table

-- |A pending transaction table records whether transactions are pending after
-- execution of a particular block.  For each account address, if there are
-- pending transactions, then it should be in the map with value @(nextNonce, highNonce)@,
-- where @nextNonce@ is the next nonce for the account address (i.e. 1+nonce of last executed transaction),
-- and @highNonce@ is the highest nonce known for a transaction associated with that account.
-- @highNonce@ should always be at least @nextNonce@ (otherwise, what transaction is pending?).
-- If an account has no pending transactions, then it should not be in the map.
data PendingTransactionTable = PTT
    { -- |Pending transactions from accounts. See $equivalence for the reason why
      -- the hashmap uses AccountAddressEq.
      _pttWithSender :: !(HM.HashMap AccountAddressEq (Nonce, Nonce)),
      -- |Pending credentials. We only store the hash because updating the
      -- pending table would otherwise be more costly with the current setup.
      _pttDeployCredential :: !(HS.HashSet TransactionHash),
      -- |Pending update instructions. We record the next and high sequence numbers.
      _pttUpdates :: !(Map.Map UpdateType (UpdateSequenceNumber, UpdateSequenceNumber))
    }
    deriving (Eq, Show)

makeLenses ''PendingTransactionTable

emptyPendingTransactionTable :: PendingTransactionTable
emptyPendingTransactionTable = PTT HM.empty HS.empty Map.empty

-- |Insert an additional element in the pending transaction table.
-- If the account does not yet exist create it.
-- NB: This only updates the pending table, and does not ensure that invariants elsewhere are maintained.
-- PRECONDITION: the next nonce should be less than or equal to the transaction nonce.
addPendingTransaction :: TransactionData t => Nonce -> t -> PendingTransactionTable -> PendingTransactionTable
addPendingTransaction nextNonce tx PTT{..} = assert (nextNonce <= nonce) $ let v = HM.alter f sender _pttWithSender in PTT{_pttWithSender = v, ..}
  where
    f Nothing = Just (nextNonce, nonce)
    f (Just (l, u)) = Just (l, max u nonce)
    nonce = transactionNonce tx
    sender = accountAddressEmbed (transactionSender tx)

-- |Insert an additional element in the pending transaction table.
-- Does nothing if the next nonce is greater than the transaction nonce.
-- If the account does not yet exist create it.
-- NB: This only updates the pending table, and does not ensure that invariants elsewhere are maintained.
checkedAddPendingTransaction :: TransactionData t => Nonce -> t -> PendingTransactionTable -> PendingTransactionTable
checkedAddPendingTransaction nextNonce tx pt =
    if nextNonce > nonce
        then pt
        else
            pt
                & pttWithSender . at' sender %~ \case
                    Nothing -> Just (nextNonce, nonce)
                    Just (l, u) -> Just (l, max u nonce)
  where
    nonce = transactionNonce tx
    sender = accountAddressEmbed (transactionSender tx)

-- |Extend the pending transaction table with a credential hash.
addPendingDeployCredential :: TransactionHash -> PendingTransactionTable -> PendingTransactionTable
addPendingDeployCredential hash pt =
    pt & pttDeployCredential %~ HS.insert hash

-- |Add an update instruction to the pending transaction table, without
-- checking that its sequence number is high enough.
-- NB: This only updates the pending table.
addPendingUpdate ::
    -- |Next sequence number at the last finalized block
    UpdateSequenceNumber ->
    UpdateInstruction ->
    PendingTransactionTable ->
    PendingTransactionTable
addPendingUpdate nextSN ui ptt = assert (nextSN <= sn) $ ptt & pttUpdates . at' ut %~ f
  where
    f Nothing = Just (nextSN, sn)
    f (Just (l, u)) = Just (l, max u sn)
    sn = updateSeqNumber (uiHeader ui)
    ut = updateType (uiPayload ui)

-- |Add an update instruction to the pending transaction table, checking
-- that its sequence number is high enough.  (Does nothing if it is not.)
-- NB: This only updates the pending table.
checkedAddPendingUpdate :: UpdateSequenceNumber -> UpdateInstruction -> PendingTransactionTable -> PendingTransactionTable
checkedAddPendingUpdate nextSN ui ptt
    | nextSN > updateSeqNumber (uiHeader ui) = ptt
    | otherwise = addPendingUpdate nextSN ui ptt

-- |Update the pending transaction table by considering the supplied 'BlockItem's
-- as no longer pending. The 'BlockItem's must be ordered correctly with respect
-- to sequence numbers, as they would appear in a block.
forwardPTT :: [BlockItem] -> PendingTransactionTable -> PendingTransactionTable
forwardPTT trs ptt0 = foldl' forward1 ptt0 trs
  where
    forward1 :: PendingTransactionTable -> BlockItem -> PendingTransactionTable
    forward1 ptt WithMetadata{wmdData = NormalTransaction tr} = ptt & pttWithSender . at' sender %~ upd
      where
        sender = accountAddressEmbed (transactionSender tr)
        upd Nothing = error "forwardPTT : forwarding transaction that is not pending"
        upd (Just (low, high)) =
            assert (low == transactionNonce tr) $
                assert (low <= high) $
                    if low == high then Nothing else Just (low + 1, high)
    forward1 ptt WithMetadata{wmdData = CredentialDeployment{}, ..} = ptt & pttDeployCredential %~ upd
      where
        upd ps
            | wmdHash `HS.member` ps = HS.delete wmdHash ps
            | otherwise = error "forwardPTT: forwarding a block item that is not pending."
    forward1 ptt WithMetadata{wmdData = ChainUpdate{..}} = ptt & pttUpdates . at' (updateType (uiPayload biUpdate)) %~ upd
      where
        upd Nothing = error "forwardPTT : forwarding a block item that is not pending"
        upd (Just (low, high)) =
            assert (low == updateSeqNumber (uiHeader biUpdate)) $
                assert (low <= high) $
                    if low == high then Nothing else Just (low + 1, high)

-- |Update the pending transaction table by considering the supplied 'BlockItem's
-- pending again. The 'BlockItem's must be ordered correctly with respect
-- to sequence numbers, as they would appear in a block.
reversePTT :: [BlockItem] -> PendingTransactionTable -> PendingTransactionTable
reversePTT trs ptt0 = foldr reverse1 ptt0 trs
  where
    reverse1 :: BlockItem -> PendingTransactionTable -> PendingTransactionTable
    reverse1 WithMetadata{wmdData = NormalTransaction tr} = pttWithSender . at' sender %~ upd
      where
        sender = accountAddressEmbed (transactionSender tr)
        upd Nothing = Just (transactionNonce tr, transactionNonce tr)
        upd (Just (low, high)) =
            assert (low == transactionNonce tr + 1) $
                Just (low - 1, high)
    reverse1 WithMetadata{wmdData = CredentialDeployment{}, ..} = pttDeployCredential %~ upd
      where
        upd ps = assert (not (HS.member wmdHash ps)) $ HS.insert wmdHash ps
    reverse1 WithMetadata{wmdData = ChainUpdate{..}} = pttUpdates . at' (updateType (uiPayload biUpdate)) %~ upd
      where
        sn = updateSeqNumber (uiHeader biUpdate)
        upd Nothing = Just (sn, sn)
        upd (Just (low, high)) =
            assert (low == sn + 1) $
                Just (low - 1, high)

-- |Returns the next available account nonce for the
-- provided account address in the first component and the
-- 'Bool' in the second component is 'True' only if all transactions from the
-- provided account are finalized.
nextAccountNonce ::
    -- |The account to look up the next account nonce for.
    AccountAddressEq ->
    -- |The transaction table to look up in.
    TransactionTable ->
    -- |("the next available account nonce", "whether all transactions from the account are finalized").
    (Nonce, Bool)
nextAccountNonce addr tt = case tt ^. ttNonFinalizedTransactions . at' addr of
    Nothing -> (minNonce, True)
    Just anfts ->
        case Map.lookupMax (anfts ^. anftMap) of
            Nothing -> (anfts ^. anftNextNonce, True)
            Just (nonce, _) -> (nonce + 1, False)
