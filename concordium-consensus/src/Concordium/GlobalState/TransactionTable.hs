{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.TransactionTable where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Lens.Micro.Platform
import qualified Data.Serialize as S
import Control.Monad
import Control.Exception
import Data.Foldable

import Concordium.Utils
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.Types.Updates

-- * Transaction status

-- |Result of a transaction is block dependent.
data TransactionStatus =
  -- |Transaction is received, but no outcomes from any blocks are known
  -- although the transaction might be known to be in some blocks. The Slot is the
  -- largest slot of a block the transaction is in.
  Received { _tsSlot :: !Slot }
  -- |Transaction is committed in a number of blocks. '_tsSlot' is the maximal slot.
  -- 'tsResults' is always a non-empty map and global state must maintain the invariant
  -- that if a block hash @bh@ is in the 'tsResults' map then
  --
  -- * @bh@ is a live block
  -- * we have blockState for the block available
  -- * if @tsResults(bh) = i@ then the transaction is the relevant transaction is the i-th transaction in the block
  --   (where we start counting from 0)
  | Committed {_tsSlot :: !Slot,
               tsResults :: !(HM.HashMap BlockHash TransactionIndex)
              }
  -- |Transaction is finalized in a given block with a specific outcome.
  -- NB: With the current implementation a transaction can appear in at most one finalized block.
  -- When that part is reworked so that branches are not pruned we will likely rework this.
  | Finalized {
      _tsSlot :: !Slot,
      tsBlockHash :: !BlockHash,
      tsFinResult :: !TransactionIndex
      }
  deriving(Eq, Show)
makeLenses ''TransactionStatus

instance S.Serialize TransactionStatus where
  put Received{..} = do
    S.putWord8 0
    S.put _tsSlot
  put Committed{..} = do
    S.putWord8 1
    S.put _tsSlot
    S.putWord32be $ fromIntegral (HM.size tsResults)
    forM_ (HM.toList tsResults) $ \(h, i) -> S.put h <> S.put i
  put Finalized{..} = do
    S.putWord8 2
    S.put _tsSlot
    S.put tsBlockHash
    S.put tsFinResult

  get = do
    tag <- S.getWord8
    case tag of
      0 -> do
        _tsSlot <- S.get
        return Received{..}
      1 -> do
        _tsSlot <- S.get
        len <- S.getWord32be
        tsResults <- HM.fromList <$> replicateM (fromIntegral len) (do
                                           k <- S.get
                                           v <- S.get
                                           return (k, v))
        return $ Committed{..}
      2 -> do
        _tsSlot <- S.get
        tsBlockHash <- S.get
        tsFinResult <- S.get
        return $ Finalized{..}
      _ -> fail $ "Unknown transaction status variant: " ++ show tag

-- |Add a transaction result. This function assumes the transaction is not finalized yet.
-- If the transaction is already finalized the function will return the original status.
addResult :: BlockHash -> Slot -> TransactionIndex -> TransactionStatus -> TransactionStatus
addResult bh slot vr = \case
  Committed{_tsSlot=currentSlot, tsResults=currentResults} -> Committed{_tsSlot = max slot currentSlot, tsResults = HM.insert bh vr currentResults}
  Received{_tsSlot=currentSlot} -> Committed{_tsSlot = max slot currentSlot, tsResults = HM.singleton bh vr}
  s@Finalized{} -> s

-- |Remove a transaction result for a given block. This can happen when a block
-- is removed from the block tree because it is not a successor of the last
-- finalized block.
-- This function will only have effect if the transaction status is 'Committed' and
-- the given block hash is in the table of outcomes.
markDeadResult :: BlockHash -> TransactionStatus -> TransactionStatus
markDeadResult bh Committed{..} =
  let newResults = HM.delete bh tsResults
  in if HM.null newResults then Received{..} else Committed{tsResults=newResults,..}
markDeadResult _ ts = ts

updateSlot :: Slot -> TransactionStatus -> TransactionStatus
updateSlot _ ts@Finalized{} = ts
updateSlot s ts = ts { _tsSlot = s}

initialStatus :: Slot -> TransactionStatus
initialStatus = Received

{-# INLINE getTransactionIndex #-}
-- |Get the outcome of the transaction in a particular block, and whether it is finalized.
getTransactionIndex :: BlockHash -> TransactionStatus -> Maybe (Bool, TransactionIndex)
getTransactionIndex bh = \case
  Committed{..} -> (False, ) <$> HM.lookup bh tsResults
  Finalized{..} -> if bh == tsBlockHash then Just (True, tsFinResult) else Nothing
  _ -> Nothing


-- * Transaction table

-- |The non-finalized transactions for a particular account.
data AccountNonFinalizedTransactions = AccountNonFinalizedTransactions {
    -- |Non-finalized transactions (for an account) indexed by nonce.
    _anftMap :: Map.Map Nonce (Set.Set Transaction),
    -- |The next available nonce at the last finalized block.
    -- 'anftMap' should only contain nonces that are at least 'anftNextNonce'.
    _anftNextNonce :: Nonce
} deriving (Eq)
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
data NonFinalizedChainUpdates = NonFinalizedChainUpdates {
    _nfcuMap :: Map.Map UpdateSequenceNumber (Set.Set (WithMetadata UpdateInstruction)),
    _nfcuNextSequenceNumber :: UpdateSequenceNumber
} deriving (Eq)
makeLenses ''NonFinalizedChainUpdates

emptyNFCU :: NonFinalizedChainUpdates
emptyNFCU = emptyNFCUWithSequenceNumber minUpdateSequenceNumber

emptyNFCUWithSequenceNumber :: UpdateSequenceNumber -> NonFinalizedChainUpdates
emptyNFCUWithSequenceNumber = NonFinalizedChainUpdates Map.empty

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
    _ttNonFinalizedTransactions :: !(HM.HashMap AccountAddress AccountNonFinalizedTransactions),
    -- |For each update types, the non-finalized update instructions, grouped by
    -- sequence number.
    _ttNonFinalizedChainUpdates :: !(Map.Map UpdateType NonFinalizedChainUpdates)
}
makeLenses ''TransactionTable

emptyTransactionTable :: TransactionTable
emptyTransactionTable = TransactionTable {
        _ttHashMap = HM.empty,
        _ttNonFinalizedTransactions = HM.empty,
        _ttNonFinalizedChainUpdates = Map.empty
    }

-- * Pending transaction table

-- |A pending transaction table records whether transactions are pending after
-- execution of a particular block.  For each account address, if there are
-- pending transactions, then it should be in the map with value @(nextNonce, highNonce)@,
-- where @nextNonce@ is the next nonce for the account address (i.e. 1+nonce of last executed transaction),
-- and @highNonce@ is the highest nonce known for a transaction associated with that account.
-- @highNonce@ should always be at least @nextNonce@ (otherwise, what transaction is pending?).
-- If an account has no pending transactions, then it should not be in the map.
data PendingTransactionTable = PTT {
  _pttWithSender :: !(HM.HashMap AccountAddress (Nonce, Seq.Seq Nonce)),
  -- |Pending credentials. We only store the hash because updating the
  -- pending table would otherwise be more costly with the current setup.
  _pttDeployCredential :: !(HS.HashSet TransactionHash),
  -- |Pending update instructions. We record the next and high sequence numbers.
  _pttUpdates :: !(Map.Map UpdateType (UpdateSequenceNumber, UpdateSequenceNumber))
  } deriving(Eq, Show)

makeLenses ''PendingTransactionTable

emptyPendingTransactionTable :: PendingTransactionTable
emptyPendingTransactionTable = PTT HM.empty HS.empty Map.empty

numPendingCredentials :: PendingTransactionTable -> Int
numPendingCredentials = HS.size . _pttDeployCredential

-- |Find the next free nonce in the sequence of nonces starting at the given nonce.
-- Concretely, @nextFreeNonce 1 [3,4,5] == 1@, @nextFreeNonce 1 [1, 3, 5] == 2@
--
-- The complexity of this function is log^2 in the length of the list (the
-- additional log comes from the fact that indexing in the Seq is log(n)).
nextFreeNonce ::
  Nonce -- ^The starting nonce.
  -> Seq.Seq Nonce -- ^ The sequence of used nonces.
  -> Nonce
nextFreeNonce def nnces = go def 0 (Seq.length nnces)
    where go ret start end
              | start >= end = ret
              | otherwise =
                let midPoint = start + (end - start) `div` 2
                    midElement = nnces `Seq.index` fromIntegral midPoint
                in if midElement > def + fromIntegral midPoint then go ret start midPoint
                   else go (midElement + 1) (midPoint + 1) end

-- |Insert the given nonce in the sequence, respecting the order.
-- If the nonce already exists in the sequence do nothing and return 'Nothing'.
--
-- The complexity of this function is log in the length of the sequence
insertInOrder :: Nonce -> Seq.Seq Nonce -> Maybe (Seq.Seq Nonce)
insertInOrder new oldSeq = do
  nexIdx <- go 0 (Seq.length oldSeq)
  return $! Seq.insertAt nexIdx new oldSeq
    where go start end -- return the index where the new element should be inserted
              | start >= end = Just start
              | otherwise =
                let midPoint = start + (end - start) `div` 2
                    midElement = oldSeq `Seq.index` midPoint
                in if midElement == new then
                     Nothing -- the element already exists
                   else if midElement > new then go start midPoint
                   else go (midPoint + 1) end

data NonceInfo = NonceInfo { nextInLine :: Bool, isDuplicate :: Bool }

-- |Insert an additional element in the pending transaction table.
-- If the account does not yet exist create it.
-- Returns
--   - the resulting pending transaction table,
--   - whether the transaction nonce is the next in line for the table,
--   - whether the table already contains an transaction that has the input transaction's nonce.
-- NB: This only updates the pending table, and does not ensure that invariants elsewhere are maintained.
-- PRECONDITION: the next nonce should be less than or equal to the transaction nonce.
addPendingTransaction :: TransactionData t => Nonce -> t -> PendingTransactionTable -> (PendingTransactionTable, NonceInfo)
addPendingTransaction nextNonce tx PTT{..} = assert (nextNonce <= nonce) (PTT{_pttWithSender = v, ..}, nonceInfo)
  where
        f :: Maybe (Nonce, Seq.Seq Nonce) -> (NonceInfo, Maybe (Nonce, Seq.Seq Nonce))
        f Nothing = (NonceInfo { nextInLine = True, isDuplicate = False }, Just (nextNonce, Seq.singleton nonce))
        f (Just (l, known)) =
          let x = nextFreeNonce l known
          in case insertInOrder nonce known of
               Nothing -> (NonceInfo { nextInLine = False, isDuplicate = True }, Just (l, known))
               Just newKnown -> (NonceInfo { nextInLine = x == nonce, isDuplicate = False }, Just (l, newKnown))
        nonce = transactionNonce tx
        sender = transactionSender tx
        (nonceInfo, v) = HM.alterF f sender _pttWithSender

-- |Insert an additional element in the pending transaction table.
-- Does nothing if the next nonce is greater than the transaction nonce.
-- If the account does not yet exist create it.
-- NB: This only updates the pending table, and does not ensure that invariants elsewhere are maintained.
checkedAddPendingTransaction :: TransactionData t => Nonce -> t -> PendingTransactionTable -> PendingTransactionTable
checkedAddPendingTransaction nextNonce tx pt =
  if nextNonce > nonce then pt else
    fst (addPendingTransaction nextNonce tx pt)
  where nonce = transactionNonce tx

-- |Extend the pending transaction table with a credential hash.
addPendingDeployCredential :: TransactionHash -> PendingTransactionTable -> PendingTransactionTable
addPendingDeployCredential hash pt =
  pt & pttDeployCredential %~ HS.insert hash

-- |Add an update instruction to the pending transaction table, without
-- checking that its sequence number is high enough.
-- NB: This only updates the pending table.
addPendingUpdate ::
  UpdateSequenceNumber
  -- ^Next sequence number at the last finalized block
  -> UpdateInstruction
  -> PendingTransactionTable
  -> PendingTransactionTable
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
        forward1 ptt WithMetadata{wmdData=NormalTransaction tr} = ptt & pttWithSender . at' (transactionSender tr) %~ upd
            where
                upd Nothing = error "forwardPTT : forwarding transaction that is not pending"
                upd (Just (low, known)) =
                    assert (low == transactionNonce tr) $
                      case known of
                        Seq.Empty -> Nothing
                        (next Seq.:<| Seq.Empty) -> if low == next then Nothing else Just (low+1, known)
                        (next Seq.:<| rest) -> if low == next then Just (low+1, rest) else Just (low+1, known)
        forward1 ptt WithMetadata{wmdData=CredentialDeployment{},..} = ptt & pttDeployCredential %~ upd
            where
              upd ps
                | wmdHash `HS.member` ps = HS.delete wmdHash ps
                | otherwise = error "forwardPTT: forwarding a block item that is not pending."
        forward1 ptt WithMetadata{wmdData=ChainUpdate{..}} = ptt & pttUpdates . at' (updateType (uiPayload biUpdate)) %~ upd
            where
                upd Nothing = error "forwardPTT : forwarding a block item that is not pending"
                upd (Just (low, high)) =
                    assert (low == updateSeqNumber (uiHeader biUpdate)) $ assert (low <= high) $
                      if low == high then Nothing else Just (low+1, high)

-- |Update the pending transaction table by considering the supplied 'BlockItem's
-- pending again. The 'BlockItem's must be ordered correctly with respect
-- to sequence numbers, as they would appear in a block.
reversePTT :: [BlockItem] -> PendingTransactionTable -> PendingTransactionTable
reversePTT trs ptt0 = foldr reverse1 ptt0 trs
    where
        reverse1 :: BlockItem -> PendingTransactionTable -> PendingTransactionTable
        reverse1 WithMetadata{wmdData=NormalTransaction tr} = pttWithSender . at' (transactionSender tr) %~ upd
            where
                upd Nothing = Just (transactionNonce tr, Seq.singleton (transactionNonce tr))
                upd (Just (low, known)) =
                        assert (low == transactionNonce tr + 1) $
                        let newKnown =
                              case known of
                                Seq.Empty -> Seq.singleton (transactionNonce tr)
                                (h Seq.:<| _) | h > transactionNonce tr -> transactionNonce tr Seq.<| known
                                              | otherwise -> known
                        in Just (low-1, newKnown)
        reverse1 WithMetadata{wmdData=CredentialDeployment{},..} = pttDeployCredential %~ upd
            where
              upd ps = assert (not (HS.member wmdHash ps)) $ HS.insert wmdHash ps
        reverse1 WithMetadata{wmdData=ChainUpdate{..}} = pttUpdates . at' (updateType (uiPayload biUpdate)) %~ upd
            where
                sn = updateSeqNumber (uiHeader biUpdate)
                upd Nothing = Just (sn, sn)
                upd (Just (low, high)) =
                        assert (low == sn + 1) $
                        Just (low - 1, high)
