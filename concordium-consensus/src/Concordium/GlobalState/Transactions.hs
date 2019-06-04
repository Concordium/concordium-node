{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Concordium.GlobalState.Transactions where

import Control.Exception
import qualified Data.Serialize as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Crypto.SignatureScheme(SchemeId, Signature, KeyPair)
import qualified Concordium.Crypto.AccountSignatureSchemes as SigScheme
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.ID.Account as AH

import Concordium.Types
import Concordium.Types.HashableTo

newtype TransactionSignature = TransactionSignature { tsSignature :: Signature }
  deriving (Eq, Show)

-- |NB: Relies on the scheme and signature serialization to be sensibly defined as specified on the wiki!
instance S.Serialize TransactionSignature where
  put TransactionSignature{..} = S.put tsSignature
  get = TransactionSignature <$> S.get

-- | Data common to all transaction types.
--
--    * INVARIANT: First byte of 'thSender' matches the signature 'thScheme' field,
--    and @thSender = AH.accountAddress thSenderKey thScheme@.
--    * The last field is strictly redundant, but is here to avoid needless recomputation. In
--    serialization we do not output it, and when deserializing we compute it from other data.
data TransactionHeader = TransactionHeader {
    -- |Signature scheme used by the account.
    thScheme :: !SchemeId,
    -- |Verification key of the sender.
    thSenderKey :: !IDTypes.AccountVerificationKey,
    -- |Per account nonce, strictly increasing, no gaps.
    thNonce :: !Nonce,
    -- |Amount of gas dedicated for the execution of this transaction.
    thGasAmount :: !Energy,
    -- |Pointer to a finalized block. If this is too out of date at the time of
    -- execution the transaction is dropped.
    thFinalizedPointer :: !BlockHash,

    -- |Sender account. Derived from the sender key as specified.
    thSender :: AccountAddress
    } deriving (Eq, Show)

-- |NB: Relies on the verify key serialization being defined as specified on the wiki.
instance S.Serialize TransactionHeader where
  put TransactionHeader{..} =
      S.put thScheme <>
      S.put thSenderKey <>
      S.put thNonce <>
      S.put thGasAmount <>
      S.put thFinalizedPointer

  get = do
    thScheme <- S.get
    thSenderKey <- S.get
    thNonce <- S.get
    thGasAmount <- S.get
    thFinalizedPointer <- S.get
    return $ makeTransactionHeader thScheme thSenderKey thNonce thGasAmount thFinalizedPointer

type TransactionHash = H.Hash


-- |The last field is strictly redundant, but is here to avoid needless
-- recomputation. In serialization we do not output it.
data Transaction = Transaction {
  trSignature :: !TransactionSignature,
  trHeader :: !TransactionHeader,
  trPayload :: EncodedPayload,

  trHash :: TransactionHash  -- ^Hash of the transaction. Derived from the previous three fields.
  } deriving(Show) -- show is needed in testing

-- |NOTE: Eq and Ord instances based on hash comparison!
-- FIXME? Possibly we want to be defensive and check true equality in case hashes are equal.
instance Eq Transaction where
  t1 == t2 = trHash t1 == trHash t2

instance Ord Transaction where
  compare t1 t2 = compare (trHash t1) (trHash t2)

instance S.Serialize Transaction where
  put Transaction{..} =
    S.put trSignature <>
    S.put trHeader <>
    S.put trPayload

  get = do
    trSignature <- S.get
    trHeader <- S.get
    trPayload <- S.get
    return $ makeTransaction trSignature trHeader trPayload

makeTransactionHeader ::
  SchemeId
  -> IDTypes.AccountVerificationKey
  -> Nonce
  -> Energy
  -> BlockHash
  -> TransactionHeader
makeTransactionHeader thScheme thSenderKey thNonce thGasAmount thFinalizedPointer =
  TransactionHeader{thSender = AH.accountAddress thSenderKey thScheme,..}

-- |Make a transaction out of minimal data needed.
makeTransaction :: TransactionSignature -> TransactionHeader -> EncodedPayload -> Transaction
makeTransaction trSignature trHeader trPayload =
  let trHash = H.hash . S.runPut $ S.put trSignature <> S.put trHeader <> S.put trPayload
  in Transaction{..}

-- |NB: We do not use the serialize instance of the body (trPayload) here, since
-- that is already serialized and there is no need to add additional length
-- information.
-- FIXME: This method is inefficient (it creates temporary bytestrings which are
-- probably not necessary if we had a more appropriate sign function.

-- |Sign a transaction with the given header and body. Uses serialization as defined on the wiki.
signTransaction :: KeyPair -> TransactionHeader -> EncodedPayload -> Transaction
signTransaction keys trHeader trPayload =
  let body = S.runPut (S.put trHeader <> S.put trPayload)
      tsScheme = thScheme trHeader
      tsSignature = SigScheme.sign tsScheme keys body
      trHash = H.hash (S.encode trSignature <> body)
      trSignature = TransactionSignature{..}
  in Transaction{..}

-- |Verify that the given transaction was signed by the sender's key.
-- In contrast to 'verifyTransactionSignature' this method takes a structured transaction.
verifyTransactionSignature' :: TransactionData msg => IDTypes.AccountVerificationKey -> msg -> TransactionSignature -> Bool
verifyTransactionSignature' vfkey tx (TransactionSignature sig) =
  SigScheme.verify (thScheme (transactionHeader tx))
                   vfkey (S.runPut (S.put (transactionHeader tx) <> S.put (transactionPayload tx)))
                   sig


-- |The 'TransactionData' class abstracts away from the particular data
-- structure. It makes it possible to unify operations on 'Transaction' as well
-- as other types providing the same data (such as partially serialized
-- transactions).
class TransactionData t where
    transactionHeader :: t -> TransactionHeader
    transactionSender :: t -> AccountAddress
    transactionNonce :: t -> Nonce
    transactionGasAmount :: t -> Energy
    transactionPayload :: t -> EncodedPayload
    transactionSignature :: t -> TransactionSignature
    transactionHash :: t -> H.Hash


instance TransactionData Transaction where
    transactionHeader = trHeader
    transactionSender = thSender . trHeader
    transactionNonce = thNonce . trHeader
    transactionGasAmount = thGasAmount . trHeader
    transactionPayload = trPayload
    transactionSignature = trSignature
    transactionHash = getHash

instance HashableTo H.Hash Transaction where
    getHash = trHash

data AccountNonFinalizedTransactions = AccountNonFinalizedTransactions {
    -- |Non-finalized transactions (for an account) indexed by nonce.
    _anftMap :: Map.Map Nonce (Set.Set Transaction),
    -- |The next available nonce at the last finalized block.
    -- 'anftMap' should only contain nonces that are at least 'anftNextNonce'.
    _anftNextNonce :: Nonce
} deriving (Eq)
makeLenses ''AccountNonFinalizedTransactions

emptyANFT :: AccountNonFinalizedTransactions
emptyANFT = AccountNonFinalizedTransactions Map.empty minNonce

data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions.  Contains all transactions.
    _ttHashMap :: HM.HashMap TransactionHash (Transaction, Slot),
    _ttNonFinalizedTransactions :: HM.HashMap AccountAddress AccountNonFinalizedTransactions
}
makeLenses ''TransactionTable

emptyTransactionTable :: TransactionTable
emptyTransactionTable = TransactionTable {
        _ttHashMap = HM.empty,
        _ttNonFinalizedTransactions = HM.empty
    }

-- |A pending transaction table records whether transactions are pending after
-- execution of a particular block.  For each account address, if there are
-- pending transactions, then it should be in the map with value @(nextNonce, highNonce)@,
-- where @nextNonce@ is the next nonce for the account address (i.e. 1+nonce of last executed transaction),
-- and @highNonce@ is the highest nonce known for a transaction associated with that account.
-- @highNonce@ should always be at least @nextNonce@ (otherwise, what transaction is pending?).
-- If an account has no pending transactions, then it should not be in the map.
type PendingTransactionTable = HM.HashMap AccountAddress (Nonce, Nonce)

emptyPendingTransactionTable :: PendingTransactionTable
emptyPendingTransactionTable = HM.empty

-- |Insert an additional element in the pending transaction table.
-- If the account does not yet exist create it.
-- NB: This only updates the pending table, and does not ensure that invariants elsewhere are maintained.
extendPendingTransactionTable :: TransactionData t => Nonce -> t -> PendingTransactionTable -> PendingTransactionTable
extendPendingTransactionTable nextNonce tx pt = assert (nextNonce <= nonce) $
  HM.alter (\case Nothing -> Just (nextNonce, nonce)
                  Just (l, u) -> Just (l, max u nonce)) (transactionSender tx) pt
  where nonce = transactionNonce tx

forwardPTT :: [Transaction] -> PendingTransactionTable -> PendingTransactionTable
forwardPTT trs ptt0 = foldl forward1 ptt0 trs
    where
        forward1 :: PendingTransactionTable -> Transaction -> PendingTransactionTable
        forward1 ptt tr = ptt & at (transactionSender tr) %~ upd
            where
                upd Nothing = error "forwardPTT : forwarding transaction that is not pending"
                upd (Just (low, high)) =
                    assert (low == transactionNonce tr) $ assert (low <= high) $
                        if low == high then Nothing else Just (low+1,high)

reversePTT :: [Transaction] -> PendingTransactionTable -> PendingTransactionTable
reversePTT trs ptt0 = foldr reverse1 ptt0 trs
    where
        reverse1 :: Transaction -> PendingTransactionTable -> PendingTransactionTable
        reverse1 tr = at (transactionSender tr) %~ upd
            where
                upd Nothing = Just (transactionNonce tr, transactionNonce tr)
                upd (Just (low, high)) =
                        assert (low == transactionNonce tr + 1) $
                        Just (low-1,high)
