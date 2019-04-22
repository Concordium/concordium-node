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
import qualified Data.ByteString as BS
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Crypto.SignatureScheme(SchemeId, Signature, KeyPair)
import qualified Concordium.Crypto.AccountSignatureSchemes as SigScheme
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.ID.AccountHolder as AH

import Concordium.Types
import Concordium.Types.HashableTo

data TransactionSignature = TransactionSignature {
  tsScheme :: !SchemeId
  ,tsSignature :: !Signature
  }
  deriving (Eq, Show)

-- |NB: Relies on the scheme and signature serialization to be sensibly defined as specified on the wiki!
instance S.Serialize TransactionSignature where
  put TransactionSignature{..} = S.put tsScheme <> S.put tsSignature
  get = TransactionSignature <$> S.get <*> S.get

-- INVARIANT: First byte of 'thSender' matches the signature 'thScheme' field, and
-- @thSender = AH.accountAddress' thSenderKey thScheme@.
data TransactionHeader = TransactionHeader {
    thSenderKey :: !IDTypes.AccountVerificationKey,  -- |Verification key of the sender.
    thNonce :: !Nonce,  -- |Per account nonce, strictly increasing, no gaps.
    thGasAmount :: !Amount,  -- |Amount dedicated for the execution of this transaction.
    thFinalizedPointer :: !BlockHash,   -- |Pointer to a finalized block. If this
                                       -- is too out of date at the time of
                                       -- execution the transaction is dropped.

    -- * The following fields are strictly redundant, but are here to avoid needless recomputation.
    -- In serialization we do not output them.
    thSender :: AccountAddress  -- | Sender account. Derived from the sender key as specified.
    } deriving (Eq, Show)

-- |NB: Relies on the verify key serialization being defined as specified on the wiki.
putTransactionHeader :: TransactionHeader -> S.Put
putTransactionHeader TransactionHeader{..} =
    S.put thSenderKey <>
    S.put thNonce <>
    S.put thGasAmount <>
    S.put thFinalizedPointer

getTransactionHeader :: SchemeId -> S.Get TransactionHeader
getTransactionHeader tsScheme = do
    thSenderKey <- S.get
    thNonce <- S.get
    thGasAmount <- S.get
    thFinalizedPointer <- S.get
    return $ makeTransactionHeader tsScheme thSenderKey thNonce thGasAmount thFinalizedPointer


type TransactionHash = H.Hash

data Transaction = Transaction {
  trSignature :: !TransactionSignature,
  trHeader :: !TransactionHeader,
  trPayload :: EncodedPayload, -- NB: It is intendent that this field is lazy.
                        -- This allows us to only deserialize on demand at execution time.

  -- * The following fields are strictly redundant, but are here to avoid needless recomputation.
  -- In serialization we do not output them.
  trHash :: TransactionHash  -- | Hash of the transaction. Derived from the previous three fields.
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
    putTransactionHeader trHeader <>
    S.put trPayload

  get = do
    trSignature <- S.get
    trHeader <- getTransactionHeader (tsScheme trSignature)
    trPayload <- S.get
    return $ makeTransaction trSignature trHeader trPayload

makeTransactionHeader ::
  SchemeId
  -> IDTypes.AccountVerificationKey
  -> Nonce
  -> Amount
  -> BlockHash
  -> TransactionHeader
makeTransactionHeader sch thSenderKey thNonce thGasAmount thFinalizedPointer =
  TransactionHeader{thSender = AH.accountAddress' thSenderKey sch,..}

-- |Make a transaction out of minimal data needed.
makeTransaction :: TransactionSignature -> TransactionHeader -> EncodedPayload -> Transaction
makeTransaction trSignature trHeader trPayload =
  let trHash = H.hash . S.runPut $ S.put trSignature <> putTransactionHeader trHeader <> S.put trPayload
  in Transaction{..}

-- |NB: We do not use the serialize instance of the body (trPayload) here, since
-- that is already serialized and there is no need to add additional length
-- information.
-- FIXME: This method is inefficient (it creates temporary bytestrings which are
-- probably not necessary if we had a more appropriate sign function.

-- |Sign a transaction with the given header and body. Uses serialization as defined on the wiki.
signTransaction :: KeyPair -> SchemeId -> TransactionHeader -> EncodedPayload -> Transaction
signTransaction keys tsScheme trHeader trPayload =
  let body = S.runPut (putTransactionHeader trHeader <> S.put trPayload)
      tsSignature = SigScheme.sign tsScheme keys body
      trHash = H.hash (S.encode trSignature <> body)
      trSignature = TransactionSignature{..}
  in Transaction{..}

-- |Verify that the given transaction was signed by the sender's key.
verifyTransactionSignature :: IDTypes.AccountVerificationKey -> BS.ByteString -> TransactionSignature -> Bool
verifyTransactionSignature vfkey bs (TransactionSignature sid sig) = SigScheme.verify sid vfkey bs sig

-- |Verify that the given transaction was signed by the sender's key.
-- In contrast to 'verifyTransactionSignature' this method takes a structured transaction.
verifyTransactionSignature' :: TransactionData msg => IDTypes.AccountVerificationKey -> msg -> TransactionSignature -> Bool
verifyTransactionSignature' vfkey tx (TransactionSignature sid sig) =
  SigScheme.verify sid
                   vfkey (S.runPut (putTransactionHeader (transactionHeader tx) <> S.put (transactionPayload tx)))
                   sig


-- |The 'TransactionData' class abstracts away from the particular data
-- structure. It makes it possible to unify operations on 'Transaction' as well
-- as other types providing the same data (such as partially serialized
-- transactions).
class TransactionData t where
    transactionHeader :: t -> TransactionHeader
    transactionSender :: t -> AccountAddress
    transactionNonce :: t -> Nonce
    transactionGasAmount :: t -> Amount
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
