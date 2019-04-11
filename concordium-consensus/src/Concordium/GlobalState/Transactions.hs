{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Concordium.GlobalState.Transactions where

import Control.Exception
import GHC.Generics
import qualified Data.Serialize as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.Signature as Sig
import Concordium.ID.AccountHolder

import Concordium.GlobalState.Types
import Concordium.GlobalState.HashableTo

newtype TransactionSignature = TransactionSignature Sig.Signature
    deriving (S.Serialize)

class TransactionData t where
    transactionSender :: t -> AccountAddress
    transactionNonce :: t -> Nonce
    transactionGasAmount :: t -> Amount
    transactionPayload :: t -> SerializedPayload
    transactionSignature :: t -> TransactionSignature

data TransactionHeader = TransactionHeader {
    thSender :: AccountAddress,
    thNonce :: Nonce,
    thGasAmount :: Amount
} deriving (Generic)

instance S.Serialize TransactionHeader

data Transaction = Transaction {
    trHeader :: TransactionHeader,
    trPayload :: SerializedPayload,
    trSignature :: TransactionSignature
} deriving (Generic)

instance TransactionData Transaction where
    transactionSender = thSender . trHeader
    transactionNonce = thNonce . trHeader
    transactionGasAmount = thGasAmount . trHeader
    transactionPayload = trPayload
    transactionSignature = trSignature

instance S.Serialize Transaction

instance HashableTo H.Hash Transaction where
    getHash = H.hashLazy . S.runPutLazy . S.put

type TransactionHash = H.Hash

type HashedTransaction = Hashed Transaction

instance TransactionData (Hashed Transaction) where
    transactionSender = transactionSender . unhashed
    transactionNonce = transactionNonce . unhashed
    transactionGasAmount = transactionGasAmount . unhashed
    transactionPayload = transactionPayload . unhashed
    transactionSignature = transactionSignature . unhashed

data AccountNonFinalizedTransactions = AccountNonFinalizedTransactions {
    -- |Non-finalized transactions (for an account) indexed by nonce.
    _anftMap :: Map.Map Nonce (Set.Set HashedTransaction),
    -- |The next available nonce at the last finalized block.
    -- 'anftMap' should only contain nonces that are at least 'anftNextNonce'.
    _anftNextNonce :: Nonce
} deriving (Eq)
makeLenses ''AccountNonFinalizedTransactions

emptyANFT :: AccountNonFinalizedTransactions
emptyANFT = AccountNonFinalizedTransactions Map.empty minNonce

data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions.  Contains all transactions.
    _ttHashMap :: HM.HashMap TransactionHash (HashedTransaction, Slot),
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
-- where @nextNonce@ is the next nonce for the account address, and @highNonce@ is the
-- highest nonce known for a transaction associated with that account.  @highNonce@ should
-- always be at least @nextNonce@ (otherwise, what transaction is pending?).  If an account
-- has no pending transactions, then it should not be in the map.
type PendingTransactionTable = HM.HashMap AccountAddress (Nonce, Nonce)

emptyPendingTransactionTable :: PendingTransactionTable
emptyPendingTransactionTable = HM.empty

forwardPTT :: [HashedTransaction] -> PendingTransactionTable -> PendingTransactionTable
forwardPTT trs ptt0 = foldl forward1 ptt0 trs
    where
        forward1 :: PendingTransactionTable -> HashedTransaction -> PendingTransactionTable
        forward1 ptt tr = ptt & at (transactionSender tr) %~ upd
            where
                upd Nothing = error "forwardPTT : forwarding transaction that is not pending"
                upd (Just (low, high)) = assert (low == transactionNonce tr && low <= high) $
                        if low == high then Nothing else Just (low+1,high)

reversePTT :: [HashedTransaction] -> PendingTransactionTable -> PendingTransactionTable
reversePTT trs ptt0 = foldr reverse1 ptt0 trs
    where
        reverse1 :: HashedTransaction -> PendingTransactionTable -> PendingTransactionTable
        reverse1 tr = at (transactionSender tr) %~ upd
            where
                upd Nothing = Just (transactionNonce tr, transactionNonce tr)
                upd (Just (low, high)) = assert (low == transactionNonce tr + 1) $
                        Just (low-1,high)