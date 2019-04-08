{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Concordium.GlobalState.Transactions where

import GHC.Generics
import qualified Data.Serialize as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.Signature as Sig
import Concordium.ID.AccountHolder

import Concordium.GlobalState.Types
import Concordium.GlobalState.HashableTo

data TransactionHeader = TransactionHeader {
    transactionSender :: AccountAddress,
    transactionNonce :: Nonce,
    transactionGasAmount :: Amount
} deriving (Generic)

instance S.Serialize TransactionHeader

newtype TransactionSignature = TransactionSignature Sig.Signature
    deriving (S.Serialize)

data Transaction = Transaction {
    transactionHeader :: TransactionHeader,
    transactionPayload :: SerializedPayload,
    transactionSignature :: TransactionSignature
} deriving (Generic)

instance S.Serialize Transaction

instance HashableTo H.Hash Transaction where
    getHash = H.hashLazy . S.runPutLazy . S.put

type TransactionHash = H.Hash

type HashedTransaction = Hashed Transaction

data AccountNonFinalizedTransactions = AccountNonFinalizedTransactions {
    -- |Non-finalized transactions (for an account) indexed by nonce.
    anftMap :: Map.Map Nonce (Set.Set HashedTransaction),
    -- |The next available nonce at the last finalized block.
    -- 'anftMap' should only contain nonces that are at least 'anftNextNonce'.
    anftNextNonce :: Nonce
}

data TransactionTable = TransactionTable {
    -- |Map from transaction hashes to transactions.  Contains all transactions.
    ttHashMap :: HM.HashMap TransactionHash HashedTransaction,
    ttNonFinalizedTransactions :: HM.HashMap AccountAddress AccountNonFinalizedTransactions

}
