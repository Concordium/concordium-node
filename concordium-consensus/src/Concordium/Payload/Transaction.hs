{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# Language OverloadedStrings #-}
module Concordium.Payload.Transaction where

import GHC.Generics
import Data.Word
import Data.ByteString.Char8(ByteString, pack)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Serialize
import Data.Hashable
import Data.Bits
import Numeric

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad(foldM)
import Control.Monad.IO.Class

import Data.String(fromString)

import Scheduler(Metadata, Payload)
import qualified Interpreter as I
import qualified Init
import qualified Scheduler as Sch

data TransactionNonce = TransactionNonce !Word64 !Word64 !Word64 !Word64
    deriving (Eq, Ord, Generic)

instance Hashable TransactionNonce where
    hashWithSalt salt (TransactionNonce a _ _ _) = fromIntegral a `xor` salt
    hash (TransactionNonce a _ _ _) = fromIntegral a

instance Show TransactionNonce where
    show (TransactionNonce a b c d) =
        LBS.unpack (toLazyByteString $ word64HexFixed a <> word64HexFixed b <> word64HexFixed c <> word64HexFixed d)

instance Serialize TransactionNonce

data Transaction = Transaction {
    transactionNonce :: TransactionNonce,
    transactionMetadata :: !Metadata,
    transactionPayload :: !Payload
} deriving (Generic)

instance Sch.Message Transaction where
  getMeta = transactionMetadata
  getPayload = transactionPayload

instance Serialize Transaction

instance Show Transaction where
    showsPrec l (Transaction nonce meta payload) = showsPrec l nonce . (':':) . showsPrec l meta . (':':) . showsPrec l payload

toTransactions :: ByteString -> Maybe [Transaction]
toTransactions bs = case decode bs of
        Left _ -> Nothing
        Right r -> Just r

fromTransactions :: [Transaction] -> ByteString
fromTransactions = encode

executeBlock :: [Transaction] -> GlobalState -> Sch.BlockResult Sch.FailureKind ([Sch.TxCommit [Sch.Event]]) GlobalState
executeBlock = Sch.handleBlock

makeBlock :: [Transaction] -> GlobalState -> ([(Transaction, Sch.TxCommit [Sch.Event])], GlobalState, [(Transaction, Sch.FailureKind)])
makeBlock = Sch.makeValidBlock 

initState = Init.initialState

type GlobalState = I.GlobalState

