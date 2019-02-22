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

import Data.Foldable(toList)
import qualified Data.Sequence as Seq

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad(foldM)
import Control.Monad.IO.Class

import Data.String(fromString)

import Acorn.Types(Metadata, Payload, Message(..))
import qualified Acorn.Types as Types
import qualified Acorn.Utils.Init as Init
import qualified Acorn.Scheduler as Sch

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

type GlobalState = Types.GlobalState

instance Message Transaction where
  getMeta = transactionMetadata
  getPayload = transactionPayload

instance Serialize Transaction

instance Show Transaction where
    showsPrec l (Transaction nonce meta payload) = showsPrec l nonce . (':':) . showsPrec l meta . (':':) . showsPrec l payload

toTransactions :: ByteString -> Maybe [Transaction]
toTransactions bs = case decode bs of
        Left _ -> Nothing
        Right r -> Just r

fromTransactions :: Seq.Seq Transaction -> ByteString
fromTransactions = encode . toList

executeBlockForState :: [Transaction] -> Types.GlobalState -> Types.BlockResult Types.FailureKind () Types.GlobalState
executeBlockForState = Sch.handleBlockOnlyState

makeBlock :: [Transaction] -> Types.GlobalState -> (Seq.Seq ((Transaction, Types.TxCommit [Types.Event])), Types.GlobalState, Seq.Seq (Transaction, Types.FailureKind))
makeBlock = Sch.makeValidBlock 

initState :: Int -> Types.GlobalState
initState = Init.initialState
