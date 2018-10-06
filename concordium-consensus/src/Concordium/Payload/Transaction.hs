{-# LANGUAGE DeriveGeneric #-}
module Concordium.Payload.Transaction where

import GHC.Generics
import Data.Word
import Data.ByteString
import Data.Serialize
import Data.Hashable
import Data.Bits
import Numeric

data TransactionNonce = TransactionNonce !Word64 !Word64 !Word64 !Word64
    deriving (Eq, Ord, Generic)

instance Hashable TransactionNonce where
    hashWithSalt salt (TransactionNonce a _ _ _) = fromIntegral a `xor` salt
    hash (TransactionNonce a _ _ _) = fromIntegral a

instance Show TransactionNonce where
    showsPrec _ (TransactionNonce a b c d) = showHex a . showHex b . showHex c . showHex d

instance Serialize TransactionNonce

data Transaction = Transaction {
    transactionNonce :: TransactionNonce,
    transactionData :: ByteString
} deriving (Generic)

instance Serialize Transaction

instance Show Transaction where
    showsPrec l (Transaction nonce d) = showsPrec l nonce . (':':) . showsPrec l d

toTransactions :: ByteString -> Maybe [Transaction]
toTransactions bs = case decode bs of
        Left _ -> Nothing
        Right r -> Just r

fromTransactions :: [Transaction] -> ByteString
fromTransactions = encode