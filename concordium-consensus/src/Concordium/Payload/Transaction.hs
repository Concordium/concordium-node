{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# Language OverloadedStrings #-}
module Concordium.Payload.Transaction where

import GHC.Generics
import Data.Word
import Data.ByteString.Char8(ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Serialize
import Data.Hashable
import Data.Bits

import Data.Foldable(toList)



import Concordium.GlobalState.Types
import Acorn.Types(Message(..))
import qualified Acorn.Types as Types
import qualified Acorn.EnvironmentImplementation as Types
import qualified Acorn.Utils.Init.Example as Init
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
    transactionHeader :: !Header,
    transactionPayload :: !SerializedPayload
} deriving (Generic)

type GlobalState = Types.GlobalState

instance Message Transaction where
  getHeader = transactionHeader
  getPayload = decode . _spayload . transactionPayload

instance Serialize Transaction

instance Show Transaction where
    showsPrec l (Transaction txnonce meta payload) = showsPrec l txnonce . (':':) . showsPrec l meta . (':':) . showsPrec l payload

toTransactions :: ByteString -> Maybe [Transaction]
toTransactions bs = case decode bs of
        Left _ -> Nothing
        Right r -> Just r

fromTransactions :: [Transaction] -> ByteString
fromTransactions = encode . toList

executeBlockForState :: [Transaction] -> ChainMetadata -> Types.GlobalState -> Either Types.FailureKind Types.GlobalState
executeBlockForState txs cm gs = let (mres, gs') = Types.runSI (Sch.execBlock txs) cm gs
                                 in case mres of
                                      Nothing -> Right gs'
                                      Just fk -> Left fk
                              
makeBlock :: [Transaction] -> ChainMetadata -> Types.GlobalState -> ([(Transaction, Types.ValidResult)], [(Transaction, Types.FailureKind)], Types.GlobalState)
makeBlock msg cm gs = let ((suc, failure), gs') = Types.runSI (Sch.makeValidBlock msg) cm gs
                      in (suc, failure, gs')

initState :: Int -> Types.GlobalState
initState = Init.initialState
