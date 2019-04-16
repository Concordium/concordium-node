{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.Payload.Transaction where
{- 
import GHC.Generics
import Data.ByteString.Char8(ByteString)
import Concordium.Crypto.SHA256
import Data.Serialize
import Data.Hashable

import Data.Foldable(toList)

import Concordium.Types
import Concordium.Scheduler.Types(Message(..))
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler.Utils.Init.Example as Init
import qualified Concordium.Scheduler as Sch

newtype TransactionNonce = TransactionNonce Hash
    deriving (Eq, Ord, Hashable, Generic)

instance Show TransactionNonce where
    show (TransactionNonce s) = show s

instance Serialize TransactionNonce

data Transaction = Transaction {
    transactionNonce :: TransactionNonce,
    transactionHeader :: !Header,
    transactionPayload :: !SerializedPayload
} deriving (Generic, Eq)

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
initState n = Init.initialState n
-}
