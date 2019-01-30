{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Concordium.Getters where

import GHC.Generics(Generic)

import Concordium.Payload.Transaction
import Concordium.Types as T

import Data.String

import qualified Data.Map as Map

import Data.ByteString.Builder
import Data.Monoid((<>))

import Data.Text
import Data.Text.Encoding

import Data.Aeson

import Data.Word

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Base16.Lazy as BSL16
import qualified Data.ByteString.Base16 as BS16

import Interpreter.CallContract(State(..))

data TxOut = TxOut {txNonce :: Text -- base16 encoded nonce
                   ,txSender :: Text  -- base16 encoded address
                   ,txAddr :: Text -- base16 encoded address
                   ,txMessage :: Text -- message converted to text
                   }
     deriving(Generic)

data BlockInfo = BlockInfo {blockHash :: Text -- base16 encoding 
                           ,blockParent :: Text  -- base 16 encoding
                           ,blockBaker :: Word64          -- id of the baker of the block
                           ,transactions :: [TxOut]       -- list of transactions in this block
                           ,globalState :: [(Text, Text)] -- global state at the end of this block
                           }
                 deriving(Generic)

instance ToJSON TxOut

instance ToJSON BlockInfo

transactionToTxOut :: Transaction -> TxOut
transactionToTxOut (Transaction (TransactionNonce a b c d) meta (Update to msg)) = TxOut {..}
  where txNonce = decodeUtf8 . BS.toStrict . BSL16.encode . toLazyByteString $ word64LE a <> word64LE b <> word64LE c <> word64LE d
        txSender = decodeUtf8 . BS16.encode . sender $ meta
        txAddr = decodeUtf8 . BS16.encode $ to
        txMessage = case msg of
                      Increment -> "Increment"
                      Decrement -> "Decrement"

globalStateToPairs :: GlobalState -> [(Text, Text)]
globalStateToPairs = Prelude.map (\(addr, State s) -> (decodeUtf8 . BS16.encode $ addr, s)) . Map.toList . instances

blockDataToTxOut :: Block -> Maybe [TxOut]
blockDataToTxOut b =  Prelude.map transactionToTxOut <$> toTransactions (blockData b)

mkBlockInfo :: Block -> GlobalState -> BlockInfo
mkBlockInfo block gs =
  let bh = fromString . show $ hashBlock block
      bp = fromString . show $ blockPointer block
      bb = T.blockBaker block
  in case blockDataToTxOut block of
       Nothing -> BlockInfo bh bp bb [] (globalStateToPairs gs)
       Just txouts -> BlockInfo bh bp bb txouts (globalStateToPairs gs)
