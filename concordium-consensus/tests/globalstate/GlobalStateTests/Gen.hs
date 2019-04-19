{-# LANGUAGE RecordWildCards #-}
module GlobalStateTests.Gen where

import Test.QuickCheck

import Concordium.GlobalState.Transactions
import Concordium.Crypto.SignatureScheme
import Concordium.ID.AccountHolder

import Concordium.Types

import qualified Data.FixedByteString as FBS
import qualified Data.ByteString as BS

schemes :: [SchemeId]
schemes = [CL,Ed25519]

genSchemeId :: Gen SchemeId
genSchemeId = elements schemes 

genAccountAddress :: Gen (AccountAddress, SchemeId)
genAccountAddress = do
  tsScheme <- genSchemeId
  addr <- AccountAddress <$> (FBS.pack . (fromIntegral (fromEnum tsScheme) :) <$> vector 20)
  return (addr, tsScheme)

genTransactionHeader :: Gen (TransactionHeader, SchemeId)
genTransactionHeader = do
  (thSender, tsScheme) <- genAccountAddress
  thNonce <- Nonce <$> arbitrary
  thGasAmount <- Amount <$> arbitrary
  return (TransactionHeader{..}, tsScheme)

genTransaction :: Gen Transaction
genTransaction = do
  (trHeader, tsScheme) <- genTransactionHeader
  n <- getSize
  l <- choose (1, n)
  trPayload <- SerializedPayload . BS.pack <$>  (vector l)
  s <- choose (1, 500)
  trSignature <- TransactionSignature tsScheme . Signature . BS.pack <$> vector s
  return Transaction{..}
