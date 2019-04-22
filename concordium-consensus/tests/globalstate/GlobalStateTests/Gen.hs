{-# LANGUAGE RecordWildCards #-}
module GlobalStateTests.Gen where

import Test.QuickCheck

import Concordium.Crypto.SHA256(Hash(..))
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

genTransactionHeader :: Gen TransactionHeader
genTransactionHeader = do
  thSenderKey <- VerifyKey . BS.pack <$> (vector 32)
  thNonce <- Nonce <$> arbitrary
  thGasAmount <- Amount <$> arbitrary
  thFinalizedPointer <- Hash . FBS.pack <$> vector 32
  return TransactionHeader{..}

genTransaction :: Gen Transaction
genTransaction = do
  trHeader <- genTransactionHeader
  n <- getSize
  l <- choose (1, n)
  trPayload <- EncodedPayload . BS.pack <$>  (vector l)
  s <- choose (1, 500)
  tsScheme <- genSchemeId
  trSignature <- TransactionSignature tsScheme . Signature . BS.pack <$> vector s
  return $! makeTransaction trSignature trHeader trPayload
