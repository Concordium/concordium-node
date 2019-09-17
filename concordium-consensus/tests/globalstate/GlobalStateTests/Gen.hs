{-# LANGUAGE RecordWildCards #-}
module GlobalStateTests.Gen where

import Test.QuickCheck

import Concordium.Crypto.SHA256(Hash(..))
import Concordium.GlobalState.Transactions
import Concordium.Crypto.SignatureScheme
import Concordium.Crypto.Ed25519Signature(genKeyPair)

import Concordium.Types

import qualified Data.FixedByteString as FBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS

schemes :: [SchemeId]
schemes = [Ed25519]

genSchemeId :: Gen SchemeId
genSchemeId = elements schemes 

genAccountAddress :: Gen (AccountAddress, SchemeId)
genAccountAddress = do
  tsScheme <- genSchemeId
  addr <- AccountAddress <$> (FBS.pack . (fromIntegral (fromEnum tsScheme) :) <$> vector 20)
  return (addr, tsScheme)

genTransactionHeader :: Gen TransactionHeader
genTransactionHeader = do
  thScheme <- genSchemeId
  thSenderKey <- VerifyKey . BSS.pack <$> (vector 32)
  thSize <- (`mod` 5000) <$> arbitrary
  thNonce <- Nonce <$> arbitrary
  thGasAmount <- Energy <$> arbitrary
  thFinalizedPointer <- Hash . FBS.pack <$> vector 32
  return $ makeTransactionHeader thScheme thSenderKey thSize thNonce thGasAmount

genTransaction :: Gen Transaction
genTransaction = do
  trHeader <- genTransactionHeader
  trPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thSize trHeader))
  s <- choose (1, 500)
  trSignature <- TransactionSignature . Signature . BSS.pack <$> vector s
  return $! makeTransaction trSignature trHeader trPayload


genSignedTransaction :: Gen Transaction
genSignedTransaction = do
  kp <- genKeyPair
  trHeader <- genTransactionHeader
  trPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thSize trHeader))
  s <- choose (1, 500)
  trSignature <- TransactionSignature . Signature . BSS.pack <$> vector s
  return $! signTransaction kp (trHeader {thSenderKey = verifyKey kp, thScheme = Ed25519}) trPayload
