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
  thPayloadSize <- (`mod` 5000) <$> arbitrary
  thNonce <- Nonce <$> arbitrary
  thGasAmount <- Energy <$> arbitrary
  thFinalizedPointer <- Hash . FBS.pack <$> vector 32
  return $ makeTransactionHeader thScheme thSenderKey thPayloadSize thNonce thGasAmount

genTransaction :: Gen BareTransaction
genTransaction = do
  btrHeader <- genTransactionHeader
  btrPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thPayloadSize btrHeader))
  s <- choose (1, 500)
  btrSignature <- TransactionSignature . Signature . BSS.pack <$> vector s
  return $! BareTransaction{..}


genSignedTransaction :: Gen BareTransaction
genSignedTransaction = do
  kp <- genKeyPair
  btrHeader <- genTransactionHeader
  btrPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thPayloadSize btrHeader))
  s <- choose (1, 500)
  btrSignature <- TransactionSignature . Signature . BSS.pack <$> vector s
  return $! signTransaction kp (btrHeader {thSenderKey = verifyKey kp, thScheme = Ed25519}) btrPayload
