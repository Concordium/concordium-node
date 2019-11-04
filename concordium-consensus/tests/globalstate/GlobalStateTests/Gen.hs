{-# LANGUAGE RecordWildCards #-}
module GlobalStateTests.Gen where

import Test.QuickCheck

import Concordium.Crypto.SHA256(hash)
import Concordium.GlobalState.Transactions
import Concordium.Crypto.SignatureScheme
import Data.Time.Clock

import Concordium.Types

import qualified Data.FixedByteString as FBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize(encode)

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
  thSenderKey' <- correspondingVerifyKey <$> genKeyPair
  thSenderKey <- correspondingVerifyKey <$> genKeyPair
  thPayloadSize <- (`mod` 5000) <$> arbitrary
  thNonce <- Nonce <$> arbitrary
  thGasAmount <- Energy <$> arbitrary
  return $ makeTransactionHeader thSenderKey thPayloadSize thNonce thGasAmount

genBareTransaction :: Gen BareTransaction
genBareTransaction = do
  btrHeader <- genTransactionHeader
  btrPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thPayloadSize btrHeader))
  s <- choose (50,70)
  btrSignature <- TransactionSignature . Signature . BSS.pack <$> vector s
  return $! BareTransaction{..}

baseTime :: UTCTime
baseTime = read "2019-09-23 13:27:13.257285424 UTC"

genTransaction :: Gen Transaction
genTransaction = do
  trBareTransaction <- genBareTransaction
  trArrivalTime <- arbitrary
  let body = encode trBareTransaction
  let trHash = hash body
  let trSize = BS.length body
  return $ Transaction{..}


genSignedTransaction :: Gen BareTransaction
genSignedTransaction = do
  kp <- genKeyPair
  btrHeader <- genTransactionHeader
  btrPayload <- EncodedPayload . BSS.pack <$> vector (fromIntegral (thPayloadSize btrHeader))
  return $! signTransaction kp (btrHeader {thSenderKey = correspondingVerifyKey kp}) btrPayload
