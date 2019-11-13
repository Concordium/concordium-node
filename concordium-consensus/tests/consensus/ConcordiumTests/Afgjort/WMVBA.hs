{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, RankNTypes, BangPatterns #-}
module ConcordiumTests.Afgjort.WMVBA where

import Test.QuickCheck
import Test.Hspec
import System.Random
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import Data.Serialize as S

import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import qualified Concordium.Crypto.BlsSignature as Bls


genSecretKey :: Gen Bls.SecretKey
genSecretKey = fst . Bls.randomSecretKey . mkStdGen <$> arbitrary

genKeyPair :: Gen (Bls.SecretKey, Bls.PublicKey)
genKeyPair = fmap (\sk -> (sk, Bls.derivePublicKey sk)) genSecretKey

-- generates a list of 10 to 100 keypairs, length chosen randomly in (10,100)
genKeys :: Gen [(Bls.SecretKey, Bls.PublicKey)]
genKeys = do
  n <- choose (10, 100)
  vectorOf n genKeyPair

genByteString :: Gen BS.ByteString
genByteString = BS.pack <$> vector 8

genKeysByteStringCulprits :: Gen ([(Bls.SecretKey, Bls.PublicKey)], BS.ByteString, [Int])
genKeysByteStringCulprits = do
  keys <- genKeys
  tosign <- genByteString
  nculprits <- choose (1, 3)
  culprits <- suchThat (vectorOf nculprits $ choose (0, length keys - 1)) noDuplicates
  return (keys, tosign, culprits)
    where
      noDuplicates lst = length (List.nub lst) == length lst

makePartiesAndSignatures :: [(Bls.SecretKey, Bls.PublicKey)] -> BS.ByteString -> [Int] -> ([(Party, (Bls.SecretKey, Bls.PublicKey))], [(Party, (Int, Bls.Signature))])
makePartiesAndSignatures keys toSign culpritixs =
  let (parties, sigs, _) = foldl f ([], [], 0) keys
  in (parties, sigs)
  where
    f (parties, sigs, i) (sk, pk) =
      ((fromIntegral i, (sk, pk)) : parties, (fromIntegral i, g i sk) : sigs, i+1)
    g i sk = if elem i culpritixs then (0, Bls.emptySignature) else (0, Bls.sign toSign sk)

-- Tests that no parties are returned when there are no malicious signatures
findCulpritsNoMaliciousTest :: Property
findCulpritsNoMaliciousTest = forAll genKeysByteStringCulprits $ \(keys, toSign, _) ->
  let (parties, sigs) = makePartiesAndSignatures keys toSign []
      keyMap = Map.fromList parties
      keylookup p = snd $ keyMap Map.! p
      culprits = findCulprits sigs toSign keylookup
  in
    culprits === []

-- Tests that parties find the signatures that didn't verify
findCulpritsTest :: Property
findCulpritsTest = forAll genKeysByteStringCulprits $ \(keys, toSign, culpritixs) ->
  let (parties, sigs) = makePartiesAndSignatures keys toSign culpritixs
      keyMap = Map.fromList parties
      keylookup p = snd $ keyMap Map.! p
      culprits = findCulprits sigs toSign keylookup
      correctCulprits = List.sort $ map fromIntegral culpritixs
  in List.sort culprits === correctCulprits

tests :: Word -> Spec
tests lvl = describe "Concordium.Afgjort.WMVBA" $ do
    it "WMVBA_findCulpritsNoMaliciousTest" $ withMaxSuccess 100 findCulpritsNoMaliciousTest
    it "WMVBA_findCulpritsTest" $ withMaxSuccess 100 findCulpritsTest
