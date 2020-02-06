{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, RankNTypes, BangPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module ConcordiumTests.Afgjort.WMVBA where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import System.Random
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.ByteString as BS
import qualified Data.Vector as Vec
import Control.Monad
import Control.Monad.Trans

import qualified Concordium.Afgjort.PartyMap as PM
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as H

import Debug.Trace

import Concordium.Crypto.DummyData

genBlsSecretKey :: Gen Bls.SecretKey
genBlsSecretKey =  secretBlsKeyGen

genBlsKeyPair :: Gen (Bls.SecretKey, Bls.PublicKey)
genBlsKeyPair = fmap (\sk -> (sk, Bls.derivePublicKey sk)) $ genBlsSecretKey

genVRFKeyPair :: Gen (VRF.KeyPair)
genVRFKeyPair = resize (2^(30::Int)) $ do fst . VRF.randomKeyPair . mkStdGen <$> arbitrary

genKey :: Gen (VRF.KeyPair, Bls.SecretKey)
genKey = liftM2 (,) genVRFKeyPair genBlsSecretKey

genKeys :: Int -> Gen (Vec.Vector (VRF.KeyPair, Bls.SecretKey))
genKeys n = fmap Vec.fromList $ vectorOf n genKey

genBlsKeys :: Int -> Gen [(Bls.SecretKey, Bls.PublicKey)]
genBlsKeys n = do
  vectorOf n genBlsKeyPair

-- TODO: generate random string
genByteString :: Gen BS.ByteString
genByteString = BS.pack <$> vector 8

-- Used to build streams of test data
data WMVBAInput
  = ReceiveWMVBAMessage Party WMVBAMessage
  | StartWMVBA Val
  deriving (Eq, Ord, Show)

data WMVBAOutput
  = DummyOutput Val
  deriving (Eq, Ord, Show)

makeInput :: WMVBAInput -> WMVBA () ()
makeInput (ReceiveWMVBAMessage p m) = receiveWMVBAMessage p () m
makeInput (StartWMVBA v) = startWMVBA v

invariantWMVBAState :: WMVBAInstance () -> WMVBAState () -> Either String ()
invariantWMVBAState inst WMVBAState{..} = return ()
  -- TODO: check invariant here. Invariant after every step should be:
  -- 1. Output isn't WMVBAComplete
  -- 2. badJV total weight doesn't exceept corruptWeight (NOT YET IMPLEMENTED)
  -- 3. Justifications weight for each existing blockvalue doesnt exceep corrupted weight

runWMVBATest :: Int -> BS.ByteString -> Int -> Vec.Vector (VRF.KeyPair, Bls.SecretKey) -> [WMVBAInput] -> IO Property
runWMVBATest me baid nparties keys = go (inst 0) initialWMVBAState
    where
        corruptWeight  = nparties `div` 3
        inst i = WMVBAInstance baid (fromIntegral nparties) (fromIntegral corruptWeight) (const 1) (fromIntegral nparties) vrfKeyMap i myVRFKeypair blsPublicKeyMap myBlsKey
        myInstance = inst (fromIntegral me)
        go _ins st [] = return $ label ("Done processing input stream") $ checkFinalState st
        go ins st (e : es) = do
          (_, st', output) <- runWMVBA (makeInput e) ins st
          case invariantWMVBAState myInstance st' of
            Left err -> return $ counterexample "something went wrong" False
            Right _ -> go ins st' es
        vrfKeyMap = VRF.publicKey . fst . (keys Vec.!) . fromIntegral
        blsPublicKeys = Vec.map (\(_, blssk) -> Bls.derivePublicKey blssk) keys
        blsPublicKeyMap = (blsPublicKeys Vec.!) . fromIntegral
        myVRFKeypair = fst $ keys Vec.! fromIntegral me
        myBlsKey = snd $ keys Vec.! fromIntegral me
        checkFinalState :: WMVBAState () -> Property
        checkFinalState st = traceShow st $ 1 === 1 -- TODO: add proper check

blockA :: Val
blockA = H.hash "A"

testData1 :: Int -> BS.ByteString -> Val -> Vec.Vector (VRF.KeyPair, Bls.SecretKey) -> [WMVBAInput]
testData1 me baid v keys = StartWMVBA blockA : inputs
  where
    inputs = let (inps, _) = Vec.foldl f ([], 0) keys in inps
    f (inps, i) (_, blssk) = (ReceiveWMVBAMessage (fromIntegral i) (makeWMVBAWitnessCreatorMessage baid v blssk) : inps, i+1)

runTest1 :: Property
runTest1 = monadicIO $ do
  keys <- pick $ genKeys nparties
  baid <- pick $ genByteString
  let stream = StartWMVBA blockA : (testData1 me baid blockA keys)
  traceShowM keys
  traceShowM stream
  return (1 === 1)
  liftIO $ runWMVBATest me baid nparties keys stream
    where
      nparties = 10
      me = 0

------------------------------------------------------------------------------------------
-- Unittests for findCulprits

genKeysByteStringCulprits :: Int -> Gen ([(Bls.SecretKey, Bls.PublicKey)], BS.ByteString, [Party])
genKeysByteStringCulprits n = do
  keys <- genBlsKeys n
  tosign <- genByteString
  nculprits <- choose (1, 3)
  culprits <- suchThat (vectorOf nculprits $ choose (0, fromIntegral $ length keys - 1)) noDuplicates
  return (keys, tosign, culprits)
    where
      noDuplicates lst = length (List.nub lst) == length lst

makePartiesAndSignatures :: [(Bls.SecretKey, Bls.PublicKey)] -> BS.ByteString -> [Party] -> ([(Party, (Bls.SecretKey, Bls.PublicKey))], [(Party, Bls.Signature)])
makePartiesAndSignatures keys toSign culpritixs =
  let (parties, sigs, _) = foldl f ([], [], 0) keys
  in (parties, sigs)
  where
    f (parties, sigs, i) (sk, pk) =
      ((fromIntegral i, (sk, pk)) : parties, (fromIntegral i, g i sk) : sigs, i+1)
    g i sk = if elem i culpritixs then Bls.emptySignature else Bls.sign toSign sk

-- Tests that no parties are returned when there are no malicious signatures
findCulpritsNoMaliciousTest :: Property
findCulpritsNoMaliciousTest = forAll (genKeysByteStringCulprits 17) $ \(keys, toSign, _) ->
  let (parties, sigs) = makePartiesAndSignatures keys toSign []
      keyMap = Map.fromList parties
      keylookup p = snd $ keyMap Map.! p
      culprits = findCulprits sigs toSign keylookup
  in
    culprits === []

-- Tests that parties find the signatures that didn't verify
findCulpritsTest :: Property
findCulpritsTest = forAll (genKeysByteStringCulprits 17) $ \(keys, toSign, culpritixs) ->
  let (parties, sigs) = makePartiesAndSignatures keys toSign culpritixs
      keyMap = Map.fromList parties
      keylookup p = snd $ keyMap Map.! p
      culprits = findCulprits sigs toSign keylookup
      correctCulprits = List.sort $ map fromIntegral culpritixs
  in List.sort culprits === correctCulprits

createAggSigTest :: Gen Property
createAggSigTest = do
    (keys, baid, culpritIxs) <- genKeysByteStringCulprits 17
    let
        pWeight p = if p < 17 then 1 else 0
        pubKeys = (snd . (keys List.!!) . fromIntegral)
        wi = WMVBAInstance baid 17 5 pWeight 16 undefined 0 undefined pubKeys undefined
    val <- H.hash . BS.pack <$> arbitrary
    let
        toSign = (witnessMessage baid val)
        (_, signatures) = makePartiesAndSignatures keys toSign culpritIxs
        culprits = PS.fromList pWeight culpritIxs
        ((good, sig), bad) = createAggregateSig wi val (PM.fromList pWeight signatures) PS.empty
    return $ (bad === culprits .&&. Bls.verifyAggregate toSign (pubKeys <$> good) sig)

tests :: Word -> Spec
tests _lvl = describe "Concordium.Afgjort.WMVBA" $ do
    it "Finds no culprits when everyone signs correctly" $ withMaxSuccess 100 findCulpritsNoMaliciousTest
    it "Finds the misbehaving signers" $ withMaxSuccess 100 findCulpritsTest
    it "Test createAggregateSig" $ withMaxSuccess 100 createAggSigTest
    it "wip" $ withMaxSuccess 1 runTest1
