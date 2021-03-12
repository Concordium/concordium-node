{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module ConcordiumTests.Afgjort.WMVBA where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec
import System.Random
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Data.ByteString as BS
import qualified Data.Vector as Vec
import qualified Data.Maybe as Mb
import Control.Monad
import Control.Monad.Trans

import qualified Concordium.Afgjort.PartyMap as PM
import qualified Concordium.Afgjort.PartySet as PS
import Concordium.Afgjort.Types
import Concordium.Afgjort.WMVBA
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Types as T

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

invariantWMVBAState :: WMVBAInstance -> WMVBAState () -> [WMVBAOutputEvent ()] -> Either String ()
invariantWMVBAState (WMVBAInstance baid _totalWeight corruptWeight _partyWeight _maxParty
                                   _publicKeys _me _privateKey publicBlsKeys _privateBlsKey)
                    (WMVBAState _ _ _ justifications badJustifications)
                    outputs = do
    -- For each bad party checks that their blssignature doesn't verify
    checkBadJustificationsActuallyBad $ PS.toList badjstfc
    -- If the weight of the good justifiers exceeds the threshold, a finalizationproof should have been outputted
    when (PM.weight jstfc - PS.weight badjstfc > corruptWeight) $ containsFinalizationProof outputs
    -- If the weight of the good justifiers does NOT exceed the threshold, a finalizationproof should NOT have been outputted
    when (PM.weight jstfc - PS.weight badjstfc <= corruptWeight) $ containsNoFinalizationProof outputs
        where
          containsNoFinalizationProof [] = Right ()
          containsNoFinalizationProof (x : xs) = case x of
            WMVBAComplete p -> Left $ "a finalizationproof was produced before its weight could exceed the corrupted weight. Proof:\n" ++ (show p)
            _ -> containsNoFinalizationProof xs
          containsFinalizationProof [] = Left "outputs contains no finalization proof while it should"
          containsFinalizationProof (x : xs) = case x of
            WMVBAComplete (Just (v, (parties, sig))) -> checkFinalizationProof v sig parties
            _ -> containsFinalizationProof xs
          checkFinalizationProof v sig parties = unless (Bls.verifyAggregate (witnessMessage baid v) (map publicBlsKeys parties) sig) $ Left "finalizationproof does not verify"
          checkBadJustificationsActuallyBad [] = Right ()
          checkBadJustificationsActuallyBad (p : ps) =
            case Map.lookup p (PM.partyMap jstfc) of
                Just (_, blssig) -> if Bls.verify (witnessMessage baid blockA) (publicBlsKeys p) blssig
                                    then Left $ "party " ++ show p ++ " is marked as bad for block " ++ show blockA ++ " but its blssignature verifies"
                                    else checkBadJustificationsActuallyBad ps
                _ -> Left $ "party " ++ (show p) ++ " is marked as bad for block " ++ show blockA ++ " but its justification isn't found in justifications"
          jstfc = Mb.fromMaybe PM.empty $ Map.lookup blockA justifications
          badjstfc = Mb.fromMaybe PS.empty $ Map.lookup blockA badJustifications


runWMVBATest :: Int -> BS.ByteString -> Int -> Vec.Vector (VRF.KeyPair, Bls.SecretKey) -> [WMVBAInput] -> IO Property
runWMVBATest me baid nparties keys = go myInstance initialWMVBAState
    where
        corruptWeight  = nparties `div` 3
        inst i = WMVBAInstance baid (fromIntegral nparties) (fromIntegral corruptWeight) (const 1) (fromIntegral nparties) vrfKeyMap i myVRFKeypair blsPublicKeyMap myBlsKey
        myInstance = inst (fromIntegral me)
        go _ins _st [] = return (property True)
        go ins st (e : es) = do
          (_, st', outputs) <- runWMVBA (makeInput e) ins st
          case invariantWMVBAState myInstance st' outputs of
            Left err -> return $ counterexample err False
            Right _ -> go ins st' es
        vrfKeyMap = VRF.publicKey . fst . (keys Vec.!) . fromIntegral
        blsPublicKeys = Vec.map (\(_, blssk) -> Bls.derivePublicKey blssk) keys
        blsPublicKeyMap = (blsPublicKeys Vec.!) . fromIntegral
        myVRFKeypair = fst $ keys Vec.! fromIntegral me
        myBlsKey = snd $ keys Vec.! fromIntegral me


blockA :: Val
blockA = T.BlockHash (H.hash "A")

testData1 :: BS.ByteString -> Val -> Vec.Vector (VRF.KeyPair, Bls.SecretKey) -> [WMVBAInput]
testData1 baid v keys = inputs
  where
    inputs = let (inps, _) = Vec.foldl f ([], 0) keys in inps
    f (inps, i) (_, blssk) = (ReceiveWMVBAMessage i (makeWMVBAWitnessCreatorMessage baid v blssk) : inps, i+1)

testData2 :: BS.ByteString -> Val -> Vec.Vector (VRF.KeyPair, Bls.SecretKey) -> Int -> Bls.SecretKey -> [WMVBAInput]
testData2 baid v keys corrupted wrongkey = inputs
  where
    inputs = let (inps, _) = Vec.foldl f ([], 0) keys in inps
    f (inps, i) (_, blssk) =
      if i < corrupted then
        (ReceiveWMVBAMessage (fromIntegral i) (makeWMVBAWitnessCreatorMessage baid v wrongkey) : inps, i+1)
      else
        (ReceiveWMVBAMessage (fromIntegral i) (makeWMVBAWitnessCreatorMessage baid v blssk) : inps, i+1)

runNonCorruptedTest :: Property
runNonCorruptedTest = monadicIO $ do
  keys <- pick $ genKeys nparties
  baid <- pick $ genByteString
  stream <- pick $ shuffle $ testData1 baid blockA keys
  liftIO $ runWMVBATest me baid nparties keys (StartWMVBA blockA : stream)
    where
      nparties = 10
      me = 0

runCorruptedTest :: Int -> Property
runCorruptedTest corrupted = monadicIO $ do
  keys <- pick $ genKeys nparties
  baid <- pick $ genByteString
  wrongkey <- pick $ genBlsSecretKey
  stream <- pick $ shuffle $ testData2 baid blockA keys corrupted wrongkey
  liftIO $ runWMVBATest me baid nparties keys (StartWMVBA blockA : stream)
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
        toSign = (witnessMessage baid (T.BlockHash (val)))
        (_, signatures) = makePartiesAndSignatures keys toSign culpritIxs
        culprits = PS.fromList pWeight culpritIxs
        (proof, bad) = createAggregateSig wi (T.BlockHash (val)) (PM.fromList pWeight signatures) PS.empty
    case proof of
      Just (good, sig) -> return $ (bad === culprits .&&. Bls.verifyAggregate toSign (pubKeys <$> good) sig)
      Nothing -> return $ property False

tests :: Word -> Spec
tests lvl = parallel $ describe "Concordium.Afgjort.WMVBA" $ do
    it "Finds no culprits when everyone signs correctly" $ withMaxSuccess (10 * (10^lvl)) findCulpritsNoMaliciousTest
    it "Finds the misbehaving signers" $ withMaxSuccess (10 * (10^lvl)) findCulpritsTest
    it "Test createAggregateSig" $ withMaxSuccess (10 * (10^lvl)) createAggSigTest
    it "Test WMVBA with no culprits" $ withMaxSuccess (10 * (10^lvl)) runNonCorruptedTest
    it "Test WMVBA with 3/10 culprits" $ withMaxSuccess (10 * (10^lvl)) $ runCorruptedTest 3
    it "Test WMVBA with 7/10 culprits" $ withMaxSuccess (10 * (10^lvl)) $ runCorruptedTest 7
