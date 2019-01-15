module Concordium.Birk.LeaderElection where

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as L
import           Data.ByteString

import qualified Concordium.Crypto.DummyVRF    as VRF
import           Concordium.Types

electionProbability :: LotteryPower -> ElectionDifficulty -> Double
electionProbability alpha diff = 1 - (1 - diff) ** alpha

leaderElectionMessage :: LeadershipElectionNonce -> Slot -> ByteString
leaderElectionMessage nonce (Slot sl) =
  L.toStrict
    $  toLazyByteString
    $  stringUtf8 "LE"
    <> byteString nonce
    <> word64BE sl

leaderElection
  :: LeadershipElectionNonce
  -> ElectionDifficulty
  -> Slot
  -> BakerElectionPrivateKey
  -> LotteryPower
  -> Maybe BlockProof
leaderElection nonce diff slot privKey lotPow = if won
  then Just proof
  else Nothing
 where
  msg   = leaderElectionMessage nonce slot
  hsh   = VRF.hash privKey msg
  won   = VRF.hashToDouble hsh < electionProbability lotPow diff
  proof = VRF.prove privKey msg

verifyProof
  :: LeadershipElectionNonce
  -> ElectionDifficulty
  -> Slot
  -> BakerElectionVerifyKey
  -> LotteryPower
  -> BlockProof
  -> Bool
verifyProof nonce diff slot verifKey lotPow proof =
  VRF.verifyKey verifKey
    && VRF.verify verifKey (leaderElectionMessage nonce slot) proof
    && VRF.hashToDouble (VRF.proofToHash proof)
    <  electionProbability lotPow diff

electionLuck :: ElectionDifficulty -> LotteryPower -> BlockProof -> Double
electionLuck diff lotPow proof =
  1 - VRF.hashToDouble (VRF.proofToHash proof) / electionProbability lotPow diff


blockNonceMessage :: LeadershipElectionNonce -> Slot -> ByteString
blockNonceMessage nonce (Slot slot) =
  L.toStrict
    $  toLazyByteString
    $  stringUtf8 "NONCE"
    <> byteString nonce
    <> word64BE slot

computeBlockNonce
  :: LeadershipElectionNonce -> Slot -> BakerElectionPrivateKey -> BlockNonce
computeBlockNonce nonce slot privKey =
  (VRF.hash privKey msg, VRF.prove privKey msg)
  where msg = blockNonceMessage nonce slot

verifyBlockNonce
  :: LeadershipElectionNonce
  -> Slot
  -> BakerElectionVerifyKey
  -> BlockNonce
  -> Bool
verifyBlockNonce nonce slot verifKey (hsh, prf) =
  VRF.verifyKey verifKey
    && VRF.verify verifKey (blockNonceMessage nonce slot) prf
