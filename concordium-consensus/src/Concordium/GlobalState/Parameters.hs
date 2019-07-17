{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- |This module defines types for blockchain parameters, including genesis data,
-- baker parameters and finalization parameters.
module Concordium.GlobalState.Parameters(
    module Concordium.GlobalState.Parameters,
    BakerInfo,
    BakerCreationInfo(..)
) where

import GHC.Generics
import Data.Word
import Data.Ratio
import Data.Serialize
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Crypto.FFIDataTypes
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as AE

-- |Cryptographic parameters needed to verify on-chain proofs, e.g.,
-- group parameters (generators), commitment keys, in the future also
-- common reference strings, etc.
data CryptographicParameters = CryptographicParameters {
  -- |Generator of the group used for elgamal encryption, also serving as the
  -- base of the discrete logarithm parameter in the dlog sigma protocol.
  elgamalGenerator :: ElgamalGen,
  -- |Commitment key used to construct pedersen commitments to individual
  -- attributes of the attribute list.
  attributeCommitmentKey :: PedersenKey
} deriving (Show, Generic)

instance Serialize CryptographicParameters where

data BirkParameters = BirkParameters {
    _birkLeadershipElectionNonce :: LeadershipElectionNonce,
    _birkElectionDifficulty :: ElectionDifficulty,
    _birkBakers :: !Bakers
} deriving (Eq, Generic, Show)
instance Serialize BirkParameters where

makeLenses ''BirkParameters

birkBaker :: BakerId -> BirkParameters -> Maybe (BakerInfo, LotteryPower)
birkBaker bid bps = (bps ^. birkBakers . bakerMap . at bid) <&>
                        \bkr -> (bkr, (bkr ^. bakerStake) % (bps ^. birkBakers . bakerTotalStake))


data VoterInfo = VoterInfo {
    voterVerificationKey :: VoterVerificationKey,
    voterVRFKey :: VoterVRFPublicKey,
    voterPower :: VoterPower
} deriving (Eq, Generic, Show)
instance Serialize VoterInfo where

data FinalizationParameters = FinalizationParameters {
    finalizationCommittee :: [VoterInfo],
    -- |The minimum interval between finalizations will be @1 + finalizationMinimumSkip@
    finalizationMinimumSkip :: BlockHeight
} deriving (Eq, Generic, Show)
instance Serialize FinalizationParameters where

-- | Time in seconds since the epoch
type Timestamp = Word64
-- | Time duration in seconds
type Duration = Word64

data GenesisData = GenesisData {
    genesisTime :: Timestamp,
    genesisSlotDuration :: Duration,
    genesisBirkParameters :: BirkParameters,
    genesisBakerAccounts :: [Account],
    genesisFinalizationParameters :: FinalizationParameters,
    genesisCryptographicParameters :: CryptographicParameters,
    genesisIdentityProviders :: [IdentityProviderData]
} deriving (Generic, Show)

instance Serialize GenesisData where

instance AE.FromJSON CryptographicParameters where
  parseJSON = AE.withObject "CryptoGraphicParameters" $ \v ->
    do elgamalGeneratorbs <- b16 <$> (v AE..: "dLogBaseChain")
       commitmentKeybs <- b16 <$> (v AE..: "onChainCommitmentKey")
       case (decode elgamalGeneratorbs, decode (lenbs commitmentKeybs)) of
         (Right elgamalGenerator, Right attributeCommitmentKey) ->
             return CryptographicParameters{..}
         _ -> fail "Could not decode keys."
    where b16 = fst . BS16.decode . Text.encodeUtf8
          lenbs bs = runPut (putWord32be (fromIntegral (BS.length bs))) <> bs

readIdentityProviders :: BSL.ByteString -> Maybe [IdentityProviderData]
readIdentityProviders = AE.decode

readCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
readCryptographicParameters = AE.decode


-- |NB: Only for testing.
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case d of
    Nothing -> error "Cannot decode dummy cryptographic parameters. Something's changed."
    Just dummy -> dummy

  where d = AE.decode "{\"dLogBaseChain\": \"97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb\",\"onChainCommitmentKey\": \"0000000199e4a085f8d083de689f79e5b296593644037499db92534071d1d5d607fe8594c398442ef20445a8eafae6695c4ed4a3b38a61d0ddd52fae990294114a2c2d20705c868bc979a07ccece02234b5b2f60a16edf7a17b676be108442417aecf34d\"}"
