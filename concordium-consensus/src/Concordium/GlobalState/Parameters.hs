{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders
import Data.ByteString(ByteString)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as AE

-- |FIXME: These should wrap pointers so that they can be passed to rust with no
-- overhead of checking whether they are really valid elements (since we know
-- they are).
newtype ElgamalGenerator = ElgamalGenerator ByteString
    deriving(Show, Serialize)
newtype PedersenKey = PedersenKey ByteString
    deriving(Show, Serialize)

-- |Cryptographic parameters needed to verify on-chain proofs, e.g.,
-- group parameters (generators), commitment keys, in the future also
-- common reference strings, etc.
data CryptographicParameters = CryptographicParameters {
  -- |Generator of the group used for elgamal encryption, also serving as the
  -- base of the discrete logarithm parameter in the dlog sigma protocol.
  elgamalGenerator :: ElgamalGenerator,
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
    do elgamalGeneratorText <- v AE..: "dLogBaseChain"
       commitmentKeyText <- v AE..: "onChainCommitmentKey"
       return CryptographicParameters{
         elgamalGenerator = ElgamalGenerator $ fst . BS16.decode . Text.encodeUtf8 $ elgamalGeneratorText,
         attributeCommitmentKey = PedersenKey $ fst . BS16.decode . Text.encodeUtf8 $ commitmentKeyText
       }

readIdentityProviders :: BSL.ByteString -> Maybe [IdentityProviderData]
readIdentityProviders = AE.decode

readCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
readCryptographicParameters = AE.decode
