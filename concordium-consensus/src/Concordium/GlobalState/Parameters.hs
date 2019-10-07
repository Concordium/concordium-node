{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude hiding (fail)
import GHC.Generics
import Data.Word
import Data.Serialize
import Lens.Micro.Platform
import Control.Monad.Fail
import Control.Monad hiding (fail)

import Concordium.Types
import Concordium.Crypto.FFIDataTypes
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState.IdentityProviders
import qualified Concordium.ID.Account as ID

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), (.:), (.:?), (.!=), withObject)

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
    _birkElectionDifficulty :: ElectionDifficulty,
    -- |The current stake of bakers. All updates should be to this state.
    _birkCurrentBakers :: !Bakers,
    -- |The state of bakers at the end of the previous epoch,
    -- will be used as lottery bakers in next epoch.
    _birkPrevEpochBakers :: !Bakers,
    -- |The state of the bakers fixed before previous epoch, 
    -- the lottery power and reward account is used in leader election.
    _birkLotteryBakers :: !Bakers,
    _birkSeedState :: !SeedState
} deriving (Eq, Generic, Show)
instance Serialize BirkParameters where

makeLenses ''BirkParameters

_birkLeadershipElectionNonce :: BirkParameters -> LeadershipElectionNonce
_birkLeadershipElectionNonce = currentSeed . _birkSeedState

birkBaker :: BakerId -> BirkParameters -> Maybe (BakerInfo, LotteryPower)
birkBaker bid bps = bakerData bid $ bps ^. birkCurrentBakers

birkEpochBaker :: BakerId -> BirkParameters -> Maybe (BakerInfo, LotteryPower)
birkEpochBaker bid bps = bakerData bid $ bps ^. birkLotteryBakers

birkEpochBakerByKeys :: BakerSignVerifyKey -> BirkParameters -> Maybe (BakerId, BakerInfo, LotteryPower)
birkEpochBakerByKeys sigKey bps = case bps ^? birkLotteryBakers . bakersByKey . ix sigKey of
        Just bid -> birkEpochBaker bid bps <&> \(binfo, lotPow) -> (bid, binfo, lotPow)
        _ -> Nothing

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
    genesisAccounts :: [Account],
    -- |Special admin accounts used during beta for chain management, e.g.,
    -- adding, removing bakers.
    genesisSpecialBetaAccounts :: [Account],
    genesisFinalizationParameters :: FinalizationParameters,
    genesisCryptographicParameters :: CryptographicParameters,
    genesisIdentityProviders :: [IdentityProviderData],
    genesisMintPerSlot :: Amount
} deriving (Generic, Show)

instance Serialize GenesisData where

instance FromJSON CryptographicParameters where
  parseJSON = withObject "CryptoGraphicParameters" $ \v ->
    do elgamalGenerator <- v .: "dLogBaseChain"
       attributeCommitmentKey <- v .: "onChainCommitmentKey"
       return CryptographicParameters{..}

readIdentityProviders :: BSL.ByteString -> Maybe [IdentityProviderData]
readIdentityProviders = AE.decode

eitherReadIdentityProviders :: BSL.ByteString -> Either String [IdentityProviderData]
eitherReadIdentityProviders = AE.eitherDecode

readCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
readCryptographicParameters = AE.decode

-- 'GenesisBaker' is an abstraction of a baker at genesis.
-- It includes the minimal information for generating a 
-- baker and its account.
data GenesisBaker = GenesisBaker {
    -- |The baker's public VRF key
    gbElectionVerifyKey :: BakerElectionVerifyKey,
    -- |The baker's public signature key
    gbSignatureVerifyKey :: BakerSignVerifyKey,
    -- |The baker's account signature scheme
    gbAccountSignatureScheme :: SchemeId,
    -- |The baker's account public signature key
    gbAccountSignatureKey :: AccountVerificationKey,
    -- |The baker's initial balance
    gbAccountBalance :: Amount,
    -- |Whether the baker should be included in the initial
    -- finalization committee.
    gbFinalizer :: Bool
}

instance FromJSON GenesisBaker where
    parseJSON = withObject "GenesisBaker" $ \v -> do
            gbElectionVerifyKey <- v .: "electionVerifyKey"
            gbSignatureVerifyKey <- v .: "signatureVerifyKey"
            acct <- v .: "account"
            (gbAccountSignatureScheme, gbAccountSignatureKey, gbAccountBalance) <- flip (withObject "GenesisBakerAccount") acct $ \v' -> do
                ss <- v' .: "signatureScheme"
                sk <- v' .: "verifyKey"
                ab <- Amount <$> v' .: "balance"
                return (ss, sk, ab)
            gbFinalizer <- v .: "finalizer"
            return GenesisBaker{..}

-- |'GenesisAccount' are special account existing in the genesis block, in
-- addition to baker accounts which are defined by the 'GenesisBaker' structure.
data GenesisAccount = GenesisAccount {
  gaAccountSignatureScheme :: !SchemeId,
  gaAccountVerifyKey :: !AccountVerificationKey,
  gaAccountBalance :: !Amount,
  gaDelegate :: !(Maybe BakerId)
  -- TODO: credentials
}

instance FromJSON GenesisAccount where
  parseJSON = withObject "GenesisAccount" $ \v -> do
    gaAccountSignatureScheme <- v .: "signatureScheme"
    gaAccountVerifyKey <- v .: "verifyKey"
    gaAccountBalance <- Amount <$> v .: "balance"
    gaDelegate <- fmap BakerId <$> v .:? "delegate"
    return GenesisAccount{..}

-- 'GenesisParameters' provides a convenient abstraction for
-- constructing 'GenesisData'.
data GenesisParameters = GenesisParameters { 
    gpGenesisTime :: Timestamp,
    gpSlotDuration :: Duration,
    gpLeadershipElectionNonce :: LeadershipElectionNonce,
    gpEpochLength :: EpochLength,
    gpElectionDifficulty :: ElectionDifficulty,
    gpFinalizationMinimumSkip :: BlockHeight,
    gpBakers :: [GenesisBaker],
    gpCryptographicParameters :: CryptographicParameters,
    gpIdentityProviders :: [IdentityProviderData],
    gpBetaAccounts :: [GenesisAccount],
    gpMintPerSlot :: Amount
}

instance FromJSON GenesisParameters where
    parseJSON = withObject "GenesisParameters" $ \v -> do
        gpGenesisTime <- v .: "genesisTime"
        gpSlotDuration <- v .: "slotDuration"
        gpLeadershipElectionNonce <- v .: "leadershipElectionNonce"
        gpEpochLength <- Slot <$> v .: "epochLength"
        when(gpEpochLength == 0) $ fail "Epoch length should be non-zero"
        gpElectionDifficulty <- v .: "electionDifficulty"
        gpFinalizationMinimumSkip <- BlockHeight <$> v .: "finalizationMinimumSkip"
        gpBakers <- v .: "bakers"
        when (null gpBakers) $ fail "There should be at least one baker."
        gpCryptographicParameters <- v .: "cryptographicParameters"
        gpIdentityProviders <- v .:? "identityProviders" .!= []
        gpBetaAccounts <- v .:? "betaAccounts" .!= []
        gpMintPerSlot <- Amount <$> v .: "mintPerSlot"
        return GenesisParameters{..}

-- |Implementation-defined parameters, such as block size. They are not
-- protocol-level parameters hence do not fit into 'GenesisParameters'.
data RuntimeParameters = RuntimeParameters {
  -- |Maximum block size produced by the baker (in bytes). Note that this only
  -- applies to the blocks produced by this baker, we will still accept blocks
  -- of arbitrary size from other bakers.
  rpBlockSize :: !Int
  }

-- |Default runtime parameters, block size = 10MB.
defaultRuntimeParameters :: RuntimeParameters
defaultRuntimeParameters = RuntimeParameters{
  rpBlockSize = 10 * 10^(6 :: Int) -- 10MB
  }

instance FromJSON RuntimeParameters where
  parseJSON = withObject "RuntimeParameters" $ \v -> do
    rpBlockSize <- v .: "blockSize"
    when (rpBlockSize <= 0) $
      fail "Block size must be a positive integer."
    return RuntimeParameters{..}

-- |NB: This function will silently ignore bakers with duplicate signing keys.
parametersToGenesisData :: GenesisParameters -> GenesisData
parametersToGenesisData GenesisParameters{..} = GenesisData{..}
    where
        genesisMintPerSlot = gpMintPerSlot
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisBakers = fst (bakersFromList (mkBaker <$> gpBakers))
        genesisBirkParameters = BirkParameters {
            _birkElectionDifficulty = gpElectionDifficulty,
            _birkCurrentBakers = genesisBakers,
            _birkPrevEpochBakers = genesisBakers,
            _birkLotteryBakers = genesisBakers,
            _birkSeedState = genesisSeedState gpLeadershipElectionNonce gpEpochLength
        }
        mkBaker GenesisBaker{..} = BakerInfo 
                gbElectionVerifyKey
                gbSignatureVerifyKey
                gbAccountBalance 
                (ID.accountAddress gbAccountSignatureKey gbAccountSignatureScheme)
        -- special accounts will have some special privileges during beta.
        genesisSpecialBetaAccounts =
          [(newAccount gaAccountVerifyKey gaAccountSignatureScheme) {_accountAmount = gaAccountBalance,
                                                                     _accountStakeDelegate = gaDelegate}
            | GenesisAccount{..} <- gpBetaAccounts]
        -- Baker accounts will have no special privileges.
        genesisAccounts = [(newAccount gbAccountSignatureKey gbAccountSignatureScheme) {_accountAmount = gbAccountBalance,
                                                                                        _accountStakeDelegate = Just bid}
                          | (GenesisBaker{..}, bid) <- zip gpBakers [0..]]
        genesisFinalizationParameters =
            FinalizationParameters
                [VoterInfo {voterVerificationKey = gbSignatureVerifyKey, voterVRFKey = gbElectionVerifyKey, voterPower = 1} 
                    | GenesisBaker{..} <- gpBakers, gbFinalizer]
                gpFinalizationMinimumSkip
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders
