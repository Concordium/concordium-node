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
import qualified Concordium.ID.Account as ID
import Data.ByteString(ByteString)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), Parser, Value(..), (.:), withText, withObject, typeMismatch)

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

-- |A convenience wrapper for a bytestring to allow conversion
-- from a JSON string encoded in base 16.
newtype Base16ByteString = Base16ByteString {unBase16 :: BS.ByteString}

instance FromJSON Base16ByteString where
    parseJSON = withText "base-16 encoded bytestring" $ \t ->
                    let (bs, rest) = BS16.decode (Text.encodeUtf8 t) in
                    if BS.null rest then
                        return (Base16ByteString bs)
                    else
                        typeMismatch "base-16 encoded bytestring" (String t)


deserializeBase16 :: (Serialize a) => Text.Text -> Parser a
deserializeBase16 t =
        if BS.null rest then
            case decode bs of
                Left er -> fail er
                Right r -> return r
        else
            fail $ "Could not decode as base-16: " ++ show t
    where
        (bs, rest) = BS16.decode (Text.encodeUtf8 t)

serializeBase16 :: (Serialize a) => a -> Text.Text
serializeBase16 = Text.decodeUtf8 . BS16.encode . encode

instance FromJSON CryptographicParameters where
  parseJSON = withObject "CryptoGraphicParameters" $ \v ->
    do elgamalGenerator <- deserializeBase16 =<< v .: "dLogBaseChain"
       attributeCommitmentKey <- deserializeBase16 =<< v .: "onChainCommitmentKey"
       return CryptographicParameters{..}
{-
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
-}

readIdentityProviders :: BSL.ByteString -> Maybe [IdentityProviderData]
readIdentityProviders = AE.decode

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
            gbElectionVerifyKey <- deserializeBase16 =<< v .: "electionVerifyKey"
            gbSignatureVerifyKey <- deserializeBase16 =<< v .: "signatureVerifyKey"
            acct <- v .: "account"
            (gbAccountSignatureScheme, gbAccountSignatureKey, gbAccountBalance) <- flip (withObject "GenesisBakerAccount") acct $ \v' -> do
                ss <- toEnum <$> v' .: "signatureScheme"
                sk <- deserializeBase16 =<< v' .: "signatureKey"
                ab <- Amount <$> v' .: "balance"
                return (ss, sk, ab)
            gbFinalizer <- v .: "finalizer"
            return GenesisBaker{..}

-- 'GenesisParameters' provides a convenient abstraction for
-- constructing 'GenesisData'.
data GenesisParameters = GenesisParameters {
    gpGenesisTime :: Timestamp,
    gpSlotDuration :: Duration,
    gpLeadershipElectionNonce :: LeadershipElectionNonce,
    gpElectionDifficulty :: ElectionDifficulty,
    gpFinalizationMinimumSkip :: BlockHeight,
    gpBakers :: [GenesisBaker],
    gpCryptographicParameters :: CryptographicParameters,
    gpIdentityProviders :: [IdentityProviderData]
}

instance FromJSON GenesisParameters where
    parseJSON = withObject "GenesisParameters" $ \v -> do
        gpGenesisTime <- v .: "genesisTime"
        gpSlotDuration <- v .: "slotDuration"
        gpLeadershipElectionNonce <- unBase16 <$> v .: "leadershipElectionNonce"
        gpElectionDifficulty <- v .: "electionDifficulty"
        gpFinalizationMinimumSkip <- BlockHeight <$> v .: "finalizationMinimumSkip"
        gpBakers <- v .: "bakers"
        gpCryptographicParameters <- v .: "cryptographicParameters"
        gpIdentityProviders <- v .: "identityProviders"
        return GenesisParameters{..}

parametersToGenesisData :: GenesisParameters -> GenesisData
parametersToGenesisData GenesisParameters{..} = GenesisData{..}
    where
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisBirkParameters = BirkParameters {
            _birkLeadershipElectionNonce = gpLeadershipElectionNonce,
            _birkElectionDifficulty = gpElectionDifficulty,
            _birkBakers = bakersFromList (mkBaker <$> gpBakers)
        }
        mkBaker GenesisBaker{..} = BakerInfo 
                gbElectionVerifyKey
                gbSignatureVerifyKey
                gbAccountBalance 
                (ID.accountAddress gbAccountSignatureKey gbAccountSignatureScheme)
        genesisBakerAccounts =
            [(newAccount gbAccountSignatureKey gbAccountSignatureScheme) {_accountAmount = gbAccountBalance, _accountStakeDelegate = Just bid}
                | (GenesisBaker{..}, bid) <- zip gpBakers [0..]]
        genesisFinalizationParameters =
            FinalizationParameters
                [VoterInfo {voterVerificationKey = gbSignatureVerifyKey, voterVRFKey = gbElectionVerifyKey, voterPower = 1} 
                    | GenesisBaker{..} <- gpBakers, gbFinalizer]
                gpFinalizationMinimumSkip
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders