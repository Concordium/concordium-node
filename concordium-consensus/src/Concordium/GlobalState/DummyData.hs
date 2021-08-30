{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Concordium.GlobalState.DummyData where

import qualified Data.Vector as Vec
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.ID.Types
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards as Rewards
import Concordium.Types.Updates
import Concordium.Types.Accounts

import qualified Concordium.Types.SeedState as SeedState
import Concordium.GlobalState.Basic.BlockState.AccountTable(toList)

import Concordium.Types
import System.Random
import Data.FileEmbed
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString as BS
import Concordium.Crypto.DummyData
import Concordium.ID.DummyData
import Concordium.Types.DummyData
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.P1 as P1

cryptoParamsFileContents :: BS.ByteString
cryptoParamsFileContents = $(makeRelativeToProject "testdata/global.json" >>= embedFile)

{-# WARNING dummyCryptographicParameters "Do not use in production" #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case getExactVersionedCryptographicParameters (BSL.fromStrict cryptoParamsFileContents) of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

ipsFileContents :: BS.ByteString
ipsFileContents = $(makeRelativeToProject "testdata/identity_providers.json" >>= embedFile)

{-# WARNING dummyIdentityProviders "Do not use in production." #-}
dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders =
  case eitherReadIdentityProviders (BSL.fromStrict ipsFileContents) of
    Left err -> error $ "Could not load identity provider test data: " ++ err
    Right ips -> ips

arsFileContents :: BS.ByteString
arsFileContents = $(makeRelativeToProject "testdata/anonymity_revokers.json" >>= embedFile)

{-# WARNING dummyArs "Do not use in production." #-}
dummyArs :: AnonymityRevokers
dummyArs =
  case eitherReadAnonymityRevokers (BSL.fromStrict arsFileContents) of
    Left err -> error $ "Could not load anonymity revoker data: " ++ err
    Right ars -> ars

dummyFinalizationCommitteeMaxSize :: FinalizationCommitteeSize
dummyFinalizationCommitteeMaxSize = 1000

{-# NOINLINE dummyAuthorizationKeyPair #-}
dummyAuthorizationKeyPair :: SigScheme.KeyPair
dummyAuthorizationKeyPair = uncurry SigScheme.KeyPairEd25519 . fst $ randomEd25519KeyPair (mkStdGen 881856792)

{-# NOINLINE dummyAuthorizations #-}
{-# WARNING dummyAuthorizations "Do not use in production." #-}
dummyAuthorizations :: Authorizations
dummyAuthorizations = Authorizations {
      asKeys = Vec.singleton (correspondingVerifyKey dummyAuthorizationKeyPair),
      asEmergency = theOnly,
      asProtocol = theOnly,
      asParamElectionDifficulty = theOnly,
      asParamEuroPerEnergy = theOnly,
      asParamMicroGTUPerEuro = theOnly,
      asParamFoundationAccount = theOnly,
      asParamMintDistribution = theOnly,
      asParamTransactionFeeDistribution = theOnly,
      asParamGASRewards = theOnly,
      asBakerStakeThreshold = theOnly,
      asAddAnonymityRevoker = theOnly,
      asAddIdentityProvider = theOnly
    }
  where
    theOnly = AccessStructure (Set.singleton 0) 1

{-# NOINLINE dummyHigherLevelKeys #-}
{-# WARNING dummyHigherLevelKeys "Do not use in production." #-}
dummyHigherLevelKeys :: HigherLevelKeys a
dummyHigherLevelKeys = HigherLevelKeys {
  hlkKeys = Vec.singleton (correspondingVerifyKey dummyAuthorizationKeyPair),
  hlkThreshold = 1
  }

{-# NOINLINE dummyKeyCollection #-}
{-# WARNING dummyKeyCollection "Do not use in production." #-}
dummyKeyCollection :: UpdateKeysCollection
dummyKeyCollection = UpdateKeysCollection {
  rootKeys = dummyHigherLevelKeys,
  level1Keys = dummyHigherLevelKeys,
  level2Keys = dummyAuthorizations
  }

{-# WARNING makeFakeBakers "Do not use in production" #-}
-- |Make a given number of baker accounts for use in genesis.
-- These bakers should be the first accounts in a genesis block (because
-- the baker ids must match the account indexes).
makeFakeBakers :: Word -> [GenesisAccount]
makeFakeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = account : mbs gen''' (bid + 1)
            where
                (VRF.KeyPair _ epk, gen') = VRF.randomKeyPair gen
                (sk, gen'') = randomBlockKeyPair gen'
                spk = Sig.verifyKey sk
                (blssk, gen''') = randomBlsSecretKey gen''
                blspk = Bls.derivePublicKey blssk
                account = makeFakeBakerAccount bid epk spk blspk

-- |Make a baker deterministically from a given seed and with the given reward account.
-- Uses 'bakerElectionKey' and 'bakerSignKey' with the given seed to generate the keys.
-- The baker has 0 lottery power.
-- mkBaker :: Int -> AccountAddress -> (BakerInfo
{-# WARNING mkFullBaker "Do not use in production." #-}
mkFullBaker :: Int -> BakerId -> (FullBakerInfo, VRF.SecretKey, Sig.SignKey, Bls.SecretKey)
mkFullBaker seed _bakerIdentity = (FullBakerInfo {
    _bakerInfo = BakerInfo {
      _bakerElectionVerifyKey = VRF.publicKey electionKey,
      _bakerSignatureVerifyKey = Sig.verifyKey sk,
      _bakerAggregationVerifyKey = Bls.derivePublicKey blssk,
      ..
    },
    _bakerStake = 0
  }, VRF.privateKey electionKey, Sig.signKey sk, blssk)
  where electionKey = bakerElectionKey seed
        sk = bakerSignKey seed
        blssk = bakerAggregationKey seed

{-# WARNING makeTestingGenesisDataP1 "Do not use in production" #-}
makeTestingGenesisDataP1 ::
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> FinalizationCommitteeSize -- ^Maximum number of parties in the finalization committee
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> IdentityProviders   -- ^List of initial identity providers.
    -> AnonymityRevokers -- ^Initial anonymity revokers.
    -> Energy  -- ^Maximum limit on the total stated energy of the transactions in a block
    -> UpdateKeysCollection -- ^Initial update authorizations
    -> ChainParameters -- ^Initial chain parameters
    -> GenesisData 'P1
makeTestingGenesisDataP1
  genesisTime
  nBakers
  genesisSlotDuration
  finalizationMinimumSkip
  finalizationCommitteeMaxSize
  genesisCryptographicParameters
  genesisIdentityProviders
  genesisAnonymityRevokers
  genesisMaxBlockEnergy
  genesisUpdateKeys
  genesisChainParameters
    = GDP1 P1.GDP1Initial {
        genesisCore=GenesisData.CoreGenesisParameters{..},
        genesisInitialState=GenesisData.GenesisState{..}
      }
    where
        -- todo hardcoded epoch length (and initial seed)
        genesisEpochLength = 10
        genesisLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"
        genesisFinalizationParameters =
          FinalizationParameters {
           finalizationWaitingTime = 100,
           finalizationSkipShrinkFactor = 0.8,
           finalizationSkipGrowFactor = 2,
           finalizationDelayShrinkFactor = 0.8,
           finalizationDelayGrowFactor = 2,
           finalizationAllowZeroDelay = False,
           ..
         }
        genesisAccounts = Vec.fromList $ makeFakeBakers nBakers


{-# WARNING emptyBirkParameters "Do not use in production." #-}
emptyBirkParameters :: Accounts pv -> BasicBirkParameters
emptyBirkParameters accounts = initialBirkParameters (snd <$> AT.toList (accountTable accounts)) (SeedState.initialSeedState (Hash.hash "NONCE") 360)

dummyRewardParameters :: RewardParameters
dummyRewardParameters = RewardParameters {
    _rpMintDistribution = MintDistribution {
      _mdMintPerSlot = MintRate 1 12,
      _mdBakingReward = RewardFraction 60000, -- 60%
      _mdFinalizationReward = RewardFraction 30000 -- 30%
    },
    _rpTransactionFeeDistribution = TransactionFeeDistribution {
      _tfdBaker = RewardFraction 45000, -- 45%
      _tfdGASAccount = RewardFraction 45000 -- 45%
    },
    _rpGASRewards = GASRewards {
      _gasBaker = RewardFraction 25000, -- 25%
      _gasFinalizationProof = RewardFraction 50, -- 0.05%
      _gasAccountCreation = RewardFraction 200, -- 0.2%
      _gasChainUpdate = RewardFraction 50 -- 0.05%
    }
}

dummyChainParameters :: ChainParameters
dummyChainParameters = makeChainParameters (makeElectionDifficulty 50000) 0.0001 1000000 168 10 dummyRewardParameters 0 300000000000

{-# WARNING createBlockState "Do not use in production" #-}
createBlockState :: Accounts pv -> BlockState pv
createBlockState accounts =
    emptyBlockState (emptyBirkParameters accounts) dummyCryptographicParameters dummyKeyCollection dummyChainParameters &
      (blockAccounts .~ accounts) .
      (blockBank . unhashed . Rewards.totalGTU .~ sum (map (_accountAmount . snd) (toList (accountTable accounts)))) .
      (blockIdentityProviders . unhashed .~ dummyIdentityProviders) .
      (blockAnonymityRevokers . unhashed .~ dummyArs)

{-# WARNING blockStateWithAlesAccount "Do not use in production" #-}
blockStateWithAlesAccount :: IsProtocolVersion pv => Amount -> Accounts pv -> BlockState pv
blockStateWithAlesAccount alesAmount otherAccounts =
    createBlockState $ putAccountWithRegIds (mkAccount alesVK alesAccount alesAmount) otherAccounts

-- This generates an account with a single credential and single keypair, which has sufficiently
-- late expiry date, but is otherwise not well-formed.
{-# WARNING mkAccount "Do not use in production." #-}
mkAccount :: IsProtocolVersion pv => SigScheme.VerifyKey -> AccountAddress -> Amount -> Account pv
mkAccount key addr amnt = newAccount dummyCryptographicParameters addr cred & accountAmount .~ amnt
  where
    cred = dummyCredential dummyCryptographicParameters addr key dummyMaxValidTo dummyCreatedAt

-- This generates an account with a single credential and single keypair, where
-- the credential should already be considered expired. (Its valid-to date will be
-- Jan 1000, which precedes the earliest expressible timestamp by 970 years.)
{-# WARNING mkAccountExpiredCredential "Do not use in production." #-}
mkAccountExpiredCredential :: IsProtocolVersion pv => SigScheme.VerifyKey -> AccountAddress -> Amount -> Account pv
mkAccountExpiredCredential key addr amnt = newAccount dummyCryptographicParameters addr cred & accountAmount .~ amnt
  where
    cred = dummyCredential dummyCryptographicParameters addr key dummyLowValidTo dummyCreatedAt

-- This generates an account with a single credential, the given list of keys and signature threshold,
-- which has sufficiently late expiry date, but is otherwise not well-formed.
-- The keys are indexed in ascending order starting from 0
-- The list of keys should be non-empty.
{-# WARNING mkAccountMultipleKeys "Do not use in production." #-}
mkAccountMultipleKeys :: IsProtocolVersion pv => [SigScheme.VerifyKey] -> SignatureThreshold -> AccountAddress -> Amount -> Account pv
mkAccountMultipleKeys keys threshold addr amount = newAccount dummyCryptographicParameters addr cred & accountAmount .~ amount & accountVerificationKeys .~ ai

  where
    cred = dummyCredential dummyCryptographicParameters addr (head keys) dummyMaxValidTo dummyCreatedAt
    ai = AccountInformation (Map.singleton 0 $ makeCredentialPublicKeys keys threshold) 1

-- |Make a baker account with the given baker verification keys and account keys that are seeded from the baker id.
{-# WARNING makeFakeBakerAccount "Do not use in production." #-}
makeFakeBakerAccount :: BakerId -> BakerElectionVerifyKey -> BakerSignVerifyKey -> BakerAggregationVerifyKey -> GenesisAccount
makeFakeBakerAccount gbBakerId gbElectionVerifyKey gbSignatureVerifyKey gbAggregationVerifyKey = GenesisAccount {..}
  where
    gaBalance = 1000000000000
    gbStake = 999000000000
    gbRestakeEarnings = True    
    gaBaker = Just GenesisBaker {..}
    vfKey = SigScheme.correspondingVerifyKey kp
    gaCredentials = Map.singleton 0 $ dummyCredential dummyCryptographicParameters gaAddress vfKey dummyMaxValidTo dummyCreatedAt
    -- gaVerifyKeys = makeSingletonAC vfKey
    gaThreshold = 1
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral gbBakerId) - 1
    (gaAddress, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')

createCustomAccount :: Amount -> SigScheme.KeyPair -> AccountAddress -> GenesisAccount
createCustomAccount amount kp address = GenesisAccount {
      gaAddress = address,
      gaThreshold = 1,
      gaBalance = amount,
      gaCredentials = Map.singleton 0 credential,
      gaBaker = Nothing
    }
  where credential = dummyCredential dummyCryptographicParameters address vfKey dummyMaxValidTo dummyCreatedAt
        vfKey = SigScheme.correspondingVerifyKey kp
