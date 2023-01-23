{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Concordium.GlobalState.DummyData where

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.ID.Types
import Concordium.Types.Accounts
import Concordium.Types.AnonymityRevokers
import Concordium.Types.IdentityProviders
import Concordium.Types.Updates
import qualified Data.Map.Strict as Map
import Data.Ratio
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform

import qualified Concordium.GlobalState.Basic.BlockState.PoolRewards as PoolRewards

import Concordium.Crypto.DummyData
import qualified Concordium.Genesis.Data as GenesisData
import qualified Concordium.Genesis.Data.P1 as P1
import qualified Concordium.Genesis.Data.P5 as P5
import Concordium.ID.DummyData
import Concordium.Types
import Concordium.Types.DummyData
import Concordium.Types.Execution
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.FileEmbed
import Data.Singletons
import System.Random

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
dummyAuthorizations :: forall auv. IsAuthorizationsVersion auv => Authorizations auv
dummyAuthorizations =
    Authorizations
        { asKeys = Vec.singleton (correspondingVerifyKey dummyAuthorizationKeyPair),
          asEmergency = theOnly,
          asProtocol = theOnly,
          asParamConsensusParameters = theOnly,
          asParamEuroPerEnergy = theOnly,
          asParamMicroGTUPerEuro = theOnly,
          asParamFoundationAccount = theOnly,
          asParamMintDistribution = theOnly,
          asParamTransactionFeeDistribution = theOnly,
          asParamGASRewards = theOnly,
          asPoolParameters = theOnly,
          asAddAnonymityRevoker = theOnly,
          asAddIdentityProvider = theOnly,
          asCooldownParameters = conditionally (sSupportsCooldownParametersAccessStructure (sing @auv)) theOnly,
          asTimeParameters = conditionally (sSupportsTimeParameters (sing @auv)) theOnly
        }
  where
    theOnly = AccessStructure (Set.singleton 0) 1

{-# NOINLINE dummyHigherLevelKeys #-}
{-# WARNING dummyHigherLevelKeys "Do not use in production." #-}
dummyHigherLevelKeys :: HigherLevelKeys a
dummyHigherLevelKeys =
    HigherLevelKeys
        { hlkKeys = Vec.singleton (correspondingVerifyKey dummyAuthorizationKeyPair),
          hlkThreshold = 1
        }

{-# NOINLINE dummyKeyCollection #-}
{-# WARNING dummyKeyCollection "Do not use in production." #-}
dummyKeyCollection :: IsAuthorizationsVersion auv => UpdateKeysCollection auv
dummyKeyCollection =
    UpdateKeysCollection
        { rootKeys = dummyHigherLevelKeys,
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
mkFullBaker seed _bakerIdentity =
    ( FullBakerInfo
        { _theBakerInfo =
            BakerInfo
                { _bakerElectionVerifyKey = VRF.publicKey electionKey,
                  _bakerSignatureVerifyKey = Sig.verifyKey sk,
                  _bakerAggregationVerifyKey = Bls.derivePublicKey blssk,
                  ..
                },
          _bakerStake = 0
        },
      VRF.privateKey electionKey,
      Sig.signKey sk,
      blssk
    )
  where
    electionKey = bakerElectionKey seed
    sk = bakerSignKey seed
    blssk = bakerAggregationKey seed

{-# WARNING makeTestingGenesisDataP1 "Do not use in production" #-}
makeTestingGenesisDataP1 ::
    -- |Genesis time
    Timestamp ->
    -- |Initial number of bakers.
    Word ->
    -- |Slot duration in seconds.
    Duration ->
    -- |Minimum finalization interval - 1
    BlockHeight ->
    -- |Maximum number of parties in the finalization committee
    FinalizationCommitteeSize ->
    -- |Initial cryptographic parameters.
    CryptographicParameters ->
    -- |List of initial identity providers.
    IdentityProviders ->
    -- |Initial anonymity revokers.
    AnonymityRevokers ->
    -- |Maximum limit on the total stated energy of the transactions in a block
    Energy ->
    -- |Initial update authorizations
    UpdateKeysCollection 'AuthorizationsVersion0 ->
    -- |Initial chain parameters
    ChainParameters 'P1 ->
    GenesisData 'P1
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
    genesisChainParameters =
        GDP1
            P1.GDP1Initial
                { genesisCore = GenesisData.CoreGenesisParameters{..},
                  genesisInitialState = GenesisData.GenesisState{..}
                }
      where
        genesisEpochLength = 10
        genesisLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"
        genesisFinalizationParameters =
            FinalizationParameters
                { finalizationWaitingTime = 100,
                  finalizationSkipShrinkFactor = 0.8,
                  finalizationSkipGrowFactor = 2,
                  finalizationDelayShrinkFactor = 0.8,
                  finalizationDelayGrowFactor = 2,
                  finalizationAllowZeroDelay = False,
                  ..
                }
        genesisAccounts = Vec.fromList $ makeFakeBakers nBakers

{-# WARNING makeTestingGenesisDataP5 "Do not use in production" #-}
makeTestingGenesisDataP5 ::
    -- |Genesis time
    Timestamp ->
    -- |Initial number of bakers.
    Word ->
    -- |Slot duration in seconds.
    Duration ->
    -- |Minimum finalization interval - 1
    BlockHeight ->
    -- |Maximum number of parties in the finalization committee
    FinalizationCommitteeSize ->
    -- |Initial cryptographic parameters.
    CryptographicParameters ->
    -- |List of initial identity providers.
    IdentityProviders ->
    -- |Initial anonymity revokers.
    AnonymityRevokers ->
    -- |Maximum limit on the total stated energy of the transactions in a block
    Energy ->
    -- |Initial update authorizations
    UpdateKeysCollection 'AuthorizationsVersion1 ->
    -- |Initial chain parameters
    ChainParameters 'P5 ->
    GenesisData 'P5
makeTestingGenesisDataP5
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
    genesisChainParameters =
        GDP5
            P5.GDP5Initial
                { genesisCore = GenesisData.CoreGenesisParameters{..},
                  genesisInitialState = GenesisData.GenesisState{..}
                }
      where
        -- todo hardcoded epoch length (and initial seed)
        genesisEpochLength = 10
        genesisLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"
        genesisFinalizationParameters =
            FinalizationParameters
                { finalizationWaitingTime = 100,
                  finalizationSkipShrinkFactor = 0.8,
                  finalizationSkipGrowFactor = 2,
                  finalizationDelayShrinkFactor = 0.8,
                  finalizationDelayGrowFactor = 2,
                  finalizationAllowZeroDelay = False,
                  ..
                }
        genesisAccounts = Vec.fromList $ makeFakeBakers nBakers

dummyRewardParametersV0 :: RewardParameters 'ChainParametersV0
dummyRewardParametersV0 =
    RewardParameters
        { _rpMintDistribution =
            MintDistribution
                { _mdMintPerSlot = CTrue $ MintRate 1 12,
                  _mdBakingReward = AmountFraction 60000, -- 60%
                  _mdFinalizationReward = AmountFraction 30000 -- 30%
                },
          _rpTransactionFeeDistribution =
            TransactionFeeDistribution
                { _tfdBaker = AmountFraction 45000, -- 45%
                  _tfdGASAccount = AmountFraction 45000 -- 45%
                },
          _rpGASRewards =
            GASRewards
                { _gasBaker = AmountFraction 25000, -- 25%
                  _gasFinalizationProof = CTrue $ AmountFraction 50, -- 0.05%
                  _gasAccountCreation = AmountFraction 200, -- 0.2%
                  _gasChainUpdate = AmountFraction 50 -- 0.05%
                }
        }

dummyRewardParametersV1 :: RewardParameters 'ChainParametersV1
dummyRewardParametersV1 =
    RewardParameters
        { _rpMintDistribution =
            MintDistribution
                { _mdMintPerSlot = CFalse,
                  _mdBakingReward = AmountFraction 60000, -- 60%
                  _mdFinalizationReward = AmountFraction 30000 -- 30%
                },
          _rpTransactionFeeDistribution =
            TransactionFeeDistribution
                { _tfdBaker = AmountFraction 45000, -- 45%
                  _tfdGASAccount = AmountFraction 45000 -- 45%
                },
          _rpGASRewards =
            GASRewards
                { _gasBaker = AmountFraction 25000, -- 25%
                  _gasFinalizationProof = CTrue $ AmountFraction 50, -- 0.05%
                  _gasAccountCreation = AmountFraction 200, -- 0.2%
                  _gasChainUpdate = AmountFraction 50 -- 0.05%
                }
        }

dummyRewardParametersV2 :: RewardParameters 'ChainParametersV2
dummyRewardParametersV2 =
    RewardParameters
        { _rpMintDistribution =
            MintDistribution
                { _mdMintPerSlot = CFalse,
                  _mdBakingReward = AmountFraction 60000, -- 60%
                  _mdFinalizationReward = AmountFraction 30000 -- 30%
                },
          _rpTransactionFeeDistribution =
            TransactionFeeDistribution
                { _tfdBaker = AmountFraction 45000, -- 45%
                  _tfdGASAccount = AmountFraction 45000 -- 45%
                },
          _rpGASRewards =
            GASRewards
                { _gasBaker = AmountFraction 25000, -- 25%
                  _gasFinalizationProof = CFalse,
                  _gasAccountCreation = AmountFraction 200, -- 0.2%
                  _gasChainUpdate = AmountFraction 50 -- 0.05%
                }
        }

dummyConsensusParametersV1 :: ConsensusParameters' 'ConsensusParametersVersion1
dummyConsensusParametersV1 =
    ConsensusParametersV1
        { _cpTimeoutParameters =
            TimeoutParameters
                { tpTimeoutBase = 10000,
                  tpTimeoutIncrease = 2 % 1,
                  tpTimeoutDecrease = 4 % 5
                },
          _cpMinBlockTime = 1000,
          _cpBlockEnergyLimit = maxBound
        }

dummyChainParameters :: forall cpv. IsChainParametersVersion cpv => ChainParameters' cpv
dummyChainParameters = case chainParametersVersion @cpv of
    SChainParametersV0 ->
        ChainParameters
            { _cpConsensusParameters = ConsensusParametersV0 $ makeElectionDifficulty 50000,
              _cpExchangeRates = makeExchangeRates 0.0001 1000000,
              _cpCooldownParameters =
                CooldownParametersV0
                    { _cpBakerExtraCooldownEpochs = 168
                    },
              _cpTimeParameters = NoParam,
              _cpAccountCreationLimit = 10,
              _cpRewardParameters = dummyRewardParametersV0,
              _cpFoundationAccount = 0,
              _cpPoolParameters =
                PoolParametersV0
                    { _ppBakerStakeThreshold = 300000000000
                    }
            }
    SChainParametersV1 ->
        ChainParameters
            { _cpConsensusParameters = ConsensusParametersV0 $ makeElectionDifficulty 50000,
              _cpExchangeRates = makeExchangeRates 0.0001 1000000,
              _cpCooldownParameters =
                CooldownParametersV1
                    { _cpPoolOwnerCooldown = cooldown,
                      _cpDelegatorCooldown = cooldown
                    },
              _cpTimeParameters =
                SomeParam
                    TimeParametersV1
                        { _tpRewardPeriodLength = 2,
                          _tpMintPerPayday = MintRate 1 8
                        },
              _cpAccountCreationLimit = 10,
              _cpRewardParameters = dummyRewardParametersV1,
              _cpFoundationAccount = 0,
              _cpPoolParameters =
                PoolParametersV1
                    { _ppMinimumEquityCapital = 300000000000,
                      _ppCapitalBound = CapitalBound (makeAmountFraction 100000),
                      _ppLeverageBound = 5,
                      _ppPassiveCommissions =
                        CommissionRates
                            { _finalizationCommission = makeAmountFraction 100000,
                              _bakingCommission = makeAmountFraction 5000,
                              _transactionCommission = makeAmountFraction 5000
                            },
                      _ppCommissionBounds =
                        CommissionRanges
                            { _finalizationCommissionRange = fullRange,
                              _bakingCommissionRange = fullRange,
                              _transactionCommissionRange = fullRange
                            }
                    }
            }
    SChainParametersV2 ->
        ChainParameters
            { _cpConsensusParameters = dummyConsensusParametersV1,
              _cpExchangeRates = makeExchangeRates 0.0001 1000000,
              _cpCooldownParameters =
                CooldownParametersV1
                    { _cpPoolOwnerCooldown = cooldown,
                      _cpDelegatorCooldown = cooldown
                    },
              _cpTimeParameters =
                SomeParam
                    TimeParametersV1
                        { _tpRewardPeriodLength = 2,
                          _tpMintPerPayday = MintRate 1 8
                        },
              _cpAccountCreationLimit = 10,
              _cpRewardParameters = dummyRewardParametersV2,
              _cpFoundationAccount = 0,
              _cpPoolParameters =
                PoolParametersV1
                    { _ppMinimumEquityCapital = 300000000000,
                      _ppCapitalBound = CapitalBound (makeAmountFraction 100000),
                      _ppLeverageBound = 5,
                      _ppPassiveCommissions =
                        CommissionRates
                            { _finalizationCommission = makeAmountFraction 100000,
                              _bakingCommission = makeAmountFraction 5000,
                              _transactionCommission = makeAmountFraction 5000
                            },
                      _ppCommissionBounds =
                        CommissionRanges
                            { _finalizationCommissionRange = fullRange,
                              _bakingCommissionRange = fullRange,
                              _transactionCommissionRange = fullRange
                            }
                    }
            }
  where
    fullRange = InclusiveRange (makeAmountFraction 0) (makeAmountFraction 100000)
    cooldown = DurationSeconds (24 * 60 * 60)

createPoolRewards :: Accounts pv -> PoolRewards.PoolRewards
createPoolRewards accounts = PoolRewards.makeInitialPoolRewards capDist 1 (MintRate 1 10)
  where
    (bakersMap, passive) = foldr accumDelegations (Map.empty, []) (AT.toList (accountTable accounts))
    bakers = [(bid, amt, dlgs) | (bid, (amt, dlgs)) <- Map.toList bakersMap]
    capDist = makeCapitalDistribution bakers passive
    accumDelegations (ai, acct) acc@(bm, lp) = case acct ^. accountStaking of
        AccountStakeNone -> acc
        AccountStakeBaker bkr -> (bm & at (BakerId ai) . non (0, []) . _1 .~ bkr ^. stakedAmount, lp)
        AccountStakeDelegate dlg -> case dlg ^. delegationTarget of
            DelegatePassive -> (bm, d : lp)
            DelegateToBaker bkrid -> (bm & at bkrid . non (0, []) . _2 %~ (d :), lp)
          where
            d = (dlg ^. delegationIdentity, dlg ^. delegationStakedAmount)

-- |Make a baker account with the given baker verification keys and account keys that are seeded from the baker id.
{-# WARNING makeFakeBakerAccount "Do not use in production." #-}
makeFakeBakerAccount :: BakerId -> BakerElectionVerifyKey -> BakerSignVerifyKey -> BakerAggregationVerifyKey -> GenesisAccount
makeFakeBakerAccount gbBakerId gbElectionVerifyKey gbSignatureVerifyKey gbAggregationVerifyKey = GenesisAccount{..}
  where
    gaBalance = 1000000000000
    gbStake = 999000000000
    gbRestakeEarnings = True
    gaBaker = Just GenesisBaker{..}
    vfKey = SigScheme.correspondingVerifyKey kp
    gaCredentials = Map.singleton 0 $ dummyCredential dummyCryptographicParameters gaAddress vfKey dummyMaxValidTo dummyCreatedAt
    -- gaVerifyKeys = makeSingletonAC vfKey
    gaThreshold = 1
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = -(fromIntegral gbBakerId) - 1
    (gaAddress, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')

createCustomAccount :: Amount -> SigScheme.KeyPair -> AccountAddress -> GenesisAccount
createCustomAccount amount kp address =
    GenesisAccount
        { gaAddress = address,
          gaThreshold = 1,
          gaBalance = amount,
          gaCredentials = Map.singleton 0 credential,
          gaBaker = Nothing
        }
  where
    credential = dummyCredential dummyCryptographicParameters address vfKey dummyMaxValidTo dummyCreatedAt
    vfKey = SigScheme.correspondingVerifyKey kp
