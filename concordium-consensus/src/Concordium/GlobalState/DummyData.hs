{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Concordium.GlobalState.DummyData where

import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform
import qualified Acorn.Utils.Init as Acorn
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.ID.Types
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.Modules as Modules
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards as Rewards

import qualified Concordium.GlobalState.SeedState as SeedState
import Concordium.GlobalState.Basic.BlockState.AccountTable(toList)

import Concordium.Types
import System.Random
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO.Unsafe
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.Crypto.DummyData
import Concordium.ID.DummyData
import Concordium.Types.DummyData

{-# WARNING basicGenesisState "Do not use in production" #-}
basicGenesisState :: GenesisData -> Basic.BlockState
basicGenesisState genData = Basic.initialState
                       (Basic.BasicBirkParameters {
                            _birkElectionDifficulty = genesisElectionDifficulty genData,
                            _birkCurrentBakers = genesisBakers genData,
                            _birkPrevEpochBakers = genesisBakers genData,
                            _birkLotteryBakers = genesisBakers genData,
                            _birkSeedState = genesisSeedState genData
                        })
                       (genesisCryptographicParameters genData)
                       (genesisAccounts genData ++ genesisControlAccounts genData)
                       (genesisIdentityProviders genData)
                       (genesisAnonymityRevokers genData)
                       (genesisMintPerSlot genData)

-- kp :: Int -> Sig.KeyPair
-- kp n = fst (Sig.randomKeyPair (mkStdGen n))

-- proofKP :: Int -> VRF.KeyPair
-- proofKP n = fst (VRF.randomKeyPair (mkStdGen n))

{-# WARNING dummyCryptographicParameters "Do not use in production" #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (readCryptographicParameters <$> BSL.readFile "testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

{-# NOINLINE dummyIdentityProviders #-}
{-# WARNING dummyIdentityProviders "Do not use in production." #-}
dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders =
  case unsafePerformIO (eitherReadIdentityProviders <$> BSL.readFile "testdata/identity_providers.json") of
    Left err -> error $ "Could not load identity provider test data: " ++ err
    Right ips -> IdentityProviders (HM.fromList (map (\r -> (ipIdentity r, r)) ips))


{-# NOINLINE dummyArs #-}
{-# WARNING dummyArs "Do not use in production." #-}
dummyArs :: AnonymityRevokers
dummyArs = 
  case unsafePerformIO (eitherReadAnonymityRevokers <$> BSL.readFile "testdata/anonymity_revokers.json") of
    Left err -> error $ "Could not load anonymity revoker data: " ++ err
    Right ars -> ars

dummyFinalizationCommitteeMaxSize :: FinalizationCommitteeSize
dummyFinalizationCommitteeMaxSize = 1000

{-# WARNING makeFakeBakers "Do not use in production" #-}
makeFakeBakers :: Word -> [(FullBakerInfo, Account)]
makeFakeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = (FullBakerInfo (BakerInfo epk spk blspk accAddress) stake, account):mbs gen''' (bid+1)
            where
                ((VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = randomBlockKeyPair gen'
                spk = Sig.verifyKey sk
                (blssk, gen''') = randomBlsSecretKey gen''
                blspk = Bls.derivePublicKey blssk
                accAddress = account ^. accountAddress
                stake = account ^. accountAmount
                account = makeFakeBakerAccount bid

-- |Make a baker deterministically from a given seed and with the given reward account.
-- Uses 'bakerElectionKey' and 'bakerSignKey' with the given seed to generate the keys.
-- The baker has 0 lottery power.
-- mkBaker :: Int -> AccountAddress -> (BakerInfo
{-# WARNING mkFullBaker "Do not use in production." #-}
mkFullBaker :: Int -> AccountAddress -> (FullBakerInfo, VRF.SecretKey, Sig.SignKey, Bls.SecretKey)
mkFullBaker seed acc = (FullBakerInfo {
    _bakerInfo = BakerInfo {
      _bakerElectionVerifyKey = VRF.publicKey electionKey,
      _bakerSignatureVerifyKey = Sig.verifyKey sk,
      _bakerAggregationVerifyKey = Bls.derivePublicKey blssk,
      _bakerAccount = acc
    },
    _bakerStake = 0
  }, VRF.privateKey electionKey, Sig.signKey sk, blssk)
  where electionKey = bakerElectionKey seed
        sk = bakerSignKey seed
        blssk = bakerAggregationKey seed

{-# WARNING makeTestingGenesisData "Do not use in production" #-}
makeTestingGenesisData ::
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> ElectionDifficulty  -- ^Initial election difficulty.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> FinalizationCommitteeSize -- ^Maximum number of parties in the finalization committee
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> [IpInfo]   -- ^List of initial identity providers.
    -> AnonymityRevokers -- ^Initial anonymity revokers.
    -> [Account]  -- ^List of starting genesis special accounts (in addition to baker accounts).
    -> Energy  -- ^Maximum limit on the total stated energy of the transactions in a block
    -> GenesisData
makeTestingGenesisData
  genesisTime
  nBakers
  genesisSlotDuration
  elecDiff
  finalizationMinimumSkip
  finalizationCommitteeMaxSize
  genesisCryptographicParameters
  genesisIdentityProviders
  genesisAnonymityRevokers
  genesisControlAccounts
  genesisMaxBlockEnergy
    = GenesisData{..}
    where
        genesisMintPerSlot = 10 -- default value, OK for testing.
        genesisBakers = fst (bakersFromList bakers)
        genesisSeedState = SeedState.genesisSeedState (Hash.hash "LeadershipElectionNonce") 10 -- todo hardcoded epoch length (and initial seed)
        genesisElectionDifficulty = elecDiff
        genesisFinalizationParameters =
          FinalizationParameters{
           finalizationWaitingTime = 100,
           finalizationIgnoreFirstWait = False,
           finalizationOldStyleSkip = False,
           finalizationSkipShrinkFactor = 0.8,
           finalizationSkipGrowFactor = 2,
           finalizationDelayShrinkFactor = 0.8,
           finalizationDelayGrowFactor = 2,
           finalizationAllowZeroDelay = False,
           ..
           }
        (bakers, genesisAccounts) = unzip (makeFakeBakers nBakers)

{-# WARNING emptyBirkParameters "Do not use in production." #-}
emptyBirkParameters :: BasicBirkParameters
emptyBirkParameters = BasicBirkParameters {
  _birkElectionDifficulty = 0.5,
  _birkCurrentBakers = emptyBakers,
  _birkPrevEpochBakers = emptyBakers,
  _birkLotteryBakers = emptyBakers,
  _birkSeedState = SeedState.genesisSeedState (Hash.hash "NONCE") 360
  }

{-# WARNING createBlockState "Do not use in production" #-}
createBlockState :: Accounts -> BlockState
createBlockState accounts =
    emptyBlockState emptyBirkParameters dummyCryptographicParameters &
      (blockAccounts .~ accounts) .
      (blockBank . Rewards.totalGTU .~ sum (map (_accountAmount . snd) (toList (accountTable accounts)))) .
      (blockModules .~ (let (_, _, gs) = Acorn.baseState in Modules.fromModuleList (Acorn.moduleList gs))) .
      (blockIdentityProviders .~ dummyIdentityProviders) .
      (blockAnonymityRevokers .~ dummyArs)

{-# WARNING blockStateWithAlesAccount "Do not use in production" #-}
blockStateWithAlesAccount :: Amount -> Accounts -> BlockState
blockStateWithAlesAccount alesAmount otherAccounts =
    createBlockState $ putAccountWithRegIds (mkAccount alesVK alesAccount alesAmount) otherAccounts

-- This generates an account with a single credential and single keypair, which has sufficiently
-- late expiry date, but is otherwise not well-formed.
{-# WARNING mkAccount "Do not use in production." #-}
mkAccount :: SigScheme.VerifyKey -> AccountAddress -> Amount -> Account
mkAccount key addr amnt = newAccount (makeSingletonAC key) addr cred & accountAmount .~ amnt
  where
    cred = dummyCredential addr dummyMaxValidTo dummyCreatedAt

-- This generates an account with a single credential and single keypair, where
-- the credential should already be considered expired. (Its valid-to date will be
-- Jan 1000, which precedes the earliest expressible timestamp by 970 years.)
{-# WARNING mkAccountExpiredCredential "Do not use in production." #-}
mkAccountExpiredCredential :: SigScheme.VerifyKey -> AccountAddress -> Amount -> Account
mkAccountExpiredCredential key addr amnt = newAccount (makeSingletonAC key) addr cred & accountAmount .~ amnt
  where
    cred = dummyCredential addr dummyLowValidTo dummyCreatedAt


{-# WARNING makeFakeBakerAccount "Do not use in production." #-}
makeFakeBakerAccount :: BakerId -> Account
makeFakeBakerAccount bid =
    acct & accountAmount .~ 1000000000000
      & accountStakeDelegate ?~ bid
  where
    vfKey = SigScheme.correspondingVerifyKey kp
    credential = dummyCredential address dummyMaxValidTo dummyCreatedAt
    acct = newAccount (makeSingletonAC vfKey) address credential
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral bid) - 1
    (address, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')