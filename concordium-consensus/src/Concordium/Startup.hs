{-# LANGUAGE
    OverloadedStrings,
    TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- |This module provides functionality for generating startup data for
-- testing purposes.  It should not be used in production.
module Concordium.Startup {-# WARNING "This module should not be used in production code." #-} where

import System.Random
import Lens.Micro.Platform
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.BakerInfo
import qualified Concordium.Types.SeedState as SeedState
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Birk.Bake
import Concordium.Types
import Concordium.Types.Updates
import Concordium.ID.Types(randomAccountAddress, makeSingletonAC)
import Concordium.Crypto.DummyData
import Concordium.GlobalState.DummyData
import Concordium.ID.DummyData

makeBakersByStake :: [Amount] -> [(BakerIdentity, FullBakerInfo, GenesisAccount, SigScheme.KeyPair)]
makeBakersByStake = mbs 0
    where
        mbs _ [] = []
        mbs bid (s:ss)= (ident, binfo, account, kp):mbs (bid+1) ss
            where
                (account, kp, ident) = makeBakerAccountKeys bid s
                binfo = FullBakerInfo {
                    _bakerInfo = BakerInfo {
                        _bakerIdentity = bakerId ident,
                        _bakerElectionVerifyKey = bakerElectionPublicKey ident,
                        _bakerSignatureVerifyKey = bakerSignPublicKey ident,
                        _bakerAggregationVerifyKey = bakerAggregationPublicKey ident
                    },
                    _bakerStake = gbStake (fromJust (gaBaker account))
                }

makeBakers :: Word -> [(BakerIdentity, FullBakerInfo, GenesisAccount, SigScheme.KeyPair)]
makeBakers nBakers = makeBakersByStake [if even bid then 1200000000000 else 800000000000 | bid <- [0..nBakers-1]]

generateBakerKeys :: BakerId -> BakerIdentity
generateBakerKeys bakerId = BakerIdentity{..}
    where
        gen0 = mkStdGen (fromIntegral bakerId)
        (bakerSignKey, gen1) = randomBlockKeyPair gen0
        (bakerElectionKey, gen2) = VRF.randomKeyPair gen1
        (bakerAggregationKey, _) = randomBlsSecretKey gen2
        bakerAggregationPublicKey = Bls.derivePublicKey bakerAggregationKey

-- |Creates a baker account and keys, with 99% of the account's balance staked.
-- Note that the credentials on the baker account are not valid, apart from their expiry is the maximum possible.
makeBakerAccountKeys :: BakerId -> Amount -> (GenesisAccount, SigScheme.KeyPair, BakerIdentity)
makeBakerAccountKeys bid amount =
    (acct, kp, bkr)
  where
    vfKey = SigScheme.correspondingVerifyKey kp
    credential = dummyCredential dummyCryptographicParameters address vfKey dummyMaxValidTo dummyCreatedAt
    acct = GenesisAccount {
        gaAddress = address,
        gaVerifyKeys = makeSingletonAC vfKey,
        gaBalance = amount,
        gaCredentials = credential :| [],
        gaBaker = Just GenesisBaker {
                gbElectionVerifyKey = VRF.publicKey (bakerElectionKey bkr),
                gbSignatureVerifyKey = Sig.verifyKey (bakerSignKey bkr),
                gbAggregationVerifyKey = bakerAggregationPublicKey bkr,
                gbStake = amount - (amount `div` 100),
                gbRestakeEarnings = False,
                gbBakerId = bid
            }
        }
    bkr = generateBakerKeys bid
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral bid) - 1
    (address, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')

makeBakerAccount :: BakerId -> Amount -> GenesisAccount
makeBakerAccount bid = (^. _1) . makeBakerAccountKeys bid

defaultFinalizationParameters :: FinalizationParameters
defaultFinalizationParameters = FinalizationParameters {
    finalizationMinimumSkip = 0,
    finalizationCommitteeMaxSize = 1000,
    finalizationWaitingTime = 100,
    finalizationIgnoreFirstWait = False,
    finalizationOldStyleSkip = False,
    finalizationSkipShrinkFactor = 0.8,
    finalizationSkipGrowFactor = 2,
    finalizationDelayShrinkFactor = 0.8,
    finalizationDelayGrowFactor = 2,
    finalizationAllowZeroDelay = False
}

makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration (miliseconds).
    -> FinalizationParameters -- ^Finalization parameters
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> IdentityProviders   -- ^List of initial identity providers.
    -> AnonymityRevokers -- ^Initial anonymity revokers.
    -> [GenesisAccount] -- ^Additional accounts.
    -> Energy -- ^Maximum energy allowed to be consumed by the transactions in a block
    -> Authorizations -- ^Authorizations for chain updates
    -> ChainParameters -- ^Initial chain parameters
    -> (GenesisData, [(BakerIdentity, FullBakerInfo)])
makeGenesisData
        genesisTime
        nBakers
        genesisSlotDuration
        genesisFinalizationParameters
        genesisCryptographicParameters
        genesisIdentityProviders
        genesisAnonymityRevokers
        additionalAccounts
        genesisMaxBlockEnergy
        genesisAuthorizations
        genesisChainParameters
    = (GenesisDataV2{..}, bakers)
    where
        genesisSeedState = SeedState.initialSeedState (Hash.hash "LeadershipElectionNonce") 10 -- todo hardcoded epoch length (and initial seed)
        mbkrs = makeBakers nBakers
        bakers = (\(bid,binfo,_,_) -> (bid,binfo)) <$> mbkrs
        bakerAccounts = (\(_,_,bacc,_) -> bacc) <$> mbkrs
        genesisAccounts = bakerAccounts ++ additionalAccounts
