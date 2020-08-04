{-# LANGUAGE
    OverloadedStrings,
    TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- |This module provides functionality for generating startup data for
-- testing purposes.  It should not be used in production.
module Concordium.Startup {-# WARNING "This module should not be used in production code." #-} where

import System.Random
import Lens.Micro.Platform

import qualified Data.ByteString.Lazy.Char8 as BSL
import System.IO.Unsafe

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers (bakersFromList)
import qualified Concordium.GlobalState.SeedState as SeedState
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.Birk.Bake
import Concordium.Types
import Concordium.Types.Updates
import Concordium.ID.Types(randomAccountAddress, makeSingletonAC)
import Concordium.Crypto.DummyData
import Concordium.ID.DummyData

makeBakers :: Word -> [((BakerIdentity, FullBakerInfo), Account)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = ((BakerIdentity sk ek blssk, FullBakerInfo (BakerInfo epk spk blspk accAddress) stake), account):mbs gen''' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = randomBlockKeyPair gen'
                spk = Sig.verifyKey sk
                (blssk, gen''') = randomBlsSecretKey gen''
                blspk = Bls.derivePublicKey blssk
                accAddress = account ^. accountAddress
                stake = account ^. accountAmount
                account = makeBakerAccount bid (if bid `mod` 2 == 0 then 1200000000000 else 800000000000)

-- Note that the credentials on the baker account are not valid, apart from their expiry is the maximum possible.
makeBakerAccountKP :: BakerId -> Amount -> (Account, SigScheme.KeyPair)
makeBakerAccountKP bid amount =
    (acct, kp)
  where
    vfKey = SigScheme.correspondingVerifyKey kp
    credential = dummyCredential address dummyMaxValidTo dummyCreatedAt
    acct = newAccount (makeSingletonAC vfKey) address credential
                & accountAmount .~ amount
                & accountStakeDelegate ?~ bid
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral bid) - 1
    (address, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')

makeBakerAccount :: BakerId -> Amount -> Account
makeBakerAccount bid = fst . makeBakerAccountKP bid

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
    -> Duration  -- ^Slot duration in seconds.
    -> FinalizationParameters -- ^Finalization parameters
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> IdentityProviders   -- ^List of initial identity providers.
    -> AnonymityRevokers -- ^Initial anonymity revokers.
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
        genesisMaxBlockEnergy
        genesisAuthorizations
        genesisChainParameters
    = (GenesisData{..}, bakers)
    where
        genesisMintPerSlot = 10 -- default value, OK for testing.
        genesisBakers = fst (bakersFromList (snd <$> bakers))
        genesisSeedState = SeedState.genesisSeedState (Hash.hash "LeadershipElectionNonce") 10 -- todo hardcoded epoch length (and initial seed)
        (bakers, genesisAccounts) = unzip (makeBakers nBakers)


{-# WARNING dummyCryptographicParameters "Do not use in production" #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (getExactVersionedCryptographicParameters <$> BSL.readFile "scheduler/testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params
