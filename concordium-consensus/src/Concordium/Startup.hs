{-# LANGUAGE
    OverloadedStrings,
    TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- |This module provides functionality for generating startup data for
-- testing purposes.  It should not be used in production.
module Concordium.Startup {-# WARNING "This module should not be used in production code." #-} where

import System.Random
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import qualified Data.PQueue.Prio.Max as Queue


import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState.IdentityProviders
import Concordium.Birk.Bake
import Concordium.Types
import Concordium.ID.Types(randomAccountAddress, makeSingletonAC)
import Concordium.Crypto.DummyData
import Concordium.Scheduler.Utils.Init.Example(dummyCredential, dummyMaxExpiryTime, dummyCreationTime)

import TH.RelativePaths

makeBakers :: Word -> [((BakerIdentity,BakerInfo), Account)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = ((BakerIdentity sk ek blssk, BakerInfo epk spk blspk stake accAddress), account):mbs gen''' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = randomBlockKeyPair gen'
                spk = Sig.verifyKey sk
                (blssk, gen''') = randomBlsSecretKey gen''
                blspk = Bls.derivePublicKey blssk
                accAddress = _accountAddress account
                stake = _accountAmount account
                account = makeBakerAccount bid

-- Note that the credentials on the baker account are not valid, apart from their expiry is the maximum possible.
makeBakerAccount :: BakerId -> Account
makeBakerAccount bid =
    acct {_accountAmount = 1000000000000,
          _accountStakeDelegate = Just bid,
          _accountCredentials = credentialList}
  where
    vfKey = SigScheme.correspondingVerifyKey kp
    credentialList = Queue.singleton dummyMaxExpiryTime (dummyCredential address dummyMaxExpiryTime dummyCreationTime)
    acct = newAccount (makeSingletonAC vfKey) address
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral bid) - 1
    (address, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (randomEd25519KeyPair seed')

makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> ElectionDifficulty  -- ^Initial election difficulty.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> [IpInfo]   -- ^List of initial identity providers.
    -> [Account]  -- ^List of starting genesis special accounts (in addition to baker accounts).
    -> Energy -- ^Maximum energy allowed to be consumed by the transactions in a block
    -> (GenesisData, [(BakerIdentity,BakerInfo)])
makeGenesisData
        genesisTime
        nBakers
        genesisSlotDuration
        elecDiff
        finMinSkip
        genesisCryptographicParameters
        genesisIdentityProviders
        genesisSpecialBetaAccounts
        genesisMaxBlockEnergy
    = (GenesisData{..}, bakers)
    where
        genesisMintPerSlot = 10 -- default value, OK for testing.
        genesisBakers = fst (bakersFromList (snd <$> bakers))
        genesisBirkParameters =
            BirkParameters elecDiff -- voting power
                          genesisBakers
                          genesisBakers
                          genesisBakers
                          (genesisSeedState (Hash.hash "LeadershipElectionNonce") 10) -- todo hardcoded epoch length (and initial seed)
        genesisFinalizationParameters = FinalizationParameters [VoterInfo vvk vrfk 1 vblsk | (_, BakerInfo vrfk vvk vblsk _ _) <- bakers] finMinSkip
        (bakers, genesisAccounts) = unzip (makeBakers nBakers)

-- Need to return string because Bytestring does not implement Lift
dummyCryptographicParametersFile :: String
dummyCryptographicParametersFile = $(do
  fileContents <- qReadFileString "../scheduler/testdata/global.json"
  [| fileContents |])

dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  fromMaybe (error "Could not read crypto params.") $
    readCryptographicParameters (BSL.pack dummyCryptographicParametersFile)
