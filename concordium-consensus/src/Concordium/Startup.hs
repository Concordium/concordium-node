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

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.Ed25519Signature as Ed25519
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState.IdentityProviders
import Concordium.Birk.Bake
import Concordium.Types

import TH.RelativePaths

makeBakers :: Word -> [((BakerIdentity,BakerInfo), Account)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = ((BakerIdentity sk ek, BakerInfo epk spk stake accAddress), account):mbs gen'' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = Sig.randomKeyPair gen'
                spk = Sig.verifyKey sk
                accAddress = _accountAddress account
                stake = _accountAmount account
                account = makeBakerAccount bid

makeBakerAccount :: BakerId -> Account
makeBakerAccount bid = acct {_accountAmount = 1000000000000, _accountStakeDelegate = Just bid}
  where
    acct = newAccount (SigScheme.correspondingVerifyKey kp)
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (Ed25519.randomKeyPair (mkStdGen (- (fromIntegral bid) - 1)))
    
makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> ElectionDifficulty  -- ^Initial election difficulty.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> [IpInfo]   -- ^List of initial identity providers.
    -> [Account]  -- ^List of starting genesis special accounts (in addition to baker accounts).
    -> (GenesisData, [(BakerIdentity,BakerInfo)])
makeGenesisData genesisTime nBakers genesisSlotDuration elecDiff finMinSkip genesisCryptographicParameters genesisIdentityProviders genesisSpecialBetaAccounts
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
        genesisFinalizationParameters = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, BakerInfo vrfk vvk _ _) <- bakers] finMinSkip
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
