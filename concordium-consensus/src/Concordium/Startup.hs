{-# LANGUAGE RecordWildCards, OverloadedStrings, TemplateHaskell #-}
module Concordium.Startup where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Concordium.Crypto.SignatureScheme as SigScheme
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
    acct = newAccount (Sig.verifyKey kp) SigScheme.Ed25519
    kp = fst (Sig.randomKeyPair (mkStdGen (- (fromIntegral bid) - 1))) -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    
makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> ElectionDifficulty  -- ^Initial election difficulty.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> [IdentityProviderData]   -- ^List of initial identity providers.
    -> (GenesisData, [(BakerIdentity,BakerInfo)])
makeGenesisData genesisTime nBakers genesisSlotDuration elecDiff finMinSkip genesisCryptographicParameters genesisIdentityProviders
    = (GenesisData{..}, bakers)
    where
        genesisBirkParameters =
            BirkParameters elecDiff -- voting power
                           (bakersFromList (snd <$> bakers))
                           (genesisSeedState (Hash.hash "LeadershipElectionNonce") 360) -- todo hardcoded epoch length (and initial seed)
        genesisFinalizationParameters = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, BakerInfo vrfk vvk _ _) <- bakers] finMinSkip
        (bakers, genesisBakerAccounts) = unzip (makeBakers nBakers)

-- Need to return string because Bytestring does not implement Lift
dummyCryptographicParametersFile :: String
dummyCryptographicParametersFile = $(do
  fileContents <- qReadFileString "../scheduler/testdata/global.json"
  [| fileContents |])

dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case readCryptographicParameters (BSL.pack dummyCryptographicParametersFile) of
    Nothing -> error "Could not read crypto params."
    Just x -> x
  
