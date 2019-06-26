{-# LANGUAGE RecordWildCards #-}
module Concordium.Startup where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.Birk.Bake
import Concordium.Types

makeBakers :: Word -> [((BakerIdentity,BakerInfo), Account)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = ((BakerIdentity bid sk spk ek epk, BakerInfo epk spk stake accAddress), account):mbs gen'' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = Sig.randomKeyPair gen'
                spk = Sig.verifyKey sk
                accAddress = _accountAddress account
                stake = _accountAmount account
                account = makeBakerAccount bid

makeBakerAccount :: BakerId -> Account
makeBakerAccount bid = acct {_accountAmount = 1000000, _accountStakeDelegate = Just bid}
  where
    acct = newAccount (Sig.verifyKey kp) SigScheme.Ed25519
    kp = fst (Sig.randomKeyPair (mkStdGen (- (fromIntegral bid) - 1))) -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    
makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> Word
    -> Duration
    -> ElectionDifficulty
    -> (GenesisData, [(BakerIdentity,BakerInfo)])
makeGenesisData genTime nBakers slotTime elecDiff = (GenesisData genTime
                                               slotTime -- slot time in seconds
                                               bps
                                               bakerAccounts
                                               fps,
                                   bakers)
    where
        bps = BirkParameters (BS.pack "LeadershipElectionNonce")
                             elecDiff -- voting power
                             (Bakers (Map.fromList $ [(bid, binfo) | (BakerIdentity bid _ _ _ _, binfo) <- bakers])
                                (sum [_bakerStake binfo | (_, binfo) <- bakers])
                                (fromIntegral nBakers) -- next available baker id (since baker ids start with 0
                             )
        fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, BakerInfo vrfk vvk _ _) <- bakers]
        (bakers, bakerAccounts) = unzip (makeBakers nBakers)
