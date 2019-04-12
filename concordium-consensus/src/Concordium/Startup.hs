module Concordium.Startup where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map

import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Parameters
import Concordium.Birk.Bake


makeBakers :: Word -> [(BakerIdentity,BakerInfo)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        lot = 1.0 / fromIntegral nBakers
        mbs gen bid = (BakerIdentity bid sk spk ek epk, BakerInfo epk spk lot):mbs gen'' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk@(Sig.KeyPair _ spk), gen'') = Sig.randomKeyPair gen'

makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> [(BakerIdentity, BakerInfo)] -- ^Information about initial bakers
    -> GenesisData
makeGenesisData genTime bakers = GenesisData genTime
                                             10 -- slot time in seconds
                                             bps
                                             fps
    where
        bps = BirkParameters (BS.pack "LeadershipElectionNonce")
                             0.5 -- voting power
                             (Map.fromList $ [(bid, binfo) | (BakerIdentity bid _ _ _ _, binfo) <- bakers])
        fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, BakerInfo vrfk vvk _) <- bakers]
