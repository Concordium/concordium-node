module Concordium.Startup where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Serialize

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF

import Concordium.Types
import Concordium.Birk.Bake


makeBakers :: Word -> [(BakerIdentity,BakerInfo)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        lot = 1.0 / fromIntegral nBakers
        mbs gen bid = (BakerIdentity bid ssk spk esk epk, BakerInfo epk spk lot):mbs gen'' (bid+1)
            where
                (VRF.KeyPair esk epk, gen') = random gen
                (Sig.KeyPair ssk spk, gen'') = random gen'

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
