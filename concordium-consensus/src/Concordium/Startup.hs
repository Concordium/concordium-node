module Concordium.Startup where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BBS
import qualified Data.Map as Map
import qualified Data.FixedByteString as FBS

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Parameters
import Concordium.Birk.Bake
import Concordium.Types(AccountAddress(..))

makeBakers :: Word -> [(BakerIdentity,BakerInfo)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        lot = 1.0 / fromIntegral nBakers
        mbs gen bid = (BakerIdentity bid sk spk ek epk, BakerInfo epk spk lot account):mbs gen'' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = Sig.randomKeyPair gen'
                spk = Sig.verifyKey sk
                account = makeBakerAccount bid 

makeBakerAccount :: BakerId -> AccountAddress
makeBakerAccount bid = 
  let sbid = BBS.pack (digits bid)
  in AccountAddress . FBS.fromByteString $ (BBS.pack [1] <> BBS.pack (replicate (20 - BS.length sbid) 0) <> sbid)

  where 
    digits 0 = [0]
    digits n = reverse $ go n
      where go 0 = []
            go m = (fromIntegral (m `mod` 10)) : go (m `div` 10)

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
        fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, BakerInfo vrfk vvk _ _) <- bakers]
