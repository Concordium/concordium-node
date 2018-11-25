{-# LANGUAGE TupleSections  #-}
module Main where

import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Serialize

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF

import Concordium.Types
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Runner
import Concordium.Show

import System.Console.GetOpt
import System.Environment

makeBakers :: Word -> [(BakerIdentity,BakerInfo)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        lot = 1.0 / fromIntegral nBakers
        mbs gen bid = (BakerIdentity bid ssk esk, BakerInfo epk spk lot):mbs gen'' (bid+1)
            where
                (VRF.KeyPair esk epk, gen') = random gen
                (Sig.KeyPair ssk spk, gen'') = random gen'

makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> [(BakerIdentity, BakerInfo)] -- ^Public information about initial bakers
    -> GenesisData
makeGenesisData genTime bakers = GenesisData genTime
                                             10 -- slot time in seconds
                                             bps
                                             fps
    where
        bps = BirkParameters (BS.pack "LeadershipElectionNonce")
                             0.5 -- voting power
                             (Map.fromList $ [(bid, binfo) | (BakerIdentity bid _ _, binfo) <- bakers])
        fps = FinalizationParameters Map.empty

data CmdOption = NumBakers Word | GenTime Timestamp deriving (Show)

options :: [OptDescr CmdOption]
options = [ Option ['n'] ["nBakers"] (OptArg maybeNum "10") "number of bakers to generate",
            Option ['g'] ["genTime"] (OptArg maybeGenTime "0") "genesis time"]

maybeNum :: Maybe String -> CmdOption
maybeNum (Just x) = NumBakers $ read x
maybeNum Nothing = NumBakers 10

maybeGenTime :: Maybe String -> CmdOption
maybeGenTime (Just x) = GenTime $ read x
maybeGenTime Nothing = GenTime 0


parseCmdLine :: [String] -> IO [CmdOption]
parseCmdLine argv = 
    case getOpt Permute options argv of
      (o, n, []) -> return o
      (_, _, errs) -> ioError (userError (concat errs ++ usageInfo "Usage: Concordium-genesis [OPTION ...]" options))

main :: IO ()
main = do
    opts <- getArgs >>= parseCmdLine
    let (nBakers, genTime) = foldr (\o (n, g) -> case o of
                                                   NumBakers n' -> (n', g)
                                                   GenTime g -> (n, g)) (10,0) opts
        genFile = "gendata.bin"
        bakers = makeBakers nBakers
        bakersPrivate = map fst bakers
        gendata = makeGenesisData genTime bakers
    BS.writeFile genFile (encode gendata)
    mapM_ (\bkr@(BakerIdentity bid _ _) -> BS.writeFile ("baker_" ++ show bid ++ ".bin") (encode bkr)) bakersPrivate
