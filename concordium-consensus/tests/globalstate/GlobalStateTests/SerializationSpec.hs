module GlobalStateTests.SerializationSpec where


import Test.Hspec
import Test.QuickCheck as QC

import Data.Serialize

import qualified Data.Serialize.Put as P
import qualified Data.Serialize.Get as G
import Data.ByteString.Lazy as BS    

import GlobalStateTests.Gen
import Concordium.GlobalState.Transactions

groupIntoSize :: (Show a, Integral a) => a -> String
groupIntoSize s = 
  let kb = s
      nd = if kb > 0 then truncate (logBase 10 (fromIntegral kb)) else 0
  in if nd == 0 then show kb ++ "B"
     else let lb = 10^nd
              ub = 10^(nd+1)
          in show lb ++ " -- " ++ show ub ++ "B"

checkTransaction :: Transaction -> Property
checkTransaction tx = let bs = P.runPutLazy (put tx)
              in  case G.runGet get (BS.toStrict bs) of
                    Left err -> counterexample err False
                    Right tx' -> QC.label (groupIntoSize (BS.length bs)) $ tx === tx'

testTransaction :: Int -> Property
testTransaction size = forAll (resize size $ genTransaction) checkTransaction

tests :: Spec
tests = parallel $ do
  specify ("Transaction serialization with size = 100.") $ withMaxSuccess 10000 $ testTransaction 100
  specify ("Transaction serialization with size = 1000.") $ withMaxSuccess 10000 $ testTransaction 1000
  specify ("Transaction serialization with size = 100000.") $ withMaxSuccess 500 $ testTransaction 100000
