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
      nd = if kb > 0 then truncate (logBase 10 (fromIntegral kb) :: Double) else 0 :: Int
  in if nd == 0 then show kb ++ "B"
     else let lb = 10^nd :: Integer
              ub = 10^(nd+1) :: Integer
          in show lb ++ " -- " ++ show ub ++ "B"

checkTransaction :: BareTransaction -> Property
checkTransaction tx = let bs = P.runPutLazy (put tx)
              in  case G.runGet get (BS.toStrict bs) of
                    Left err -> counterexample err False
                    Right tx' -> QC.label (groupIntoSize (BS.length bs)) $ tx === tx'

testTransaction :: Int -> Property
testTransaction size = forAll (resize size $ genBareTransaction) checkTransaction

checkTransactionWithSig :: BareTransaction -> Property
checkTransactionWithSig tx = let bs = P.runPutLazy (put tx)
              in case G.runGet (getVerifiedTransaction dummyTime) (BS.toStrict bs) of
                   Left err -> counterexample err False
                   Right tx' -> QC.label (groupIntoSize (BS.length bs)) $ tx === trBareTransaction tx'

dummyTime :: TransactionTime
dummyTime = 37

checkTransactionWithRandomSig :: BareTransaction -> Property
checkTransactionWithRandomSig tx = let bs = P.runPutLazy (put tx)
              in  case G.runGet (getVerifiedTransaction dummyTime) (BS.toStrict bs) of
                    Left _ -> True === True
                    Right tx' -> counterexample (show (tx, tx')) False


-- |These should all fail, since the signature is generated to be a random bytestring.
testTransactionWithRandomSig :: Int -> Property
testTransactionWithRandomSig size = forAll (resize size $ genBareTransaction) checkTransactionWithRandomSig

testTransactionWithSig :: Int -> Property
testTransactionWithSig size = forAll (resize size $ genSignedTransaction) checkTransactionWithSig


tests :: Spec
tests = parallel $ do
  specify ("Transaction serialization with size = 100.") $ withMaxSuccess 10000 $ testTransaction 100
  specify ("Transaction serialization with size = 1000.") $ withMaxSuccess 10000 $ testTransaction 1000
  specify ("Transaction serialization with size = 100000.") $ withMaxSuccess 500 $ testTransaction 100000

  specify ("Verified transaction serialization with size = 100.") $ withMaxSuccess 10000 $ testTransactionWithSig 100
  specify ("Verified transaction serialization with size = 1000.") $ withMaxSuccess 10000 $ testTransactionWithSig 1000
  specify ("Verified transaction serialization with size = 100000.") $ withMaxSuccess 500 $ testTransactionWithSig 100000

  specify ("Verified transaction serialization with random sig size = 100.") $ withMaxSuccess 10000 $ testTransactionWithRandomSig 100
  specify ("Verified transaction serialization with random sig size = 1000.") $ withMaxSuccess 10000 $ testTransactionWithRandomSig 1000
  specify ("Verified transaction serialization with random sig size = 100000.") $ withMaxSuccess 500 $ testTransactionWithRandomSig 100000
