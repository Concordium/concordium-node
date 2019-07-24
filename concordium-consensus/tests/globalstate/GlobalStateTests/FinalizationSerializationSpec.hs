{-# LANGUAGE RecordWildCards #-}
module GlobalStateTests.FinalizationSerializationSpec where

import Test.Hspec
import Test.QuickCheck as QC

import Data.Serialize

import Control.Monad

import qualified Data.Serialize.Put as P
import qualified Data.Serialize.Get as G
import Data.ByteString.Lazy as LBS    
import Concordium.Crypto.SHA256(Hash(..))
import Concordium.Crypto.SignatureScheme(Signature(..))
import Data.FixedByteString as FBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS

import Concordium.Types
import Concordium.GlobalState.Finalization

groupIntoSize :: (Show a, Integral a) => a -> String
groupIntoSize s = 
  let kb = s
      nd = if kb > 0 then truncate (logBase 10 (fromIntegral kb) :: Double) else 0 :: Int
  in if nd == 0 then show kb ++ "B"
     else let lb = 10^nd :: Integer
              ub = 10^(nd+1) :: Integer
          in show lb ++ " -- " ++ show ub ++ "B"

genFinalizationRecord :: Gen FinalizationRecord
genFinalizationRecord = do
  finalizationIndex <- FinalizationIndex <$> arbitrary
  finalizationBlockPointer <- Hash . FBS.pack <$> vector 32
  finalizationProof <- FinalizationProof <$> do
    l <- choose (0,200) -- between 0 and 200 parties, inclusive
    replicateM l $ do
      party <- arbitrary
      sl <- choose(40,80) -- signature really is 64 bytes, but serialization should work with any size (at the moment)
      bs <- BSS.pack <$> vector sl
      return (party, Signature bs)
  finalizationDelay <- BlockHeight <$> arbitrary
  return FinalizationRecord{..}


checkFinalizationRecord :: FinalizationRecord -> Property
checkFinalizationRecord tx = let bs = P.runPutLazy (put tx)
              in  case G.runGet get (LBS.toStrict bs) of
                    Left err -> counterexample err False
                    Right tx' -> QC.label (groupIntoSize (LBS.length bs)) $ tx === tx'

testFinalizationRecord :: Int -> Property
testFinalizationRecord size = forAll (resize size $ genFinalizationRecord) checkFinalizationRecord

tests :: Spec
tests = parallel $ do
  specify ("FinalizationRecord serialization with size = 100.") $ withMaxSuccess 10000 $ testFinalizationRecord 100
  specify ("FinalizationRecord serialization with size = 1000.") $ withMaxSuccess 10000 $ testFinalizationRecord 1000
