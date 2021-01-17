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
import Concordium.Crypto.BlsSignature as Bls
import Data.FixedByteString as FBS

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
  finalizationBlockPointer <- BlockHashV0 . Hash . FBS.pack <$> vector 32
  finalizationProof <- FinalizationProof <$> do
    l <- choose (0,200) -- between 0 and 200 parties, inclusive
    parties <- replicateM l $ do
      party <- arbitrary
      return party
    return (parties, makesig l)
  finalizationDelay <- BlockHeight <$> arbitrary
  return FinalizationRecord{..}
    where
      -- TODO: simplistic way of generating some different signatures for different amount of signers.
      --       Note that they are not generated randomly based on the seed.
      makesig 0 = Bls.emptySignature
      makesig n = Bls.aggregate Bls.emptySignature $ makesig (n-1)


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
