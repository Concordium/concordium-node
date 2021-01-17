{-# LANGUAGE RecordWildCards, TupleSections #-}
module ConcordiumTests.Afgjort.Types where

import Data.Serialize
import Concordium.Afgjort.Types
import qualified Data.Map.Strict as Map
import Control.Monad

import Test.QuickCheck
import Test.Hspec

serializePartyMapTest :: Property
serializePartyMapTest = forAll genMap $ \(s, m) -> Right m === runGet (getPartyMap s) (runPut (putPartyMap s m))
    where
        genMap = do
            s <- fromIntegral . max 0 <$> getSize
            kvs <- forM [0..s] $ \k -> (k,) <$> (arbitrary :: Gen String)
            return (s, Map.fromAscList kvs)

tests :: Word -> Spec
tests lvl = describe "Concordium.Afgjort.Types" $ do
    it "serialization of party maps" $ withMaxSuccess (10 * 10^lvl) serializePartyMapTest