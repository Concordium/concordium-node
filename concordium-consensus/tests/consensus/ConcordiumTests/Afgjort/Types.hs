module ConcordiumTests.Afgjort.Types where

import Concordium.Afgjort.Types
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Serialize

import Test.Hspec
import Test.QuickCheck

serializePartyMapTest :: Property
serializePartyMapTest = forAll genMap $ \(s, m) -> Right m === runGet (getPartyMap s) (runPut (putPartyMap s m))
  where
    genMap = do
        s <- fromIntegral . max 0 <$> getSize
        kvs <- forM [0 .. s] $ \k -> (k,) <$> (arbitrary :: Gen String)
        return (s, Map.fromAscList kvs)

tests :: Word -> Spec
tests lvl = describe "Concordium.Afgjort.Types" $ do
    it "serialization of party maps" $ withMaxSuccess (10 * 10 ^ lvl) serializePartyMapTest
