{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConcordiumTests.Afgjort.CSS.NominationSet where

import Data.Serialize
import qualified Concordium.Afgjort.CSS.BitSet as Set

import Concordium.Afgjort.Types
import Concordium.Afgjort.CSS.NominationSet

import Test.QuickCheck
import Test.Hspec

instance Arbitrary NominationSet where
    arbitrary = do
        nomMax <- fromIntegral . abs <$> getSize
        nomTop <- oneof [
            Set.fromAscList <$> sublistOf [minParty..nomMax],
            return Set.empty
            ]
        nomBot <- oneof [
            Set.fromAscList <$> sublistOf [minParty..nomMax],
            return Set.empty
            ]
        return NominationSet{..}
    shrink (NominationSet mx ts bs) =
        [NominationSet mx Set.empty bs | not (Set.null ts)]
        ++ [NominationSet mx ts Set.empty | not (Set.null bs)]
        ++ [NominationSet (mx - 1) (Set.filter (< mx) ts) (Set.filter (< mx) bs) | mx > 0]

serializeTest :: Property
serializeTest = property $ \ns -> Right ns === runGet (getUntaggedNominationSet (nomTag ns)) (runPut $ putUntaggedNominationSet ns)

tests :: Word -> Spec
tests lvl = describe "Concordium.Afgjort.CSS.NominationSet" $ do
    it "serialization of nomination set" $ withMaxSuccess (10 * 10^lvl) $ serializeTest