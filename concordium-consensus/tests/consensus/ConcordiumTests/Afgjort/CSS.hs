module ConcordiumTests.Afgjort.CSS where

import Control.Monad
import qualified Data.Map as Map
import Lens.Micro.Platform
import Concordium.Afgjort.CSS

invariantCSSState :: (Ord party, Show party, Show sig) => Int -> Int -> (party -> Int) -> party -> (party -> sig -> CSSMessage party sig -> Bool) -> CSSState party sig -> Either String ()
invariantCSSState totalWeight corruptWeight partyWeight me checkSig s = do
        checkBinary (==) (computedManySawWeight) (s ^. manySawWeight) "==" "computed manySaw weight" "given manySaw weight"
        when (s ^. botJustified && s ^. topJustified) $
            checkBinary (\a b -> Map.null $ Map.intersection a b) (s ^. inputTop) (s ^. inputBot) "is disjoint from" "parties declared top" "parties declared bottom"
        return ()
    where
        computedManySawWeight = Map.foldlWithKey (\w k _ -> w + partyWeight k) 0 (s ^. manySaw)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ sy ++ " (" ++ show y ++ ")"