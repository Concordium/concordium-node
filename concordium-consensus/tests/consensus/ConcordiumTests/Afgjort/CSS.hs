module ConcordiumTests.Afgjort.CSS where

import Control.Monad
import qualified Data.Map as Map
import Lens.Micro.Platform
import Concordium.Afgjort.CSS

invariantCSSState :: (Ord party, Show party, Show sig) => Int -> Int -> (party -> Int) -> party -> (party -> sig -> CSSMessage party sig -> Bool) -> CSSState party sig -> Either String ()
invariantCSSState totalWeight corruptWeight partyWeight me checkSig s = do
        checkBinary (==) computedManySawWeight (s ^. manySawWeight) "==" "computed manySaw weight" "given manySaw weight"
        when (s ^. topJustified) $ checkBinary (\m1 m2 -> Map.null (m1 Map.\\ m2)) (s ^. inputTop) (s ^. iSaw) "subsumed by" "[justified] top inputs" "iSaw"
        when (s ^. botJustified) $ checkBinary (\m1 m2 -> Map.null (m1 Map.\\ m2)) (s ^. inputBot) (s ^. iSaw) "subsumed by" "[justified] bottom inputs" "iSaw"
        forM_ (Map.toList $ s ^. sawTop) $ \(src, (tot, m)) -> checkBinary (==) (sumPartyWeights m) tot "==" ("computed weight of parties seeing (" ++ show party ++ ", top)") "given weight"
        forM_ (Map.toList $ s ^. sawBot) $ \(src, (tot, m)) -> checkBinary (==) (sumPartyWeights m) tot "==" ("computed weight of parties seeing (" ++ show party ++ ", bottom)") "given weight"
    where
        sumPartyWeights = Map.foldlWithKey (\w k _ -> w + partyWeight k) 0
        computedManySawWeight = sumPartyWeights (s ^. manySaw)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ sy ++ " (" ++ show y ++ ")"

data CSSInput party sig
        = JustifyChoice Choice
        | ReceiveCSSMessage party sig (CSSMessage party sig)
