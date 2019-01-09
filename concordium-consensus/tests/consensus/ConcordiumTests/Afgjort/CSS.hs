{-# LANGUAGE RecordWildCards, TupleSections #-}
module ConcordiumTests.Afgjort.CSS where

import Data.Monoid
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map
import Lens.Micro.Platform
import Concordium.Afgjort.CSS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Expectations


invariantCSSState :: (Ord party, Show party, Show sig) => Int -> Int -> (party -> Int) -> CSSState party sig -> Either String ()
invariantCSSState totalWeight corruptWeight partyWeight s = do
        checkBinary (==) computedManySawWeight (s ^. manySawWeight) "==" "computed manySaw weight" "given manySaw weight"
        when (s ^. report) $ do
            when (s ^. topJustified) $ checkBinary (\m1 m2 -> Map.null (m1 Map.\\ m2)) (s ^. inputTop) (s ^. iSaw) "subsumed by" "[justified] top inputs" "iSaw"
            when (s ^. botJustified) $ checkBinary (\m1 m2 -> Map.null (m1 Map.\\ m2)) (s ^. inputBot) (s ^. iSaw) "subsumed by" "[justified] bottom inputs" "iSaw"
        forM_ (Map.toList $ s ^. sawTop) $ \(src, (tot, m)) -> checkBinary (==) (sumPartyWeights m) tot "==" ("computed weight of parties seeing (" ++ show src ++ ", top)") "given weight"
        forM_ (Map.toList $ s ^. sawBot) $ \(src, (tot, m)) -> checkBinary (==) (sumPartyWeights m) tot "==" ("computed weight of parties seeing (" ++ show src ++ ", bottom)") "given weight"
    where
        sumPartyWeights = Map.foldlWithKey (\w k _ -> w + partyWeight k) 0
        computedManySawWeight = sumPartyWeights (s ^. manySaw)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

data CSSInput party sig
        = JustifyChoice Choice
        | ReceiveCSSMessage party sig (CSSMessage party sig)
        deriving (Eq, Ord, Show)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.

selectFromSeq :: (Ord a) => Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)
                

multiCSSTest :: Int -> Gen Property
multiCSSTest nparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p () (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs p <$> arbitrary
        go (justs <> mconcat choices) iStates iCores
    where
        parties = [0..nparties-1]
        corruptWeight = (nparties - 1) `div` 3
        CSSInstance{..} = newCSSInstance nparties corruptWeight (const 1)
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)
        fromOut src (SendCSSMessage msg) = (Seq.fromList [(i,ReceiveCSSMessage src () msg)|i <- parties], mempty)
        fromOut _ (SelectCoreSet theCore) = (mempty, First (Just theCore))
        go :: Seq.Seq (Int, CSSInput Int ()) -> Vec.Vector (CSSState Int ()) -> Vec.Vector (First (CoreSet Int ())) -> Gen Property
        go msgs sts cores
            | null msgs = return $ counterexample "Not all core sets found" $ all (isJust . getFirst) cores
            | otherwise = do
                ((rcpt, inp), msgs') <- selectFromSeq msgs
                let a = case inp of
                            JustifyChoice c -> justifyChoice c
                            ReceiveCSSMessage p s msg -> receiveCSSMessage p s msg
                let (_, s', out) = runCSS a (sts Vec.! rcpt)
                {-return $ counterexample (show rcpt ++ ": " ++ show inp) $ -}
                case invariantCSSState nparties corruptWeight (const 1) s' of
                        Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                        Right _ -> do
                            let sts' = sts & ix rcpt .~ s'
                            let (msgs'', core') = mconcat $ fromOut rcpt <$> out
                            go (msgs'' <> msgs') sts' (cores & ix rcpt %~ (<> core'))

tests :: Spec
tests = describe "Concordium.Afgjort.CSS" $ do
    it "5 parties" $ withMaxSuccess 1000 $ multiCSSTest 5
    it "10 parties" $ withMaxSuccess 100 $ multiCSSTest 10
    it "15 parties" $ withMaxSuccess 100 $ multiCSSTest 15
    it "30 parties" $ withMaxSuccess 10 $ multiCSSTest 30