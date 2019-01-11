{-# LANGUAGE RecordWildCards #-}
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
        | ReceiveCSSMessage party sig (CSSMessage party)
        deriving (Eq, Ord, Show)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.

selectFromSeq :: (Ord a) => Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

coreIntersection :: (Ord party) => CoreSet party sig -> CoreSet party sig -> CoreSet party sig
coreIntersection c1 c2 = CoreSet (liftM2 Map.intersection (coreTop c1) (coreTop c2)) (liftM2 Map.intersection (coreBot c1) (coreBot c2))

coreIntersections :: (Ord party) => [CoreSet party sig] -> CoreSet party sig
coreIntersections [c] = c
coreIntersections (c:cs) = coreIntersection c (coreIntersections cs)
coreIntersections [] = undefined

coreSize :: (Ord party) => (party -> Int) -> CoreSet party sig -> Int
coreSize _ (CoreSet Nothing Nothing) = 0
coreSize partyWeight (CoreSet (Just c) Nothing) = sum (partyWeight <$> Map.keys c)
coreSize partyWeight (CoreSet Nothing (Just c)) = sum (partyWeight <$> Map.keys c)
coreSize partyWeight (CoreSet (Just c1) (Just c2)) = sum (partyWeight <$> Map.keys (Map.union c1 c2))

coresCheck :: Int -> Int -> Vec.Vector (First (CoreSet Int ())) -> Property
coresCheck allparties corruptWeight cores = (counterexample "Not all core sets found" $ all (isJust . getFirst) cores)
            .&&. (let theCore = (coreIntersections $ Vec.toList $ (fromJust . getFirst) <$> cores)
                    in counterexample ("Core too small: " ++ show theCore) $ allparties - corruptWeight <= coreSize (const 1) theCore )

noCoresCheck :: Int -> Int -> Vec.Vector (First (CoreSet Int ())) -> Property
noCoresCheck _ _ _ = property True

runCSSTest :: Int -> Int -> Int -> Seq.Seq (Int, CSSInput Int ()) -> Vec.Vector (CSSState Int ()) -> Vec.Vector (First (CoreSet Int ())) -> Gen Property
runCSSTest = runCSSTest' coresCheck

runCSSTest' :: (Int -> Int -> Vec.Vector (First (CoreSet Int ())) -> Property) -> Int -> Int -> Int -> Seq.Seq (Int, CSSInput Int ()) -> Vec.Vector (CSSState Int ()) -> Vec.Vector (First (CoreSet Int ())) -> Gen Property
runCSSTest' ccheck allparties nparties corruptWeight = go
    where
        go :: Seq.Seq (Int, CSSInput Int ()) -> Vec.Vector (CSSState Int ()) -> Vec.Vector (First (CoreSet Int ())) -> Gen Property
        go msgs sts cores
            | null msgs = return $ ccheck allparties corruptWeight cores
            | otherwise = do
                ((rcpt, inp), msgs') <- selectFromSeq msgs
                let a = case inp of
                            JustifyChoice c -> justifyChoice c
                            ReceiveCSSMessage p s msg -> receiveCSSMessage p s msg
                let (_, s', out) = runCSS a (sts Vec.! rcpt)
                {-return $ counterexample (show rcpt ++ ": " ++ show inp) $ -}
                case invariantCSSState allparties corruptWeight (const 1) s' of
                        Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                        Right _ -> do
                            let sts' = sts & ix rcpt .~ s'
                            let (msgs'', core') = mconcat $ fromOut rcpt <$> out
                            go (msgs'' <> msgs') sts' (cores & ix rcpt %~ (<> core'))
        fromOut src (SendCSSMessage msg) = (Seq.fromList [(i,ReceiveCSSMessage src () msg)|i <- parties], mempty)
        fromOut _ (SelectCoreSet theCore) = (mempty, First (Just theCore))
        CSSInstance{..} = newCSSInstance allparties corruptWeight (const 1)
        parties = [0..nparties-1]

multiCSSTest :: Int -> Gen Property
multiCSSTest nparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p () (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs p <$> arbitrary
        runCSSTest nparties nparties corruptWeight (justs <> mconcat choices) iStates iCores
    where
        parties = [0..nparties-1]
        corruptWeight = (nparties - 1) `div` 3
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

multiCSSTestWithSilentCorrupt :: Int -> Gen Property
multiCSSTestWithSilentCorrupt allparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p () (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs p <$> arbitrary
        runCSSTest allparties nparties corruptWeight (justs <> mconcat choices) iStates iCores
    where
        nparties = allparties - corruptWeight
        parties = [0..nparties-1]
        corruptWeight = (allparties - 1) `div` 3
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

multiCSSTestWithActiveCorrupt :: Int -> Gen Property
multiCSSTestWithActiveCorrupt allparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p () (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs p <$> arbitrary
        let corruptMsgs = Seq.fromList $
                            [(r, ReceiveCSSMessage cp () (Input c)) | r <- parties, cp <- corruptParties, c <- [True,False]]
                            ++ [(r, ReceiveCSSMessage cp () (Seen p c)) | r <- parties, cp <- corruptParties, p <- parties ++ corruptParties, c <- [True,False]]
                            ++ [(r, ReceiveCSSMessage cp () (DoneReporting Map.empty)) | r <- parties, cp <- corruptParties]
        runCSSTest allparties nparties corruptWeight (justs <> mconcat choices <> corruptMsgs) iStates iCores
    where
        nparties = allparties - corruptWeight
        parties = [0..nparties-1]
        corruptWeight = (allparties - 1) `div` 3
        corruptParties = [nparties .. allparties-1]
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

singleCSSWithBadEnv :: Int -> Gen Property
singleCSSWithBadEnv allparties = do
        let justs = Seq.fromList [(0, JustifyChoice c) | c <- [True,False]]
        let cmsgs c = Seq.fromList [(0, ReceiveCSSMessage 0 () (Input c))]
        choices <- cmsgs <$> arbitrary
        let corruptMsgs = Seq.fromList $
                            [(0, ReceiveCSSMessage cp () (Input c)) | cp <- corruptParties, c <- [True,False]]
                            ++ [(0, ReceiveCSSMessage cp () (Seen p c)) | cp <- corruptParties, p <- 0: corruptParties, c <- [True,False]]
                            ++ [(0, ReceiveCSSMessage cp () (DoneReporting Map.empty)) | cp <- corruptParties]
        runCSSTest' noCoresCheck allparties 1 corruptWeight (justs <> choices <> corruptMsgs <> corruptMsgs) iStates iCores
    where
        corruptWeight = (allparties - 1) `div` 3
        corruptParties = [1..allparties - 1]
        iStates = Vec.singleton initialCSSState
        iCores = Vec.singleton (First Nothing)


tests :: Spec
tests = describe "Concordium.Afgjort.CSS" $ do
    it "5 parties" $ withMaxSuccess 1000 $ multiCSSTest 5
    it "10 parties" $ withMaxSuccess 100 $ multiCSSTest 10
    it "15 parties" $ withMaxSuccess 100 $ multiCSSTest 15
    it "30 parties" $ withMaxSuccess 10 $ multiCSSTest 30
    it "7 parties (2 silent)" $ withMaxSuccess 10000 $ multiCSSTestWithSilentCorrupt 7
    it "15 parties (4 silent)" $ withMaxSuccess 1000 $ multiCSSTestWithSilentCorrupt 15
    it "4 parties (1 active corrupt)" $ withMaxSuccess 50000 $ multiCSSTestWithActiveCorrupt 4
    it "1 party + 4 corrupt" $ withMaxSuccess 10000 $ singleCSSWithBadEnv 5