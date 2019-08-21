{-# LANGUAGE RecordWildCards, TemplateHaskell, RankNTypes, TupleSections, TypeFamilies #-}
module ConcordiumTests.Afgjort.CSS where

import Data.Monoid
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map
import Lens.Micro.Platform
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import Data.List

import Concordium.Afgjort.Types
import Concordium.Afgjort.CSS
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet
import qualified Concordium.Afgjort.PartySet as PS
import qualified Concordium.Afgjort.PartyMap as PM


import Test.QuickCheck
import Test.Hspec

invariantCSSState :: VoterPower -> VoterPower -> (Party -> VoterPower) -> CSSState sig -> Either String ()
invariantCSSState totalWeight corruptWeight partyWeight s = do
        checkBinary (==) computedManySawWeight (PS.weight $ s ^. manySaw) "==" "computed manySaw weight" "given manySaw weight"
        when (s ^. report) $ do
            let iSawSet = (nomTop $ s ^. iSaw) `BitSet.union` (nomBot $ s ^. iSaw)
            -- Every party that has sent a justified input should be in iSaw, so long as we are reporting.
            when (s ^. topJustified) $ checkBinary BitSet.isSubsetOf (BitSet.fromList . Map.keys $ s ^. inputTop) iSawSet "subsumed by" "[justified] top inputs" "iSaw"
            when (s ^. botJustified) $ checkBinary BitSet.isSubsetOf (BitSet.fromList . Map.keys $ s ^. inputBot) iSawSet "subsumed by" "[justified] bottom inputs" "iSaw"
        forM_ (Map.toList $ s ^. sawTop) $ \(src, ps) -> checkPartySet ("parties seeing (" ++ show src ++ ", top)") ps
        forM_ (Map.toList $ s ^. sawBot) $ \(src, ps) -> checkPartySet ("parties seeing (" ++ show src ++ ", bottom)") ps
        forM_ (nominationSetToList (s ^. iSaw)) $ \(p,c) -> do
            unless (s ^. justified c) $ Left $ "iSaw contains " ++ show (p,c) ++ " but the choice is not justified"
            unless (p `Map.member` (s ^. input c)) $ Left $ "iSaw contains " ++ show (p,c) ++ ", which is not in the input"
        unless (BitSet.null $ BitSet.intersection (nomTop $ s ^. iSaw) (nomBot $ s ^. iSaw)) $
            Left $ "iSaw contains mutiple choices for one party"
        checkBinary (==) computedManySaw (parties $ s ^. manySaw) "==" "computed manySaw" "given manySaw"
        checkPartyMap "justified done reporting" (s ^. justifiedDoneReporting)
        -- checkBinary (==) computedJustifiedDoneReportingWeight (s ^. justifiedDoneReportingWeight) "==" "computed justifiedDoneReporting weight" "given value"
        forM_ (Map.toList (s ^. unjustifiedDoneReporting)) $ \((seen,c),m) ->
            forM_ (Map.toList m) $ \(seer,_) ->
                when (s ^. sawJustified seer c seen) $ Left $ "unjustifiedDoneReporting " ++ show seer ++ " waiting on " ++ show (seen,c) ++ " which is seen and justified"
        checkBinary (==) (isJust $ s ^. core) (PM.weight (s ^. justifiedDoneReporting) >= totalWeight - corruptWeight) "<->" "core determined" "justifiedDoneReporting >= totalWeight - corruptWeight"
        checkBinary (==) partySaw partySawMsgs "==" "recorded seen values" "held seen message values"
    where
        sumPartyWeights = BitSet.foldl (\w k -> w + partyWeight k) 0
        computedManySawWeight = sumPartyWeights (parties $ s ^. manySaw)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        computedManySaw' c = if s ^. justified c then (const (Just c)) <$> Map.filter (\m -> PS.weight m >= totalWeight - corruptWeight) ((s ^. saw c) `Map.intersection` (const c <$> s ^. input c)) else Map.empty
        computedManySaw = BitSet.fromList $ Map.keys $ Map.unionWith (const $ const Nothing) (computedManySaw' True) (computedManySaw' False)
        checkPartyMap desc pm = checkPartySet desc (PM.keysSet pm)
        checkPartySet desc ps = checkBinary (==) (PS.weight ps) (sum $ partyWeight <$> PS.toList ps) "==" (desc ++ " weight") "computed value"
        partySaw :: Map.Map (Party, Bool) BitSet.BitSet
        partySaw = foldr doPartySaw Map.empty [(s ^. sawTop, True), (s ^. sawBot, False)]
        doPartySaw :: (Map.Map Party PartySet, Bool) -> Map.Map (Party, Bool) BitSet.BitSet -> Map.Map (Party, Bool) BitSet.BitSet
        doPartySaw (sawb, b) m = Map.foldrWithKey (doPartySawX b) m sawb
        doPartySawX :: Bool -> Party -> PartySet -> Map.Map (Party, Bool) BitSet.BitSet -> Map.Map (Party, Bool) BitSet.BitSet
        doPartySawX b k ps m = foldl (doPartyYSawX b k) m (PS.toList ps)
        doPartyYSawX b k m y = m & at (y, b) . non BitSet.empty %~ BitSet.insert k
        partySawMsgs = Map.foldrWithKey (\k v -> let (t, b) = collapseSawMsgs v in (at (k, True) . non BitSet.empty .~ t) . (at (k, False) . non BitSet.empty .~ b)) Map.empty (s ^. sawMessages)
        collapseSawMsgs [] = (BitSet.empty, BitSet.empty)
        collapseSawMsgs ((ns, _) : r) = let (sTop, sBot) = collapseSawMsgs r in (sTop `BitSet.union` nomTop ns, sBot `BitSet.union` nomBot ns)

data History = History {
    _receivedDoneReporting :: Map.Map Party [[(Party, Choice)]]
}
makeLenses ''History

initialHistory :: History
initialHistory = History Map.empty

checkUpdateHistory :: CSSState () -> CSSInput -> [CSSOutputEvent] -> History -> Either String History
checkUpdateHistory s inp _outp hist0 = do
        let ujdrs = [(p, (reverse $ (q, c) : l)) | ((q, c), m) <- Map.toList $ s ^. unjustifiedDoneReporting, (p, (l, _)) <- Map.toList m]
        forM_ ujdrs $ \(p, l) -> unless (any (justifiedAfterPrefix p l) (hist ^. receivedDoneReporting . at p . non [])) $ Left $ "unjustifiedDoneReporting invalid for party " ++ show p
        forM_ (PM.keys $ s ^. justifiedDoneReporting) $ \p -> unless (any (sawAllJustified p) (hist ^. receivedDoneReporting . at p . non [])) $ Left $ "justifiedDoneReporting invalid for party " ++ show p
        return hist
    where
        hist = case inp of
                ReceiveCSSMessage p (DoneReporting m) -> hist0 & receivedDoneReporting . at p %~ Just . (reverse (nominationSetToList m):) . fromMaybe []
                _ -> hist0
        justifiedAfterPrefix seer l rdr = isJust $ sawAllJustified seer <$> stripPrefix l rdr
        sawAllJustified seer = all (\(seen, c) -> s ^. sawJustified seer c seen)

data CSSInput
        = JustifyChoice Choice
        | ReceiveCSSMessage Party CSSMessage
        deriving (Eq, Ord, Show)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

coreIntersection :: CoreSet -> CoreSet -> CoreSet
coreIntersection c1 c2 = NominationSet (min (nomMax c1) (nomMax c2)) (BitSet.intersection (nomTop c1) (nomTop c2)) (BitSet.intersection (nomBot c1) (nomBot c2))

coreIntersections :: [CoreSet] -> CoreSet
coreIntersections [c] = c
coreIntersections (c:cs) = coreIntersection c (coreIntersections cs)
coreIntersections [] = undefined

coreSize :: (Party -> Int) -> CoreSet -> Int
coreSize partyWeight (NominationSet _ c1 c2) = sum (partyWeight <$> BitSet.toList (BitSet.union c1 c2))

coresCheck :: Int -> Int -> Vec.Vector (First CoreSet) -> Property
coresCheck allparties corruptWeight cores = (counterexample "Not all core sets found" $ all (isJust . getFirst) cores)
            .&&. (let theCore = (coreIntersections $ Vec.toList $ (fromJust . getFirst) <$> cores)
                    in counterexample ("Core too small: " ++ show theCore) $ allparties - corruptWeight <= coreSize (const 1) theCore )

noCoresCheck :: Int -> Int -> Vec.Vector (First CoreSet) -> Property
noCoresCheck _ _ _ = property True

atParty :: Party -> Traversal' (Vec.Vector a) a
atParty = ix . fromIntegral

runCSSTest :: Int -> Int -> Int -> Seq.Seq (Party, CSSInput) -> Vec.Vector (CSSState ()) -> Vec.Vector (First CoreSet) -> Gen Property
runCSSTest = runCSSTest' coresCheck

filterCSSMessages :: Party -> [CSSOutputEvent] -> Seq.Seq (Party, CSSInput) -> Seq.Seq (Party, CSSInput)
filterCSSMessages src oes msgs = if any isSendSeen oes then Seq.filter f msgs else msgs
    where
        isSendSeen (SendCSSMessage (Seen _)) = True
        isSendSeen _ = False
        f (_, ReceiveCSSMessage src' (Seen _)) = src /= src'
        f _ = True

runCSSTest' :: (Int -> Int -> Vec.Vector (First CoreSet) -> Property) -> Int -> Int -> Int -> Seq.Seq (Party, CSSInput) -> Vec.Vector (CSSState ()) -> Vec.Vector (First CoreSet) -> Gen Property
runCSSTest' ccheck allparties nparties corruptWeight = go initialHistory
    where
        go :: History -> Seq.Seq (Party, CSSInput) -> Vec.Vector (CSSState ()) -> Vec.Vector (First CoreSet) -> Gen Property
        go hist msgs sts cores
            | null msgs = return $ ccheck allparties corruptWeight cores
            | otherwise = do
                ((rcpt, inp), msgs') <- selectFromSeq msgs
                let a = case inp of
                            JustifyChoice c -> justifyChoice c
                            ReceiveCSSMessage p msg -> receiveCSSMessage p msg ()
                let (_, s', out) = runCSS a cssInst (sts Vec.! fromIntegral rcpt)
                {-return $ counterexample (show rcpt ++ ": " ++ show inp) $ -}
                case invariantCSSState (fromIntegral allparties) (fromIntegral corruptWeight) (const 1) s' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                    Right _ -> case checkUpdateHistory s' inp out hist of
                        Left err -> return $ counterexample ("History invariant failed: " ++ err ++ "\n" ++ show s') False
                        Right hist' -> do
                            let sts' = sts & atParty rcpt .~ s'
                            let (msgs'', core') = mconcat $ fromOut rcpt <$> out
                            go hist' (msgs'' <> filterCSSMessages rcpt out msgs') sts' (cores & atParty rcpt %~ (<> core'))
        fromOut src (SendCSSMessage msg) = (Seq.fromList [(i,ReceiveCSSMessage src msg)|i <- parties], mempty)
        fromOut _ (SelectCoreSet theCore) = (mempty, First (Just theCore))
        cssInst = CSSInstance (fromIntegral allparties) (fromIntegral corruptWeight) (const 1) (fromIntegral nparties)
        parties = [0..fromIntegral nparties-1]

multiCSSTest :: Int -> Gen Property
multiCSSTest nparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p (Input c)) | r <- parties]
        choices <- forM parties $ \p -> cmsgs p <$> arbitrary
        runCSSTest nparties nparties corruptWeight (justs <> mconcat choices) iStates iCores
    where
        parties = [0..fromIntegral nparties-1]
        corruptWeight = (nparties - 1) `div` 3
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

multiCSSTestWithSilentCorrupt :: Int -> Gen Property
multiCSSTestWithSilentCorrupt allparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs (fromIntegral p) <$> arbitrary
        runCSSTest allparties nparties corruptWeight (justs <> mconcat choices) iStates iCores
    where
        nparties = allparties - corruptWeight
        parties = [0..fromIntegral nparties-1]
        corruptWeight = (allparties - 1) `div` 3
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

multiCSSTestWithActiveCorrupt :: Int -> Gen Property
multiCSSTestWithActiveCorrupt allparties = do
        let justs = Seq.fromList [(r, JustifyChoice c) | r <- parties, c <- [True,False]]
        let cmsgs p c = Seq.fromList [(r, ReceiveCSSMessage p (Input c)) | r <-parties]
        choices <- forM parties $ \p -> cmsgs (fromIntegral p) <$> arbitrary
        let corruptMsgs = Seq.fromList $
                            [(r, ReceiveCSSMessage cp (Input c)) | r <- parties, cp <- corruptParties, c <- [True,False]]
                            ++ [(r, ReceiveCSSMessage cp (Seen $ singletonNominationSet p c)) | r <- parties, cp <- corruptParties, p <- parties ++ corruptParties, c <- [True,False]]
                            ++ [(r, ReceiveCSSMessage cp (DoneReporting emptyNominationSet)) | r <- parties, cp <- corruptParties]
        runCSSTest allparties nparties corruptWeight (justs <> mconcat choices <> corruptMsgs) iStates iCores
    where
        nparties = allparties - corruptWeight
        parties = [0..fromIntegral nparties-1]
        corruptWeight = (allparties - 1) `div` 3
        corruptParties = [fromIntegral nparties .. fromIntegral allparties-1]
        iStates = Vec.replicate nparties initialCSSState
        iCores = Vec.replicate nparties (First Nothing)

singleCSSWithBadEnv :: Int -> Gen Property
singleCSSWithBadEnv allparties = do
        let justs = Seq.fromList [(0, JustifyChoice c) | c <- [True,False]]
        let cmsgs c = Seq.fromList [(0, ReceiveCSSMessage 0 (Input c))]
        choices <- cmsgs <$> arbitrary
        let corruptMsgs = Seq.fromList $
                            [(0, ReceiveCSSMessage cp (Input c)) | cp <- corruptParties, c <- [True,False]]
                            ++ [(0, ReceiveCSSMessage cp (Seen $ singletonNominationSet p c)) | cp <- corruptParties, p <- 0: corruptParties, c <- [True,False]]
                            ++ [(0, ReceiveCSSMessage cp (DoneReporting emptyNominationSet)) | cp <- corruptParties]
        runCSSTest' noCoresCheck allparties 1 corruptWeight (justs <> choices <> corruptMsgs <> corruptMsgs) iStates iCores
    where
        corruptWeight = (allparties - 1) `div` 3
        corruptParties = [1..fromIntegral allparties - 1]
        iStates = Vec.singleton initialCSSState
        iCores = Vec.singleton (First Nothing)


tests :: Spec
tests = parallel $ describe "Concordium.Afgjort.CSS" $ do
    it "5 parties" $ withMaxSuccess 1000 $ multiCSSTest 5
    it "10 parties" $ withMaxSuccess 100 $ multiCSSTest 10
    it "15 parties" $ withMaxSuccess 100 $ multiCSSTest 15
    it "30 parties" $ withMaxSuccess 10 $ multiCSSTest 30
    it "7 parties (2 silent)" $ withMaxSuccess 10000 $ multiCSSTestWithSilentCorrupt 7
    it "15 parties (4 silent)" $ withMaxSuccess 1000 $ multiCSSTestWithSilentCorrupt 15
    it "4 parties (1 active corrupt)" $ withMaxSuccess 50000 $ multiCSSTestWithActiveCorrupt 4
    it "1 party + 4 corrupt" $ withMaxSuccess 10000 $ singleCSSWithBadEnv 5