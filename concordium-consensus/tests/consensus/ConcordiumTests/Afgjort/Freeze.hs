{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ParallelListComp #-}
module ConcordiumTests.Afgjort.Freeze where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Concordium.Afgjort.Freeze
import Control.Monad.RWS
import Control.Monad.Writer.Class
import Control.Monad.State.Class
import Lens.Micro.Platform
import Control.Exception
import Data.Either
import Control.Monad.Identity
import Data.List

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Expectations

-- |An invariant predicate over 'FreezeState's.
invariantFreezeState :: (Ord val, Ord party, Show val, Show party) => Int -> Int -> (party -> Int) -> FreezeState val party -> Bool
invariantFreezeState tw cw pw fs = isRight (invariantFreezeState' tw cw pw fs)

invariantFreezeState' :: (Ord val, Ord party, Show val, Show party) => Int -> Int -> (party -> Int) -> FreezeState val party -> Either String ()
invariantFreezeState' totalWeight corruptWeight partyWeight fs = do
    checkBinary (<=) allProps totalWeight "<=" "total proposal weight" "total party weight"
    checkBinary (==) justProps (_totalProposals fs) "==" "computeted justified proposal weight" "given proposal weight"
    checkBinary (==) pers (_proposers fs) "==" "computed set of proposers" "given set"
    propOK
    checkBinary (==) distJProps (_distinctJustifiedProposals fs) "==" "computed distinct justified proposals" "given distinct justified proposals"
    checkBinary (<=) allVotes totalWeight "<=" "total vote weight" "total party weight"
    checkBinary (==) justVotes (_totalVotes fs) "==" "computed justified vote weight" "given vote weight"
    checkBinary (==) vers (_voters fs) "==" "computed set of voters" "given set"
    voteOK
    where
        propAcc (aps, jps, prs, djps, po) v (vd, wt, pts) = (aps + wt, if vd then jps + wt else jps, prs `Set.union` pts, if vd then djps + 1 else djps,
                                                        po >> checkBinary (==) wt (sum (partyWeight <$> Set.toList pts)) "==" ("given proposal weight for " ++ show v) "calculated proposal weight"
                                                        >> checkBinary (\s1 s2 -> Set.null (Set.intersection s1 s2)) prs pts "disjoint from" "already proposed parties" ("parties proposing " ++ show v)
                                                        )
        (allProps, justProps, pers, distJProps, propOK) = Map.foldlWithKey propAcc (0, 0, Set.empty, 0, pure ()) (_proposals fs)
        voteAcc (avs, jvs, vrs, vo) v (vd, wt, pts) = (avs + wt, if vd then jvs + wt else jvs, vrs `Set.union` pts,
                                                        vo >> checkBinary (==) wt (sum (partyWeight <$> Set.toList pts)) "==" ("given vote weight for " ++ show v) "calculated vote weight"
                                                        >> checkBinary (\s1 s2 -> Set.null (Set.intersection s1 s2)) vrs pts "disjoint from" "already voted parties" ("parties voting for " ++ show v)
                                                        >> checkBinary (==) vd (voteJustified v) "<->" ("vote for " ++ show v ++ " recorded as justified") ("vote is justified"))
        voteJustified Nothing = distJProps >= 2
        voteJustified (Just v) = case Map.lookup v (_proposals fs) of
            Nothing -> False
            (Just (j, wt, _)) -> j && wt >= totalWeight - 2 * corruptWeight
        (allVotes, justVotes, vers, voteOK) = Map.foldlWithKey voteAcc (0, 0, Set.empty, pure ()) (_votes fs)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

data FreezeInput party val
    = FICandidate val
    | FIRequestProposal val
    | FIProposal party val
    | FIVote party (Maybe val)
    deriving (Eq, Ord, Show)

data FreezeOutput val
    = FOMessage (FreezeMessage val)
    | FOComplete (Maybe val)
    | FOJustifiedDecision (Maybe val)
    deriving (Eq, Ord, Show)

type FreezeTState val party m = (Map.Map val (Maybe (FreezeT val party m ())), FreezeState val party)
type FreezeTContext party = (Int, Int, party -> Int)

newtype FreezeT val party m a = FreezeT {
    runFreezeT :: RWST (FreezeTContext party) [Either (FreezeOutput val) (FreezeState val party)] (FreezeTState val party m) m a
} deriving (Functor, Applicative, Monad)

instance (Monad m) => (MonadState (FreezeState val party) (FreezeT val party m)) where
    get = snd <$> FreezeT get
    put v = FreezeT (state (\(a, _) -> ((), (a,v))))
    state f = FreezeT (state (\(a, v) -> let (r, v') = f v in (r, (a, v'))))

{-
assertInvariant :: (Monad m, Ord val, Ord party) => FreezeT val party m ()
assertInvariant = FreezeT $ do
    (totalWeight, corruptWeight, partyWeight) <- ask
    state <- use _2
    assert (invariantFreezeState totalWeight corruptWeight partyWeight state) (return ())
-}

tellState :: Monad m => FreezeT val party m ()
tellState = FreezeT $ do
    state <- use _2
    tell [Right state]

instance (Monad m, Ord val, Ord party) => FreezeMonad val party (FreezeT val party m) where
    awaitCandidate v x = (FreezeT $ do
        candWait <- use (_1 . at v)
        case candWait of
            Nothing -> (_1 . at v) ?= Just x
            (Just Nothing) -> runFreezeT x
            (Just (Just a)) -> (_1 . at v) ?= Just (a >> x))
    broadcastFreezeMessage m = FreezeT (tell [Left $ FOMessage m])
    frozen v = FreezeT (tell [Left $ FOComplete v])
    decisionJustified v = FreezeT (tell [Left $ FOJustifiedDecision v])

notifyCandidate :: (Monad m, Ord val, Ord party) => val -> FreezeT val party m ()
notifyCandidate val = FreezeT $ do
        waiters <- (_1 . at val) <<.= Just Nothing
        case waiters of
            Just (Just a) -> runFreezeT a
            _ -> return ()

doFreezeT :: (Monad m, Ord val, Ord party) => FreezeTContext party -> FreezeTState val party m -> FreezeT val party m a -> m (a, FreezeTState val party m, [Either (FreezeOutput val) (FreezeState val party)])
doFreezeT context state a = runRWST (runFreezeT (a >>= \r -> tellState >> return r)) context state
 
runFreezeSequence :: forall val party. (Ord val, Ord party) => FreezeTContext party -> party -> [FreezeInput party val] -> [Either (FreezeInput party val) (Either (FreezeOutput val) (FreezeState val party))]
runFreezeSequence ctx@(totalWeight, corruptWeight, partyWeight) me ins = go (Map.empty, initialFreezeState) ins
    where
        fi :: FreezeInstance val party (FreezeT val party Identity)
        fi = newFreezeInstance totalWeight corruptWeight partyWeight me
        go st [] = []
        go st (e : es) = let (_, st', out) = runIdentity (doFreezeT ctx st (exec e)) in (Left e) : (Right <$> out) ++ go st' es
        exec (FICandidate v) = notifyCandidate v
        exec (FIRequestProposal v) = propose fi v
        exec (FIProposal p v) = freezeMessage fi p (Proposal v)
        exec (FIVote p v) = freezeMessage fi p (Vote v)
        

equalParties :: Int -> Int -> FreezeTContext Int
equalParties n c = (n, c, wt)
        where
            wt x
                | x >= 0 && x < n   = 1
                | otherwise         = 0

type FreezeExample = (FreezeTContext Int, Int, [FreezeInput Int String], [FreezeOutput String]) 

ex1 :: FreezeExample
ex1 = (equalParties 2 0, 0, [FICandidate "A", FICandidate "B", FIProposal 1 "B", FIRequestProposal "B", FIVote 1 (Just "B")],
    [FOMessage (Vote (Just "B")), FOMessage (Proposal "B"), FOJustifiedDecision (Just "B"), FOComplete (Just "B")])

ex2 :: FreezeExample
ex2 = (equalParties 2 0, 0, [FIProposal 1 "A", FIRequestProposal "B", FICandidate "A", FICandidate "B", FIVote 1 Nothing],
    [FOMessage (Vote (Nothing)), FOMessage (Proposal "B"), FOJustifiedDecision Nothing, FOComplete Nothing])
    
testFreezeExampleAllPerms :: FreezeExample -> Spec
testFreezeExampleAllPerms (ctx, me, inp, outp) = sequence_ [describe ("permutation " ++ show n) $ testFreezeExample (ctx, me, inp', outp) | inp' <- permutations inp | n <- [0..]]

checkInvariant :: (HasCallStack, Ord val, Ord party, Show val, Show party) => FreezeTContext party -> FreezeState val party -> Expectation
checkInvariant ctx@(tw,cw,pw) st = case invariantFreezeState' tw cw pw st of
        Left e -> expectationFailure $ show st ++ ": " ++ e
        Right () -> return ()

genInputs :: [party] -> [val] -> Gen [FreezeInput party val]
genInputs ps vs = listOf1 $ oneof [FICandidate <$> elements vs, FIRequestProposal <$> elements vs, FIProposal <$> elements ps <*> elements vs, FIVote <$> elements ps <*> elements (Nothing : (Just <$> vs))]

qcInvariant :: Property
qcInvariant = withMaxSuccess 100000 $ forAll (genInputs [1..5] ['A'..'D']) $ \inp -> let out = outp inp in classify (length out == 3) "completed protocol" (testInv $ rights $ out)    where
        outp = rights . runFreezeSequence ctx 0
        ctx@(tw, cw, pw) = (equalParties 6 1)
        testInv l = conjoin [ counterexample (show st) $ invariantFreezeState' tw cw pw st === Right () | st <- l]

-- This example was a failing case generated by QuickCheck
testStream1 :: (Int, Int, [FreezeInput Int Char])
testStream1 = (6, 1, [FIProposal 4 'C',FIRequestProposal 'C',FICandidate 'B',FICandidate 'A',FIRequestProposal 'C',FICandidate 'B',FIProposal 5 'C',FICandidate 'A',FIProposal 5 'B',FIRequestProposal 'C',FICandidate 'D',FICandidate 'B',FICandidate 'D',FIRequestProposal 'C',FIProposal 4 'A',FIVote 1 (Just 'C'),FIProposal 4 'D',FIVote 3 (Just 'C'),FIRequestProposal 'C',FIRequestProposal 'B',FICandidate 'D',FIProposal 1 'C',FIVote 5 (Just 'C'),FIRequestProposal 'A',FIRequestProposal 'D',FIRequestProposal 'A',FICandidate 'A',FIProposal 4 'D',FIVote 2 (Just 'C'),FIProposal 3 'B',FIVote 1 (Just 'C'),FIProposal 3 'B',FIProposal 4 'C',FIVote 4 (Just 'C'),FIRequestProposal 'B',FICandidate 'A',FIProposal 2 'D',FIRequestProposal 'C',FICandidate 'A',FICandidate 'B',FIProposal 5 'C',FIVote 4 Nothing,FIProposal 1 'B',FIVote 5 (Just 'D'),FIRequestProposal 'A',FIProposal 3 'A',FICandidate 'B',FIProposal 2 'C',FICandidate 'C',FIRequestProposal 'C'])

testQCInvariantExample :: (Int, Int, [FreezeInput Int Char]) -> Spec
testQCInvariantExample (tp, cp, inp) = sequence_ [it "state satisfies invariant" $ checkInvariant ctx st | st <- rights $ rights $ runFreezeSequence ctx 0 inp]
    where
        ctx@(tw, cw, pw) = (equalParties tp cp)

testStream2 = (4, 1, [FICandidate 'A', FICandidate 'B', FICandidate 'C', FIRequestProposal 'A', FIProposal 1 'A', FIProposal 2 'B', FIProposal 3 'B', FIVote 1 (Just 'A'), FIVote 2 (Just 'B')])

testStream3 = (6, 1, [FIVote 4 (Just 'C'),FIProposal 3 'D',FICandidate 'B',FIVote 2 (Just 'C'),FIProposal 2 'C',FICandidate 'B',FICandidate 'A',FIRequestProposal 'C',FIVote 1 (Just 'B'),FIProposal 5 'D',FIVote 2 Nothing,FIRequestProposal 'C',FICandidate 'D',FIVote 1 (Just 'A'),FIVote 4 (Just 'B'),FIVote 1 (Just 'B'),FIProposal 2 'C',FIRequestProposal 'B',FIProposal 5 'C',FIProposal 2 'C',FIVote 3 Nothing,FIProposal 2 'C',FIVote 4 (Just 'B'),FIProposal 1 'C',FICandidate 'D',FIVote 3 Nothing,FIRequestProposal 'B',FICandidate 'A',FIVote 4 (Just 'A'),FIVote 2 (Just 'D'),FIVote 5 Nothing,FICandidate 'D',FIProposal 1 'C',FIRequestProposal 'A',FIRequestProposal 'C',FIRequestProposal 'C',FIVote 5 (Just 'B'),FIProposal 4 'C',FICandidate 'C'])

testFreezeExample :: FreezeExample -> Spec
testFreezeExample (ctx@(tw,cw,pw), me, inp, outp) = do
        sequence_ [it "state satisfies invariant" $ checkInvariant ctx st | st <- rights (rights res)]
        it "Expected trace" $ (Set.fromList $ lefts $ rights res) `shouldBe` (Set.fromList outp)
    where
        res = runFreezeSequence ctx me inp
    
testInitialInvariant = it "Invariant holds for intitial freeze state" (invariantFreezeState 0 0 (\_ -> undefined) (initialFreezeState :: FreezeState Int Int))

tests :: Spec
tests = parallel $ describe "Concordium.Afgjort.Freeze" $ do
    testInitialInvariant
    describe "ex1" $ testFreezeExampleAllPerms ex1
    describe "ex2" $ testFreezeExampleAllPerms ex2
    describe "testStream1" $ testQCInvariantExample testStream1
    describe "testStream2" $ testQCInvariantExample testStream2
    describe "testStream3" $ testQCInvariantExample testStream3
    it "invariant on random trace" $ qcInvariant