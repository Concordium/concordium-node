{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, ParallelListComp, OverloadedStrings #-}
module ConcordiumTests.Afgjort.Freeze where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Data.Either
import Control.Monad.Identity
import Data.List

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Afgjort.Types
import Concordium.Afgjort.Freeze

import Test.QuickCheck
import Test.Hspec

blockA :: Val
blockA = H.hash "A"

blockB :: Val
blockB = H.hash "B"

blockC :: Val
blockC = H.hash "C"

blockD :: Val
blockD = H.hash "D"

blocks :: [Val]
blocks = [blockA, blockB, blockC, blockD]

-- |An invariant predicate over 'FreezeState's.
invariantFreezeState :: Int -> Int -> (Party -> Int) -> FreezeState -> Bool
invariantFreezeState tw cw pw fs = isRight (invariantFreezeState' tw cw pw fs)

invariantFreezeState' :: Int -> Int -> (Party -> Int) -> FreezeState -> Either String ()
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
        propAcc (aps, jps, prs, djps, po) v (vd, wt, pts) = (aps + wt, if vd then jps + wt else jps, prs `Set.union` pts, if vd && wt > 0 then djps + 1 else djps,
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

data FreezeInput
    = FICandidate Val
    | FIRequestProposal Val
    | FIProposal Party Val
    | FIVote Party (Maybe Val)
    deriving (Eq, Ord, Show)

data FreezeOutput
    = FOMessage FreezeMessage
    | FOComplete (Maybe Val)
    | FOJustifiedDecision (Maybe Val)
    deriving (Eq, Ord, Show)

newtype FreezeT m a = FreezeT {
    runFreezeT :: RWST (FreezeInstance) [Either (FreezeOutput) (FreezeState)] (FreezeState) m a
} deriving (Functor, Applicative, Monad)

instance (Monad m) => MonadReader (FreezeInstance) (FreezeT m) where
    ask = FreezeT ask
    reader = FreezeT . reader
    local f = FreezeT . local f . runFreezeT


instance (Monad m) => (MonadState (FreezeState) (FreezeT m)) where
    get = FreezeT get
    put = FreezeT . put
    state = FreezeT . state

{-
assertInvariant :: (Monad m, Ord val, Ord party) => FreezeT m ()
assertInvariant = FreezeT $ do
    (totalWeight, corruptWeight, partyWeight) <- ask
    state <- use _2
    assert (invariantFreezeState totalWeight corruptWeight partyWeight state) (return ())
-}

tellState :: Monad m => FreezeT m ()
tellState = FreezeT $ do
    st <- get
    tell [Right st]

instance (Monad m) => FreezeMonad (FreezeT m) where
    sendFreezeMessage m = FreezeT (tell [Left $ FOMessage m])
    frozen v = FreezeT (tell [Left $ FOComplete v])
    decisionJustified v = FreezeT (tell [Left $ FOJustifiedDecision v])

doFreezeT :: (Monad m) => FreezeInstance -> FreezeState -> FreezeT m a -> m (a, FreezeState, [Either (FreezeOutput) (FreezeState)])
doFreezeT ctxt st a = runRWST (runFreezeT (a >>= \r -> tellState >> return r)) ctxt st
 
runFreezeSequence :: FreezeInstance -> [FreezeInput] -> [Either (FreezeInput) (Either (FreezeOutput) (FreezeState))]
runFreezeSequence fi ins = go initialFreezeState ins
    where
        go :: FreezeState -> [FreezeInput] -> [Either (FreezeInput) (Either (FreezeOutput) (FreezeState))]
        go _ [] = []
        go st (e : es) = let (_, st', out) = runIdentity (doFreezeT fi st (exec e)) in (Left e) : (Right <$> out) ++ go st' es
        exec :: FreezeInput -> FreezeT Identity ()
        exec (FICandidate v) = justifyCandidate v
        exec (FIRequestProposal v) = propose v
        exec (FIProposal p v) = receiveFreezeMessage p (Proposal v)
        exec (FIVote p v) = receiveFreezeMessage p (Vote v)
        

equalParties :: Int -> Int -> Party -> FreezeInstance
equalParties n c me = FreezeInstance n c wt me
        where
            wt x
                | x >= 0 && x < fromIntegral n   = 1
                | otherwise         = 0

type FreezeExample = (FreezeInstance, [FreezeInput], [FreezeOutput]) 

ex1 :: FreezeExample
ex1 = (equalParties 2 0 0, [FICandidate blockA, FICandidate blockB, FIProposal 1 blockB, FIRequestProposal blockB, FIVote 1 (Just blockB)],
    [FOMessage (Vote (Just blockB)), FOMessage (Proposal blockB), FOJustifiedDecision (Just blockB), FOComplete (Just blockB)])

ex2 :: FreezeExample
ex2 = (equalParties 2 0 0, [FIProposal 1 blockA, FIRequestProposal blockB, FICandidate blockA, FICandidate blockB, FIVote 1 Nothing],
    [FOMessage (Vote (Nothing)), FOMessage (Proposal blockB), FOJustifiedDecision Nothing, FOComplete Nothing])
    
testFreezeExampleAllPerms :: FreezeExample -> Spec
testFreezeExampleAllPerms (ctx, inp, outp) = sequence_ [it ("permutation " ++ show n) $ testFreezeExample (ctx, inp', outp) | inp' <- permutations inp | n <- [(0::Int)..]]

checkInvariant :: (HasCallStack) => FreezeInstance -> FreezeState -> Expectation
checkInvariant (FreezeInstance tw cw pw _) st = case invariantFreezeState' tw cw pw st of
        Left e -> expectationFailure $ show st ++ ": " ++ e
        Right () -> return ()

genInputs :: [Party] -> [Val] -> Gen [FreezeInput]
genInputs ps vs = listOf1 $ oneof [FICandidate <$> elements vs, FIRequestProposal <$> elements vs, FIProposal <$> elements ps <*> elements vs, FIVote <$> elements ps <*> elements (Nothing : (Just <$> vs))]

qcInvariant :: Word -> Property
qcInvariant lvl = withMaxSuccess (100 * 10^lvl) $ forAll (genInputs [1..5] blocks) $ \inp -> let out = outp inp in classify (length out == 3) "completed protocol" (testInv $ rights $ out)    where
        outp = rights . runFreezeSequence ctx
        ctx@(FreezeInstance tw cw pw _) = equalParties 6 1 0
        testInv l = conjoin [ counterexample (show st) $ invariantFreezeState' tw cw pw st === Right () | st <- l]

-- This example was a failing case generated by QuickCheck
testStream1 :: (Int, Int, [FreezeInput])
testStream1 = (6, 1, [FIProposal 4 blockC,FIRequestProposal blockC,FICandidate blockB,FICandidate blockA,FIRequestProposal blockC,FICandidate blockB,FIProposal 5 blockC,FICandidate blockA,FIProposal 5 blockB,FIRequestProposal blockC,FICandidate blockD,FICandidate blockB,FICandidate blockD,FIRequestProposal blockC,FIProposal 4 blockA,FIVote 1 (Just blockC),FIProposal 4 blockD,FIVote 3 (Just blockC),FIRequestProposal blockC,FIRequestProposal blockB,FICandidate blockD,FIProposal 1 blockC,FIVote 5 (Just blockC),FIRequestProposal blockA,FIRequestProposal blockD,FIRequestProposal blockA,FICandidate blockA,FIProposal 4 blockD,FIVote 2 (Just blockC),FIProposal 3 blockB,FIVote 1 (Just blockC),FIProposal 3 blockB,FIProposal 4 blockC,FIVote 4 (Just blockC),FIRequestProposal blockB,FICandidate blockA,FIProposal 2 blockD,FIRequestProposal blockC,FICandidate blockA,FICandidate blockB,FIProposal 5 blockC,FIVote 4 Nothing,FIProposal 1 blockB,FIVote 5 (Just blockD),FIRequestProposal blockA,FIProposal 3 blockA,FICandidate blockB,FIProposal 2 blockC,FICandidate blockC,FIRequestProposal blockC])

testQCInvariantExample :: (Int, Int, [FreezeInput]) -> Spec
testQCInvariantExample (tp, cp, inp) = sequence_ [it "state satisfies invariant" $ checkInvariant ctx st | st <- rights $ rights $ runFreezeSequence ctx inp]
    where
        ctx = (equalParties tp cp 0)

testStream2 :: (Int, Int, [FreezeInput])
testStream2 = (4, 1, [FICandidate blockA, FICandidate blockB, FICandidate blockC, FIRequestProposal blockA, FIProposal 1 blockA, FIProposal 2 blockB, FIProposal 3 blockB, FIVote 1 (Just blockA), FIVote 2 (Just blockB)])

testStream3 :: (Int, Int, [FreezeInput])
testStream3 = (6, 1, [FIVote 4 (Just blockC),FIProposal 3 blockD,FICandidate blockB,FIVote 2 (Just blockC),FIProposal 2 blockC,FICandidate blockB,FICandidate blockA,FIRequestProposal blockC,FIVote 1 (Just blockB),FIProposal 5 blockD,FIVote 2 Nothing,FIRequestProposal blockC,FICandidate blockD,FIVote 1 (Just blockA),FIVote 4 (Just blockB),FIVote 1 (Just blockB),FIProposal 2 blockC,FIRequestProposal blockB,FIProposal 5 blockC,FIProposal 2 blockC,FIVote 3 Nothing,FIProposal 2 blockC,FIVote 4 (Just blockB),FIProposal 1 blockC,FICandidate blockD,FIVote 3 Nothing,FIRequestProposal blockB,FICandidate blockA,FIVote 4 (Just blockA),FIVote 2 (Just blockD),FIVote 5 Nothing,FICandidate blockD,FIProposal 1 blockC,FIRequestProposal blockA,FIRequestProposal blockC,FIRequestProposal blockC,FIVote 5 (Just blockB),FIProposal 4 blockC,FICandidate blockC])

testFreezeExample :: FreezeExample -> Expectation
testFreezeExample (ctx, inp, outp) = do
        sequence_ [checkInvariant ctx st | st <- rights (rights res)]
        (Set.fromList $ lefts $ rights res) `shouldBe` (Set.fromList outp)
    where
        res = runFreezeSequence ctx inp

testInitialInvariant :: Spec    
testInitialInvariant = it "Invariant holds for intitial freeze state" (invariantFreezeState 0 0 (\_ -> undefined) (initialFreezeState :: FreezeState))

tests :: Word -> Spec
tests lvl = parallel $ describe "Concordium.Afgjort.Freeze" $ do
    testInitialInvariant
    describe "ex1" $ testFreezeExampleAllPerms ex1
    describe "ex2" $ testFreezeExampleAllPerms ex2
    describe "testStream1" $ testQCInvariantExample testStream1
    describe "testStream2" $ testQCInvariantExample testStream2
    describe "testStream3" $ testQCInvariantExample testStream3
    it "invariant on random trace" $ qcInvariant lvl