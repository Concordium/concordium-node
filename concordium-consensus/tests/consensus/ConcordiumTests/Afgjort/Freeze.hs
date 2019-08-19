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
import qualified Concordium.Afgjort.PartySet as PS
import qualified Concordium.Afgjort.PartyMap as PM

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
invariantFreezeState :: VoterPower -> VoterPower -> (Party -> VoterPower) -> FreezeState -> Bool
invariantFreezeState tw cw pw fs = isRight (invariantFreezeState' tw cw pw fs)

invariantFreezeState' :: VoterPower -> VoterPower -> (Party -> VoterPower) -> FreezeState -> Either String ()
invariantFreezeState' totalWeight corruptWeight partyWeight fs = do
    checkBinary (==) pers (_justifiedProposers fs) "==" "computed set of justified proposers" "given set"
    checkBinary (==) distJProps (_distinctJustifiedProposals fs) "==" "computed distinct justified proposals" "given distinct justified proposals"
    checkBinary (<=) (PS.weight (_justifiedProposers fs)) totalWeight "<=" "weight of justified proposers" "total weight"
    propOK
    checkBinary (==) justVoters (_justifiedVoters fs) "==" "computed set of justified voters" "given set"
    checkBinary (==) justDecs (_justifiedDecisions fs) "==" "computed set of justified decisions" "given set"
    checkBinary (<=) (PS.weight justVoters) totalWeight "<=" "total justified vote weight" "total party weight"
    checkBinary (==) (PS.weight justVoters >= totalWeight - corruptWeight && not (Set.null justDecs)) (_completed fs)
        "<->" "justified voter weight >= n - t && some decision is justified" "freeze is complete"
    voteOK
    where
        propAcc (prs, djps, po) v (vd, pts) = (if vd then PS.union partyWeight prs (PM.keysSet pts) else prs, if vd && PM.weight pts > 0 then djps + 1 else djps,
                                                        po >> checkPartyMap (show v ++ " proposal") pts
                                                        )
        (pers, distJProps, propOK) = Map.foldlWithKey propAcc (PS.empty, 0, pure ()) (_proposals fs)
        voteAcc (jvs, jds, vo) v (vd, pts) = (if vd then PS.union partyWeight jvs (PM.keysSet pts) else jvs,
                                                if vd && PM.weight pts > corruptWeight then Set.insert v jds else jds,
                                                        vo >> checkPartyMap (show v ++ " vote") pts
                                                        >> checkBinary (==) vd (voteJustified v) "<->" ("vote for " ++ show v ++ " recorded as justified") ("vote is justified"))
        voteJustified Nothing = distJProps >= 2 && PS.size pers >= 2
        voteJustified (Just v) = case Map.lookup v (_proposals fs) of
            Nothing -> False
            (Just (j, pts)) -> j && PM.weight pts >= totalWeight - 2 * corruptWeight
        (justVoters, justDecs, voteOK) = Map.foldlWithKey voteAcc (PS.empty, Set.empty, pure ()) (_votes fs)
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        checkPartyMap s pm = checkPartySet s (PM.keysSet pm)
        checkPartySet s ps = checkBinary (==) (PS.weight ps) (sum $ partyWeight <$> PS.toList ps) "==" (s ++ " weight") "computed value"

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
equalParties n c me = FreezeInstance (fromIntegral n) (fromIntegral c) wt me
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

ex3 :: FreezeExample
ex3 = (equalParties 4 1 0, [FICandidate blockA, FIProposal 1 blockA, FIRequestProposal blockA, FIProposal 2 blockA, FIProposal 3 blockA, FIProposal 3 blockB, FIVote 1 Nothing, FIVote 2 Nothing, FICandidate blockB],
    [FOMessage (Proposal blockA), FOMessage (Vote $ Just blockA), FOComplete Nothing, FOJustifiedDecision Nothing])

testFreezeExampleAllPerms :: FreezeExample -> Spec
testFreezeExampleAllPerms (ctx, inp, outp) = sequence_ [it ("permutation " ++ show n) $ testFreezeExample (ctx, inp', outp) | inp' <- permutations inp | n <- [(0::Int)..]]

checkInvariant :: (HasCallStack) => FreezeInstance -> FreezeState -> Expectation
checkInvariant (FreezeInstance tw cw pw _) st = case invariantFreezeState' tw cw pw st of
        Left e -> expectationFailure $ show st ++ ": " ++ e
        Right () -> return ()

genInputs :: [Party] -> [Val] -> Gen [FreezeInput]
genInputs ps vs = listOf1 $ oneof [FICandidate <$> elements vs, FIRequestProposal <$> elements vs, FIProposal <$> elements ps <*> elements vs, FIVote <$> elements ps <*> elements (Nothing : (Just <$> vs))]

qcInvariant :: Property
qcInvariant = withMaxSuccess 100000 $ forAll (genInputs [1..5] blocks) $ \inp -> let out = outp inp in classify (completed out) "completed protocol" (testInv $ rights $ out)
    where
        outp = rights . runFreezeSequence ctx
        ctx@(FreezeInstance tw cw pw _) = equalParties 6 1 0
        testInv l = conjoin [ counterexample (show st) $ invariantFreezeState' tw cw pw st === Right () | st <- l]
        completed [] = False
        completed (Left (FOComplete _) : _) = True
        completed (_ : r) = completed r

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

tests :: Spec
tests = parallel $ describe "Concordium.Afgjort.Freeze" $ do
    testInitialInvariant
    describe "ex1" $ testFreezeExampleAllPerms ex1
    describe "ex2" $ testFreezeExampleAllPerms ex2
    it "ex3" $ testFreezeExample ex3
    describe "testStream1" $ testQCInvariantExample testStream1
    describe "testStream2" $ testQCInvariantExample testStream2
    describe "testStream3" $ testQCInvariantExample testStream3
    it "invariant on random trace" $ qcInvariant
