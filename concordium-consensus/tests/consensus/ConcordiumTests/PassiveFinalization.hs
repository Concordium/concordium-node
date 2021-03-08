{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module ConcordiumTests.PassiveFinalization where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Lens.Micro.Platform

import Test.Hspec

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.Types (Party)
import Concordium.Afgjort.WMVBA

import Concordium.Birk.Bake

import Concordium.Crypto.SHA256

import Concordium.GlobalState
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.Types.IdentityProviders
import qualified Concordium.GlobalState.Basic.TreeState as TS
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import qualified Concordium.Types.SeedState as SeedState
import Concordium.GlobalState.DummyData (dummyAuthorizations, dummyChainParameters)

import Concordium.Logger

import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations

import Concordium.Startup (defaultFinalizationParameters, makeBakersByStake)

import Concordium.Types
import Concordium.Types.HashableTo

import qualified Concordium.GlobalState.DummyData as Dummy

-- Test that Concordium.Afgjort.Finalize.newPassiveRound has an effect on finalization;
-- specifically, that non-finalizers can successfully gather signatures from the pending-
-- finalization-message buffer and create finalization proofs from them. This is necessary
-- for cases where finalization messages are received out of order (i.e. messages for a later
-- finalization round arrive before the messages for an earlier finalization round).

dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

type Config t = SkovConfig MemoryTreeMemoryBlockConfig (ActiveFinalization t) NoHandler

finalizationParameters :: FinalizationParameters
finalizationParameters = defaultFinalizationParameters{finalizationMinimumSkip=100} -- setting minimum skip to 100 to prevent finalizers to finalize blocks when they store them

type MyHandlers = SkovHandlers DummyTimer (Config DummyTimer) (StateT () LogIO)

newtype DummyTimer = DummyTimer Integer

type MySkovT = SkovT MyHandlers (Config DummyTimer) (StateT () LogIO)

instance MonadFail MySkovT where
  fail = error

dummyHandlers :: MyHandlers
dummyHandlers = SkovHandlers {..}
    where
        shBroadcastFinalizationMessage _ = return ()
        shBroadcastFinalizationRecord _ = return ()
        shOnTimeout _ _ = return $ DummyTimer 0
        shCancelTimer _ = return ()
        shPendingLive = return ()

myRunSkovT :: (MonadIO m)
           => MySkovT a
           -> MyHandlers
           -> SkovContext (Config DummyTimer)
           -> SkovState (Config DummyTimer)
           -> m (a, SkovState (Config DummyTimer), ())
myRunSkovT a handlers ctx st = liftIO $ flip runLoggerT doLog $ do
        ((res, st'), _) <- runStateT (runSkovT a handlers ctx st) ()
        return (res, st', ())
    where
        doLog src LLError msg = error $ show src ++ ": " ++ msg
        doLog _ _ _ = return () -- traceM $ show src ++ ": " ++ msg

type BakerState = (BakerIdentity, SkovContext (Config DummyTimer), SkovState (Config DummyTimer))
type BakerInformation = (FullBakerInfo, BakerIdentity, Account)

-- This test has the following set up:
-- There are two bakers, baker1 and baker2, and a finalization-committee member, finMember.
-- (i) baker1 bakes two blocks A and B
-- (ii) baker2 receives finalization message for B at fin index 2
-- (iii) baker2 receives finalization message for A at fin index 1
-- (iv) baker2 bakes two blocks C and D
-- Then we test that C includes the record for A and D includes the record for B.
--
-- When baker2 receives a finalization message for fin index 2 (ii), this triggers an
-- attempt to finalize block B. However, Since the fin index is 2, and no finalization
-- has happened yet for fin index 1, the finalization message will be put into a buffer.
-- When baker2 subsequently receives a fin message for fin index 1 (iii), this triggers
-- the successful finalization of block A, and that first finalization record is put into
-- the finalization queue.
-- At the end of this finalization, a new finalization round for fin index 2 is attempted.
-- Since baker2 is not in the committee, this triggers `newPassiveRound`, which is what is being
-- tested.
-- `newPassiveRound` looks into the pending-finalization-message buffer. Due to (ii), this buffer
-- contains a finalization message for block B. As a result, baker2, a non-finalizer, can
-- successfully produce a finalization proof out of the existing signature created by finMember.
-- This resulting finalization record is also put into the finalization queue.
-- When afterwards block C is baked, baker2 takes the fin record for block A from the finalization
-- queue and adds it to C.
-- Similarly, when block D is baked, the fin record for block B is included in D.
--
-- If `newPassiveRound` were not called, the successful finalization of block A would not trigger
-- the finalization of a block based on messages from the pending queue. I.e., if finalization
-- messages are received out of order, non-finalizers will not be able to create finalization records,
-- and finalization will depend on bakers being also in the finalization committee.
runTestSimple :: BakerState -> BakerState -> BakerState -> [BakerState] -> IO ()
runTestSimple = runTest 2 [(1, ResultPendingFinalization), (0, ResultSuccess)]

-- Testing more messages. Creating finalization messages for blocks of the following slots:
--      1 -> 2 ->   (normal order, newPassiveRound unnecessary)
--      4 -> 3 ->   (reversed order, newPassiveRound necessary)
--      6 -> 5 ->   (one more reversed order, newPassiveRound necessary)
--      7 -> 8 ->   (normal order again)
--      12 -> 11 -> (too large indices, should be discarded)
--      8 ->        (index already finalized, should be discarded)
--      10 ->       (goes into pending, newPassiveReound will be necessary)
--      12 ->       (too large index, should be discarded)
--      9           (normal order, after this we should process 10 with newPassiveRound)
-- and making baker2 receive them before baker2 starts baking blocks.
runTestMany :: BakerState -> BakerState -> BakerState -> [BakerState] -> IO ()
runTestMany = runTest 12 [(0,  ResultSuccess),
                          (1,  ResultSuccess),
                          (3,  ResultPendingFinalization),
                          (2,  ResultSuccess),
                          (5,  ResultPendingFinalization),
                          (4,  ResultSuccess),
                          (6,  ResultSuccess),
                          (7,  ResultSuccess),
                          (11, ResultInvalid),
                          (10, ResultInvalid),
                          (7,  ResultDuplicate),
                          (9,  ResultPendingFinalization),
                          (10, ResultInvalid),
                          (8,  ResultSuccess)]

-- Same set up as runTest2, but test active finalization by making the finalization memeber (instead of baker2)
-- receive finalization messages.
runTestActiveMany :: BakerState -> BakerState -> BakerState -> [BakerState] -> IO ()
runTestActiveMany b1 _ fm = runTestMany b1 fm fm

runTest :: Int
        -- ^How many blocks should baker1 bake
        -> [(FinalizationIndex, UpdateResult)]
        -- ^This sequence indicates for which indices baker2 will receive finalization messages
        --  and which UpdateResult we should expect from the corresponding `finalizationReceiveMessage` call.
        -- For example, if the initial finalization index is 1, a pair (2, ResultSuccess) indicates that for
        -- round 1+2=3, we expect the receipt of a finalization message to result in ResultSuccess.
        -> BakerState
        -- ^Initial state for the first baker
        -> BakerState
        -- ^Initial state for the second baker who will receive finalization messages
        -> BakerState
        -- ^Initial state for one finalization committee member
        -> [BakerState]
        -- ^Initial states for the additional finalization committee members
        -> IO ()
runTest firstBlocks
        receivedIndicesAndExpectedResults
        (bid1, fi1, fs1)
        (bid2, fi2, fs2)
        finMember@(_, _, SkovState _ FinalizationState{_finsSessionId = sessionId, _finsCommittee = finCom} _ _)
        additionalFinMembers = do
            (blocks, _, _) <- myRunSkovT (mapM (bake bid1) [1..fromIntegral firstBlocks]) dummyHandlers fi1 fs1
            void $ myRunSkovT (do
                    -- Baker2 stores baker1's blocks
                    mapM_ store blocks
                    -- Baker2 receives finalization messages generated by the finalization-committee members
                    mapM_ (\(ind, res) ->
                            mapM_ (receiveFM (blocks !! fromIntegral ind) ind res) $ finMember : additionalFinMembers
                          ) receivedIndicesAndExpectedResults
                    -- If we expected n successful receipts of finalization messages, bake n more blocks and verify that they contain the first n blocks in their finalization records
                    let bakeVerify slot b ind = bakeForSlotAndVerify bid2 slot b ind sessionId finCom
                        successfulFins = length $ filter (\(_, r) -> r `elem` [ResultSuccess, ResultPendingFinalization]) receivedIndicesAndExpectedResults
                    mapM (\(i, b) -> bakeVerify (fromIntegral firstBlocks + i) b $ fromIntegral i) $ zip [1..] $ take (fromIntegral successfulFins) blocks
                ) dummyHandlers fi2 fs2
        where receiveFM block ind res (fmId, _, SkovState TS.SkovData{..} FinalizationState{..} _ _) =
                case _finsCurrentRound of
                    ActiveCurrentRound FinalizationRound{..} ->
                        receiveFinMessage (_finsIndex + ind) block roundDelta _finsSessionId roundMe fmId res
                    _ ->
                        fail "Finalizer should have active finalization round."

bake :: BakerIdentity -> Slot -> MySkovT BakedBlock
bake bid n = do
    mb <- bakeForSlot bid n
    maybe (fail $ "Could not bake for slot " ++ show n)
          (\BS.BlockPointer {_bpBlock = NormalBlock block} -> return block)
          mb

store :: (SkovMonad m, MonadFail m) => BakedBlock -> m ()
store block = storeBlock (makePendingBlock block dummyTime) >>= \case
    ResultSuccess -> return()
    result        -> fail $ "Could not store block " ++ show block ++ ". Reason: " ++ show result

receiveFinMessage :: (FinalizationMonad m, MonadIO m, MonadFail m)
                  => FinalizationIndex
                  -> BakedBlock -- the block to be finalized
                  -> BlockHeight
                  -> FinalizationSessionId
                  -> Party -- finalization committee member whose signature we create
                  -> BakerIdentity -- baker identity of finalization committee member
                  -> UpdateResult -- expected result
                  -> m ()
receiveFinMessage ind block delta sessId me bId expectedResult = do
    let msgHdr = FinalizationMessageHeader {
                   msgSessionId = sessId,
                   msgFinalizationIndex = ind,
                   msgDelta = delta,
                   msgSenderIndex = me
               }
        wmvbaMsg = makeWMVBAWitnessCreatorMessage (roundBaid sessId ind delta)
                                                (getHash block)
                                                (bakerAggregationKey bId)
        fmsg = signFinalizationMessage (bakerSignKey bId) msgHdr wmvbaMsg
    finalizationReceiveMessage (FPMMessage fmsg) >>= \result -> do
        unless (result == expectedResult || (result == ResultPendingBlock && expectedResult == ResultSuccess)) $
            fail $ "Could not receive finalization message for index " ++ show (theFinalizationIndex ind)
                ++ "\nfor the following block:\n" ++ show block
                ++ ".\nExpected result: " ++ show expectedResult ++ ". Actual result: " ++ show result
        liftIO $ putStrLn $ "Received finalization message for block " ++ show (bbSlot block) ++ " and fin index " ++ show (theFinalizationIndex ind)
            ++ " (" ++ show result ++ " as expected)"

bakeForSlotAndVerify :: BakerIdentity
              -> Slot
              -> BakedBlock
              -> FinalizationIndex
              -> FinalizationSessionId
              -> FinalizationCommittee
              -> MySkovT ()
bakeForSlotAndVerify bid slot finBlock finInd sessId finCom = do
    block <- bake bid slot
    -- Check that block contains finalization record for finBlock
    case bfBlockFinalizationData $ bbFields block of
        BlockFinalizationData fr@FinalizationRecord{..} -> do
            assertEqual "Wrong finalization index" finInd finalizationIndex
            assertEqual "Wrong finalization block hash" (getHash finBlock :: BlockHash) finalizationBlockPointer
            assertEqual "Finalization proof not verified" True $ verifyFinalProof sessId finCom fr
            liftIO $ putStrLn $ "Block at slot " ++ show slot ++ " contains finalization proof for block at slot "
                                ++ show (bbSlot finBlock) ++ " and finalization index " ++ show (theFinalizationIndex finInd)
                                ++ " verified by finalization committee of " ++ show (length $ parties finCom) ++ " parties."
        _ ->
            fail "Block 3 does not include finalization record"

assertEqual :: (Show x, Eq x, Monad m) => String -> x -> x -> m ()
assertEqual msg expected actual =
    unless (expected == actual) $ error $ msg ++ ":\nExpected: " ++ show expected ++ "\n=Actual:" ++ show actual

-- Create initial states for two bakers, a finalization committee member, and a list of additional finalization committee members
createInitStates :: Int -> IO (BakerState, BakerState, BakerState, [BakerState])
createInitStates additionalFinMembers = do
    let bakerAmount = 10 ^ (4 :: Int)
        finMemberAmount = bakerAmount * 10 ^ (6 :: Int)
    let bis@(baker1:baker2:finMember:finMembers) = makeBakersByStake ([bakerAmount, bakerAmount, finMemberAmount] ++ take additionalFinMembers (repeat finMemberAmount))
    let 
        seedState = SeedState.initialSeedState (hash "LeadershipElectionNonce") 10
        bakerAccounts = map (\(_, _, acc, _) -> acc) bis
        cps = dummyChainParameters & cpElectionDifficulty .~ makeElectionDifficultyUnchecked 100000
        gen = GenesisDataV2 {
                genesisTime = 0,
                genesisSlotDuration = 1,
                genesisSeedState = seedState,
                genesisAccounts = bakerAccounts,
                genesisFinalizationParameters = finalizationParameters,
                genesisCryptographicParameters = Dummy.dummyCryptographicParameters,
                genesisIdentityProviders = emptyIdentityProviders,
                genesisAnonymityRevokers = Dummy.dummyArs,
                genesisMaxBlockEnergy = Energy maxBound,
                genesisAuthorizations = dummyAuthorizations,
                genesisChainParameters = cps
            }

        createState = liftIO . (\(bid, _, _, _) -> do
                                   let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                       config = SkovConfig
                                           (MTMBConfig defaultRuntimeParameters gen)
                                           (ActiveFinalization fininst)
                                           NoHandler
                                   (initCtx, initState) <- runSilentLogger (initialiseSkov config)
                                   return (bid, initCtx, initState))
    b1 <- createState baker1
    b2 <- createState baker2
    fState <- createState finMember
    fStates <- mapM createState finMembers
    return (b1, b2, fState, fStates)

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

withInitialStates :: Int -> (BakerState -> BakerState -> BakerState -> [BakerState] -> IO ()) -> IO ()
withInitialStates addFinMembers r = do
    (b1, b2, f, fs) <- createInitStates addFinMembers
    r b1 b2 f fs

test :: Spec
test = describe "Concordium.PassiveFinalization" $ do
    it "2 non-fin bakers, 1 fin member, received fin messages: round 2 -> round 1" $ withInitialStates 0 runTestSimple
    it "2 non-fin bakers, 1 fin member, multiple fin rounds" $ withInitialStates 0 runTestMany
    it "1 non-fin baker, 2 fin bakers, received fin messages by fin member, multiple fin rounds" $ withInitialStates 0 runTestActiveMany
    it "2 non-fin bakers, 5 fin members, multiple fin rounds" $ withInitialStates 4 runTestMany
    it "1 non-fin baker, 5 fin bakers, received fin messages by fin member, multiple fin rounds" $ withInitialStates 4 runTestActiveMany
