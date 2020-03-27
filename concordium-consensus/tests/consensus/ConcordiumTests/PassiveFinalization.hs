{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module ConcordiumTests.PassiveFinalization where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Lens.Micro.Platform
import System.IO.Unsafe
import System.Random

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.WMVBA

import Concordium.Birk.Bake

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Crypto.DummyData
import Concordium.Crypto.SHA256
import qualified Concordium.Crypto.VRF as VRF

import Concordium.GlobalState
import Concordium.GlobalState.Bakers
import qualified Concordium.GlobalState.Basic.TreeState as TS
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.SeedState

import Concordium.Logger

import qualified Concordium.Scheduler.Utils.Init.Example as Example

import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations

import Concordium.Startup (makeBakerAccount)

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions

-- Test that Concordium.Afgjort.Finalize.newPassiveRound has an effect on finalization;
-- specifically, that non-finalizers can successfully gather signatures from the pending-
-- finalization-message buffer and create finalization proofs from them. This is necessary
-- for cases where finalization messages are received out of order (i.e. messages for a later
-- finalization round arrive before the messages for an earlier finalization round).
--
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

{-# NOINLINE dummyCryptographicParameters #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
    fromMaybe
        (error "Could not read cryptographic parameters.")
        (unsafePerformIO (readCryptographicParameters <$> BSL.readFile "../scheduler/testdata/global.json"))

dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

type Config t = SkovConfig MemoryTreeMemoryBlockConfig (ActiveFinalization t) NoHandler

finalizationParameters :: FinalizationParameters
finalizationParameters = FinalizationParameters 2 1000

type MyHandlers = SkovHandlers DummyTimer (Config DummyTimer) (StateT () LogIO)

newtype DummyTimer = DummyTimer Integer

dummyHandlers :: MyHandlers
dummyHandlers = SkovHandlers {..}
    where
        shBroadcastFinalizationMessage _ = return ()
        shBroadcastFinalizationRecord _ = return ()
        shOnTimeout _ _ = return $ DummyTimer 0
        shCancelTimer _ = return ()
        shPendingLive = return ()

myRunSkovT :: (MonadIO m)
           => SkovT MyHandlers (Config DummyTimer) (StateT () LogIO) a
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
type BakerInformation = (BakerInfo, BakerIdentity, Account)

runTest :: BakerState
        -- ^State for the first baker
        -> BakerState
        -- ^State for the second baker
        -> BakerState
        -- ^State for the finalization committee member
        -> IO Property
runTest (bid1, fi1, fs1)
        (bid2, fi2, fs2)
        (fmId, _, SkovState TS.SkovData{..} FinalizationState{..} _ _) = do
            let bakeFirstSlots bid = do
                  b1 <- bake bid 1
                  b2 <- bake bid 2
                  return (b1, b2)
            -- Baker1 bakes first two blocks
            ((BS.BlockPointer {_bpBlock = NormalBlock block1},
              BS.BlockPointer {_bpBlock = NormalBlock block2}), _, _) <- myRunSkovT (bakeFirstSlots bid1) dummyHandlers fi1 fs1
            -- Baker2 stores baker1's blocks
            _ <- myRunSkovT (do
                    store block1
                    store block2
                    case _finsCurrentRound of
                        Right FinalizationRound{..} -> do
                            -- Creating finalization message for block2 and then for block1
                            -- and making baker2 receive them before baker2 starts baking blocks.
                            receiveFinMessage (_finsIndex + 1) block2 roundDelta roundMe ResultPendingFinalization
                            receiveFinMessage _finsIndex block1 roundDelta roundMe ResultSuccess
                            bake bid2 3 >>= \BS.BlockPointer {_bpBlock = NormalBlock block3} ->
                                -- Check that block3 contains finalization record for block1
                                verifyFinRec block3 1 block1 _finsSessionId _finsCommittee
                            bake bid2 4 >>= \BS.BlockPointer {_bpBlock = NormalBlock block4} ->
                                -- Check that block4 contains finalization record for block2
                                verifyFinRec block4 2 block2 _finsSessionId _finsCommittee
                        _ ->
                            fail "Finalizer should have active finalization round."
                ) dummyHandlers fi2 fs2
            return $ property True


                where bake bid n = do
                          mb <- bakeForSlot bid n
                          maybe (fail $ "Could not bake for slot " ++ show n) return mb

                      store block =
                        storeBlock (makePendingBlock block dummyTime) >>= \case
                            ResultSuccess -> return()
                            result        -> fail $ "Could not store block " ++ show block ++ ". Reason: " ++ show result

                      receiveFinMessage ind block delta me expectedResult = do
                          let msgHdr = FinalizationMessageHeader {
                                           msgSessionId = _finsSessionId,
                                           msgFinalizationIndex = ind,
                                           msgDelta = delta,
                                           msgSenderIndex = me
                                       }
                              wmvbaMsg = makeWMVBAWitnessCreatorMessage (roundBaid _finsSessionId ind delta)
                                                                        (getHash block)
                                                                        (bakerAggregationKey fmId)
                              fmsg = signFinalizationMessage (bakerSignKey fmId) msgHdr wmvbaMsg
                          finalizationReceiveMessage (FPMMessage fmsg) >>= \result ->
                              unless (result == expectedResult) $
                                fail $ "Could not receive finalization message for the following block:\n" ++ show block
                                        ++ ".\nExpected result: " ++ show result ++ ". Actual result: " ++ show expectedResult

                      verifyFinRec block finInd finBlock sessId finCom = case bfBlockFinalizationData $ bbFields block of
                          BlockFinalizationData fr@FinalizationRecord{..} -> do
                              assertEqual "Wrong finalization index" finInd finalizationIndex
                              assertEqual "Wrong finalization block hash" (getHash finBlock :: BlockHash) finalizationBlockPointer
                              assertEqual "Finalization proof not verified" True $ verifyFinalProof sessId finCom fr
                          _ ->
                              fail "Block 3 does not include finalization record"


assertEqual :: (Show x, Eq x, Monad m) => String -> x -> x -> m ()
assertEqual msg expected actual =
    unless (expected == actual) $ error $ msg ++ ":\nExpected: " ++ show expected ++ "\n=Actual:" ++ show actual

makeBaker :: BakerId -> Amount -> Gen BakerInformation
makeBaker bid initAmount = resize 0x20000000 $ do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk                     <- genBlockKeyPair
        blssk                  <- fst . randomBlsSecretKey . mkStdGen <$> arbitrary
        let spk     = Sig.verifyKey sk
        let blspk   = Bls.derivePublicKey blssk
        let account = makeBakerAccount bid initAmount
        return (BakerInfo epk spk blspk initAmount (_accountAddress account), BakerIdentity sk ek blssk, account)

-- Create initial states for two bakers and a finalization committee member
createInitStates :: PropertyM IO (BakerState, BakerState, BakerState)
createInitStates = do
    let bakerAmount = 10 ^ (4 :: Int)
    baker1 <- pick $ makeBaker 0 bakerAmount
    baker2 <- pick $ makeBaker 1 bakerAmount
    finMember <- pick $ makeBaker 2 (bakerAmount * 10 ^ (6 :: Int))
    let bis = [baker1, baker2, finMember]
        genesisBakers = fst . bakersFromList $ (^. _1) <$> bis
        bps = BirkParameters 1 genesisBakers genesisBakers genesisBakers (genesisSeedState (hash "LeadershipElectionNonce") 10)
        bakerAccounts = map (\(_, _, acc) -> acc) bis
        gen = GenesisData 0 1 bps bakerAccounts [] finalizationParameters dummyCryptographicParameters [] 10 $ Energy maxBound
        createState = liftIO . (\(_, bid, _) -> do
                                   let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                       config = SkovConfig
                                           (MTMBConfig defaultRuntimeParameters gen (Example.initialState bps dummyCryptographicParameters bakerAccounts [] 2 []))
                                           (ActiveFinalization fininst gen)
                                           NoHandler
                                   (initCtx, initState) <- liftIO $ initialiseSkov config
                                   return (bid, initCtx, initState))
    b1 <- createState baker1
    b2 <- createState baker2
    fState <- createState finMember
    return (b1, b2, fState)

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

instance Show FinalizationInstance where
    show _ = "[Finalization Instance]"

withInitialStates :: (BakerState -> BakerState -> BakerState -> IO Property) -> Property
withInitialStates r = monadicIO $ do
    (b1, b2, fs) <- createInitStates
    liftIO $ r b1 b2 fs

tests :: Word -> Spec
tests lvl = describe "Concordium.PassiveFinalization" $
    it "non-finalizer creates finalization records out of existing finalizer signatures" $ withMaxSuccess (10^lvl) $ withInitialStates runTest