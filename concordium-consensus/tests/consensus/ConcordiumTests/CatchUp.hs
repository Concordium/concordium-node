{-# LANGUAGE TupleSections, OverloadedStrings, InstanceSigs, FlexibleContexts, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
module ConcordiumTests.CatchUp where

import qualified Data.Sequence as Seq
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import Data.Foldable
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform
import System.Random
import Data.Serialize
import Data.Maybe

import Concordium.Crypto.SHA256
import qualified Concordium.Crypto.SignatureScheme as SigScheme

import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.IdentityProviders
import qualified Concordium.GlobalState.Basic.TreeState as BTS
import qualified Concordium.GlobalState.Basic.BlockState as BState
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.SeedState as SeedState
import Concordium.GlobalState
import Concordium.GlobalState.Finalization
import Concordium.Types (Amount(..))
import Concordium.Types.HashableTo
import Concordium.GlobalState.DummyData (dummyChainParameters, dummyAuthorizations)

import Concordium.Logger
import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations
import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Types (Energy(..))
import Concordium.Startup (defaultFinalizationParameters)

import ConcordiumTests.Konsensus hiding (tests)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

runKonsensus :: RandomGen g => Int -> g -> States -> ExecState -> IO States
runKonsensus steps g states es
    | steps <= 0 || null (es ^. esEventPool) = return states
    | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g (es ^. esEventPool)
            let es1 = es & esEventPool .~ events'
            let (bkr, _, _, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            let continue fs' es' = do
                        let states' = states & ix rcpt . _5 .~ fs'
                        runKonsensus (steps - 1) g' states' es'
            let handlers = dummyHandlers rcpt btargets
            case ev of
                EBake sl -> do
                    (mb, fs', es2) <- myRunSkovT (bakeForSlot bkr sl) handlers fi fs es1
                    case mb of
                        Nothing -> continue fs' (es2 & esEventPool %~ ((rcpt, EBake (sl + 1)) Seq.<|))
                        Just BlockPointer{_bpBlock = NormalBlock b} ->
                            continue fs' (es2 & esEventPool %~ (<> Seq.fromList ((rcpt, EBake (sl + 1)) : [(r, EBlock b) | r <- btargets])))
                        Just _ -> error "Baked genesis block"

                EBlock block -> do
                    (_, fs', es') <- myRunSkovT (storeBlock (B.makePendingBlock block dummyTime)) handlers fi fs es1
                    continue fs' es'
                ETransaction tr -> do
                    (_, fs', es') <- myRunSkovT (receiveTransaction tr) handlers fi fs es1
                    continue fs' es'
                EFinalization fmsg -> do
                    (_, fs', es') <- myRunSkovT (finalizationReceiveMessage fmsg) handlers fi fs es1
                    continue fs' es'
                EFinalizationRecord frec -> do
                    (_, fs', es') <- myRunSkovT (finalizationReceiveRecord False frec) handlers fi fs es1
                    continue fs' es'
                ETimer t timerEvent ->
                    if t `Set.member` (es ^. esCancelledTimers) then
                        runKonsensus steps g' states (es1 & esCancelledTimers %~ Set.delete t)
                    else do
                        (_, fs', es') <- myRunSkovT timerEvent handlers fi fs es1
                        continue fs' es'

-- |Create initial states where the first baker is a dictator with respect to finalization.
initialiseStatesDictator :: Int -> PropertyM IO States
initialiseStatesDictator n = do
        let bns = [0..fromIntegral n - 1]
        bis <- mapM (\i -> (i,) <$> pick (makeBaker i 1)) bns
        let genesisBakers = fst . bakersFromList $ (^. _2 . _1) <$> bis
        let seedState = SeedState.genesisSeedState (hash "LeadershipElectionNonce") 10
            bps = BState.BasicBirkParameters genesisBakers genesisBakers genesisBakers seedState
            fps = defaultFinalizationParameters
            bakerAccounts = map (\(_, (_, _, acc, _)) -> acc) bis
            gen = GenesisData 0 1 genesisBakers seedState bakerAccounts fps dummyCryptographicParameters emptyIdentityProviders dummyArs 10 (Energy maxBound) dummyAuthorizations dummyChainParameters
        res <- liftIO $ mapM (\(_, (binfo, bid, _, kp)) -> do
                                let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                let config = SkovConfig
                                        (MTMBConfig defaultRuntimeParameters gen (Example.initialStateWithMateuszAccount bps dummyCryptographicParameters bakerAccounts emptyIdentityProviders nAccounts (Amount (2 ^ (40 :: Int))) dummyAuthorizations dummyChainParameters))
                                        (ActiveFinalization fininst)
                                        NoHandler
                                (initCtx, initState) <- liftIO $ runSilentLogger (initialiseSkov config)
                                return (bid, binfo, kp, initCtx, initState)
                             ) bis
        return $ Vec.fromList res

simpleCatchUpCheck :: States -> Property
simpleCatchUpCheck ss =
        conjoin [monadicIO $ catchUpCheck s1 s2 | s1 <- toList ss, s2 <- toList ss ]

type TrivialHandlers = SkovHandlers DummyTimer (Config DummyTimer) LogIO

trivialHandlers :: TrivialHandlers
trivialHandlers = SkovHandlers {..}
    where
        shBroadcastFinalizationMessage _ = error "Unimplemented"
        shBroadcastFinalizationRecord _ = error "Unimplemented"
        shOnTimeout _ _ = error "Unimplemented"
        shCancelTimer _ = error "Unimplemented"
        shPendingLive = error "Unimplemented"

trivialEvalSkovT :: (MonadIO m) => SkovT TrivialHandlers (Config DummyTimer) LogIO a -> SkovContext (Config DummyTimer) -> SkovState (Config DummyTimer) -> m a
trivialEvalSkovT a ctx st = liftIO $ flip runLoggerT doLog $ evalSkovT a trivialHandlers ctx st
    where
        doLog src LLError msg = error $ show src ++ ": " ++ msg
        doLog _ _ _ = return ()

catchUpCheck :: (BakerIdentity, FullBakerInfo, SigScheme.KeyPair, SkovContext (Config DummyTimer), SkovState (Config DummyTimer)) -> (BakerIdentity, FullBakerInfo, SigScheme.KeyPair, SkovContext (Config DummyTimer), SkovState (Config DummyTimer)) -> PropertyM IO Bool
catchUpCheck (_, _, _, c1, s1) (_, _, _, c2, s2) = do
        request <- myLoggedEvalSkovT (getCatchUpStatus True) c1 s1
        (response, result) <- trivialEvalSkovT (handleCatchUpStatus request 2000) c2 s2
        let
            formatMsg (MessageBlock, b) = show (hash b)
            formatMsg (MessageFinalizationRecord, fr) = case runGet getExactVersionedFinalizationRecord fr of
                    Left e -> error e
                    Right fr' -> "Proof(" ++ show (finalizationIndex fr') ++ ", " ++ show (finalizationBlockPointer fr') ++ ")"
        monitor $ counterexample $ "== REQUESTOR ==\n" ++ show (ssGSState s1) ++ "\n== RESPONDENT ==\n" ++ show (ssGSState s2) ++ "\n== REQUEST ==\n" ++ show request ++ "\n== RESPONSE ==\n" ++ show (fmap (_1 %~ fmap formatMsg) response) ++ "\n"
        cuwp <- case result of
            ResultSuccess -> return False
            ResultPendingBlock -> fail "ResultPendingBlock should not be the result when message is not a response"
            ResultContinueCatchUp -> return True
            ResultInvalid -> fail "Unexpected invalid result"
            _ -> fail "Unexpected result"
        case response of
            Nothing -> fail "Response expected (to catch-up request), but none given"
            Just (l, rstatus) -> do
                unless (cusIsResponse rstatus) $ fail "Response flag not set"
                lfh1 <- myLoggedEvalSkovT (bpHeight <$> lastFinalizedBlock) c1 s1
                checkBinary (==) (cusLastFinalizedHeight request) lfh1 "==" "catch-up status last fin height" "actual last fin height"
                lfh2 <- myLoggedEvalSkovT (bpHeight <$> lastFinalizedBlock) c2 s2
                checkBinary (==) (cusLastFinalizedHeight rstatus) lfh2 "==" "catch-up status last fin height" "actual last fin height"
                -- Blocks/records should only be sent if the respondent's last finalized height is above the reqestor's
                unless (null l) $
                    checkBinary (>=) lfh2 lfh1 ">=" "respondent last fin height" "requestor last fin height"
                let reqLive = Set.fromList [bh | (bh, bs) <- HM.toList (ssGSState s1 ^. BTS.blockTable), isLive bs]
                let respLive = Set.fromList [bh | (bh, bs) <- HM.toList (ssGSState s2 ^. BTS.blockTable), isLive bs]
                unless cuwp $ do
                    when (lfh2 < lfh1) $ fail "Respondent is behind, but not requesting catch-up"
                    checkBinary Set.isSubsetOf (Set.fromList $ cusLeaves request) respLive "is a subset of" "resquestor leaves" "respondent nodes, given no counter-request"
                unless (lfh2 < lfh1) $ do
                    -- If the respondent should be able to send us something meaningful, then make sure they do
                    let recBHs = [getHash bp | (MessageBlock, runGet (B.getExactVersionedBlock 0) -> Right bp) <- l]
                    let recBlocks = Set.fromList recBHs
                    -- Check that the requestor's live blocks + received blocks include all live blocks for respondent
                    checkBinary Set.isSubsetOf respLive (reqLive `Set.union` recBlocks) "is a subset of" "respondent live blocks" "requestor live blocks + received blocks"
                    let reqFin = Set.fromList $ finalizationBlockPointer . fst <$> toList (ssGSState s1 ^. BTS.finalizationList)
                    let respFin = Set.fromList $ finalizationBlockPointer . fst <$> toList (ssGSState s2 ^. BTS.finalizationList)
                    let
                        testList _ knownFin [] = checkBinary (==) knownFin respFin "==" "finalized blocks after catch-up" "respondent finalized blocks"
                        testList knownBlocks knownFin ((MessageFinalizationRecord, runGet getExactVersionedFinalizationRecord -> Right finRec) : rs) = do
                            checkBinary Set.member (finalizationBlockPointer finRec) knownBlocks "in" "finalized block" "known blocks"
                            testList knownBlocks (Set.insert (finalizationBlockPointer finRec) knownFin) rs
                        testList knownBlocks knownFin ((MessageBlock, runGet (B.getExactVersionedBlock 0) -> Right (B.NormalBlock bp)) : rs) = do
                            checkBinary Set.member (blockPointer bp) knownBlocks "in" "block parent" "known blocks"
                            knownFin' <- case blockFinalizationData bp of
                                NoFinalizationData -> return knownFin
                                BlockFinalizationData finRec -> do
                                    checkBinary Set.member (finalizationBlockPointer finRec) knownBlocks "in" "finalized block" "known blocks"
                                    return (Set.insert (finalizationBlockPointer finRec) knownFin)
                            testList (Set.insert (getHash bp) knownBlocks) knownFin' rs
                        testList _ _ _ = error "Serialization failure"
                    -- Check that blocks and finalization records are ordered correctly in the following sense:
                    -- * A block is not sent before its parent
                    -- * A block is not sent before finalization of its last finalized block
                    -- * A finalization record is not sent before the block it finalizes
                    -- Furthermore, check that the finalization records + the requestor's finalized blocks
                    -- add up to the respondent's finalized blocks.
                    testList reqLive reqFin l
                    recBPs <- myLoggedEvalSkovT (forM recBHs (\bh -> fromJust <$> resolveBlock bh)) c2 s2
                    case recBPs of
                        [] -> return ()
                        (hbp : bps) -> forM_ bps $ \bp -> checkBinary (<=) (bpArriveTime hbp) (bpArriveTime bp) "<=" "first block time" "other block time"
                return True
    where
        isLive TS.BlockAlive{} = True
        isLive TS.BlockFinalized{} = True
        isLive _ = False

doCatchUpCheck :: Int -> Int -> Property
doCatchUpCheck n steps = monadicIO $ do
        s0 <- initialiseStatesDictator n
        gen <- pick $ mkStdGen <$> arbitrary
        s1 <- liftIO $ runKonsensus steps gen s0 (makeExecState $ initialEvents s0)
        return $ simpleCatchUpCheck s1

tests :: Word -> Spec
tests lvl = parallel $ describe "Concordium.CatchUp" $ do
    it "catch-up check 5 parties, 1000 steps" $ withMaxSuccess (10*10^lvl) $ doCatchUpCheck 5 1000
    it "catch-up check 50 parties, 1000 steps" $ withMaxSuccess (10*10^lvl) $ doCatchUpCheck 50 1000
