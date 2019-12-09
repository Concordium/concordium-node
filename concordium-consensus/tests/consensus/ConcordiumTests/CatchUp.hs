{-# LANGUAGE TupleSections, OverloadedStrings, InstanceSigs, FlexibleContexts #-}
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

import Concordium.Crypto.SHA256

import qualified Concordium.GlobalState.Basic.Block as B
import qualified Concordium.GlobalState.Basic.BlockPointer as BS
import qualified Concordium.GlobalState.Basic.TreeState as BTS
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization

import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations
import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Startup(dummyCryptographicParameters)
import Concordium.Skov.CatchUp

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

import ConcordiumTests.Konsensus hiding (tests)

runKonsensus :: RandomGen g => Int -> g -> States -> ExecState -> IO States
runKonsensus steps g states es
    | steps <= 0 || null (es ^. esEventPool) = return states
    | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g (es ^. esEventPool)
            let es1 = es & esEventPool .~ events'
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            let continue fs' es' = do
                        let states' = states & ix rcpt . _3 .~ fs'
                        runKonsensus (steps - 1) g' states' es'
            let handlers = dummyHandlers rcpt btargets
            case ev of
                EBake sl -> do
                    (mb, fs', es2) <- myRunSkovT (bakeForSlot bkr sl) handlers fi fs es1
                    case mb of
                        Nothing -> continue fs' (es2 & esEventPool %~ ((rcpt, EBake (sl + 1)) Seq.<|))
                        Just BS.BasicBlockPointer{_bpBlock = B.NormalBlock b} ->
                            continue fs' (es2 & esEventPool %~ (<> Seq.fromList ((rcpt, EBake (sl + 1)) : [(r, EBlock b) | r <- btargets])))
                        Just _ -> error "Baked genesis block"

                EBlock block -> do
                    (_, fs', es') <- myRunSkovT (storeBlock (B.makePendingBlock block dummyTime)) handlers fi fs es1
                    continue fs' es'
                ETransaction tr -> do
                    (_, fs', es') <- myRunSkovT (receiveTransaction tr) handlers fi fs es1
                    continue fs' es'
                EFinalization fmsg -> do
                    (_, fs', es') <- myRunSkovT (receiveFinalizationPseudoMessage fmsg) handlers fi fs es1
                    continue fs' es'
                EFinalizationRecord frec -> do
                    (_, fs', es') <- myRunSkovT (finalizeBlock frec) handlers fi fs es1
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
        let bps = BirkParameters 0.5 genesisBakers genesisBakers genesisBakers (genesisSeedState (hash "LeadershipElectionNonce") 10)
            fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _ _, _, _)) <- take 1 bis] 2
            bakerAccounts = map (\(_, (_, _, acc)) -> acc) bis
            gen = GenesisData 0 1 bps bakerAccounts [] fps dummyCryptographicParameters dummyIdentityProviders 10
        res <- liftIO $ mapM (\(_, (_, bid, _)) -> do
                                let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)
                                let config = SkovConfig 
                                        (MTMBConfig defaultRuntimeParameters gen (Example.initialState bps dummyCryptographicParameters bakerAccounts [] nAccounts))
                                        (ActiveFinalization fininst gen)
                                        NoHandler
                                (initCtx, initState) <- liftIO $ initialiseSkov config
                                return (bid, initCtx, initState)
                             ) bis
        return $ Vec.fromList res

simpleCatchUpCheck :: States -> Property
simpleCatchUpCheck ss = 
        conjoin [monadicIO $ catchUpCheck s1 s2 | s1 <- toList ss, s2 <- toList ss ]

catchUpCheck :: (BakerIdentity, SkovContext (Config DummyTimer), SkovState (Config DummyTimer)) -> (BakerIdentity, SkovContext (Config DummyTimer), SkovState (Config DummyTimer)) -> PropertyM IO Bool
catchUpCheck (_, c1, s1) (_, c2, s2) = do
        request <- myEvalSkovT (getCatchUpStatus True) c1 s1
        response <- myEvalSkovT (handleCatchUp request) c2 s2
        monitor $ counterexample $ "== REQUESTOR ==\n" ++ show (ssGSState s1) ++ "\n== RESPONDENT ==\n" ++ show (ssGSState s2) ++ "\n== REQUEST ==\n" ++ show request ++ "\n== RESPONSE ==\n" ++ show response ++ "\n"
        case response of
            Left err -> fail $ "Catch-up failed: " ++ err
            Right (Nothing, _) -> fail "Response expected (to catch-up request), but none given"
            Right (Just (l, rstatus), cuwp) -> do
                unless (cusIsResponse rstatus) $ fail "Response flag not set"
                lfh1 <- myEvalSkovT (bpHeight <$> lastFinalizedBlock) c1 s1
                checkBinary (==) (cusLastFinalizedHeight request) lfh1 "==" "catch-up status last fin height" "actual last fin height"
                lfh2 <- myEvalSkovT (bpHeight <$> lastFinalizedBlock) c2 s2
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
                    let recBlocks = Set.fromList [bpHash bp | (Right bp) <- l]
                    -- Check that the requestor's live blocks + received blocks include all live blocks for respondent
                    checkBinary Set.isSubsetOf respLive (reqLive `Set.union` recBlocks) "is a subset of" "respondent live blocks" "requestor live blocks + received blocks" 
                    let reqFin = Set.fromList $ finalizationBlockPointer . fst <$> toList (ssGSState s1 ^. BTS.finalizationList)
                    let respFin = Set.fromList $ finalizationBlockPointer . fst <$> toList (ssGSState s2 ^. BTS.finalizationList)
                    let
                        testList _ knownFin [] = checkBinary (==) knownFin respFin "==" "finalized blocks after catch-up" "respondent finalized blocks"
                        testList knownBlocks knownFin (Left finRec : rs) = do
                            checkBinary Set.member (finalizationBlockPointer finRec) knownBlocks "in" "finalized block" "known blocks"
                            testList knownBlocks (Set.insert (finalizationBlockPointer finRec) knownFin) rs
                        testList knownBlocks knownFin (Right bp : rs) = do
                            checkBinary Set.member (bpHash (bpParent bp)) knownBlocks "in" "block parent" "known blocks"
                            checkBinary Set.member (bpHash (bpLastFinalized bp)) knownFin "in" "block parent" "known finalized blocks"
                            testList (Set.insert (bpHash bp) knownBlocks) knownFin rs
                    -- Check that blocks and finalization records are ordered correctly in the following sense:
                    -- * A block is not sent before its parent
                    -- * A block is not sent before finalization of its last finalized block
                    -- * A finalization record is not sent before the block it finalizes
                    -- Furthermore, check that the finalization records + the requestor's finalized blocks
                    -- add up to the respondent's finalized blocks.
                    testList reqLive reqFin l
                    let recBPs = [bp | (Right bp) <- l]
                    case recBPs of
                        [] -> return ()
                        (hbp : bps) -> forM_ bps $ \bp -> checkBinary (<=) (bpArriveTime hbp) (bpArriveTime bp) "<=" "first block time" "other block time"
                return True
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ fail $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
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