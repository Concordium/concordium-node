{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings, InstanceSigs #-}
module ConcordiumTests.Konsensus where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS hiding (get)
import Data.Functor.Identity
import qualified Data.ByteString as BS
import Lens.Micro.Platform
import Data.Monoid
import GHC.Stack
import Data.Time.Clock.POSIX
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.TreeState.Basic
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block

import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.Signature as Sig
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Scheduler.Utils.Init.Example(makeTransaction,initialState)
import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.Birk.Bake
import Concordium.Skov.Monad
import Concordium.TimeMonad

import Debug.Trace

import Test.QuickCheck
import Test.Hspec

type Trs = HM.HashMap TransactionHash (HashedTransaction, Slot)
type ANFTS = HM.HashMap AccountAddress AccountNonFinalizedTransactions

invariantSkovData :: SkovData -> Either String ()
invariantSkovData SkovData{..} = do
        -- Finalization list
        when (Seq.null _skovFinalizationList) $ Left "Finalization list is empty"
        (finMap, lastFin, _) <- foldM checkFin (HM.empty, _skovGenesisBlockPointer, 0) _skovFinalizationList
        -- Live blocks
        (liveFinMap, _) <- foldM checkLive (finMap, [lastFin]) _skovBranches
        unless (HM.filter notDead _skovBlockTable == liveFinMap) $ Left "non-dead blocks do not match finalized and branch blocks"
        -- Pending blocks
        queue <- foldM (checkPending (blockSlot $ bpBlock $ lastFin)) (Set.empty) (HM.toList _skovPossiblyPendingTable)
        checkBinary (Set.isSubsetOf) queue (Set.fromList (MPQ.toListU _skovPossiblyPendingQueue)) "is a subset of" "pending blocks" "pending queue"
        -- Finalization pool
        forM_ (Map.toList _skovFinalizationPool) $ \(fi, frs) -> do
            checkBinary (>=) fi (fromIntegral (Seq.length _skovFinalizationList)) ">=" "pending finalization record index" "length of finalization list"
            forM_ frs $ \fr -> do
                checkBinary (==) fi (finalizationIndex fr) "==" "key in finalization pool" "finalization index"
        -- Transactions
        (nonFinTrans, anftNonces) <- walkTransactions _skovGenesisBlockPointer lastFin (_ttHashMap _skovTransactionTable) (HM.empty)
        let anft' = foldr (\(tr, _) nft -> nft & at (transactionSender tr) . non emptyANFT . anftMap . at (transactionNonce tr) . non Set.empty %~ Set.insert tr) anftNonces nonFinTrans
        unless (anft' == _ttNonFinalizedTransactions _skovTransactionTable) $ Left "Incorrect non-finalized transactions"
        (pendingTrans, pendingNonces) <- walkTransactions lastFin _skovFocusBlock nonFinTrans anftNonces
        let ptt = foldr (\(tr, _) -> extendPendingTransactionTable (pendingNonces ^. at (transactionSender tr) . non emptyANFT . anftNextNonce) tr) emptyPendingTransactionTable pendingTrans
        checkBinary (==) ptt _skovPendingTransactions "==" "expected pending transactions" "recorded pending transactions"
        {-
        checkFinTrans lastFin _skovTransactionsFinalized
        unless (null (Map.intersection _skovTransactionsPending _skovTransactionsFinalized)) $
            Left $ "Pending and finalized transaction sets are not disjoint" -}
        return ()
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        checkFin (finMap, lastFin, i) (fr, bp) = do
            checkBinary (==) (finalizationIndex fr) i "==" "record finalization index" "index in sequence"
            if i == 0 then
                checkBinary (==) bp _skovGenesisBlockPointer "==" "first finalized block" "genesis block"
            else do
                unless (verifyFinalProof finSes finCom fr) $ Left $ "Could not verify finalization record at index " ++ show i
            let overAncestors a m
                    | a == lastFin = return m
                    | a == _skovGenesisBlockPointer = Left $ "Finalized block" ++ show bp ++ "does not descend from previous finalized block " ++ show lastFin
                    | otherwise = overAncestors (bpParent a) (HM.insert (bpHash a) (BlockFinalized a fr) m)
            finMap' <- overAncestors (bpParent bp) (HM.insert (bpHash bp) (BlockFinalized bp fr) finMap)
            return (finMap', bp, i+1)
        checkLive (liveMap, parents) l = do
            forM_ l $ \b -> do
                unless (bpParent b `elem` parents) $ Left $ "Block in branches with invalid parent: " ++ show b
                checkBinary (==) (bpHeight b) (bpHeight (bpParent b) + 1) "==" "block height" "1 + parent height"
            let liveMap' = foldr (\b -> HM.insert (bpHash b) (BlockAlive b)) liveMap l
            return (liveMap', l)
        checkPending lfSlot queue (parent, children) = do
            when (null children) $ Left $ "Empty list of blocks pending parent"
            let checkChild q child = do
                    checkBinary (==) (_skovBlockTable ^. at (pbHash child)) Nothing "==" "pending block status" "Nothing"
                    checkBinary (==) (blockPointer (pbBlock child)) parent "==" "pending block's parent" "pending parent"
                    checkBinary (>) (blockSlot (pbBlock child)) lfSlot ">" "pending block's slot" "last finalized slot"
                    return (Set.insert ((blockSlot (pbBlock child)), (pbHash child, parent)) q)
            checkBinary (==) (_skovBlockTable ^. at parent) Nothing "==" "pending parent status" "Nothing"
            foldM checkChild queue children
        walkTransactions :: BlockPointer -> BlockPointer -> Trs -> ANFTS -> Either String (Trs, ANFTS)
        walkTransactions src dest trMap anfts
            | src == dest = return (trMap, anfts)
            | otherwise = do
                (trMap', anfts') <- walkTransactions src (bpParent dest) trMap anfts
                foldM checkTransaction (trMap', anfts') (blockTransactions dest)
        checkTransaction :: (Trs, ANFTS) -> HashedTransaction -> Either String (Trs, ANFTS)
        checkTransaction (trMap, anfts) tr = do
            let updMap Nothing = Left $ "Transaction missing: " ++ show (unhashed tr)
                updMap (Just _) = Right Nothing
            trMap' <- (at (transactionHash tr)) updMap trMap
            let updNonce n = if n == transactionNonce tr then Right (n + 1) else Left $ "Expected " ++ show (transactionNonce tr) ++ " but found " ++ show n ++ " for account " ++ show (transactionSender tr)
            anfts' <- (at (transactionSender tr) . non emptyANFT . anftNextNonce) updNonce anfts
            return (trMap', anfts')
        finSes = FinalizationSessionId (bpHash _skovGenesisBlockPointer) 0
        finCom = makeFinalizationCommittee (genesisFinalizationParameters _skovGenesisData)
        notDead BlockDead = False
        notDead _ = True

data DummyM a = DummyM {runDummy :: a}

instance Functor DummyM where
    fmap f (DummyM a) = DummyM (f a)

instance Applicative DummyM where
    pure = DummyM
    (DummyM f) <*> (DummyM v) = DummyM (f v)

instance Monad DummyM where
    -- Bind is slightly stricter than identity monad
    (DummyM m) >>= k = k m

{-
instance MonadIO DummyM where
    liftIO :: HasCallStack => IO a -> DummyM a
    liftIO = return (error "Dummy IO")
-}

instance TimeMonad DummyM where
    currentTime = return (posixSecondsToUTCTime 1)

instance LoggerMonad DummyM where
    logEvent src LLError msg = error $ show src ++ ": " ++ msg
    logEvent _ _ _ = return ()

data Event
    = EBake Slot
    | EBlock Block
    | ETransaction Transaction
    | EFinalization BS.ByteString
    | EFinalizationRecord FinalizationRecord

instance Show Event where
    show (EBake sl) = "bake for " ++ show sl
    show (EBlock b) = "block: " ++ show (getHash b :: BlockHash)
    show (ETransaction tr) = "transaction: " ++ show tr
    show (EFinalization _) = "finalization message"
    show (EFinalizationRecord fr) = "finalize: " ++ show (finalizationBlockPointer fr)

type EventPool = Seq (Int, Event)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

type States = Vec.Vector (BakerIdentity, FinalizationInstance, SkovFinalizationState)

runKonsensusTest :: Int -> States -> EventPool -> Gen Property
runKonsensusTest steps states events
        | steps <= 0 = return $ property True
        | null events = return $ property True
        | otherwise = do
            ((rcpt, ev), events') <- selectFromSeq events
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            (fs', events'') <- {- trace (show rcpt ++ ": " ++ show ev) $ -} case ev of
                EBake sl -> do
                    let (mb, fs', Endo evs) = runDummy (runFSM (bakeForSlot bkr sl) fi fs)
                    let blockEvents = case mb of
                                        Nothing -> Seq.empty
                                        Just b -> Seq.fromList [(r, EBlock $ bpBlock b) | r <- btargets]
                    let events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                    return (fs', events'')
                EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
            case invariantSkovData $ fs' ^. skov of
                Left err -> return $ counterexample ("Invariant failed: " ++ err) False
                Right _ -> do
                    let states' = states & ix rcpt . _3 .~ fs'
                    runKonsensusTest (steps - 1) states' (events'' <> events')
    where
        handleMessages :: [Int] -> [FinalizationOutputEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (BroadcastFinalizationMessage fmsg : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (BroadcastFinalizationRecord frec : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        runAndHandle a fi fs btargets = do
            let (_, fs', Endo evs) = runDummy (runFSM a fi fs)
            return (fs', handleMessages btargets (evs []))

runKonsensusTestSimple :: Int -> States -> EventPool -> Gen Property
runKonsensusTestSimple steps states events
        | steps <= 0 || null events = return
            (case forM_ states $ \(_, _, s) -> invariantSkovData (s ^. skov) of
                Left err -> counterexample ("Invariant failed: " ++ err) False
                Right _ -> property True)
        | otherwise = do
            ((rcpt, ev), events') <- selectFromSeq events
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            (fs', events'') <- case ev of
                EBake sl -> do
                    let (mb, fs', Endo evs) = runDummy (runFSM (bakeForSlot bkr sl) fi fs)
                    let blockEvents = case mb of
                                        Nothing -> Seq.empty
                                        Just b -> Seq.fromList [(r, EBlock $ bpBlock b) | r <- btargets]
                    let events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                    return (fs', events'')
                EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
            let states' = states & ix rcpt . _3 .~ fs'
            runKonsensusTestSimple (steps - 1) states' (events'' <> events')
    where
        handleMessages :: [Int] -> [FinalizationOutputEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (BroadcastFinalizationMessage fmsg : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (BroadcastFinalizationRecord frec : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        runAndHandle a fi fs btargets = do
            let (_, fs', Endo evs) = runDummy (runFSM a fi fs)
            return (fs', handleMessages btargets (evs []))

nAccounts :: Int
nAccounts = 2

genTransactions :: Int -> Gen [Transaction]
genTransactions n = mapM gent (take n [minNonce..])
    where
        gent nnce = do
            f <- arbitrary
            g <- arbitrary
            return $ makeTransaction f (ContractAddress (fromIntegral $ g `mod` nAccounts) 0) nnce


initialEvents :: States -> EventPool
initialEvents states = Seq.fromList [(x, EBake 1) | x <- [0..length states -1]]

makeBaker :: BakerId -> LotteryPower -> Gen (BakerInfo, BakerIdentity)
makeBaker bid lot = do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk@(Sig.KeyPair _ spk) <- arbitrary
        return (BakerInfo epk spk lot, BakerIdentity bid sk spk ek epk)

initialiseStates :: Int -> Gen States
initialiseStates n = do
        let bns = [1..fromIntegral n]
        let bakeShare = 1.0 / fromIntegral n
        bis <- mapM (\i -> (i,) <$> makeBaker i bakeShare) bns
        let bps = BirkParameters "LeadershipElectionNonce" 0.5
                (Map.fromList [(i, b) | (i, (b, _)) <- bis])
            fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _, _)) <- bis]
            gen = GenesisData 0 1 bps fps
        return $ Vec.fromList [(bid, fininst, initialSkovFinalizationState fininst gen (initialState nAccounts)) | (_, (_, bid)) <- bis, let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)] 

withInitialStates :: Int -> (States -> EventPool -> Gen Property) -> Gen Property
withInitialStates n run = do
        s0 <- initialiseStates n
        run s0 (initialEvents s0)

withInitialStatesTransactions :: Int -> Int -> (States -> EventPool -> Gen Property) -> Gen Property
withInitialStatesTransactions n trcount run = do
        s0 <- initialiseStates n
        trs <- genTransactions trcount
        run s0 (initialEvents s0 <> Seq.fromList [(x, ETransaction tr) | x <- [0..n-1], tr <- trs])

tests :: Spec
tests = parallel $ describe "Concordium.Konsensus" $ do
    it "2 parties, 100 steps, 10 transactions, check at every step" $ withMaxSuccess 10000 $ withInitialStatesTransactions 2 10 $ runKonsensusTest 100
    it "2 parties, 1000 steps, 50 transactions, check at every step" $ withMaxSuccess 1000 $ withInitialStatesTransactions 2 50 $ runKonsensusTest 1000
    it "2 parties, 100 steps, check at every step" $ withMaxSuccess 10000 $ withInitialStates 2 $ runKonsensusTest 100
    it "2 parties, 100 steps, check at end" $ withMaxSuccess 50000 $ withInitialStates 2 $ runKonsensusTestSimple 100
    it "2 parties, 1000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 1000
    it "2 parties, 10000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 10000
    it "4 parties, 10000 steps, check every step" $ withMaxSuccess 10 $ withInitialStates 4 $ runKonsensusTest 10000
    
