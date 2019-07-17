{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings, InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ConcordiumTests.Konsensus where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Data.Bits
import Data.Monoid
import Data.Time.Clock.POSIX
import qualified Data.PQueue.Prio.Min as MPQ
import System.Random

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.BlockState(BlockPointerData(..))
import qualified Concordium.GlobalState.TreeState as TreeState
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Block
import Concordium.GlobalState.Bakers

import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov
import Concordium.Skov.Update (SkovFinalizationState(..))
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.Birk.Bake
import Concordium.TimeMonad

import Concordium.Startup(makeBakerAccount)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

-- import Debug.Trace

type Trs = HM.HashMap TransactionHash (Transaction, Slot)
type ANFTS = HM.HashMap AccountAddress AccountNonFinalizedTransactions

invariantSkovData :: SkovData -> Either String ()
invariantSkovData SkovData{..} = do
        -- Finalization list
        when (Seq.null _skovFinalizationList) $ Left "Finalization list is empty"
        (finMap, lastFin, _) <- foldM checkFin (HM.empty, _skovGenesisBlockPointer, 0) _skovFinalizationList
        -- Live blocks
        (liveFinMap, _) <- foldM checkLive (finMap, [lastFin]) _skovBranches
        unless (HM.filter notDeadOrPending _skovBlockTable == liveFinMap) $ Left "non-dead blocks do not match finalized and branch blocks"
        -- Pending blocks
        queue <- foldM (checkPending (blockSlot $ bpBlock $ lastFin)) (Set.empty) (HM.toList _skovPossiblyPendingTable)
        let pendingSet = Set.fromList (MPQ.toListU _skovPossiblyPendingQueue)
        checkBinary (Set.isSubsetOf) queue pendingSet "is a subset of" "pending blocks" "pending queue"
        let allPossiblyPending = Set.fromList ((fst <$> MPQ.elemsU _skovPossiblyPendingQueue) ++ (getHash <$> MPQ.elemsU _skovBlocksAwaitingLastFinalized))
        checkBinary Set.isSubsetOf (Set.fromList $ HM.keys $ HM.filter onlyPending _skovBlockTable) allPossiblyPending "is a subset of" "blocks marked pending" "pending queues"
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
        let ptt = foldr (\(tr, _) -> checkedExtendPendingTransactionTable (pendingNonces ^. at (transactionSender tr) . non emptyANFT . anftNextNonce) tr) emptyPendingTransactionTable pendingTrans
        checkBinary (==) ptt _skovPendingTransactions "==" "expected pending transactions" "recorded pending transactions"
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
                    | otherwise = overAncestors (bpParent a) (HM.insert (bpHash a) (TreeState.BlockFinalized a fr) m)
            finMap' <- overAncestors (bpParent bp) (HM.insert (bpHash bp) (TreeState.BlockFinalized bp fr) finMap)
            return (finMap', bp, i+1)
        checkLive (liveMap, parents) l = do
            forM_ l $ \b -> do
                unless (bpParent b `elem` parents) $ Left $ "Block in branches with invalid parent: " ++ show b
                checkBinary (==) (bpHeight b) (bpHeight (bpParent b) + 1) "==" "block height" "1 + parent height"
            let liveMap' = foldr (\b -> HM.insert (bpHash b) (TreeState.BlockAlive b)) liveMap l
            return (liveMap', l)
        checkPending lfSlot queue (parent, children) = do
            when (null children) $ Left $ "Empty list of blocks pending parent"
            let checkChild q child = do
                    let pendingBlockStatus = _skovBlockTable ^. at (pbHash child)
                    case pendingBlockStatus of
                        Just TreeState.BlockPending{} -> return ()
                        _ -> Left $ "Pending block status (" ++ show pendingBlockStatus ++ ") should be BlockPending"
                    checkBinary (==) (blockPointer (bbFields (pbBlock child))) parent "==" "pending block's parent" "pending parent"
                    checkBinary (>) (blockSlot (pbBlock child)) lfSlot ">" "pending block's slot" "last finalized slot"
                    return (Set.insert ((blockSlot (pbBlock child)), (pbHash child, parent)) q)
            let parentBlockStatus = _skovBlockTable ^. at parent
            case parentBlockStatus of
                Just TreeState.BlockPending{} -> return ()
                Nothing -> return ()
                _ -> Left $ "Pending parent status (" ++ show parentBlockStatus ++ ") should be BlockPending or Nothing"
            foldM checkChild queue children
        walkTransactions :: BlockPointer -> BlockPointer -> Trs -> ANFTS -> Either String (Trs, ANFTS)
        walkTransactions src dest trMap anfts
            | src == dest = return (trMap, anfts)
            | otherwise = do
                (trMap', anfts') <- walkTransactions src (bpParent dest) trMap anfts
                foldM checkTransaction (trMap', anfts') (blockTransactions dest)
        checkTransaction :: (Trs, ANFTS) -> Transaction -> Either String (Trs, ANFTS)
        checkTransaction (trMap, anfts) tr = do
            let updMap Nothing = Left $ "Transaction missing: " ++ show tr
                updMap (Just _) = Right Nothing
            trMap' <- (at (transactionHash tr)) updMap trMap
            let updNonce n = if n == transactionNonce tr then Right (n + 1) else Left $ "Expected " ++ show (transactionNonce tr) ++ " but found " ++ show n ++ " for account " ++ show (transactionSender tr)
            anfts' <- (at (transactionSender tr) . non emptyANFT . anftNextNonce) updNonce anfts
            return (trMap', anfts')
        finSes = FinalizationSessionId (bpHash _skovGenesisBlockPointer) 0
        finCom = makeFinalizationCommittee (genesisFinalizationParameters _skovGenesisData)
        notDeadOrPending TreeState.BlockDead = False
        notDeadOrPending (TreeState.BlockPending {}) = False
        notDeadOrPending _ = True
        onlyPending (TreeState.BlockPending {}) = True
        onlyPending _ = False

invariantSkovFinalization :: SkovFinalizationState -> Either String ()
invariantSkovFinalization (SkovFinalizationState sd@SkovData{..} FinalizationState{..}) = do
        invariantSkovData sd
        let (_ Seq.:|> (lfr, lfb)) = _skovFinalizationList
        checkBinary (==) _finsIndex (succ $ finalizationIndex lfr) "==" "current finalization index" "successor of last finalized index"
        checkBinary (==) _finsHeight (bpHeight lfb + max (1 + _finsMinSkip) ((bpHeight lfb - bpHeight (bpLastFinalized lfb)) `div` 2)) "==" "finalization height"  "calculated finalization height"
        -- This test assumes that this party should be a member of the finalization committee
        when (null _finsCurrentRound) $ Left "No current finalization round"
        forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
            checkBinary (>=) roundDelta (max 1 (finalizationDelay lfr `div` 2)) ">=" "round delta" "half last finalization delay (or 1)"
            unless (popCount (toInteger roundDelta) == 1) $ Left $ "Round delta (" ++ show roundDelta ++ ") is not a power of 2"
            -- Determine which blocks are valid candidates for finalization
            -- i.e. they are at height _finsHeight and have descendants at height _finsHeight + roundDelta
            let descendants = case _skovBranches Seq.!? (fromIntegral $ _finsHeight + roundDelta - bpHeight lfb - 1) of
                                    Nothing -> []
                                    Just bs -> bs
            let
                nthAncestor 0 b = b
                nthAncestor n b = nthAncestor (n-1) (bpParent b)
            let eligibleBlocks = Set.fromList $ bpHash . nthAncestor roundDelta <$> descendants
            let justifiedProposals = Map.keysSet $ Map.filter (\(b,_,_) -> b) $ _proposals $ _freezeState $ roundWMVBA
            checkBinary (==) justifiedProposals eligibleBlocks "==" "nominally justified finalization blocks" "actually justified finalization blocks"
            case roundInput of
                Nothing -> unless (null eligibleBlocks) $ Left "There are eligible finalization blocks, but none has been nominated"
                Just nom -> checkBinary Set.member nom eligibleBlocks "is an element of" "the nominated final block" "the set of eligible blocks"
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        

data DummyM a = DummyM {runDummy :: a}

instance Functor DummyM where
    fmap f (DummyM a) = DummyM (f a)

instance Applicative DummyM where
    pure = DummyM
    (DummyM f) <*> (DummyM v) = DummyM (f v)

instance Monad DummyM where
    -- Bind is slightly stricter than identity monad
    (DummyM m) >>= k = k m

instance TimeMonad DummyM where
    currentTime = return (posixSecondsToUTCTime 1)

instance LoggerMonad DummyM where
    logEvent src LLError msg = error $ show src ++ ": " ++ msg
    logEvent _ _ _ = return () -- trace (show src ++ ": " ++ msg) $ return ()

data Event
    = EBake Slot
    | EBlock BakedBlock
    | ETransaction Transaction
    | EFinalization FinalizationMessage
    | EFinalizationRecord FinalizationRecord

instance Show Event where
    show (EBake sl) = "bake for " ++ show sl
    show (EBlock b) = "block: " ++ show (getHash b :: BlockHash)
    show (ETransaction tr) = "transaction: " ++ show tr
    show (EFinalization fmsg) = "finalization message: " ++ show fmsg
    show (EFinalizationRecord fr) = "finalize: " ++ show (finalizationBlockPointer fr)

type EventPool = Seq (Int, Event)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: (RandomGen g) => g -> Seq a -> (a, Seq a, g)
selectFromSeq g s =
    let (n , g') = randomR (0, length s - 1) g in
    (Seq.index s n, Seq.deleteAt n s, g')

type States = Vec.Vector (BakerIdentity, FinalizationInstance, SkovFinalizationState)

myRunFSM :: (MonadIO m) => FSM LogIO a -> FinalizationInstance -> SkovFinalizationState -> m (a, SkovFinalizationState, Endo [SkovFinalizationEvent])
myRunFSM a fi sfs = liftIO $ runLoggerT (runFSM a fi sfs) doLog
    where
        doLog src LLError msg = error $ show src ++ ": " ++ msg
        doLog _ _ _ = return ()


runKonsensusTest :: RandomGen g => Int -> g -> States -> EventPool -> IO Property
runKonsensusTest steps g states events
        | steps <= 0 = return $ property True
        | null events = return $ property True
        | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g events
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            (fs', events'') <- {- trace (show rcpt ++ ": " ++ show ev) $ -} case ev of
                EBake sl -> do
                    (mb, fs', Endo evs) <- myRunFSM (bakeForSlot bkr sl) fi fs
                    let blockEvents = case mb of
                                        Nothing -> Seq.empty
                                        Just b -> Seq.fromList [(r, EBlock b) | r <- btargets]
                    let events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                    return (fs', events'')
                EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
            case invariantSkovFinalization fs' of
                Left err -> return $ counterexample ("Invariant failed: " ++ err) False
                Right _ -> do
                    let states' = states & ix rcpt . _3 .~ fs'
                    runKonsensusTest (steps - 1) g' states' (events'' <> events')
    where
        handleMessages :: [Int] -> [SkovFinalizationEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (SkovFinalization (BroadcastFinalizationMessage fmsg) : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (SkovFinalization (BroadcastFinalizationRecord frec) : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (_ : r) = handleMessages targets r
        runAndHandle a fi fs btargets = do
            (_, fs', Endo evs) <- myRunFSM a fi fs
            return (fs', handleMessages btargets (evs []))

runKonsensusTestSimple :: RandomGen g => Int -> g -> States -> EventPool -> IO Property
runKonsensusTestSimple steps g states events
        | steps <= 0 || null events = return
            (case forM_ states $ \(_, _, s) -> invariantSkovFinalization s of
                Left err -> counterexample ("Invariant failed: " ++ err) False
                Right _ -> property True)
        | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g events
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            (fs', events'') <- case ev of
                EBake sl -> do
                    (mb, fs', Endo evs) <- myRunFSM (bakeForSlot bkr sl) fi fs
                    let blockEvents = case mb of
                                        Nothing -> Seq.empty
                                        Just b -> Seq.fromList [(r, EBlock b) | r <- btargets]
                    let events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                    return (fs', events'')
                EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
            let states' = states & ix rcpt . _3 .~ fs'
            runKonsensusTestSimple (steps - 1) g' states' (events'' <> events')
    where
        handleMessages :: [Int] -> [SkovFinalizationEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (SkovFinalization (BroadcastFinalizationMessage fmsg) : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (SkovFinalization (BroadcastFinalizationRecord frec) : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (_ : r) = handleMessages targets r
        runAndHandle a fi fs btargets = do
            (_, fs', Endo evs) <- myRunFSM a fi fs
            return (fs', handleMessages btargets (evs []))

nAccounts :: Int
nAccounts = 2

genTransactions :: Int -> Gen [Transaction]
genTransactions n = mapM gent (take n [minNonce..])
    where
        gent nnce = do
            f <- arbitrary
            g <- arbitrary
            return $ Example.makeTransaction f (ContractAddress (fromIntegral $ g `mod` nAccounts) 0) nnce


initialEvents :: States -> EventPool
initialEvents states = Seq.fromList [(x, EBake 1) | x <- [0..length states -1]]

makeBaker :: BakerId -> Amount -> Gen (BakerInfo, BakerIdentity, Account)
makeBaker bid lot = do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk                     <- Sig.genKeyPair
        let spk = Sig.verifyKey sk
        let account = makeBakerAccount bid
        return (BakerInfo epk spk lot (_accountAddress account), BakerIdentity sk ek, account)

dummyIdentityProviders :: [IdentityProviderData]
dummyIdentityProviders = []  

initialiseStates :: Int -> Gen States
initialiseStates n = do
        let bns = [0..fromIntegral n - 1]
        bis <- mapM (\i -> (i,) <$> makeBaker i 1) bns
        let bps = BirkParameters "LeadershipElectionNonce" 0.5
                (bakersFromList $ (^. _2 . _1) <$> bis)
            fps = FinalizationParameters [VoterInfo vvk vrfk 1 | (_, (BakerInfo vrfk vvk _ _, _, _)) <- bis] 2
            bakerAccounts = map (\(_, (_, _, acc)) -> acc) bis
            gen = GenesisData 0 1 bps bakerAccounts fps dummyCryptographicParameters dummyIdentityProviders
        return $ Vec.fromList [(bid, fininst, initialSkovFinalizationState fininst gen (Example.initialState bps dummyCryptographicParameters bakerAccounts nAccounts))
                              | (_, (_, bid, _)) <- bis, let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)]

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

instance Show FinalizationInstance where
    show _ = "[Finalization Instance]"

instance Show SkovFinalizationState where
    show sfs = show (sfs ^. skov)


withInitialStates :: Int -> (StdGen -> States -> EventPool -> IO Property) -> Property
withInitialStates n r = monadicIO $ do
        s0 <- pick $ initialiseStates n
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (initialEvents s0)

withInitialStatesTransactions :: Int -> Int -> (StdGen -> States -> EventPool -> IO Property) -> Property
withInitialStatesTransactions n trcount r = monadicIO $ do
        s0 <- pick $ initialiseStates n
        trs <- pick $ genTransactions trcount
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (initialEvents s0 <> Seq.fromList [(x, ETransaction tr) | x <- [0..n-1], tr <- trs])

withInitialStatesDoubleTransactions :: Int -> Int -> (StdGen -> States -> EventPool -> IO Property) -> Property
withInitialStatesDoubleTransactions n trcount r = monadicIO $ do
        s0 <- pick $ initialiseStates n
        trs0 <- pick $ genTransactions trcount
        trs <- (trs0 ++) <$> pick (genTransactions trcount)
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (initialEvents s0 <> Seq.fromList [(x, ETransaction tr) | x <- [0..n-1], tr <- trs])


tests :: Spec
tests = parallel $ describe "Concordium.Konsensus" $ do
    it "2 parties, 100 steps, 20 transactions with duplicates, check at every step" $ withMaxSuccess 1000 $ withInitialStatesDoubleTransactions 2 10 $ runKonsensusTest 100
    it "2 parties, 100 steps, 10 transactions, check at every step" $ withMaxSuccess 10000 $ withInitialStatesTransactions 2 10 $ runKonsensusTest 100
    it "2 parties, 1000 steps, 50 transactions, check at every step" $ withMaxSuccess 1000 $ withInitialStatesTransactions 2 50 $ runKonsensusTest 1000
    it "2 parties, 100 steps, check at every step" $ withMaxSuccess 10000 $ withInitialStates 2 $ runKonsensusTest 100
    --it "2 parties, 100 steps, check at end" $ withMaxSuccess 50000 $ withInitialStates 2 $ runKonsensusTestSimple 100
    --it "2 parties, 1000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 1000
    it "2 parties, 1000 steps, check at every step" $ withMaxSuccess 1000 $ withInitialStates 2 $ runKonsensusTest 1000
    --it "2 parties, 10000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 10000
    it "4 parties, 10000 steps, check every step" $ withMaxSuccess 50 $ withInitialStates 4 $ runKonsensusTest 10000
    
