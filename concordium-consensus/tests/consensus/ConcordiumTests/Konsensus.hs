{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings, InstanceSigs, FlexibleContexts, CPP, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-deprecations #-}
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
import Data.Time.Clock.POSIX
import Data.Time.Clock
import qualified Data.PQueue.Prio.Min as MPQ
import System.Random
import Control.Monad.Trans.State
import Data.Functor.Identity

import Concordium.Crypto.SHA256

import Concordium.Types
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.TreeState as TreeState
import qualified Concordium.GlobalState.Basic.TreeState as TS
import qualified Concordium.GlobalState.Block as B
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Basic.BlockPointer
import qualified Concordium.GlobalState.Basic.BlockState as BState
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.BlockPointer (bpHash, bpHeight)
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Block
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState

import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.FinalizationQueue
import Concordium.Logger
import Concordium.Birk.Bake
import Concordium.TimeMonad

import Concordium.Kontrol.UpdateLeaderElectionParameters(slotDependentBirkParameters)

import Concordium.Startup(makeBakerAccount, dummyCryptographicParameters)

import Concordium.Crypto.DummyData

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

-- import Debug.Trace

dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

type Trs = HM.HashMap TransactionHash (BlockItem, TransactionStatus)
type ANFTS = HM.HashMap AccountAddress AccountNonFinalizedTransactions

type Config t = SkovConfig MemoryTreeMemoryBlockConfig (ActiveFinalization t) NoHandler

invariantSkovData :: TS.SkovData BState.BlockState -> Either String ()
invariantSkovData TS.SkovData{..} = addContext $ do
        -- Finalization list
        when (Seq.null _finalizationList) $ Left "Finalization list is empty"
        (finMap, lastFin, _) <- foldM checkFin (HM.empty, _genesisBlockPointer, 0) _finalizationList
        -- Live blocks
        (liveFinMap, _) <- foldM checkLive (finMap, [lastFin]) _branches
        checkBinary (==) (HM.filter notDeadOrPending _blockTable) liveFinMap "==" "non-dead/pending blocks" "finalized and branch blocks"
        -- unless (HM.filter notDeadOrPending _blockTable == liveFinMap) $ Left "non-dead blocks do not match finalized and branch blocks"
        unless (checkLastNonEmpty _branches) $ Left $ "Last element of branches was empty. branches: " ++ show _branches
        -- Pending blocks
        queue <- foldM (checkPending (blockSlot lastFin)) (Set.empty) (HM.toList _possiblyPendingTable)
        let pendingSet = Set.fromList (MPQ.toListU _possiblyPendingQueue)
        checkBinary (Set.isSubsetOf) queue pendingSet "is a subset of" "pending blocks" "pending queue"
        let allPossiblyPending = Set.fromList (fst <$> MPQ.elemsU _possiblyPendingQueue)
        checkBinary Set.isSubsetOf (Set.fromList $ HM.keys $ HM.filter onlyPending _blockTable) allPossiblyPending "is a subset of" "blocks marked pending" "pending queues"
        checkBinary Set.isSubsetOf allPossiblyPending (Set.fromList $ HM.keys _blockTable) "is a subset of" "pending queues" "blocks in block table"
        -- Transactions
        (nonFinTrans, anftNonces) <- walkTransactions _genesisBlockPointer lastFin (_ttHashMap _transactionTable) (HM.empty)
        let anft' = foldr (\(bi, _) ->
                             case bi of
                               WithMetadata{wmdData=NormalTransaction tr,..} ->
                                 at (transactionSender tr)
                                 . non emptyANFT
                                 . anftMap
                                 . at (transactionNonce tr)
                                 . non Set.empty %~ Set.insert WithMetadata{wmdData=tr,..}
                               _ -> id
                          )
                          anftNonces
                          nonFinTrans
        unless (anft' == _ttNonFinalizedTransactions _transactionTable) $ Left "Incorrect non-finalized transactions"
        (pendingTrans, pendingNonces) <- walkTransactions lastFin _focusBlock nonFinTrans anftNonces
        let ptt = foldr (\(bi, _) ->
                           case wmdData bi of
                             NormalTransaction tr ->
                               checkedExtendPendingTransactionTable (pendingNonces ^. at (transactionSender tr) . non emptyANFT . anftNextNonce) tr
                             CredentialDeployment _ -> extendPendingTransactionTable' (wmdHash bi)
                        )
                        emptyPendingTransactionTable
                        pendingTrans
        checkBinary (==) ptt _pendingTransactions "==" "expected pending transactions" "recorded pending transactions"
        checkEpochs _focusBlock
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        -- Notice: checkFin doesn't check that finalization records actually verify. This is only done after the test run
        --         is finished using checkFinalizationRecordsVerify.
        checkFin (finMap, lastFin, i) (fr, bp) = do
            checkBinary (==) (finalizationIndex fr) i "==" "record finalization index" "index in sequence"
            if i == 0 then checkBinary (==) bp _genesisBlockPointer "==" "first finalized block" "genesis block" else Right $ ()
            -- If verifying finalization records at every step is desired, uncomment the two lines below
            -- else do
            --     unless (verifyFinalProof finSes finCom fr) $ Left $ "Could not verify finalization record at index " ++ show i
            let overAncestors a m
                    | a == lastFin = return m
                    | a == _genesisBlockPointer = Left $ "Finalized block" ++ show bp ++ "does not descend from previous finalized block " ++ show lastFin
                    | otherwise = overAncestors (runIdentity $ BS._bpParent a) (HM.insert (bpHash a) (TreeState.BlockFinalized a fr) m)
            finMap' <- overAncestors (runIdentity $ BS._bpParent bp) (HM.insert (bpHash bp) (TreeState.BlockFinalized bp fr) finMap)
            return (finMap', bp, i+1)
        checkLive (liveMap, parents) l = do
            forM_ l $ \b -> do
                unless ((runIdentity $ BS._bpParent b) `elem` parents) $ Left $ "Block in branches with invalid parent: " ++ show b
                checkBinary (==) (bpHeight b) (bpHeight (runIdentity $ BS._bpParent b) + 1) "==" "block height" "1 + parent height"
            let liveMap' = foldr (\b -> HM.insert (bpHash b) (TreeState.BlockAlive b)) liveMap l
            return (liveMap', l)
        checkLastNonEmpty Seq.Empty = True -- catches cases where branches is empty
        checkLastNonEmpty (_ Seq.:|> x) = (x /= [])
        checkPending lfSlot queue (parent, children) = do
            when (null children) $ Left $ "Empty list of blocks pending parent"
            let checkChild q child = do
                    let pendingBlockStatus = _blockTable ^. at (getHash child)
                    case pendingBlockStatus of
                        Just TreeState.BlockPending{} -> return ()
                        _ -> Left $ "Pending block status (" ++ show pendingBlockStatus ++ ") should be BlockPending"
                    checkBinary (==) (blockPointer child) parent "==" "pending block's parent" "pending parent"
                    checkBinary (>) (blockSlot child) lfSlot ">" "pending block's slot" "last finalized slot"
                    return (Set.insert ((blockSlot child), (getHash child, parent)) q)
            let parentBlockStatus = _blockTable ^. at parent
            case parentBlockStatus of
                Just TreeState.BlockPending{} -> return ()
                Nothing -> return ()
                _ -> Left $ "Pending parent status (" ++ show parentBlockStatus ++ ") should be BlockPending or Nothing"
            foldM checkChild queue children
        -- walkTransactions :: BS.BasicBlockPointer -> BS.BasicBlockPointer -> Trs -> ANFTS -> Either String (Trs, ANFTS)
        walkTransactions src dest trMap anfts
            | src == dest = return (trMap, anfts)
            | otherwise = do
                (trMap', anfts') <- walkTransactions src (runIdentity $ BS._bpParent dest) trMap anfts
                foldM checkTransaction (trMap', anfts') (blockTransactions dest)
        checkTransaction :: (Trs, ANFTS) -> BlockItem -> Either String (Trs, ANFTS)
        checkTransaction (trMap, anfts) bi = do
            let updMap Nothing = Left $ "Transaction missing: " ++ show bi
                updMap (Just _) = Right Nothing
            trMap' <- (at (getHash bi)) updMap trMap
            case wmdData bi of
              NormalTransaction tr ->
                let updNonce n =
                      if n == transactionNonce tr then Right (n + 1)
                      else Left $ "Expected " ++ show (transactionNonce tr) ++ " but found " ++ show n ++ " for account " ++ show (transactionSender tr)
                in do anfts' <- (at (transactionSender tr) . non emptyANFT . anftNextNonce) updNonce anfts
                      return (trMap', anfts')
              _ -> do
                return (trMap', anfts)

        notDeadOrPending TreeState.BlockDead = False
        notDeadOrPending (TreeState.BlockPending {}) = False
        notDeadOrPending _ = True
        onlyPending (TreeState.BlockPending {}) = True
        onlyPending _ = False
        checkEpochs :: BasicBlockPointer BState.BlockState -> Either String ()
        checkEpochs bp = do
            let params = BState._blockBirkParameters (BS._bpState bp)
            let currentEpoch = epoch $ _birkSeedState params
            let currentSlot = case BS._bpBlock bp of
                    B.GenesisBlock _ -> 0
                    B.NormalBlock block -> B.bbSlot block
            -- The slot of the block should be in the epoch of its parameters:
            unless (currentEpoch == theSlot (currentSlot `div` epochLength (_birkSeedState params))) $
                Left $ "Slot " ++ show currentSlot ++ " is not in epoch " ++ show currentEpoch
            let parentParams = BState._blockBirkParameters (BS._bpState (runIdentity $ BS._bpParent bp))
            let parentEpoch = epoch $ _birkSeedState parentParams
            unless (currentEpoch == parentEpoch) $
                    -- The leadership election nonce should change every epoch
                    checkBinary (/=) (currentSeed $ _birkSeedState params) (currentSeed $ _birkSeedState parentParams)
                            "/=" ("Epoch " ++ show currentEpoch ++ " seed: " ) ("Epoch " ++ show parentEpoch ++ " seed: " )
            let nextEpochParams = slotDependentBirkParameters (currentSlot + epochLength (_birkSeedState params)) params
            let prevEpochBakers = _birkPrevEpochBakers params
            let futureLotteryBakers = _birkLotteryBakers nextEpochParams
            -- This epoch's prevEpochBakers should be the next epoch's lotterybakers
            checkBinary (==) prevEpochBakers futureLotteryBakers "==" "baker state of previous epoch " " lottery bakers in next epoch "
        addContext (Left err) = Left $ "Blocks: " ++ show _blockTable ++ "\n\n" ++ err
        addContext r = r

invariantSkovFinalization :: SkovState (Config t) -> Either String ()
invariantSkovFinalization (SkovState sd@TS.SkovData{..} FinalizationState{..} _ _) = do
        invariantSkovData sd
        let (_ Seq.:|> (lfr, lfb)) = _finalizationList
        checkBinary (==) _finsIndex (succ $ finalizationIndex lfr) "==" "current finalization index" "successor of last finalized index"
        checkBinary (==) _finsHeight (bpHeight lfb + max (1 + _finsMinSkip) ((bpHeight lfb - bpHeight (runIdentity $ BS._bpLastFinalized lfb)) `div` 2)) "==" "finalization height"  "calculated finalization height"
        -- This test assumes that this party should be a member of the finalization committee
        when (null _finsCurrentRound) $ Left "No current finalization round"
        forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
            checkBinary (>=) roundDelta (max 1 (finalizationDelay lfr `div` 2)) ">=" "round delta" "half last finalization delay (or 1)"
            unless (popCount (toInteger roundDelta) == 1) $ Left $ "Round delta (" ++ show roundDelta ++ ") is not a power of 2"
            -- Determine which blocks are valid candidates for finalization
            -- i.e. they are at height _finsHeight and have descendants at height _finsHeight + roundDelta
            let descendants = case _branches Seq.!? (fromIntegral $ _finsHeight + roundDelta - bpHeight lfb - 1) of
                                    Nothing -> []
                                    Just bs -> bs
            let
                nthAncestor 0 b = b
                nthAncestor n b = nthAncestor (n-1) (runIdentity $ BS._bpParent b)
            let eligibleBlocks = Set.fromList $ bpHash . nthAncestor roundDelta <$> descendants
            let justifiedProposals = Map.keysSet $ Map.filter fst $ _proposals $ _freezeState $ roundWMVBA
            checkBinary (==) justifiedProposals eligibleBlocks "==" "nominally justified finalization blocks" "actually justified finalization blocks"
            case roundInput of
                Nothing -> unless (null eligibleBlocks) $ Left "There are eligible finalization blocks, but none has been nominated"
                Just nom -> checkBinary Set.member nom eligibleBlocks "is an element of" "the nominated final block" "the set of eligible blocks"
        -- The finalization queue should include all finalizations back from the last one
        -- that are not contained in finalized blocks.
        let finQLF Seq.Empty l = l
            finQLF (q Seq.:|> (fr, b)) l
                | bpHash b == BS.bpLastFinalizedHash lfb = l
                | otherwise = finQLF q ((fr, b) Seq.<| l)
            finQ = finQLF _finalizationList Seq.empty
        checkBinary (==) (_fqFirstIndex _finsQueue) (maybe 1 (finalizationIndex . fst) (finQ Seq.!? 0) ) "==" ("finalization queue first index (from " ++ show _finsQueue ++ ")") ("expected value (from " ++ show finQ ++ " and last fin's last fin: " ++ show (BS.bpLastFinalizedHash lfb) ++ ")")
        -- Check that everything in finQLF is actually in the finalization queue
        -- and check that there is at most one extra record which is for an unknown block
        let
            checkFinQ Seq.Empty Seq.Empty = return ()
            checkFinQ Seq.Empty (fp Seq.:<| Seq.Empty) = case HM.lookup (fpBlock fp) _blockTable of
                    Nothing -> return ()
                    Just (TreeState.BlockPending _) -> return ()
                    Just s -> Left $ "Status of residual finalization proof should be Nothing or BlockPending, but was " ++ show s
            checkFinQ Seq.Empty s = Left $ "There should be at most 1 residual finalization proof, but there were " ++ show (Seq.length s)
            checkFinQ ((fr, _) Seq.:<| fl') (fp Seq.:<| fps') = do
                checkBinary (==) (finalizationBlockPointer fr) (fpBlock fp) "==" "finalization list block" "finalization queue block"
                checkBinary (==) (finalizationIndex fr) (fpIndex fp) "==" "finalization list index" "finalization queue index"
                checkFinQ fl' fps'
            checkFinQ _ _ = Left $ "Finalization queue is missing finalization"
        checkFinQ finQ (_fqProofs _finsQueue)

    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

checkFinalizationRecordsVerify :: TS.SkovData BState.BlockState -> Either String ()
checkFinalizationRecordsVerify TS.SkovData{..} =
      let finSes = FinalizationSessionId (bpHash _genesisBlockPointer) 0
          finCom = makeFinalizationCommittee (genesisFinalizationParameters _genesisData)
          checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
          f (prevRes, i) (fr, bp) = case prevRes of
            Left err -> return (Left err, i+1)
            Right _ ->
              if i == (0 :: Int) then
                  return $ (checkBinary (==) bp _genesisBlockPointer "==" "first finalized block" "genesis block", i+1)
              else
                  return (unless (verifyFinalProof finSes finCom fr) $ Left $ "Could not verify finalization record at index " ++ show i, i+1)
      in do
        (res, _) <- foldM f (Right (), 0) _finalizationList
        res

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



type MyHandlers = SkovHandlers DummyTimer (Config DummyTimer) (StateT ExecState LogIO)

data Event
    = EBake Slot
    | EBlock BakedBlock
    | ETransaction BlockItem
    | EFinalization FinalizationPseudoMessage
    | EFinalizationRecord FinalizationRecord
    | ETimer Integer (SkovT MyHandlers (Config DummyTimer) (StateT ExecState LogIO) ())

instance Show Event where
    show (EBake sl) = "bake for " ++ show sl
    show (EBlock b) = "block: " ++ show (getHash b :: BlockHash)
    show (ETransaction tr) = "transaction: " ++ show tr
    show (EFinalization fmsg) = "finalization message: " ++ show fmsg
    show (EFinalizationRecord fr) = "finalize: " ++ show (finalizationBlockPointer fr)
    show (ETimer _ _) = "timer event"

type EventPool = Seq (Int, Event)

-- |Pick an element from a sequence, returning the element
-- and the sequence with that element removed.
selectFromSeq :: (RandomGen g) => g -> Seq a -> (a, Seq a, g)
selectFromSeq g s =
    let (n , g') = randomR (0, length s - 1) g in
    (Seq.index s n, Seq.deleteAt n s, g')

newtype DummyTimer = DummyTimer Integer

type States = Vec.Vector (BakerIdentity, SkovContext (Config DummyTimer), SkovState (Config DummyTimer))

data ExecState = ExecState {
    _esEventPool :: EventPool,
    _esNextTimer :: !Integer,
    _esCancelledTimers :: Set.Set Integer
}
makeLenses ''ExecState

makeExecState :: EventPool -> ExecState
makeExecState _esEventPool = ExecState {..}
    where
        _esNextTimer = 0
        _esCancelledTimers = Set.empty

dummyHandlers :: Int -> [Int] -> MyHandlers
dummyHandlers src btargets = SkovHandlers {..}
    where
        shBroadcastFinalizationMessage fm = esEventPool %= (<> Seq.fromList [(r, EFinalization fm) | r <- btargets])
        shBroadcastFinalizationRecord fr = esEventPool %= (<> Seq.fromList [(r, EFinalizationRecord fr) | r <- btargets])
        shOnTimeout _ action = do
            t <- esNextTimer <<%= (+1)
            esEventPool %= (Seq.|> (src, ETimer t (void action)))
            return $ DummyTimer t
        shCancelTimer (DummyTimer t) =
            esCancelledTimers %= Set.insert t
        shPendingLive = return ()

myRunSkovT :: (MonadIO m) => (SkovT MyHandlers (Config DummyTimer) (StateT ExecState LogIO) a) -> MyHandlers -> SkovContext (Config DummyTimer) -> SkovState (Config DummyTimer) -> ExecState -> m (a, SkovState (Config DummyTimer), ExecState)
myRunSkovT a handlers ctx st es = liftIO $ flip runLoggerT doLog $ do
        ((res, st'), es') <- runStateT (runSkovT a handlers ctx st) es
        return (res, st', es')
    where
        doLog src LLError msg = error $ show src ++ ": " ++ msg
        doLog _ _ _ = return ()

myEvalSkovT :: (MonadIO m) => (SkovT () (Config DummyTimer) IO a) -> SkovContext (Config DummyTimer) -> SkovState (Config DummyTimer) -> m a
myEvalSkovT a ctx st = liftIO $ evalSkovT a () ctx st

runKonsensusTest :: RandomGen g => Int -> g -> States -> ExecState -> IO Property
runKonsensusTest steps g states es
        | steps <= 0 = return $ label ("fin length: " ++ show (maximum $ (\s -> s ^. _3 . to ssGSState . TS.finalizationList . to Seq.length) <$> states )) $ property True
        | null (es ^. esEventPool) = return $ property True
        | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g (es ^. esEventPool)
            let es1 = es & esEventPool .~ events'
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            let continue fs' es' = case invariantSkovFinalization fs' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err) False
                    Right _ -> do
                        let states' = states & ix rcpt . _3 .~ fs'
                        runKonsensusTest (steps - 1) g' states' es'
            let handlers = dummyHandlers rcpt btargets
            case ev of
                EBake sl -> do
                    (mb, fs', es2) <- myRunSkovT (bakeForSlot bkr sl) handlers fi fs es1
                    case mb of
                        Nothing -> continue fs' (es2 & esEventPool %~ ((rcpt, EBake (sl + 1)) Seq.<|))
                        Just (BS.BlockPointer {_bpBlock = B.NormalBlock b}) ->
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
                ETimer t timerEvent -> do
                    if t `Set.member` (es ^. esCancelledTimers) then
                        runKonsensusTest steps g' states (es1 & esCancelledTimers %~ Set.delete t)
                    else do
                        (_, fs', es') <- myRunSkovT timerEvent handlers fi fs es1
                        continue fs' es'

runKonsensusTestSimple :: RandomGen g => Int -> g -> States -> ExecState -> IO Property
runKonsensusTestSimple steps g states es
        | steps <= 0 || null (es ^. esEventPool) =
            let checkInvariantAndFinalization s@(SkovState sd@TS.SkovData{..} FinalizationState{..} _ _) = do
                  checkFinalizationRecordsVerify sd
                  invariantSkovFinalization s
            in return
              (case forM_ states $ \s -> checkInvariantAndFinalization (s ^. _3) of
                  Left err -> counterexample ("Invariant failed: " ++ err) False
                  Right _ -> property True)
        | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g (es ^. esEventPool)
            let es1 = es & esEventPool .~ events'
            let (bkr, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            let continue fs' es' = do
                        let states' = states & ix rcpt . _3 .~ fs'
                        runKonsensusTest (steps - 1) g' states' es'
            let handlers = dummyHandlers rcpt btargets
            case ev of
                EBake sl -> do
                    (mb, fs', es2) <- myRunSkovT (bakeForSlot bkr sl) handlers fi fs es1
                    case mb of
                        Nothing -> continue fs' (es2 & esEventPool %~ ((rcpt, EBake (sl + 1)) Seq.<|))
                        Just (BS.BlockPointer {_bpBlock = B.NormalBlock b}) ->
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
                ETimer t timerEvent -> do
                    if t `Set.member` (es ^. esCancelledTimers) then
                        runKonsensusTest steps g' states (es1 & esCancelledTimers %~ Set.delete t)
                    else do
                        (_, fs', es') <- myRunSkovT timerEvent handlers fi fs es1
                        continue fs' es'


nAccounts :: Int
nAccounts = 2

genTransactions :: Int -> Gen [BlockItem]
genTransactions n = mapM gent (take n [minNonce..])
    where
        gent nnce = do
            f <- arbitrary
            g <- arbitrary
            return $ Example.makeTransaction f (ContractAddress (fromIntegral $ g `mod` nAccounts) 0) nnce


initialEvents :: States -> EventPool
initialEvents states = Seq.fromList [(x, EBake 1) | x <- [0..length states -1]]

makeBaker :: BakerId -> Amount -> Gen (BakerInfo, BakerIdentity, Account)
makeBaker bid lot = resize 0x20000000 $ do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk                     <- genBlockKeyPair
        blssk                  <- fst . randomBlsSecretKey . mkStdGen <$> arbitrary
        let spk = Sig.verifyKey sk
        let blspk = Bls.derivePublicKey blssk
        let account = makeBakerAccount bid
        return (BakerInfo epk spk blspk lot (_accountAddress account), BakerIdentity sk ek blssk, account)

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

initialiseStates :: Int -> PropertyM IO States
initialiseStates n = do
        let bns = [0..fromIntegral n - 1]
        bis <- mapM (\i -> (i,) <$> pick (makeBaker i 1)) bns
        let genesisBakers = fst . bakersFromList $ (^. _2 . _1) <$> bis
        let bps = BirkParameters 0.5 genesisBakers genesisBakers genesisBakers (genesisSeedState (hash "LeadershipElectionNonce") 10)
            fps = FinalizationParameters [VoterInfo vvk vrfk 1 blspk | (_, (BakerInfo vrfk vvk blspk _ _, _, _)) <- bis] 2
            bakerAccounts = map (\(_, (_, _, acc)) -> acc) bis
            gen = GenesisData 0 1 bps bakerAccounts [] fps dummyCryptographicParameters dummyIdentityProviders 10 $ Energy maxBound
        res <- liftIO $ mapM (\(_, (_, bid, _)) -> do
                                let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                let config = SkovConfig
                                        (MTMBConfig defaultRuntimeParameters gen (Example.initialState bps dummyCryptographicParameters bakerAccounts [] nAccounts))
                                        (ActiveFinalization fininst gen)
                                        NoHandler
                                (initCtx, initState) <- liftIO $ initialiseSkov config
                                return (bid, initCtx, initState)
                             ) bis
        return $ Vec.fromList res

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

instance Show FinalizationInstance where
    show _ = "[Finalization Instance]"

{-

instance Show SkovActiveState where
    show sfs = show (sfs ^. TS.skov)
-}

withInitialStates :: Int -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStates n r = monadicIO $ do
        s0 <- initialiseStates n
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0)

withInitialStatesTransactions :: Int -> Int -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStatesTransactions n trcount r = monadicIO $ do
        s0 <- initialiseStates n
        trs <- pick . genTransactions $ trcount
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0 <> Seq.fromList [(x, ETransaction tr)
                                                                            | x <- [0..n-1], tr <- trs])

withInitialStatesDoubleTransactions :: Int -> Int -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStatesDoubleTransactions n trcount r = monadicIO $ do
        s0 <- initialiseStates n
        trs0 <- pick . genTransactions $ trcount
        trs <- (trs0 ++) <$> pick (genTransactions trcount)
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0 <> Seq.fromList [(x, ETransaction tr)
                                                                            | x <- [0..n-1], tr <- trs])


tests :: Word -> Spec
tests lvl = parallel $ describe "Concordium.Konsensus" $ do
    -- it "catch up at end" $ withMaxSuccess 5 $ withInitialStates 2 $ runKonsensusTestSimple 100
    it "2 parties, 100 steps, 20 transactions with duplicates, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStatesDoubleTransactions 2 10 $ runKonsensusTest 100
    it "2 parties, 100 steps, 10 transactions, check at every step" $ withMaxSuccess (10*10^lvl) $ withInitialStatesTransactions 2 10 $ runKonsensusTest 100
    it "2 parties, 1000 steps, 50 transactions, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStatesTransactions 2 50 $ runKonsensusTest 1000
    it "2 parties, 100 steps, check at every step" $ withMaxSuccess (10*10^lvl) $ withInitialStates 2 $ runKonsensusTest 100
    --it "2 parties, 100 steps, check at end" $ withMaxSuccess 50000 $ withInitialStates 2 $ runKonsensusTestSimple 100
    --it "2 parties, 1000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 1000
    it "2 parties, 1000 steps, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStates 2 $ runKonsensusTest 10000
    --it "2 parties, 10000 steps, check at end" $ withMaxSuccess 100 $ withInitialStates 2 $ runKonsensusTestSimple 10000
    it "4 parties, 10000 steps, check every step" $ withMaxSuccess (10^lvl `div` 20) $ withInitialStates 4 $ runKonsensusTest 10000
