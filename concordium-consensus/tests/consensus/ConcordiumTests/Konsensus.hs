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
import Data.Either (isRight)

import qualified Data.ByteString.Lazy as BSL
import System.IO.Unsafe

import Concordium.Crypto.SHA256

import Concordium.Afgjort.Finalize.Types
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Rewards (BankStatus(..))
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
import qualified Concordium.Crypto.SignatureScheme as SigScheme
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

import Concordium.Startup (makeBakerAccountKP)

import Concordium.Crypto.DummyData
import Concordium.Types.DummyData (mateuszAccount)

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

-- import Debug.Trace

{-# NOINLINE dummyCryptographicParameters #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (readCryptographicParameters <$> BSL.readFile "../scheduler/testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

dummyTime :: UTCTime
dummyTime = posixSecondsToUTCTime 0

type Trs = HM.HashMap TransactionHash (BlockItem, TransactionStatus)
type ANFTS = HM.HashMap AccountAddress AccountNonFinalizedTransactions

type Config t = SkovConfig MemoryTreeMemoryBlockConfig (ActiveFinalization t) NoHandler

finalizationParameters :: FinalizationCommitteeSize -> FinalizationParameters
finalizationParameters = FinalizationParameters 2

-- Maximum finalization-committee size for most tests, where we don't try to ensure that the committee members change.
defaultMaxFinComSize :: FinalizationCommitteeSize
defaultMaxFinComSize = 1000

-- Maximum finalization-committee size for the test in which we want to ensure that the finalization committee members
-- change. To ensure that the members change it is convenient for this number to be similar to the total number of bakers.
maxFinComSizeChangingFinCommittee :: FinalizationCommitteeSize
maxFinComSizeChangingFinCommittee = 10

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

invariantSkovFinalization :: SkovState (Config t) -> BakerInfo -> FinalizationCommitteeSize -> Either String ()
invariantSkovFinalization (SkovState sd@TS.SkovData{..} FinalizationState{..} _ _) baker maxFinComSize = do
        invariantSkovData sd
        let (_ Seq.:|> (lfr, lfb)) = _finalizationList
        checkBinary (==) _finsIndex (succ $ finalizationIndex lfr) "==" "current finalization index" "successor of last finalized index"
        checkBinary (==) _finsHeight (bpHeight lfb + max (1 + _finsMinSkip) ((bpHeight lfb - bpHeight (runIdentity $ BS._bpLastFinalized lfb)) `div` 2)) "==" "finalization height"  "calculated finalization height"
        let prevState  = BS._bpState lfb
            prevBakers = _birkCurrentBakers $ BState._blockBirkParameters prevState
            prevGTU    = _totalGTU $ BState._blockBank prevState
        checkBinary (==) _finsCommittee (makeFinalizationCommittee (finalizationParameters maxFinComSize) prevGTU prevBakers) "==" "finalization committee" "calculated finalization committee"
        when (null (parties _finsCommittee)) $ Left "Empty finalization committee"
        let bakerInFinCommittee = Vec.any bakerEqParty (parties _finsCommittee)
            bakerEqParty PartyInfo{..} = voterVerificationKey (bakerInfoToVoterInfo baker) == partySignKey
        checkBinary (==) bakerInFinCommittee (isRight _finsCurrentRound) "<->" "baker is in finalization committee" "baker has current finalization round"
        forM_ _finsCurrentRound $ \FinalizationRound{..} -> do
            -- The following checks are performed only for the baker; the baker is part of the in the current finalization round
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

checkBinary :: (Show x, Show y, Monad m) => (x -> y -> Bool) -> x -> y -> String -> String -> String -> m ()
checkBinary bop x y sbop sx sy =
  unless (bop x y) $ fail $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

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

type States = Vec.Vector (BakerIdentity, BakerInfo, SigScheme.KeyPair, SkovContext (Config DummyTimer), SkovState (Config DummyTimer))

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

type FinComPartiesSet = Set.Set (Set.Set Sig.VerifyKey)

-- The `collectedFinComParties` parameter should be Nothing if we don't care about changing the finalization committee members.
-- Otherwise, it carries a set of finalization-committee-member sets. In each round of finalization, we add the
-- set of parties to this set. In the end, we want this set to be greater than 1, which means that
-- there have been at least two different sets of committee members.
runKonsensusTest :: RandomGen g => FinalizationCommitteeSize -> Maybe FinComPartiesSet -> Int -> g -> States -> ExecState -> IO Property
runKonsensusTest maxFinComSize collectedFinComParties steps g states es
        | steps <= 0 = return $
            let finReport = "fin length: " ++ show (maximum $ (\s -> s ^. _5 . to ssGSState . TS.finalizationList . to Seq.length) <$> states ) in
            case collectedFinComParties of
              Just ps -> label (finReport ++ "; number of distinct finalization committees: " ++ show (Set.size ps)) $ property True
              Nothing -> label finReport $ property True
        | null (es ^. esEventPool) = return $ property True
        | otherwise = do
            let ((rcpt, ev), events', g') = selectFromSeq g (es ^. esEventPool)
            let es1 = es & esEventPool .~ events'
            let (bkr, bkrInfo, _, fi, fs) = states Vec.! rcpt
            let btargets = [x | x <- [0..length states - 1], x /= rcpt]
            let continue fs'@(SkovState _ fState _ _) es' = case invariantSkovFinalization fs' bkrInfo maxFinComSize of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err) False
                    Right _ -> do
                        let states' = states & ix rcpt . _5 .~ fs'
                            -- Adding the current set of finalization-committee members to `collectedFinComParties`
                            newCollectedFinComParties = collectedFinComParties <&>
                              Set.insert (Set.fromList $ partySignKey <$> (Vec.toList . parties) (_finsCommittee fState))
                        runKonsensusTest maxFinComSize newCollectedFinComParties (steps - 1) g' states' es'
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
                        runKonsensusTest maxFinComSize collectedFinComParties steps g' states (es1 & esCancelledTimers %~ Set.delete t)
                    else do
                        (_, fs', es') <- myRunSkovT timerEvent handlers fi fs es1
                        continue fs' es'

runKonsensusTestForChangingFinCommittee :: RandomGen g => Int -> g -> States -> ExecState -> IO Property
runKonsensusTestForChangingFinCommittee = runKonsensusTest maxFinComSizeChangingFinCommittee (Just Set.empty)

runKonsensusTestDefault :: RandomGen g => Int -> g -> States -> ExecState -> IO Property
runKonsensusTestDefault = runKonsensusTest defaultMaxFinComSize Nothing

nAccounts :: Int
nAccounts = 2

genTransactions :: Int -> Gen [BlockItem]
genTransactions n = mapM gent (take n [minNonce..])
    where
        gent nnce = do
            f <- arbitrary
            g <- arbitrary
            return $ Example.makeTransaction f (ContractAddress (fromIntegral $ g `mod` nAccounts) 0) nnce

-- Generate transactions that transfer between `minAmount` and `maxAmount` of GTU among accounts specified in
-- `kpAccountPairs`.
genTransferTransactions :: AmountUnit -> AmountUnit -> [(SigScheme.KeyPair, AccountAddress)] -> Int -> Gen [BlockItem]
genTransferTransactions minAmount maxAmount kpAccountPairs = gtt [] . Map.fromList $ zip (snd <$> kpAccountPairs) $ repeat minNonce
  where gtt :: [BlockItem] -> Map.Map AccountAddress Nonce -> Int -> Gen [BlockItem]
        gtt ts _ 0          = return ts
        gtt ts accToNonce m = do
          amount  <- Amount <$> choose (minAmount, maxAmount)
          fromAcc <- elements kpAccountPairs
          toAcc   <- elements kpAccountPairs `suchThat` (/= fromAcc)
          let fromAddress = snd fromAcc
              nonce = accToNonce Map.! fromAddress
              t = Example.makeTransferTransaction fromAcc (snd toAcc) amount nonce
          gtt (t : ts) (Map.insert fromAddress (nonce + 1) accToNonce) (m - 1)

initialEvents :: States -> EventPool
initialEvents states = Seq.fromList [(x, EBake 1) | x <- [0..length states -1]]

makeBaker :: BakerId -> Amount -> Gen (BakerInfo, BakerIdentity, Account, SigScheme.KeyPair)
makeBaker bid initAmount = resize 0x20000000 $ do
        ek@(VRF.KeyPair _ epk) <- arbitrary
        sk                     <- genBlockKeyPair
        blssk                  <- fst . randomBlsSecretKey . mkStdGen <$> arbitrary
        let spk = Sig.verifyKey sk
        let blspk = Bls.derivePublicKey blssk
        let (account, kp) = makeBakerAccountKP bid initAmount
        return (BakerInfo epk spk blspk initAmount (_accountAddress account), BakerIdentity sk ek blssk, account, kp)

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

mateuszAmount :: Amount
mateuszAmount = Amount (2 ^ (40 :: Int))

-- Initial states for the tests that don't attempt to change the members of the finalization committee.
initialiseStates :: Int -> FinalizationCommitteeSize -> PropertyM IO States
initialiseStates n maxFinComSize = do
        let bns = [0..fromIntegral n - 1]
        bis <- mapM (\i -> (i,) <$> pick (makeBaker i (mateuszAmount * 4))) bns
        createInitStates bis maxFinComSize [Example.createCustomAccount mateuszAmount mateuszKP mateuszAccount]

-- Initial states for the test in which we want finalization-committee members to change.
-- The `averageStake` amount is a stake such that
--   * it is approximately equal to `1 / maxFinCommitteSize` of the total stake
--   * to be in the committee, a baker needs to have at least `averageStake` stake
-- We pick `f` bakers that will initially be in the finalization committee; their stake will be somewhat greater than
-- `averageStake`; and we pick `b` bakers whose stake will be lower and who therefore won't be in the committee.
initialiseStatesTransferTransactions :: Int -> Int -> Amount -> Amount -> FinalizationCommitteeSize -> PropertyM IO States
initialiseStatesTransferTransactions f b averageStake stakeDiff maxFinComSize = do
        let fs          = [0..fromIntegral f - 1]
            bs          = fromIntegral <$> [fromIntegral f..f + b - 1]
            finComStake = averageStake + stakeDiff
            restStake   = averageStake - stakeDiff
            bakers s    = mapM (\i -> (i,) <$> pick (makeBaker i s))
        finBs    <- bakers finComStake fs
        nonFinBs <- bakers restStake bs
        createInitStates (finBs ++ nonFinBs) maxFinComSize []

createInitStates :: [(BakerId, (BakerInfo, BakerIdentity, Account, SigScheme.KeyPair))] -> FinalizationCommitteeSize -> [Account] -> PropertyM IO States
createInitStates bis maxFinComSize specialAccounts = do
        let genesisBakers = fst . bakersFromList $ (^. _2 . _1) <$> bis
            bps = BirkParameters 0.5 genesisBakers genesisBakers genesisBakers (genesisSeedState (hash "LeadershipElectionNonce") 10)
            bakerAccounts = map (\(_, (_, _, acc, _)) -> acc) bis
            gen = GenesisData 0 1 bps bakerAccounts [] (finalizationParameters maxFinComSize) dummyCryptographicParameters dummyIdentityProviders 10 $ Energy maxBound
            createStates = liftIO . mapM (\(_, (binfo, bid, _, kp)) -> do
                                       let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)
                                           config = SkovConfig
                                               (MTMBConfig defaultRuntimeParameters gen (Example.initialState bps dummyCryptographicParameters bakerAccounts [] nAccounts specialAccounts))
                                               (ActiveFinalization fininst gen)
                                               NoHandler
                                       (initCtx, initState) <- liftIO $ initialiseSkov config
                                       return (bid, binfo, kp, initCtx, initState))
        Vec.fromList <$> createStates bis

instance Show BakerIdentity where
    show _ = "[Baker Identity]"

instance Show FinalizationInstance where
    show _ = "[Finalization Instance]"

{-

instance Show SkovActiveState where
    show sfs = show (sfs ^. TS.skov)
-}

withInitialStates :: Int -> FinalizationCommitteeSize -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStates n maxFinComSize r = monadicIO $ do
        s0 <- initialiseStates n maxFinComSize
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0)

withInitialStatesTransactions :: Int -> Int -> FinalizationCommitteeSize -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStatesTransactions n trcount maxFinComSize r = monadicIO $ do
        s0 <- initialiseStates n maxFinComSize
        trs <- pick . genTransactions $ trcount
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0 <> Seq.fromList [(x, ETransaction tr)
                                                                            | x <- [0..n-1], tr <- trs])

withInitialStatesTransferTransactions :: Int -> Int -> FinalizationCommitteeSize -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStatesTransferTransactions n trcount maxFinComSize r = monadicIO $ do
        let finComSize = n `div` 2
            averageStake = 10 ^ (15 :: Int)
            stakeDiff = 500
            minTransferAmount = 10 ^ (3 :: Int)
            maxTransferAmount = 10 ^ (6 :: Int)
        s0 <- initialiseStatesTransferTransactions finComSize (n - finComSize) averageStake stakeDiff maxFinComSize
        let kpAddressPairs = Vec.toList $ (\(_, binfo, kp, _, _) -> (kp, _bakerAccount binfo)) <$> s0
        trs <- pick . genTransferTransactions minTransferAmount maxTransferAmount kpAddressPairs $ trcount
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0 <> Seq.fromList [(x, ETransaction tr)
                                                                            | x <- [0..n-1], tr <- trs])

withInitialStatesDoubleTransactions :: Int -> Int -> FinalizationCommitteeSize -> (StdGen -> States -> ExecState -> IO Property) -> Property
withInitialStatesDoubleTransactions n trcount maxFinComSize r = monadicIO $ do
        s0 <- initialiseStates n maxFinComSize
        trs0 <- pick . genTransactions $ trcount
        trs <- (trs0 ++) <$> pick (genTransactions trcount)
        gen <- pick $ mkStdGen <$> arbitrary
        liftIO $ r gen s0 (makeExecState $ initialEvents s0 <> Seq.fromList [(x, ETransaction tr)
                                                                            | x <- [0..n-1], tr <- trs])


tests :: Word -> Spec
tests lvl = parallel $ describe "Concordium.Konsensus" $ do
    it "2 parties, 100 steps, 20 transactions with duplicates, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStatesDoubleTransactions 2 10 defaultMaxFinComSize $ runKonsensusTestDefault 100
    it "2 parties, 100 steps, 10 transactions, check at every step" $ withMaxSuccess (10*10^lvl) $ withInitialStatesTransactions 2 10 defaultMaxFinComSize $ runKonsensusTestDefault 100
    it "2 parties, 1000 steps, 50 transactions, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStatesTransactions 2 50 defaultMaxFinComSize $ runKonsensusTestDefault 1000
    it "2 parties, 100 steps, check at every step" $ withMaxSuccess (10*10^lvl) $ withInitialStates 2 defaultMaxFinComSize $ runKonsensusTestDefault 100
    it "2 parties, 10000 steps, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStates 2 defaultMaxFinComSize $ runKonsensusTestDefault 10000
    it "4 parties, 10000 steps, check every step" $ withMaxSuccess (10^lvl) $ withInitialStates 4 defaultMaxFinComSize $ runKonsensusTestDefault 10000
    it "10 parties, 10000 steps, 30 transfer transactions, check at every step" $ withMaxSuccess (10^lvl) $ withInitialStatesTransferTransactions 10 30 maxFinComSizeChangingFinCommittee $ runKonsensusTestForChangingFinCommittee 10000
