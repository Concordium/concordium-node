{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, LambdaCase, RecordWildCards, ViewPatterns #-}
module Concordium.MonadImplementation where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Exception(assert)
import Lens.Micro.Platform
import Data.Foldable
import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.Payload.Transaction
import Concordium.Payload.Monad
import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Kontrol.Monad
import Concordium.Birk.LeaderElection

import qualified Scheduler as Sch

data BlockStatus =
    BlockAlive !BlockPointer
    | BlockDead
    | BlockFinalized !BlockPointer !FinalizationRecord

data PendingBlock = PendingBlock {
    pbHash :: !BlockHash,
    pbBlock :: !Block
}
instance Eq PendingBlock where
    pb1 == pb2 = pbHash pb1 == pbHash pb2

data SkovData = SkovData {
    -- |Map of all received blocks by hash.
    _skovBlockTable :: HM.HashMap BlockHash BlockStatus,
    _skovPossiblyPendingTable :: HM.HashMap BlockHash [PendingBlock],
    _skovPossiblyPendingQueue :: MPQ.MinPQueue Slot (BlockHash, BlockHash),
    _skovBlocksAwaitingLastFinalized :: MPQ.MinPQueue BlockHeight PendingBlock,
    _skovFinalizationList :: Seq.Seq (FinalizationRecord, BlockPointer),
    _skovFinalizationPool :: Map.Map FinalizationIndex [FinalizationRecord],
    _skovBranches :: Seq.Seq [BlockPointer],
    _skovGenesisData :: GenesisData,
    _skovGenesisBlockPointer :: BlockPointer,
    _transactionsFinalized :: Map.Map TransactionNonce Transaction,
    _transactionsPending :: Map.Map TransactionNonce Transaction
}
makeLenses ''SkovData

initialSkovData :: GenesisData -> SkovData
initialSkovData gd = SkovData {
            _skovBlockTable = HM.singleton gbh (BlockFinalized gb gbfin),
            _skovPossiblyPendingTable = HM.empty,
            _skovPossiblyPendingQueue = MPQ.empty,
            _skovBlocksAwaitingLastFinalized = MPQ.empty,
            _skovFinalizationList = Seq.singleton (gbfin, gb),
            _skovFinalizationPool = Map.empty,
            _skovBranches = Seq.empty,
            _skovGenesisData = gd,
            _skovGenesisBlockPointer = gb,
            _transactionsFinalized = Map.empty,
            _transactionsPending = Map.empty
        }
    where
        gb = makeGenesisBlockPointer gd
        gbh = bpHash gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

lastFinalizationRecord :: SkovData -> FinalizationRecord
lastFinalizationRecord sd = case _skovFinalizationList sd of
    _ Seq.:|> (lf,_) -> lf
    _ -> error "empty _skovFinalizationList"

lastFinalized :: SkovData -> BlockPointer
lastFinalized sd = case _skovFinalizationList sd of
    _ Seq.:|> (_,lf) -> lf
    _ -> error "empty _skovFinalizationList"

lastFinalizedHeight :: SkovData -> BlockHeight
lastFinalizedHeight = bpHeight . lastFinalized

lastFinalizedSlot :: SkovData -> Slot
lastFinalizedSlot = blockSlot . bpBlock . lastFinalized

-- |Handle a block arriving that is dead.  That is, the block has never
-- been in the tree before, and now it never can be.  Any descendents of
-- this block that have previously arrived cannot have been added to the
-- tree, and we purge them recursively from '_skovPossiblyPendingTable'.
blockArriveDead :: Monad m => BlockHash -> StateT SkovData m ()
blockArriveDead cbp = do
    skovBlockTable . at cbp ?= BlockDead
    children <- fmap pbHash <$> use (skovPossiblyPendingTable . at cbp . non [])
    forM_ children blockArriveDead

-- |Purge pending blocks with slot numbers predating the last finalized slot.
purgePending :: Monad m => StateT SkovData m ()
purgePending = do
        lfSlot <- use (to lastFinalizedSlot)
        let purge ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, (cbp, parenth)), ppq') ->
                    if sl <= lfSlot then do
                        skovPossiblyPendingTable . at parenth . non [] %= filter ((/= cbp) . pbHash)
                        blockStatus <- use (skovBlockTable . at cbp)
                        when (isNothing blockStatus) $
                            blockArriveDead cbp
                        purge ppq'
                    else
                        return ppq
                Nothing -> return ppq
        ppq <- use skovPossiblyPendingQueue
        ppq' <- purge ppq
        skovPossiblyPendingQueue .= ppq'

processAwaitingLastFinalized :: Monad m => StateT SkovData m ()
processAwaitingLastFinalized = do
    lastFinHeight <- use (to lastFinalizedHeight)
    (MPQ.minViewWithKey <$> use skovBlocksAwaitingLastFinalized) >>= \case
        Nothing -> return ()
        Just ((h, pb), balf') -> when (h <= lastFinHeight) $ do
            skovBlocksAwaitingLastFinalized .= balf'
            -- This block is awaiting its last final block to be finalized.
            -- At this point, it should be or it never will.
            addBlock pb
            processAwaitingLastFinalized

processFinalizationPool :: Monad m => StateT SkovData m ()
processFinalizationPool = do
    nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use skovFinalizationList
    finPending <- use (skovFinalizationPool . at nextFinIx)
    case finPending of
        Nothing -> return ()
        Just frs -> do
            blockStatus <- use skovBlockTable
            lastFinHeight <- use (to lastFinalizedHeight)
            finParams <- getFinalizationParameters
            let
                goodFin FinalizationRecord{..} =
                    -- FIXME: verify the finalizationProof
                    finalizationIndex == nextFinIx -- Should always be true
                checkFin finRec lp = case blockStatus ^. at (finalizationBlockPointer finRec) of
                    -- If the block is not present, the finalization record is pending
                    Nothing -> (finRec :) <$> lp
                    -- If the block is alive and the finalization proof checks out,
                    -- we can use this for finalization
                    Just (BlockAlive bp) -> if goodFin finRec then Left (finRec, bp) else lp
                    -- Otherwise, the finalization record is dead because the block is
                    -- either dead or already finalized
                    Just _ -> lp
            case foldr checkFin (Right []) frs of
                -- We got a valid finalization proof, so progress finalization
                Left (finRec, newFinBlock) -> do
                    skovFinalizationPool . at nextFinIx .= Nothing
                    skovFinalizationList %= (Seq.:|> (finRec, newFinBlock))
                    oldBranches <- use skovBranches
                    let pruneHeight = fromIntegral (bpHeight newFinBlock - lastFinHeight)
                    let
                        pruneTrunk _ Seq.Empty = return ()
                        pruneTrunk keeper (brs Seq.:|> l) = do
                            forM_ l $ \bp -> if bp == keeper then
                                                skovBlockTable . at (bpHash bp) ?= BlockFinalized bp finRec
                                            else
                                                skovBlockTable . at (bpHash bp) ?= BlockDead
                            pruneTrunk (bpParent keeper) brs
                    pruneTrunk newFinBlock (Seq.take pruneHeight oldBranches)
                    -- Prune the branches
                    -- blockTable <- use skovBlockTable
                    let
                        pruneBranches [] _ = return Seq.empty
                        pruneBranches _ Seq.Empty = return Seq.empty
                        pruneBranches parents (branches Seq.:<| rest) = do
                            survivors <- foldrM (\bp l ->
                                if bpParent bp `elem` parents then
                                    return (bp:l)
                                else do
                                    skovBlockTable . at (bpHash bp) ?= BlockDead
                                    return l)
                                [] branches
                            rest' <- pruneBranches survivors rest
                            return (survivors Seq.<| rest')
                    newBranches <- pruneBranches [newFinBlock] oldBranches
                    skovBranches .= newBranches
                    skovBranches .= Seq.drop pruneHeight oldBranches
                    -- purge pending blocks with slot numbers predating the last finalized slot
                    purgePending
                    -- handle blocks in skovBlocksAwaitingLastFinalized
                    processAwaitingLastFinalized
                    processFinalizationPool
                Right frs' -> skovFinalizationPool . at nextFinIx . non [] .= frs'
            

addBlock :: Monad m => PendingBlock -> StateT SkovData m ()
addBlock pb@(PendingBlock cbp block) = do
    res <- runMaybeT (tryAddBlock pb)
    case res of
        Nothing -> blockArriveDead cbp
        Just False -> return () -- The block was not inserted
        Just True -> do
            processFinalizationPool
            -- Handle any blocks that are waiting for this one
            mchildren <- skovPossiblyPendingTable . at cbp <<.= Nothing
            forM_ mchildren $ \children ->
                forM_ children $ \childpb -> do
                    childStatus <- use (skovBlockTable . at (pbHash childpb))
                    when (isNothing childStatus) $ addBlock childpb

tryAddBlock :: Monad m => PendingBlock -> MaybeT (StateT SkovData m) Bool
tryAddBlock pb@(PendingBlock cbp block) = do
        lfs <- use (to lastFinalizedSlot)
        -- The block must be later than the last finalized block
        guard $ lfs < blockSlot block
        parentStatus <- use (skovBlockTable . at (blockPointer block))
        case parentStatus of
            Nothing -> do
                skovPossiblyPendingTable . at parent . non [] %= (pb:)
                skovPossiblyPendingQueue %= MPQ.insert (blockSlot block) (cbp, blockPointer block)
                return False
            Just BlockDead -> mzero
            Just (BlockAlive parentP) -> tryAddLiveParent parentP
            Just (BlockFinalized parentP _) -> tryAddLiveParent parentP
    where
        parent = blockPointer block
        tryAddLiveParent parentP = do -- Alive or finalized
            let lf = blockLastFinalized block
            -- Check that the blockSlot is beyond the parent slot
            guard $ blockSlot (bpBlock parentP) < blockSlot block
            lfStatus <- use (skovBlockTable . at lf)
            case lfStatus of
                -- If the block's last finalized block is live, but not finalized yet,
                -- add this block to the queue at the appropriate point
                Just (BlockAlive lfBlockP) -> do
                    skovBlocksAwaitingLastFinalized %= MPQ.insert (bpHeight lfBlockP) pb
                    return False
                -- If the block's last finalized block is finalized, we can proceed with validation.
                -- Together with the fact that the parent is alive, we know that the new node
                -- is a descendent of the finalized block.
                Just (BlockFinalized lfBlockP finRec) -> do
                    -- The last finalized pointer must be to the block that was actually finalized.
                    -- (Blocks can be implicitly finalized when a descendent is finalized.)
                    guard $ finalizationBlockPointer finRec == lf
                    -- We need to know that the slot numbers of the last finalized blocks are ordered.
                    -- If the parent block is the genesis block then its last finalized pointer is not valid,
                    -- and we skip the check.
                    genB <- genesisBlock
                    unless (parentP == genB) $ guard $
                        blockSlot (bpBlock lfBlockP) <= blockSlot (bpBlock (bpLastFinalized parentP))
                    bps@BirkParameters{..} <- getBirkParameters (blockSlot block)
                    BakerInfo{..} <- MaybeT $ pure $ birkBaker (blockBaker block) bps
                    -- Check the block proof
                    guard $ verifyProof
                                birkLeadershipElectionNonce
                                birkElectionDifficulty
                                (blockSlot block)
                                bakerElectionVerifyKey
                                bakerLotteryPower
                                (blockProof block)
                    -- The block nonce
                    guard $ verifyBlockNonce
                                birkLeadershipElectionNonce
                                (blockSlot block)
                                bakerElectionVerifyKey
                                (blockNonce block)
                    -- And the block signature
                    guard $ verifyBlockSignature bakerSignatureVerifyKey block
                    let height = bpHeight parentP + 1
                    -- TODO: need to execute block to get new state
                    -- parentP has the old state
                    -- toTransactions on the transactions in "block"
                    -- mzero signals that block is invalid

                    -- try to decode the block data into a list of transactions
                    case toTransactions (blockData block) of
                      Nothing -> mzero
                      Just ts ->
                        case executeBlock ts (bpState parentP) of
                          Sch.BlockInvalid _ -> mzero
                          Sch.BlockSuccess _ gs -> do
                            let blockP = BlockPointer {
                                  bpHash = cbp,
                                  bpBlock = block,
                                  bpParent = parentP,
                                  bpLastFinalized = lfBlockP,
                                  bpHeight = height,
                                  bpState = gs
                                  }
                            skovBlockTable . at cbp ?= BlockAlive blockP
                            finHeight <- fromIntegral . Seq.length <$> use skovFinalizationList
                            branches <- use skovBranches
                            let branchLen = fromIntegral $ Seq.length branches
                            let heightIncreased = height - finHeight < branchLen
                            if heightIncreased then
                              skovBranches . ix (fromIntegral (height - finHeight)) %= (blockP:)
                            else
                               assert (height - finHeight == branchLen)
                                      skovBranches %= (Seq.|> [blockP])
                            return True
                -- If the block's last finalized block is dead, then the block arrives dead.
                -- If the block's last finalized block is pending then it can't be an ancestor,
                -- so the block is invalid and it arrives dead.
                _ -> mzero
    

-- Tree consists of finalization list + branches
-- When adding a block that is at height in the finalization list, check if it's already there; if not, it should be dead.
-- Height 0 is the genesis block

instance Monad m => SkovMonad (StateT SkovData m) where
    {-# INLINE resolveBlock #-}
    resolveBlock cbp = use (skovBlockTable . at cbp) <&> \case
                        Just (BlockAlive bp) -> Just bp
                        Just (BlockFinalized bp _) -> Just bp
                        _ -> Nothing
    storeBlock block0 = do
            let cbp = hashBlock block0
            oldBlock <- use (skovBlockTable . at cbp)
            when (isNothing oldBlock) $
                -- The block is new, so we have some work to do.
                addBlock (PendingBlock cbp block0)
            return cbp
    finalizeBlock finRec = do
            let thisFinIx = finalizationIndex finRec
            nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use skovFinalizationList
            case compare thisFinIx nextFinIx of
                LT -> return () -- Already finalized at that index
                EQ -> do 
                     skovFinalizationPool . at thisFinIx . non [] %= (finRec:)
                     processFinalizationPool
                GT -> skovFinalizationPool . at thisFinIx . non [] %= (finRec:)
    isFinalized bp = preuse (skovBlockTable . ix bp) >>= \case
            Just (BlockFinalized _ _) -> return True
            _ -> return False
    lastFinalizedBlock = use (to lastFinalized)
    genesisData = use skovGenesisData
    genesisBlock = use skovGenesisBlockPointer
    getCurrentHeight = gets $ \skov -> lastFinalizedHeight skov + fromIntegral (Seq.length (_skovBranches skov)) - 1
    branchesFromTop = revSeqToList <$> use skovBranches
        where
            revSeqToList Seq.Empty = []
            revSeqToList (r Seq.:|> t) = t : revSeqToList r

instance Monad m => PayloadMonad (StateT SkovData m) where
    addPendingTransaction tr@Transaction{..} = do
        isFin <- Map.member transactionNonce <$> use transactionsFinalized
        unless isFin $
            transactionsPending %= Map.insert transactionNonce tr
    getPendingTransactionsAtBlock bp = do
        pts <- use transactionsPending
        getTransactionsAtBlock bp >>= \case
            Nothing -> return Nothing
            Just bts -> return $ Just $ Map.difference pts bts
    getTransactionsAtBlock bp = do
        lfp <- lastFinalizedBlock
        genp <- genesisBlock
        let getTrans cbp
                | cbp == lfp = use transactionsFinalized
                | cbp == genp = return Map.empty
                | otherwise = do
                        bts <- MaybeT . pure $ toTransactions (blockData (bpBlock cbp))
                        parentTrans <- getTrans (bpParent cbp)
                        let upd tr@Transaction{..} s = if transactionNonce `Map.member` s then Nothing else Just (Map.insert transactionNonce tr s)
                        MaybeT $ pure $ foldrM upd parentTrans bts
        runMaybeT $ getTrans bp

instance MonadIO m => KontrolMonad (StateT SkovData m)
