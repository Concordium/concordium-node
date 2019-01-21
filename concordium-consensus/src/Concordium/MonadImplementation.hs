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
import qualified Data.Set as Set
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.PQueue.Prio.Min as MPQ

import Concordium.Payload.Transaction
import Concordium.Payload.Monad
import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Kontrol.Monad
import Concordium.Birk.LeaderElection

data BlockStatus =
    BlockAlive !BlockHeight
    | BlockDead
    | BlockFinalized !BlockHeight !FinalizationRecord

data SkovData = SkovData {
    -- |Map of all received blocks by hash.
    -- TODO: delete dead blocks
    _skovBlockTable :: HM.HashMap BlockHash Block,
    _skovPossiblyPendingTable :: HM.HashMap BlockHash [BlockHash],
    _skovPossiblyPendingQueue :: MPQ.MinPQueue Slot BlockHash,
    _skovBlocksAwaitingLastFinalized :: MPQ.MinPQueue BlockHeight BlockHash,
    _skovFinalizationList :: Seq.Seq FinalizationRecord,
    _skovFinalizationPool :: Map.Map FinalizationIndex [FinalizationRecord],
    _skovBranches :: Seq.Seq [BlockHash],
    _skovValidatedBlocks :: HS.HashSet BlockHash,
    _skovGenesisData :: GenesisData,
    _skovGenesisBlock :: Block,
    _skovGenesisBlockHash :: BlockHash,
    _skovBlockStatus :: HM.HashMap BlockHash BlockStatus,
    _transactionsFinalized :: Map.Map TransactionNonce Transaction,
    _transactionsPending :: Map.Map TransactionNonce Transaction
}
makeLenses ''SkovData

initialSkovData :: GenesisData -> SkovData
initialSkovData gd = SkovData {
            _skovBlockTable = HM.singleton gbh gb,
            _skovPossiblyPendingTable = HM.empty,
            _skovPossiblyPendingQueue = MPQ.empty,
            _skovBlocksAwaitingLastFinalized = MPQ.empty,
            _skovFinalizationList = Seq.singleton gbfin,
            _skovFinalizationPool = Map.empty,
            _skovBranches = Seq.empty,
            _skovValidatedBlocks = HS.singleton gbh,
            _skovGenesisData = gd,
            _skovGenesisBlock = gb,
            _skovGenesisBlockHash = gbh,
            _skovBlockStatus = HM.singleton gbh (BlockFinalized 0 gbfin),
            _transactionsFinalized = Map.empty,
            _transactionsPending = Map.empty
        }
    where
        gb = makeGenesisBlock gd
        gbh = hashBlock gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0

lastFinalizationRecord :: SkovData -> FinalizationRecord
lastFinalizationRecord sd = case _skovFinalizationList sd of
    _ Seq.:|> lf -> lf
    _ -> error "empty _skovFinalizationList"

lastFinalizedHeight :: SkovData -> BlockHeight
lastFinalizedHeight sd = case _skovBlockStatus sd HM.! finalizationBlockPointer (lastFinalizationRecord sd) of
    BlockFinalized ht _ -> ht
    _ -> error "Last finalized block not finalized"

lastFinalizedSlot :: SkovData -> Slot
lastFinalizedSlot sd = blockSlot $ _skovBlockTable sd HM.! finalizationBlockPointer (lastFinalizationRecord sd)

-- |Handle a block arriving that is dead.  That is, the block has never
-- been in the tree before, and now it never can be.  Any descendents of
-- this block that have previously arrived cannot have been added to the
-- tree, and we purge them recursively from '_skovPossiblyPendingTable'.
blockArriveDead :: Monad m => BlockHash -> StateT SkovData m ()
blockArriveDead bh = do
    skovBlockStatus . at bh ?= BlockDead
    children <- use (skovPossiblyPendingTable . at bh . non [])
    forM_ children blockArriveDead

-- |Purge pending blocks with slot numbers predating the last finalized slot.
purgePending :: Monad m => StateT SkovData m ()
purgePending = do
        lfSlot <- use (to lastFinalizedSlot)
        let purge ppq = case MPQ.minViewWithKey ppq of
                Just ((sl, bh), ppq') ->
                    if sl <= lfSlot then do
                        blockArriveDead bh
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
        Just ((h, bh), balf') -> when (h <= lastFinHeight) $ do
            -- This block is awaiting its last final block to be finalized.
            -- At this point, it should be or it never will.
            Just block0 <- resolveBlock bh
            let parent = blockPointer block0
            parentStatus <- use (skovBlockStatus . at parent)
            assert (not (isNothing parentStatus)) $
                addBlock parentStatus block0 bh
            processAwaitingLastFinalized

processFinalizationPool :: Monad m => StateT SkovData m ()
processFinalizationPool = do
    nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use skovFinalizationList
    finPending <- use (skovFinalizationPool . at nextFinIx)
    case finPending of
        Nothing -> return ()
        Just frs -> do
            bs <- use skovBlockStatus
            lastFinRec <- use (to lastFinalizationRecord)
            finParams <- getFinalizationParameters
            let
                Just (BlockFinalized lastFinHeight _) = bs ^. at (finalizationBlockPointer lastFinRec)
                goodFin FinalizationRecord{..} =
                    -- FIXME: verify the finalizationProof
                    finalizationIndex == nextFinIx -- Should always be true
                checkFin finRec lp = case bs ^. at (finalizationBlockPointer finRec) of
                    -- If the block is not present, the finalization record is pending
                    Nothing -> (finRec :) <$> lp
                    -- If the block is alive and the finalization proof checks out,
                    -- we can use this for finalization
                    Just (BlockAlive h) -> if goodFin finRec then Left (finRec, h) else lp
                    -- Otherwise, the finalization record is dead because the block is
                    -- either dead or already finalized
                    Just _ -> lp
            case foldr checkFin (Right []) frs of
                -- We got a valid finalization proof, so progress finalization
                Left (finRec, newFinHeight) -> do
                    skovFinalizationPool . at nextFinIx .= Nothing
                    skovFinalizationList %= (Seq.:|> finRec)
                    oldBranches <- use skovBranches
                    let pruneHeight = fromIntegral (newFinHeight - lastFinHeight)
                    let
                        pruneTrunk _ _ Seq.Empty = return ()
                        pruneTrunk keeper h (brs Seq.:|> l) = do
                            forM_ l $ \bh -> if bh == keeper then
                                                skovBlockStatus . at bh ?= BlockFinalized h finRec
                                            else
                                                skovBlockStatus . at bh ?= BlockDead
                                                -- TODO: purge dead block from skovBlockTable
                            keeper' <- blockPointer . fromJust <$> use (skovBlockTable . at keeper)
                            pruneTrunk keeper' (h-1) brs
                    let finBlock = finalizationBlockPointer finRec
                    pruneTrunk finBlock newFinHeight (Seq.take pruneHeight oldBranches)
                    -- FIXME: this is not right; should prune the branches
                    blockTable <- use skovBlockTable
                    let
                        pruneBranches [] _ = return Seq.empty
                        pruneBranches _ Seq.Empty = return Seq.empty
                        pruneBranches parents (bs Seq.:<| rest) = do
                            bs' <- foldrM (\bp l ->
                                if hasLiveParent parents bp then
                                    return (bp:l)
                                else do
                                    skovBlockStatus . at bp ?= BlockDead
                                    return l)
                                [] bs
                            rest' <- pruneBranches bs' rest
                            return (bs' Seq.<| rest')
                        hasLiveParent parents bp = case HM.lookup bp blockTable of
                            Nothing -> error "Malformed SkovData - block not found in block table"
                            Just (Block{blockPointer = parent}) -> parent `elem` parents
                    newBranches <- pruneBranches [finBlock] oldBranches
                    skovBranches .= newBranches
                    skovBranches .= Seq.drop pruneHeight oldBranches
                    -- purge pending blocks with slot numbers predating the last finalized slot
                    purgePending
                    -- handle blocks in skovBlocksAwaitingLastFinalized
                    processAwaitingLastFinalized
                    processFinalizationPool
                Right frs' -> skovFinalizationPool . at nextFinIx . non [] .= frs'
            

addBlock :: (Monad m) => Maybe BlockStatus -> Block -> BlockHash -> StateT SkovData m ()
addBlock parentStatus block bh = do
    res <- runMaybeT (tryAddBlock parentStatus block bh)
    case res of
        Nothing -> blockArriveDead bh
        Just False -> return () -- The block was not inserted
        Just True -> do
            processFinalizationPool
    when (isNothing res) $ skovBlockStatus . at bh ?= BlockDead
-- tryAddBlock :: Maybe BlockStatus -> Block -> BlockHash -> MaybeT m ()
tryAddBlock :: (Monad m) => Maybe BlockStatus -> Block -> BlockHash -> MaybeT (StateT SkovData m) Bool
tryAddBlock parentStatus block bh = do
    let parent = blockPointer block
    lfs <- use (to lastFinalizedSlot)
    -- The block must be later than the last finalized block
    guard $ lfs < blockSlot block
    case parentStatus of
        Nothing -> do
            skovPossiblyPendingTable . at parent . non [] %= (bh:)
            skovPossiblyPendingQueue %= MPQ.insert (blockSlot block) bh
            return False
        Just BlockDead -> mzero
        Just parStat -> do -- Alive or finalized
            let lf = blockLastFinalized block
            -- We should be able to get the block, since it is alive or finalized
            parentB <- fromJust <$> use (skovBlockTable . at parent)
            -- Check that the blockSlot is beyond the parent slot
            guard $ blockSlot parentB < blockSlot block
            lfStatus <- use (skovBlockStatus . at lf)
            case lfStatus of
                -- If the last finalized block is live, but not finalized yet,
                -- add this block to the queue at the appropriate point
                Just (BlockAlive lfHeight) -> do
                    skovBlocksAwaitingLastFinalized %= MPQ.insert lfHeight bh
                    return False
                -- If the last finalized block is finalized, we can proceed with validation.
                -- Together with the fact that the parent is alive, we know that the new node
                -- is a descendent of the finalized block.
                Just (BlockFinalized _ finRec) -> do
                    -- The last finalized pointer must be to the block that was actually finalized.
                    -- (Blocks can be implicitly finalized when a descendent is finalized.)
                    guard $ finalizationBlockPointer finRec == lf
                    -- We need to know that the slot numbers of the last finalized blocks are ordered.
                    -- Rather than check this directly, we'll check that their heights are ordered.
                    -- This is sufficient, because blocks will only arrive in the tree, and finalization
                    -- records accepted, if the slots are appropriately ordered.
                    Just (BlockFinalized pFinHeight pFinRec) <- use $ skovBlockStatus . at (blockLastFinalized parentB)
                    guard $ finalizationIndex finRec <= finalizationIndex pFinRec
                    bps@BirkParameters{..} <- getBirkParameters bh
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
                    let height = case parStat of
                            BlockAlive h -> h + 1
                            BlockFinalized h _ -> h + 1
                            _ -> 0 -- This case is not possible
                    skovBlockStatus . at bh ?= BlockAlive height
                    finHeight <- fromIntegral . Seq.length <$> use skovFinalizationList
                    branches <- use skovBranches
                    let branchLen = fromIntegral $ Seq.length branches
                    let heightIncreased = height - finHeight < branchLen
                    if heightIncreased then
                        skovBranches . ix (fromIntegral (height - finHeight)) %= (bh:)
                    else
                        assert (height - finHeight == branchLen)
                            skovBranches %= (Seq.|> [bh])
                    return True

                -- If the last finalized block is dead
                _ -> mzero
    

-- Tree consists of finalization list + branches
-- When adding a block that is at height in the finalization list, check if it's already there; if not, it should be dead.
-- Height 0 is the genesis block

instance Monad m => SkovMonad (StateT SkovData m) where
    {-# INLINE resolveBlock #-}
    resolveBlock bh = preuse (skovBlockTable . ix bh)
    storeBlock block0 = do
            let bh = hashBlock block0
            oldBlock <- resolveBlock bh
            when (isNothing oldBlock) $ do
                -- The block is new, so we have some work to do.
                skovBlockTable . at bh ?= block0
                let parent = blockPointer block0
                parentStatus <- use (skovBlockStatus . at parent)
                addBlock parentStatus block0 bh
            return bh

                {-
            -- --
            parentStatus <- preuse (skovBlockStatus . ix (blockPointer block))
            updateStatus parentStatus bh
            return bh
        where
            updateStatus parentStatus bh = do
                finList <- use skovFinalizationList
                let finLen = fromIntegral $ Seq.length finList
                oldStatus <- preuse (skovBlockStatus . ix bh)
                case parentStatus of
                    Just (BlockFinalized fr) -> let h = finalizationIndex fr in
                        if h+1 < finLen && finalizationBlockPointer (finList `Seq.index` fromIntegral h) /= bh then
                            arrive bh oldStatus BlockDead
                        else do
                            branches <- use skovBranches
                            let branchLen = fromIntegral $ Seq.length branches
                            if h - finLen < branchLen then
                                skovBranches . ix (fromIntegral (h - finLen)) %= (bh:)
                            else 
                                assert (h - finLen == branchLen)
                                    (skovBranches %= (Seq.|> [bh]))
                            arrive bh oldStatus (BlockAlive (h+1))
                    Just (BlockAlive h) -> do
                        branches <- use skovBranches
                        let branchLen = fromIntegral $ Seq.length branches
                        if h - finLen < branchLen then
                            skovBranches . ix (fromIntegral (h - finLen)) %= (bh:)
                        else 
                            assert (h - finLen == branchLen)
                                (skovBranches %= (Seq.|> [bh]))
                        arrive bh oldStatus (BlockAlive (h+1))
                    Just BlockDead -> arrive bh oldStatus BlockDead
                    Just (BlockAwaited l) -> skovBlockStatus . at (blockPointer block) ?= BlockAwaited (bh : l)
                    Nothing -> skovBlockStatus . at (blockPointer block) ?= BlockAwaited [bh]
            arrive bh oldStatus newStatus = do
                skovBlockStatus . at bh ?= newStatus
                case oldStatus of
                    Just (BlockAwaited l) -> mapM_ (updateStatus (Just newStatus)) l
                    _ -> return () -}
    finalizeBlock finRec = do
            let thisFinIx = finalizationIndex finRec
            nextFinIx <- FinalizationIndex . fromIntegral . Seq.length <$> use skovFinalizationList
            case compare thisFinIx nextFinIx of
                LT -> return () -- Already finalized at that index
                EQ -> do 
                     skovFinalizationPool . at thisFinIx . non [] %= (finRec:)
                     processFinalizationPool
                GT -> skovFinalizationPool . at thisFinIx . non [] %= (finRec:)
{-
            let finBp = finalizationBlockPointer finRec
            bt <- use skovBlockTable
            skovFinalizationList %= (Seq.|> finRec)
            skovBlockStatus . at finBp ?= BlockFinalized finRec
            -- Now prune the dead blocks
            (oldFirstBranches Seq.:<| oldBranches) <- use skovBranches
            forM_ [b | b <- oldFirstBranches, b /= finBp] $
                \bh -> skovBlockStatus . at bh ?= BlockDead
            newBranches <- prune bt [finBp] oldBranches
            skovBranches .= newBranches
        where
            prune bt [] _ = return Seq.empty
            prune bt _ Seq.Empty = return Seq.empty
            prune bt parents (bs Seq.:<| rest) = do
                bs' <- foldrM (\bp l ->
                    if hasLiveParent bt parents bp then
                        return (bp : l)
                    else do
                        skovBlockStatus . at bp ?= BlockDead
                        return l)
                    [] bs
                rest' <- prune bt bs' rest
                return (bs' Seq.<| rest')
            hasLiveParent bt parents bp = case HM.lookup bp bt of
                Nothing -> error "Malformed SkovData - block not found in block table"
                Just (Block{blockPointer = parent}) -> parent `elem` parents -}
    isFinalized bp = preuse (skovBlockStatus . ix bp) >>= \case
            Just (BlockFinalized _ _) -> return True
            _ -> return False
    lastFinalizedBlock = do
            (_ Seq.:|> lf) <- use skovFinalizationList
            return lf
    addValidatedBlock bp = skovValidatedBlocks %= HS.insert bp
    isValidated bp = HS.member bp <$> use skovValidatedBlocks
    genesisData = use skovGenesisData
    genesisBlockHash = use skovGenesisBlockHash
    genesisBlock = use skovGenesisBlock
    getBlockHeight bp _ = preuse (skovBlockStatus . ix bp) >>= \case
            Just (BlockFinalized h _) -> return (Just h)
            Just (BlockAlive h) -> return (Just h)
            _ -> return Nothing
    getCurrentHeight = gets $ \skov -> lastFinalizedHeight skov + fromIntegral (Seq.length (_skovBranches skov)) - 1
    getBlocksAtHeight height = gets $ \skov ->
            let finLen = fromIntegral $ Seq.length (_skovFinalizationList skov) in
            if height < finLen then
                [finalizationBlockPointer $ _skovFinalizationList skov `Seq.index` (fromIntegral height)]
            else
                case _skovBranches skov Seq.!? fromIntegral (height - finLen) of
                    Nothing -> []
                    Just l -> l

instance Monad m => PayloadMonad (StateT SkovData m) where
    addPendingTransaction tr@Transaction{..} = do
        isFin <- Map.member transactionNonce <$> use transactionsFinalized
        unless isFin $ do
            transactionsPending %= Map.insert transactionNonce tr
    getPendingTransactionsAtBlock bh = do
        pts <- use transactionsPending
        getTransactionsAtBlock bh >>= \case
            Nothing -> return Nothing
            Just bts -> return $ Just $ Map.difference pts bts
    getTransactionsAtBlock bh = do
        lfp <- finalizationBlockPointer <$> lastFinalizedBlock
        genp <- genesisBlockHash
        let getTrans bh
                | bh == lfp = use transactionsFinalized
                | bh == genp = return Map.empty
                | otherwise = do
                        block <- MaybeT . preuse $ skovBlockTable . ix bh
                        bts <- MaybeT . pure $ toTransactions (blockData block)
                        parentTrans <- getTrans (blockPointer block)
                        let upd tr@Transaction{..} s = if transactionNonce `Map.member` s then Nothing else Just (Map.insert transactionNonce tr s)
                        MaybeT $ pure $ foldrM upd parentTrans bts
        runMaybeT $ getTrans bh

instance MonadIO m => KontrolMonad (StateT SkovData m)