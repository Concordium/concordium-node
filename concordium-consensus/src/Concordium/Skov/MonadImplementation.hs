{-# LANGUAGE FlexibleInstances, FlexibleContexts, TemplateHaskell, LambdaCase, RecordWildCards, ViewPatterns #-}
module Concordium.Skov.MonadImplementation where

import Control.Monad.Trans.State
import Control.Exception(assert)
import Lens.Micro.Platform
import Data.Foldable

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq


import Concordium.Types
import Concordium.Skov.Monad

data BlockStatus =
    BlockAlive !BlockHeight
    | BlockDead
    | BlockAwaited [BlockHash]
    | BlockFinalized !FinalizationRecord

data SkovData = SkovData {
    _skovBlockTable :: HM.HashMap BlockHash Block,
    _skovFinalizationList :: Seq.Seq FinalizationRecord,
    _skovBranches :: Seq.Seq [BlockHash],
    _skovValidatedBlocks :: HS.HashSet BlockHash,
    _skovGenesisData :: GenesisData,
    _skovGenesisBlock :: Block,
    _skovGenesisBlockHash :: BlockHash,
    _skovBlockStatus :: HM.HashMap BlockHash BlockStatus
}
makeLenses ''SkovData

initialSkovData :: GenesisData -> SkovData
initialSkovData gd = SkovData {
            _skovBlockTable = HM.singleton gbh gb,
            _skovFinalizationList = Seq.singleton gbfin,
            _skovBranches = Seq.empty,
            _skovValidatedBlocks = HS.singleton gbh,
            _skovGenesisData = gd,
            _skovGenesisBlock = gb,
            _skovGenesisBlockHash = gbh,
            _skovBlockStatus = HM.singleton gbh (BlockFinalized gbfin)
        }
    where
        gb = makeGenesisBlock gd
        gbh = hashBlock gb
        gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0


-- Tree consists of finalization list + branches
-- When adding a block that is at height in the finalization list, check if it's already there; if not, it should be dead.
-- Height 0 is the genesis block


instance Monad m => SkovMonad (StateT SkovData m) where
    resolveBlock bh = preuse (skovBlockTable . ix bh)
    storeBlock block = do
            let bh = hashBlock block
            skovBlockTable . at bh ?= block
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
                    _ -> return ()
    finalizeBlock finRec = do
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
                Just (Block{blockPointer = parent}) -> parent `elem` parents
    isFinalized bp = preuse (skovBlockStatus . ix bp) >>= \case
            Just (BlockFinalized _) -> return True
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
            Just (BlockFinalized fr) -> return $ Just (finalizationIndex fr)
            Just (BlockAlive h) -> return (Just h)
            _ -> return Nothing
    getCurrentHeight = gets $ \skov -> fromIntegral $ Seq.length (_skovFinalizationList skov) + Seq.length (_skovBranches skov) - 1
    getBlocksAtHeight height = gets $ \skov ->
            let finLen = fromIntegral $ Seq.length (_skovFinalizationList skov) in
            if height < finLen then
                [finalizationBlockPointer $ _skovFinalizationList skov `Seq.index` (fromIntegral height)]
            else
                case _skovBranches skov Seq.!? fromIntegral (height - finLen) of
                    Nothing -> []
                    Just l -> l



                        