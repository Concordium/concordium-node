{-# LANGUAGE LambdaCase #-}
module Concordium.Skov.Monad where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

import Concordium.Types

class Monad m => SkovMonad m where
    -- |Look up a block in the table given its hash
    resolveBlock :: BlockHash -> m (Maybe Block)
    -- |Store a block in the block table and add it to the tree
    -- if possible.
    storeBlock :: Block -> m BlockHash
    -- |Finalize a block that is in the block table and a child
    -- of the last finalized block.  These properties may not be
    -- checked.
    finalizeBlock :: FinalizationRecord -> m ()
    -- |Determine if a block has been finalized
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block
    lastFinalizedBlock :: m FinalizationRecord
    genesisData :: m GenesisData
    genesisBlockHash :: m BlockHash
    genesisBlock :: m Block
    genesisBlock = genesisBlockHash >>= (fmap fromJust . resolveBlock)
    -- |Height of a block in its chain.  If the chain cannot be
    -- traced to the genesis block, this returns 'Nothing'.
    getBlockHeight :: BlockHash -> Block -> m (Maybe BlockHeight)
    getBlockHeight = gcl 0
        where
            gcl n bh b = do
                gbh <- genesisBlockHash
                if bh == gbh then
                    return (Just n)
                else
                    resolveBlock (blockPointer b) >>= \case
                        Nothing -> return Nothing
                        Just b' -> gcl (n+1) (blockPointer b) b'
    -- |Get the height of the highest blocks in the tree.
    -- Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight
    -- |Get the blocks in the branches of the tree grouped by descending height.
    -- That is the first element of the list is all of the blocks at 'getCurrentHeight',
    -- the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockHash]]

instance SkovMonad m => SkovMonad (MaybeT m) where
    resolveBlock = lift . resolveBlock
    storeBlock = lift . storeBlock
    finalizeBlock = lift . finalizeBlock
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    genesisData = lift genesisData
    genesisBlockHash = lift genesisBlockHash
    genesisBlock = lift genesisBlock
    getBlockHeight bh b = lift (getBlockHeight bh b)
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop

getBirkParameters :: (SkovMonad m) => BlockHash -> m BirkParameters
getBirkParameters _ = genesisBirkParameters <$> genesisData

getGenesisTime :: (SkovMonad m) => m Timestamp
getGenesisTime = genesisTime <$> genesisData

getFinalizationParameters :: (SkovMonad m) => m FinalizationParameters
getFinalizationParameters = genesisFinalizationParameters <$> genesisData
