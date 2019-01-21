{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
module Concordium.Kontrol.VerifyBlock where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Maybe

import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
import Concordium.Payload.Check
-- import Concordium.Kontrol.Monad

-- |Verify that a block passes the basic tests for validity:
-- 1. It was baked by a valid baker
-- 2. The leadership election proof is valid
-- 3. The block nonce is valid
-- 4. The signature is valid
-- 5. The last finalized block pointer is indeed a finalized block
verifyBlock :: (SkovMonad m) => BirkParameters -> Block -> m Bool
verifyBlock bps@BirkParameters{..} block@Block{..} = if blockChecks then isFinalized blockLastFinalized else return False
    where
        blockChecks = isJust $ do
            BakerInfo{..} <- birkBaker blockBaker bps
            guard $ verifyProof
                        birkLeadershipElectionNonce
                        birkElectionDifficulty
                        blockSlot
                        bakerElectionVerifyKey
                        bakerLotteryPower
                        blockProof
            guard $ verifyBlockNonce
                        birkLeadershipElectionNonce
                        blockSlot
                        bakerElectionVerifyKey
                        blockNonce
            guard $ verifyBlockSignature bakerSignatureVerifyKey block

-- |Validate a block as part of a chain.  To pass, the block
-- must pass verifyBlock and also:
-- 1. Have a parent that is/can be validated
-- 2. Have a greater slot number than its parent
-- 3. Have a last-finalized block with slot number higher than the last-finalized block of its parent (unchecked if the parent is the genesis block)
validateBlock :: forall m. (SkovMonad m) => BlockHash -> m Bool
validateBlock = fmap isJust . runMaybeT . doVal
    where
        doVal :: BlockHash -> MaybeT m ()
        doVal bh = do
            -- If the block is already validated, then we're done
            v <- isValidated bh
            unless v $ do
                block <- MaybeT $ resolveBlock bh
                -- Validate the parent block
                doVal (blockPointer block)
                bps <- getBirkParameters bh
                guard =<< verifyBlock bps block
                parentBlock <- MaybeT $ resolveBlock (blockPointer block)
                guard $ blockSlot parentBlock < blockSlot block
                gbh <- genesisBlockHash
                unless (blockPointer block == gbh) $ do
                    blf <- MaybeT $ resolveBlock (blockLastFinalized block)
                    bplf <- MaybeT $ resolveBlock (blockLastFinalized parentBlock)
                    guard $ blockSlot bplf <= blockSlot blf
                isDataValid <- checkData block
                unless isDataValid mzero
                addValidatedBlock bh