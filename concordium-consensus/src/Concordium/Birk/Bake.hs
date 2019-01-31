{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Concordium.Birk.Bake where

import qualified Data.Map.Strict as Map

import GHC.Generics
import Control.Monad.Trans.Maybe
import Lens.Micro.Platform

import Data.Serialize

import Concordium.Types
import Concordium.Skov.Monad
import Concordium.Kontrol.Monad
import Concordium.Payload.Monad
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Payload.Transaction

data BakerIdentity = BakerIdentity {
    bakerId :: BakerId,
    bakerSignKey :: BakerSignPrivateKey,
    bakerSignPublicKey :: BakerSignVerifyKey,
    bakerElectionKey :: BakerElectionPrivateKey,
    bakerElectionPublicKey :: BakerElectionVerifyKey
} deriving (Eq, Generic)

instance Serialize BakerIdentity where

processInputs :: (PayloadMonad m) => BlockPointer -> m (Maybe BlockData)
processInputs bh = 
    fmap (fromTransactions . take 100 . map snd . Map.toList) <$> getPendingTransactionsAtBlock bh

bakeForSlot :: (KontrolMonad m, PayloadMonad m) => BakerIdentity -> Slot -> m (Maybe Block)
bakeForSlot BakerIdentity{..} slot = runMaybeT $ do
    -- TODO: Should check that the best block is not already in this slot!
    bb <- bestBlockBefore slot
    BirkParameters{..} <- getBirkParameters slot
    electionProof <- MaybeT . pure $ do
        lotteryPower <- bakerLotteryPower <$> birkBakers ^? ix bakerId
        leaderElection birkLeadershipElectionNonce birkElectionDifficulty slot bakerElectionKey lotteryPower
    let nonce = computeBlockNonce birkLeadershipElectionNonce slot bakerElectionKey
    lastFinal <- lastFinalizedBlock
    payload <- MaybeT $ processInputs bb
    let block = signBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce (bpHash lastFinal) payload
    storeBlock block
    return block
