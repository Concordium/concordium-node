{-# LANGUAGE
    DeriveGeneric, OverloadedStrings, UndecidableInstances, MonoLocalBinds #-}
module Concordium.Birk.Bake where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad

import Data.Serialize
import Data.Aeson(FromJSON, parseJSON, withObject, (.:))

import Concordium.Types

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState
import Concordium.Types.Transactions

import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Afgjort.Finalize
import Concordium.Skov
import Concordium.Skov.Update (updateFocusBlockTo)

import Concordium.Scheduler.TreeStateEnvironment(constructBlock)

import Concordium.Logger
import Concordium.TimeMonad


data BakerIdentity = BakerIdentity {
    bakerSignKey :: BakerSignPrivateKey,
    bakerElectionKey :: BakerElectionPrivateKey,
    bakerAggregationKey :: BakerAggregationPrivateKey
} deriving (Eq, Generic)

bakerSignPublicKey :: BakerIdentity -> BakerSignVerifyKey
bakerSignPublicKey ident = Sig.verifyKey (bakerSignKey ident)

bakerElectionPublicKey :: BakerIdentity -> BakerElectionVerifyKey
bakerElectionPublicKey ident = VRF.publicKey (bakerElectionKey ident)

instance Serialize BakerIdentity where

instance FromJSON BakerIdentity where
  parseJSON v = flip (withObject "Baker identity:") v $ \obj -> do
    bakerSignKey <- parseJSON v
    bakerElectionKey <- parseJSON v
    bakerAggregationKey <- obj .: "aggregationSignKey"
    return BakerIdentity{..}

processTransactions
    :: (TreeStateMonad m,
        SkovMonad m)
    => Slot
    -> BirkParameters
    -> BlockPointer m
    -> BlockPointer m
    -> BakerId
    -> m ([Transaction], BlockState m, Energy)
processTransactions slot ss bh finalizedP bid = do
  -- update the focus block to the parent block (establish invariant needed by constructBlock)
  updateFocusBlockTo bh
  -- at this point we can contruct the block. The function 'constructBlock' also
  -- updates the pending table and purges any transactions deemed invalid
  slotTime <- getSlotTimestamp slot
  constructBlock slot slotTime bh finalizedP bid ss
  -- NB: what remains is to update the focus block to the newly constructed one.
  -- This is done in the method below once a block pointer is constructed.


doBakeForSlot :: (FinalizationMonad m, SkovMonad m, MonadIO m, TreeStateMonad m) => BakerIdentity -> Slot -> m (Maybe (BlockPointer m))
doBakeForSlot ident@BakerIdentity{..} slot = runMaybeT $ do
    bb <- bestBlockBefore slot
    guard (blockSlot bb < slot)
    birkParams@BirkParameters{..} <- getBirkParameters slot bb
    (bakerId, _, lotteryPower) <- MaybeT . pure $ birkEpochBakerByKeys (bakerSignPublicKey ident) birkParams
    electionProof <- MaybeT . liftIO $
        leaderElection (_birkLeadershipElectionNonce birkParams) _birkElectionDifficulty slot bakerElectionKey lotteryPower
    logEvent Baker LLInfo $ "Won lottery in " ++ show slot ++ "(lottery power: " ++ show lotteryPower ++ ")"
    nonce <- liftIO $ computeBlockNonce (_birkLeadershipElectionNonce birkParams)    slot bakerElectionKey
    nfr <- lift (nextFinalizationRecord bb)
    (lastFinal, finData) <- case nfr of
        Nothing -> return (bpLastFinalized bb, NoFinalizationData)
        Just finRec ->
            resolveBlock (finalizationBlockPointer finRec) >>= \case
                -- It is possible that we have a finalization proof but we
                -- don't actually have the block that was finalized.
                -- Possibly we should not even bake in this situation.
                Nothing -> return (bpLastFinalized bb, NoFinalizationData)
                Just finBlock -> return (finBlock, BlockFinalizationData finRec)
    -- possibly add the block nonce in the seed state
    let bps = birkParams{_birkSeedState = updateSeedState slot nonce _birkSeedState}
    (transactions, newState, energyUsed) <- processTransactions slot bps bb lastFinal bakerId
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce finData transactions receiveTime
    newbp <- storeBakedBlock pb
                         bb
                         lastFinal
                         newState
                         energyUsed
    -- update the current focus block to the newly created block to maintain invariants.
    putFocusBlock newbp
    logEvent Baker LLInfo $ "Finished bake block " ++ show newbp
    return newbp

class (SkovMonad m, FinalizationMonad m) => BakerMonad m where
    bakeForSlot :: BakerIdentity -> Slot -> m (Maybe (BlockPointer m))

instance (FinalizationMonad (SkovT h c m), MonadIO m, TreeStateMonad (SkovT h c m), SkovMonad (SkovT h c m)) =>
        BakerMonad (SkovT h c m) where
    bakeForSlot = doBakeForSlot