{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Concordium.Birk.Bake where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.IO.Class

import Data.Serialize

import Concordium.Types

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.TreeState
import Concordium.Types.Transactions

import Concordium.Skov.Monad
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters

import Concordium.Skov.Update (updateFocusBlockTo)

import Concordium.Scheduler.TreeStateEnvironment(constructBlock)

import Concordium.Logger
import Concordium.TimeMonad


data BakerIdentity = BakerIdentity {
    bakerSignKey :: BakerSignPrivateKey,
    bakerElectionKey :: BakerElectionPrivateKey
} deriving (Eq, Generic)

bakerSignPublicKey :: BakerIdentity -> BakerSignVerifyKey
bakerSignPublicKey ident = Sig.verifyKey (bakerSignKey ident)

bakerElectionPublicKey :: BakerIdentity -> BakerElectionVerifyKey
bakerElectionPublicKey ident = VRF.publicKey (bakerElectionKey ident)

instance Serialize BakerIdentity where

processTransactions
    :: TreeStateMonad m
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
  constructBlock slot bh finalizedP bid ss
  -- NB: what remains is to update the focus block to the newly constructed one.
  -- This is done in the method below once a block pointer is constructed.


bakeForSlot :: (SkovMonad m, TreeStateMonad m, MonadIO m) => BakerIdentity -> Slot -> m (Maybe (BlockPointer m))
bakeForSlot ident@BakerIdentity{..} slot = runMaybeT $ do
    bb <- bestBlockBefore slot
    guard (blockSlot bb < slot)
    birkParams@BirkParameters{..} <- getBirkParameters slot bb
    (bakerId, _, lotteryPower) <- MaybeT . pure $ birkEpochBakerByKeys (bakerSignPublicKey ident) birkParams
    electionProof <- MaybeT . liftIO $
        leaderElection (_birkLeadershipElectionNonce birkParams) _birkElectionDifficulty slot bakerElectionKey lotteryPower
    logEvent Baker LLInfo $ "Won lottery in " ++ show slot ++ "(lottery power: " ++ show lotteryPower ++ ")"
    nonce <- liftIO $ computeBlockNonce (_birkLeadershipElectionNonce birkParams)    slot bakerElectionKey
    lastFinal <- lastFinalizedBlock
    -- possibly add the block nonce in the seed state
    let bps = birkParams{_birkSeedState = updateSeedState slot nonce _birkSeedState}
    (transactions, newState, energyUsed) <- processTransactions slot bps bb lastFinal bakerId
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce (bpHash lastFinal) transactions receiveTime
    newbp <- storeBakedBlock pb
                         bb
                         lastFinal
                         newState
                         energyUsed
    -- update the current focus block to the newly created block to maintain invariants.
    putFocusBlock newbp
    logEvent Baker LLInfo $ "Finished bake block " ++ show newbp
    return newbp
