{-# LANGUAGE
    DeriveGeneric, OverloadedStrings, UndecidableInstances, MonoLocalBinds #-}
module Concordium.Birk.Bake(
  BakerIdentity(..),
  bakerSignPublicKey,
  bakerElectionPublicKey,
  BakerMonad(..)) where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad

import Data.Serialize
import Data.Aeson(FromJSON, parseJSON, withObject, (.:))
import Data.List
import Lens.Micro.Platform

import Concordium.Types

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block hiding (PendingBlock, makePendingBlock)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState as TS
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)

import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Afgjort.Finalize
import Concordium.Skov
import Concordium.Skov.Update (updateFocusBlockTo)

import Concordium.Scheduler.TreeStateEnvironment(constructBlock, ExecutionResult)
import Concordium.Scheduler.Types(FilteredTransactions(..))

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
    -> BirkParameters m
    -> BlockPointerType m
    -> BlockPointerType m
    -> BakerId
    -> m (FilteredTransactions, ExecutionResult m)
processTransactions slot ss bh finalizedP bid = do
  -- update the focus block to the parent block (establish invariant needed by constructBlock)
  updateFocusBlockTo bh
  -- at this point we can contruct the block. The function 'constructBlock' also
  -- updates the pending table and purges any transactions deemed invalid
  slotTime <- getSlotTimestamp slot
  constructBlock slot slotTime bh finalizedP bid ss
  -- NB: what remains is to update the focus block to the newly constructed one.
  -- This is done in the method below once a block pointer is constructed.

-- |Reestablish all the invariants among the transaction table, pending table,
-- account non-finalized table
maintainTransactions ::
  (TreeStateMonad m)
  => BlockPointerType m
  -> FilteredTransactions
  -> m ()
maintainTransactions bp FilteredTransactions{..} = do
    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    let bh = getHash bp
    let slot = blockSlot bp
    zipWithM_ (\tx i -> do
                  commitTransaction slot bh (fst tx) i) ftAdded [0..]

    -- lookup the maximum block size as mandated by the tree state
    maxSize <- rpBlockSize <$> TS.getRuntimeParameters

    -- Now we need to try to purge each invalid transaction from the pending table.
    -- Moreover all transactions successfully added will be removed from the pending table.
    -- Or equivalently, only a subset of invalid transactions and all the
    -- transactions we have not touched and are small enough will remain in the
    -- pending table.
    stateHandle <- blockState bp

    let nextNonceFor addr = do
          macc <- getAccount stateHandle addr
          case macc of
            Nothing -> return minNonce
            Just acc -> return $ acc ^. accountNonce
    -- construct a new pending transaction table adding back some failed transactions.
    let purgeFailed cpt tx = do
          b <- purgeTransaction (NormalTransaction <$> tx)
          if b then return cpt  -- if the transaction was purged don't put it back into the pending table
          else do
            -- but otherwise do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedExtendPendingTransactionTable nonce tx cpt

    newpt <- foldM purgeFailed emptyPendingTransactionTable (map fst ftFailed)

    -- FIXME: Well there is a complication here. If credential deployment failed because
    -- of reuse of RegId then this could be due to somebody else deploying that credential,
    -- and therefore that is block dependent, and we should perhaps not remove the credential.
    -- However modulo crypto breaking, this can only happen if the user has tried to deploy duplicate
    -- credentials (with high probability), so it is likely fine to remove it.
    let purgeCredential cpt cred = do
          b <- purgeTransaction (CredentialDeployment <$> cred)
          if b then return cpt
          else return $! extendPendingTransactionTable' (wmdHash cred) cpt

    newpt' <- foldM purgeCredential newpt (map fst ftFailedCredentials)

    -- additionally add in the unprocessed transactions which are sufficiently small (here meaning < maxSize)
    let purgeTooBig cpt tx =
          if transactionSize tx < maxSize then do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedExtendPendingTransactionTable nonce tx cpt
          else do
            -- only purge a transaction from the table if it is too big **and**
            -- not already commited to a recent block. if it is in a currently
            -- live block then we must not purge it to maintain the invariant
            -- that all transactions in live blocks exist in the transaction
            -- table.
            b <- purgeTransaction (NormalTransaction <$> tx)
            if b then return cpt
            else do
              nonce <- nextNonceFor (transactionSender tx)
              return $! checkedExtendPendingTransactionTable nonce tx cpt

    ptWithUnprocessed <- foldM purgeTooBig newpt' ftUnprocessed

    -- And finally put back in all the unprocessed credentials
    -- We assume here that chain parameters are such that credentials always fit on a block
    -- and processing of one credential does not exceed maximum block energy.
    let ptWithUnprocessedCreds = foldl' (\cpt cdiwm -> extendPendingTransactionTable' (wmdHash cdiwm) cpt) ptWithUnprocessed ftUnprocessedCredentials

    -- commit the new pending transactions to the tree state
    putPendingTransactions ptWithUnprocessedCreds


doBakeForSlot :: (BlockPointerMonad m, FinalizationMonad m, SkovMonad m, MonadIO m, TreeStateMonad m) => BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))
doBakeForSlot ident@BakerIdentity{..} slot = runMaybeT $ do
    bb <- bestBlockBefore slot
    guard (blockSlot bb < slot)
    birkParams <- getBirkParameters slot bb
    (bakerId, _, lotteryPower) <- MaybeT $ birkEpochBakerByKeys (bakerSignPublicKey ident) birkParams
    leNonce <- birkLeadershipElectionNonce birkParams
    elDiff <- bpoElectionDifficulty birkParams
    electionProof <- MaybeT . liftIO $
        leaderElection leNonce elDiff slot bakerElectionKey lotteryPower
    logEvent Baker LLInfo $ "Won lottery in " ++ show slot ++ "(lottery power: " ++ show lotteryPower ++ ")"
    nonce <- liftIO $ computeBlockNonce leNonce slot bakerElectionKey
    nfr <- lift (nextFinalizationRecord bb)
    (lastFinal, finData) <- case nfr of
        Nothing -> (, NoFinalizationData) <$> bpLastFinalized bb
        Just finRec ->
            resolveBlock (finalizationBlockPointer finRec) >>= \case
                -- It is possible that we have a finalization proof but we
                -- don't actually have the block that was finalized.
                -- Possibly we should not even bake in this situation.
                Nothing -> (, NoFinalizationData) <$> bpLastFinalized bb
                Just finBlock -> return (finBlock, BlockFinalizationData finRec)
    -- possibly add the block nonce in the seed state
    bps <- bpoUpdateSeedState (updateSeedState slot nonce) birkParams
    (filteredTxs, result) <- lift (processTransactions slot bps bb lastFinal bakerId)
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce finData (map fst (ftAdded filteredTxs)) receiveTime
    newbp <- storeBakedBlock pb
                         bb
                         lastFinal
                         result
    -- reestablish invariants in the transaction table/pending table/anf table.
    maintainTransactions newbp filteredTxs
    -- update the current focus block to the newly created block to maintain invariants.
    putFocusBlock newbp
    logEvent Baker LLInfo $ "Finished bake block " ++ show newbp
    return newbp

class (SkovMonad m, FinalizationMonad m) => BakerMonad m where
    -- |Create a block pointer for the given slot.
    -- This function is in charge of accumulating the pending transactions and
    -- credential deployments, construct the block and update the transaction table,
    -- pending transaction table and block table. It will also update the focus block
    -- to the newly created block.
    bakeForSlot :: BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))

instance (FinalizationMonad (SkovT h c m), MonadIO m, TreeStateMonad (SkovT h c m), SkovMonad (SkovT h c m)) =>
        BakerMonad (SkovT h c m) where
    bakeForSlot = doBakeForSlot
