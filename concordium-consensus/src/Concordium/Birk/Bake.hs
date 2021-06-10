{-# LANGUAGE
    DeriveGeneric, OverloadedStrings, UndecidableInstances, MonoLocalBinds, ScopedTypeVariables #-}
module Concordium.Birk.Bake(
  BakerIdentity(..),
  bakerSignPublicKey,
  bakerElectionPublicKey,
  validateBakerKeys,
  BakerMonad(..)) where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Monad.Trans
import Control.Monad

import Data.Serialize
import Data.Aeson(FromJSON, parseJSON, withObject, (.:))
import Data.List (foldl')

import Concordium.Types

import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as BLS
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block hiding (PendingBlock, makePendingBlock)
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.TreeState as TS
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.Types.SeedState

import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters
import Concordium.Afgjort.Finalize
import Concordium.Skov
import Concordium.Skov.Update (blockArrive, onBlock, updateFocusBlockTo, OnSkov, makeFinalizerInfo)

import Concordium.Scheduler.TreeStateEnvironment(constructBlock, ExecutionResult, ExecutionResult'(..), FinalizerInfo)
import Concordium.Scheduler.Types(FilteredTransactions(..))

import Concordium.Logger
import Concordium.TimeMonad


data BakerIdentity = BakerIdentity {
    bakerId :: BakerId,
    bakerSignKey :: BakerSignPrivateKey,
    bakerElectionKey :: BakerElectionPrivateKey,
    bakerAggregationKey :: BakerAggregationPrivateKey,
    bakerAggregationPublicKey :: BakerAggregationVerifyKey
} deriving (Eq, Generic)

bakerSignPublicKey :: BakerIdentity -> BakerSignVerifyKey
bakerSignPublicKey ident = Sig.verifyKey (bakerSignKey ident)

bakerElectionPublicKey :: BakerIdentity -> BakerElectionVerifyKey
bakerElectionPublicKey ident = VRF.publicKey (bakerElectionKey ident)

instance Serialize BakerIdentity where

instance FromJSON BakerIdentity where
  parseJSON v = flip (withObject "Baker identity:") v $ \obj -> do
    bakerId <- obj .: "bakerId"
    bakerSignKey <- parseJSON v
    bakerElectionKey <- parseJSON v
    bakerAggregationKey <- obj .: "aggregationSignKey"
    bakerAggregationPublicKey <- obj .: "aggregationVerifyKey"
    when (bakerAggregationPublicKey /= BLS.derivePublicKey bakerAggregationKey) $
        fail "Aggregation signing key does not correspond to the verification key."
    return BakerIdentity{..}

processTransactions
    :: (TreeStateMonad pv m,
        SkovMonad pv m)
    => Slot
    -> SeedState
    -> BlockPointerType m
    -> Maybe FinalizerInfo
    -> BakerId
    -> m (FilteredTransactions, ExecutionResult m)
processTransactions slot ss bh mfinInfo bid = do
  -- update the focus block to the parent block (establish invariant needed by constructBlock)
  updateFocusBlockTo bh
  -- at this point we can construct the block. The function 'constructBlock' also
  -- updates the pending table and purges any transactions deemed invalid
  slotTime <- getSlotTimestamp slot
  constructBlock slot slotTime bh bid mfinInfo ss
  -- NB: what remains is to update the focus block to the newly constructed one.
  -- This is done in the method below once a block pointer is constructed.

-- |Re-establish all the invariants among the transaction table, pending table,
-- account non-finalized table
maintainTransactions ::
  (TreeStateMonad pv m)
  => BlockPointerType m
  -> FilteredTransactions
  -> m ()
maintainTransactions bp FilteredTransactions{..} = do
    -- We first commit all valid transactions to the current block slot to prevent them being purged.
    let bh = getHash bp
    let slot = blockSlot bp
    zipWithM_ (\tx -> commitTransaction slot bh (fst tx)) ftAdded [0..]

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
            Just (_, acc) -> getAccountNonce acc
    -- construct a new pending transaction table adding back some failed transactions.
    let purgeFailed cpt tx = do
          b <- purgeTransaction (normalTransaction tx)
          if b then return cpt  -- if the transaction was purged don't put it back into the pending table
          else do
            -- but otherwise do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedAddPendingTransaction nonce tx cpt

    newpt <- foldM purgeFailed emptyPendingTransactionTable (map fst ftFailed)

    -- FIXME: Well there is a complication here. If credential deployment failed because
    -- of reuse of RegId then this could be due to somebody else deploying that credential,
    -- and therefore that is block dependent, and we should perhaps not remove the credential.
    -- However modulo crypto breaking, this can only happen if the user has tried to deploy duplicate
    -- credentials (with high probability), so it is likely fine to remove it.
    let purgeCredential cpt cred = do
          b <- purgeTransaction (credentialDeployment cred)
          if b then return cpt
          else return $! addPendingDeployCredential (wmdHash cred) cpt

    newpt' <- foldM purgeCredential newpt (map fst ftFailedCredentials)

    let purgeUpdate cpt ui = do
          b <- purgeTransaction (chainUpdate ui)
          if b then return cpt
          else do
            sn <- getNextUpdateSequenceNumber stateHandle (updateType (uiPayload (wmdData ui)))
            return $! checkedAddPendingUpdate sn (wmdData ui) cpt
    newpt'' <- foldM purgeUpdate newpt' (map fst ftFailedUpdates)

    -- additionally add in the unprocessed transactions which are sufficiently small (here meaning < maxSize)
    let purgeTooBig cpt tx =
          if transactionSize tx < maxSize then do
            nonce <- nextNonceFor (transactionSender tx)
            return $! checkedAddPendingTransaction nonce tx cpt
          else do
            -- only purge a transaction from the table if it is too big **and**
            -- not already committed to a recent block. if it is in a currently
            -- live block then we must not purge it to maintain the invariant
            -- that all transactions in live blocks exist in the transaction
            -- table.
            b <- purgeTransaction (normalTransaction tx)
            if b then return cpt
            else do
              nonce <- nextNonceFor (transactionSender tx)
              return $! checkedAddPendingTransaction nonce tx cpt

    ptWithUnprocessed <- foldM purgeTooBig newpt'' ftUnprocessed

    -- Put back in all the unprocessed credentials
    -- We assume here that chain parameters are such that credentials always fit on a block
    -- and processing of one credential does not exceed maximum block energy.
    let ptWithUnprocessedCreds = foldl' (\cpt cdiwm -> addPendingDeployCredential (wmdHash cdiwm) cpt) ptWithUnprocessed ftUnprocessedCredentials

    let purgeUnprocessedUpdates cpt ui =
          if wmdSize ui < maxSize then do
            sn <- getNextUpdateSequenceNumber stateHandle (updateType (uiPayload (wmdData ui)))
            return $! checkedAddPendingUpdate sn (wmdData ui) cpt
          else purgeUpdate cpt ui
    ptWithAllUnprocessed <- foldM purgeUnprocessedUpdates ptWithUnprocessedCreds ftUnprocessedUpdates 

    -- commit the new pending transactions to the tree state
    putPendingTransactions ptWithAllUnprocessed

-- |Check that a baker's keys match the 'BakerInfo'.
validateBakerKeys :: BakerInfo -> BakerIdentity -> Bool
validateBakerKeys BakerInfo{..} ident =
  _bakerElectionVerifyKey == bakerElectionPublicKey ident
  && _bakerSignatureVerifyKey == bakerSignPublicKey ident
  && _bakerAggregationVerifyKey == bakerAggregationPublicKey ident

doBakeForSlot :: forall pv m. (FinalizationMonad m, SkovMonad pv m, TreeStateMonad pv m, MonadIO m, OnSkov m) => BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))
doBakeForSlot ident@BakerIdentity{..} slot = runMaybeT $ do
    -- Do not bake if consensus is shut down
    shutdown <- isShutDown
    guard (not shutdown)
    bb <- bestBlockBefore slot
    guard (blockSlot bb < slot)
    bbState <- blockState bb
    bakers <- getSlotBakers bbState slot
    (binfo, lotteryPower) <- MaybeT . return $ lotteryBaker bakers bakerId
    unless (validateBakerKeys binfo ident) $ do
      logEvent Baker LLWarning "Baker keys are incorrect."
      let logMismatch :: (Eq a, Show a) => String -> a -> a -> MaybeT m ()
          logMismatch desc x y = unless (x == y) $ logEvent Baker LLTrace $ desc ++ " mismatch. Expected: " ++ show x ++ "; actual: " ++ show y
      logMismatch "Election verify key" (_bakerElectionVerifyKey binfo) (bakerElectionPublicKey ident)
      logMismatch "Signature verify key" (_bakerSignatureVerifyKey binfo) (bakerSignPublicKey ident)
      logMismatch "Aggregate signature verify key" (_bakerAggregationVerifyKey binfo) bakerAggregationPublicKey
      fail "Baker keys are incorrect."
    oldSeedState <- getSeedState bbState
    let leNonce = computeLeadershipElectionNonce oldSeedState slot
    slotTime <- getSlotTimestamp slot
    elDiff <- getElectionDifficulty bbState slotTime
    electionProof <- MaybeT . liftIO $
        leaderElection leNonce elDiff slot bakerElectionKey lotteryPower
    logEvent Baker LLInfo $ "Won lottery in " ++ show slot ++ "(lottery power: " ++ show lotteryPower ++ "; election difficulty: " ++ show elDiff ++ ")"
    let nonce = computeBlockNonce leNonce slot bakerElectionKey
    nfr <- lift (nextFinalizationRecord bb)
    (lastFinal, mfinInfo, finData) <- case nfr of
        Nothing -> (, Nothing, NoFinalizationData) <$> bpLastFinalized bb
        Just (_, finCom, finRec) ->
            resolveBlock (finalizationBlockPointer finRec) >>= \case
                -- It is possible that we have a finalization proof but we
                -- don't actually have the block that was finalized.
                -- Possibly we should not even bake in this situation.
                Nothing -> (, Nothing, NoFinalizationData) <$> bpLastFinalized bb
                Just finBlock -> return (finBlock, Just (makeFinalizerInfo finCom), BlockFinalizationData finRec)
    -- possibly add the block nonce in the seed state
    let newSeedState = updateSeedState slot nonce oldSeedState
    -- Results = {_energyUsed, _finalState, _transactionLog}
    (filteredTxs, result) <- lift (processTransactions slot newSeedState bb mfinInfo bakerId)
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    transactionOutcomesHash <- getTransactionOutcomesHash (_finalState result)
    stateHash <- getStateHash (_finalState result)
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce finData (map fst (ftAdded filteredTxs)) stateHash transactionOutcomesHash receiveTime
    -- Add the baked block to the tree.
    newbp <- blockArrive pb bb lastFinal result
    -- re-establish invariants in the transaction table/pending table/anf table.
    maintainTransactions newbp filteredTxs
    -- update the current focus block to the newly created block to maintain invariants.
    putFocusBlock newbp
    logEvent Baker LLInfo $ "Finished bake block " ++ show newbp
    -- notify the finalization routine after the invariants are re-established.
    lift (finalizationBlockArrival newbp)
    -- notify of the baked block.
    lift (onBlock newbp)

    return newbp

class (SkovMonad pv m, FinalizationMonad m) => BakerMonad pv m where
    -- |Create a block pointer for the given slot.
    -- This function is in charge of accumulating the pending transactions and
    -- credential deployments, construct the block and update the transaction table,
    -- pending transaction table and block table. It will also update the focus block
    -- to the newly created block.
    bakeForSlot :: BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))

instance (FinalizationMonad (SkovT h c m), MonadIO m, SkovMonad pv (SkovT h c m), TreeStateMonad pv (SkovT h c m), OnSkov (SkovT h c m)) =>
        BakerMonad pv (SkovT h c m) where
    bakeForSlot = doBakeForSlot
