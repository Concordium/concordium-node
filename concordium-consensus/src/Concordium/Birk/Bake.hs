{-# LANGUAGE
    DeriveGeneric, OverloadedStrings #-}
module Concordium.Birk.Bake(
  bakeForSlot,
  BakerIdentity(..),
  bakerSignPublicKey,
  bakerElectionPublicKey) where

import GHC.Generics
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans

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
import Concordium.GlobalState.BlockState hiding (CredentialDeployment)
import Concordium.GlobalState.TreeState
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)

import Concordium.Kontrol
import Concordium.Birk.LeaderElection
import Concordium.Kontrol.BestBlock
import Concordium.Kontrol.UpdateLeaderElectionParameters

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
    -> BirkParameters
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
    maxSize <- rpBlockSize <$> getRuntimeParameters

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
    -- credentials (with high probability), so it is likely fine to
    let purgeCredential cpt cred = do
          b <- purgeTransaction (CredentialDeployment <$> cred)
          if b then return cpt
          else return $! extendPendingTransactionTable' (wmdHash cred) cpt

    newpt' <- foldM_ purgeCredential newpt (map fst ftFailedCredentials)

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


bakeForSlot :: (BlockPointerMonad m, SkovMonad m, TreeStateMonad m, MonadIO m) => BakerIdentity -> Slot -> m (Maybe (BlockPointerType m))
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
    (filteredTxs, result) <- lift (processTransactions slot bps bb lastFinal bakerId)
    logEvent Baker LLInfo $ "Baked block"
    receiveTime <- currentTime
    pb <- makePendingBlock bakerSignKey slot (bpHash bb) bakerId electionProof nonce (bpHash lastFinal) (map fst (ftAdded filteredTxs)) receiveTime
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
