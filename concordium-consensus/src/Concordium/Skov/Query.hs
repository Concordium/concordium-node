{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.Skov.Query where

import Control.Monad
import Data.Foldable
import Data.Functor
import qualified Data.List as List
import qualified Data.Sequence as Seq

import Concordium.Genesis.Data
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.TreeState
import Concordium.Skov.CatchUp.Types
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TV
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.UpdateQueues (ProtocolUpdateStatus (..))
import Concordium.Utils.InterpolationSearch

doResolveBlock :: TreeStateMonad m => BlockHash -> m (Maybe (BlockPointerType m))
{- - INLINE doResolveBlock - -}
doResolveBlock cbp =
    getBlockStatus cbp <&> \case
        Just (BlockAlive bp) -> Just bp
        Just (BlockFinalized bp _) -> Just bp
        _ -> Nothing

doIsBlockKnownAndLive :: TreeStateMonad m => BlockHash -> m Bool
doIsBlockKnownAndLive cbp =
    getRecentBlockStatus cbp <&> \case
        RecentBlock bs -> case bs of
            BlockAlive _ -> True
            BlockFinalized _ _ -> True
            _ -> False
        OldFinalized -> True
        Unknown -> False

doIsFinalized :: TreeStateMonad m => BlockHash -> m Bool
{- - INLINE doIsFinalized - -}
doIsFinalized =
    getBlockStatus >=> \case
        Just (BlockFinalized _ _) -> return True
        _ -> return False

doGetCurrentHeight :: TreeStateMonad m => m BlockHeight
{- - INLINE doGetCurrentHeight - -}
doGetCurrentHeight = do
    lfHeight <- getLastFinalizedHeight
    branchLen <- fromIntegral . Seq.length <$> getBranches
    return $ lfHeight + branchLen

doBranchesFromTop :: TreeStateMonad m => m [[BlockPointerType m]]
{- - INLINE doBranchesFromTop - -}
doBranchesFromTop = revSeqToList <$> getBranches
  where
    revSeqToList Seq.Empty = []
    revSeqToList (r Seq.:|> t) = t : revSeqToList r

doGetBlocksAtHeight :: TreeStateMonad m => BlockHeight -> m [BlockPointerType m]
{- - INLINE doGetBlocksAtHeight - -}
doGetBlocksAtHeight h = do
    lastFin <- fst <$> getLastFinalized
    case compare h (bpHeight lastFin) of
        EQ -> return [lastFin]
        GT -> do
            brs <- getBranches
            case brs Seq.!? fromIntegral (h - bpHeight lastFin - 1) of
                Nothing -> return []
                Just bs -> return bs
        LT -> do
            mb <- getFinalizedAtHeight h
            return (toList mb)

-- |Result of querying for the first finalized block in an epoch, when the query fails.
data EpochFailureResult
    = -- |There are currently no finalized blocks in the given epoch.
      FutureEpoch
    | -- |The epoch is in the past, but was empty.
      EmptyEpoch

doGetFirstFinalizedOfEpoch ::
    TreeStateMonad m =>
    Either Epoch (BlockPointerType m) ->
    m (Either EpochFailureResult (BlockPointerType m))
doGetFirstFinalizedOfEpoch epochOrBlock = do
    epochLength <- gdEpochLength <$> getGenesisData
    let blockEpoch b = fromIntegral (blockSlot b `div` epochLength)
    let targetEpoch = case epochOrBlock of
            Left epoch -> epoch
            Right block -> blockEpoch block
    -- This should be safe to call for any height up to the height of the last finalized block.
    -- Concurrent writers cannot remove finalized blocks from the persistent state, and when the
    -- state was last updated, all blocks that were considered finalized must have been finalized
    -- in the persistent state.
    let unsafeFinalizedAtHeight h =
            getFinalizedAtHeight h <&> \case
                Nothing -> error $ "Missing finalized block at height " ++ show h
                Just b -> (blockEpoch b, b)
    lastFin <- fst <$> getLastFinalized
    if targetEpoch > blockEpoch lastFin
        then return $ Left FutureEpoch
        else do
            gen <- unsafeFinalizedAtHeight 0
            let low = (0, gen)
            -- If we were given a block, use that as the upper bound, so long as it is finalized.
            let high = case epochOrBlock of
                    Right guess
                        | bpHeight guess <= bpHeight lastFin ->
                            (bpHeight guess, (blockEpoch guess, guess))
                    _ ->
                        (bpHeight lastFin, (blockEpoch lastFin, lastFin))
            res <- interpolationSearchFirstM unsafeFinalizedAtHeight targetEpoch low high
            return $! case res of
                Nothing -> Left EmptyEpoch
                Just (_, block) -> Right block

doBlockLastFinalizedIndex :: TreeStateMonad m => BlockPointerType m -> m FinalizationIndex
{- - INLINE doBlockLastFinalizedIndex - -}
doBlockLastFinalizedIndex bp =
    getBlockStatus (bpLastFinalizedHash bp) <&> \case
        Just (BlockFinalized _ fr) -> finalizationIndex fr
        _ -> error "Invariant violation: last finalized block is not finalized."

-- |Get a catch-up status message. The flag indicates if the
-- message should be a catch-up request.
doGetCatchUpStatus :: (TreeStateMonad m) => Bool -> m CatchUpStatus
doGetCatchUpStatus cusIsRequest = do
    lfb <- fst <$> getLastFinalized
    br <- toList <$> getBranches
    (leaves, branches) <- leavesBranches br
    makeCatchUpStatus cusIsRequest False lfb leaves (if cusIsRequest then branches else [])

doGetProtocolUpdateStatus :: (TreeStateMonad m) => m ProtocolUpdateStatus
doGetProtocolUpdateStatus = do
    (lastFin, _) <- getLastFinalized
    lastFinState <- blockState lastFin
    getProtocolUpdateStatus lastFinState

doIsShutDown :: (TreeStateMonad m) => m Bool
doIsShutDown = do
    status <- doGetProtocolUpdateStatus
    return $ case status of
        ProtocolUpdated _ -> True
        PendingProtocolUpdates _ -> False

-- |Construct a 'CatchUpStatus' message.
makeCatchUpStatus ::
    (BlockPointerData (BlockPointerType m), BlockPointerMonad m) =>
    -- |'True' if the message is a request
    Bool ->
    -- |'True' if the message is a response
    Bool ->
    -- |Last finalized block pointer
    BlockPointerType m ->
    -- |Leaves
    [BlockPointerType m] ->
    -- |Branches
    [BlockPointerType m] ->
    m CatchUpStatus
makeCatchUpStatus cusIsRequest cusIsResponse lfb leaves branches = return CatchUpStatus{..}
  where
    cusLastFinalizedBlock = bpHash lfb
    cusLastFinalizedHeight = bpHeight lfb
    cusLeaves = bpHash <$> leaves
    cusBranches = bpHash <$> branches

-- |Given a list of lists representing branches (ordered by height),
-- produce a pair of lists @(leaves, branches)@, which partions
-- those blocks that are leaves (@leaves@) from those that are not
-- (@branches@).
leavesBranches ::
    forall m.
    ( BlockPointerData (BlockPointerType m),
      BlockPointerMonad m
    ) =>
    [[BlockPointerType m]] ->
    m ([BlockPointerType m], [BlockPointerType m])
leavesBranches = lb ([], [])
  where
    lb :: ([BlockPointerType m], [BlockPointerType m]) -> [[BlockPointerType m]] -> m ([BlockPointerType m], [BlockPointerType m])
    lb lsbs [] = return lsbs
    lb (ls, bs) [ls'] = return (ls ++ ls', bs)
    lb (ls, bs) (s : r@(n : _)) = do
        parent <- mapM bpParent n
        let (bs', ls') = List.partition (`elem` parent) s
        lb (ls ++ ls', bs ++ bs') r

-- |Verify a transaction that was received separately from a block.
-- The return value consists of:
--
-- * A 'Bool' that is 'True' if the transaction is already in the non-finalized pool.
--
-- * The 'TV.VerificationResult' of verifying the transaction.
doVerifyTransaction :: (TreeStateMonad m, TimeMonad m) => BlockItem -> m (Bool, TV.VerificationResult)
doVerifyTransaction bi =
    getNonFinalizedTransactionVerificationResult bi >>= \case
        Nothing -> do
            gd <- getGenesisData
            lfState <- blockState . fst =<< getLastFinalized
            let verResCtx = Context lfState (gdMaxBlockEnergy gd) TV.Individual
            ts <- utcTimeToTimestamp <$> currentTime
            verRes <- runTransactionVerifierT (TV.verify ts bi) verResCtx
            return (False, verRes)
        Just verRes -> return (True, verRes)
