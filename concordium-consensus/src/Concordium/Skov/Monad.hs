{-# LANGUAGE
    DerivingVia,
    StandaloneDeriving #-}
module Concordium.Skov.Monad(
    module Concordium.Skov.CatchUp.Types,
    module Concordium.Skov.Monad
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString (ByteString)

import Concordium.Types
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.BlockState (BlockStateQuery)
import Concordium.GlobalState.TreeState (BlockPointer, BlockPointerData, BlockState, MGSTrans(..), PendingBlock)
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.CatchUp.Types

data UpdateResult
    = ResultSuccess
    -- ^Message received, validated and processed
    | ResultSerializationFail
    -- ^Message deserialization failed
    | ResultInvalid
    -- ^The message was determined to be invalid
    | ResultPendingBlock
    -- ^The message was received, but is awaiting a block to complete processing
    | ResultPendingFinalization
    -- ^The message was received, but is awaiting a finalization record to complete processing
    | ResultAsync
    -- ^The message was received, but is being processed asynchronously
    | ResultDuplicate
    -- ^The message duplicates a previously received message
    | ResultStale
    -- ^The message may have been valid in the past, but is no longer relevant
    | ResultIncorrectFinalizationSession
    -- ^The message refers to a different/unknown finalization session
    | ResultUnverifiable
    -- ^The message could not be validated with the current state
    | ResultContinueCatchUp
    -- ^The peer should be marked as pending unless catch up is already in progress
    deriving (Eq, Show)

class (Monad m, Eq (BlockPointer m), BlockPointerData (BlockPointer m), BlockStateQuery m) => SkovQueryMonad m where
    -- |Look up a block in the table given its hash
    resolveBlock :: BlockHash -> m (Maybe (BlockPointer m))
    -- |Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointer m)
    -- |Retrieve the finalized block at a given finalization index.
    blockAtFinIndex :: FinalizationIndex -> m (Maybe (BlockPointer m))
    -- |Determine the next index for finalization.
    nextFinalizationIndex :: m FinalizationIndex
    -- |Retrieves the birk parameters for a slot, given a branch (in the form of a block pointer.)
    --  Retrieves AdvanceTime and StableTime directly from genesis block
    getBirkParameters :: Slot -> BlockPointer m -> m BirkParameters
    -- |Get the genesis data.
    getGenesisData :: m GenesisData
    -- |Get the genesis block pointer.
    genesisBlock :: m (BlockPointer m)
    -- |Get the height of the highest blocks in the tree.
    -- Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight
    -- |Get the blocks in the branches of the tree grouped by descending height.
    -- That is the first element of the list is all of the blocks at 'getCurrentHeight',
    -- the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockPointer m]]
    -- |Get a list of all the blocks at a given height in the tree.
    getBlocksAtHeight :: BlockHeight -> m [BlockPointer m]
    -- |Get a block's state.
    queryBlockState :: BlockPointer m -> m (BlockState m)
    -- |Get the finalization index of a block's last finalized block.
    blockLastFinalizedIndex :: BlockPointer m -> m FinalizationIndex
    -- |Get a catch-up status message. The flag indicates if the
    -- message should be a catch-up request.
    getCatchUpStatus :: Bool -> m CatchUpStatus

data MessageType = MessageBlock | MessageFinalizationRecord
    deriving (Eq, Show)

class (SkovQueryMonad m, TimeMonad m, LoggerMonad m) => SkovMonad m where
    -- |Parse a 'ByteString' into a 'PendingBlock'.
    deserializeBlock :: ByteString -> UTCTime -> m (Either String (PendingBlock m))
    -- |Store a block in the block table and add it to the tree
    -- if possible.
    storeBlock :: PendingBlock m -> m UpdateResult
    -- |Store a block in the block table that has just been baked.
    -- This assumes the block is valid and that there can be nothing
    -- pending for it (children or finalization).
    storeBakedBlock ::
        PendingBlock m        -- ^The block to add
        -> BlockPointer m     -- ^Parent pointer
        -> BlockPointer m     -- ^Last finalized pointer
        -> BlockState m       -- ^State
        -> Energy             -- ^Energy used by the transactions in this block
        -> m (BlockPointer m)
    -- |Add a transaction to the transaction table.
    receiveTransaction :: Transaction -> m UpdateResult
    -- |Finalize a block where the finalization record is known to be for the
    -- next finalization index and have a valid finalization proof.  This
    -- checks that the block being finalized is live.
    --  * If the block being finalized is live, it is finalized and the block pointer is returned.
    --  * If the block is already finalized or dead, 'ResultInvalid' is returned
    --  * If the block is unknown or pending, 'ResultUnverifiable' is returned.
    -- Note that this function is indended to be called by the finalization implemention,
    -- and will not call the finalization implementation itself.
    trustedFinalize :: FinalizationRecord -> m (Either UpdateResult (BlockPointer m))
    -- TODO: change signature - logging can be used instead of returning a string; could return UpdateResult
    -- receiveCatchUpStatus :: CatchUpStatus -> m (Either String (Maybe ([Either FinalizationRecord (BlockPointer m)], CatchUpStatus), Bool))
    handleCatchUpStatus :: CatchUpStatus -> m (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)

instance (Monad (t m), MonadTrans t, SkovQueryMonad m) => SkovQueryMonad (MGSTrans t m) where
    resolveBlock = lift . resolveBlock
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    blockAtFinIndex = lift . blockAtFinIndex
    nextFinalizationIndex = lift nextFinalizationIndex
    getBirkParameters slot bp = lift $ getBirkParameters slot bp
    getGenesisData = lift getGenesisData
    genesisBlock = lift genesisBlock
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop
    getBlocksAtHeight = lift . getBlocksAtHeight
    queryBlockState = lift . queryBlockState
    blockLastFinalizedIndex = lift . blockLastFinalizedIndex
    getCatchUpStatus = lift . getCatchUpStatus
    {-# INLINE resolveBlock #-}
    {-# INLINE isFinalized #-}
    {-# INLINE lastFinalizedBlock #-}
    {-# INLINE nextFinalizationIndex #-}
    {-# INLINE getBirkParameters #-}
    {-# INLINE getGenesisData #-}
    {-# INLINE genesisBlock #-}
    {-# INLINE getCurrentHeight #-}
    {-# INLINE branchesFromTop #-}
    {-# INLINE getBlocksAtHeight #-}
    {-# INLINE queryBlockState #-}
    {-# INLINE blockLastFinalizedIndex #-}
    {-# INLINE getCatchUpStatus #-}

instance (Monad (t m), MonadTrans t, SkovMonad m) => SkovMonad (MGSTrans t m) where
    deserializeBlock b t = lift $ deserializeBlock b t
    storeBlock b = lift $ storeBlock b
    storeBakedBlock pb parent lastFin state energyUsed = lift $ storeBakedBlock pb parent lastFin state energyUsed
    receiveTransaction = lift . receiveTransaction
    trustedFinalize = lift . trustedFinalize
    handleCatchUpStatus = lift . handleCatchUpStatus
    {-# INLINE deserializeBlock #-}
    {-# INLINE storeBlock #-}
    {-# INLINE storeBakedBlock #-}
    {-# INLINE receiveTransaction #-}
    {-# INLINE trustedFinalize #-}
    {-# INLINE handleCatchUpStatus #-}

deriving via (MGSTrans MaybeT m) instance SkovQueryMonad m => SkovQueryMonad (MaybeT m)
deriving via (MGSTrans MaybeT m) instance SkovMonad m => SkovMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovQueryMonad m => SkovQueryMonad (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance SkovMonad m => SkovMonad (ExceptT e m)

getGenesisTime :: (SkovQueryMonad m) => m Timestamp
getGenesisTime = genesisTime <$> getGenesisData

getFinalizationParameters :: (SkovQueryMonad m) => m FinalizationParameters
getFinalizationParameters = genesisFinalizationParameters <$> getGenesisData

getSlotTime :: (SkovQueryMonad m) => Slot -> m UTCTime
getSlotTime s = do
        genData <- getGenesisData
        return $ posixSecondsToUTCTime (fromIntegral (genesisTime genData + genesisSlotDuration genData * fromIntegral s))

receiveBlock :: (SkovMonad m) => ByteString -> m UpdateResult
receiveBlock blockBS = do
        now <- currentTime
        deserializeBlock blockBS now >>= \case
            Left err -> do
                logEvent External LLDebug err
                return ResultSerializationFail
            Right pb -> storeBlock pb

