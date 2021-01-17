{-# LANGUAGE
    DerivingVia,
    StandaloneDeriving,
    RecordWildCards,
    ScopedTypeVariables,
    GeneralizedNewtypeDeriving,
    UndecidableInstances,
    DefaultSignatures #-}
module Concordium.Skov.Monad(
    module Concordium.Skov.CatchUp.Types,
    module Concordium.Skov.Monad
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.Time
import Data.Set(toList)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class

import Concordium.Skov.Query

import Concordium.Types
import Concordium.GlobalState.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Types.Transactions
import Concordium.GlobalState.Block as B
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.BlockState (BlockStateQuery, AccountOperations, BlockStateStorage, BlockStateOperations)
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Classes as C
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.CatchUp.Types
import qualified Concordium.GlobalState.TreeState as TS

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
    | ResultEarlyBlock
    -- ^The block was sent too early and should be dropped
    | ResultMissingImportFile
    -- ^The file provided for importing blocks is missing
    deriving (Eq, Show)

class (Monad m, Eq (BlockPointerType m), BlockPointerData (BlockPointerType m), BlockStateQuery m) => SkovQueryMonad m where
    -- |Look up a block in the table given its hash
    resolveBlock :: BlockHash -> m (Maybe (BlockPointerType m))
    -- |Determine if a block has been finalized.
    isFinalized :: BlockHash -> m Bool
    -- |Determine the last finalized block.
    lastFinalizedBlock :: m (BlockPointerType m)
    -- |Determine the last finalized block and return it together with the finalization record
    -- that finalizes it..
    lastFinalizedBlockWithRecord :: m (BlockPointerType m, FinalizationRecord)
    -- |Retrieve the finalized block at a given finalization index, if any.
    blockAtFinIndex :: FinalizationIndex -> m (Maybe (BlockPointerType m))
    -- |Retrieve the finalization record at a given finalization index, if any.
    recordAtFinIndex :: FinalizationIndex -> m (Maybe FinalizationRecord)
    -- |Determine the next index for finalization.
    nextFinalizationIndex :: m FinalizationIndex
    -- |Get the genesis data.
    getGenesisData :: m GenesisData
    -- |Get the genesis block pointer.
    genesisBlock :: m (BlockPointerType m)
    -- |Get the height of the highest blocks in the tree.
    -- Note: the genesis block has height 0
    getCurrentHeight :: m BlockHeight
    -- |Get the blocks in the branches of the tree grouped by descending height.
    -- That is the first element of the list is all of the blocks at 'getCurrentHeight',
    -- the next is those at @getCurrentHeight - 1@, etc.
    branchesFromTop :: m [[BlockPointerType m]]
    -- |Get a list of all the blocks at a given height in the tree.
    getBlocksAtHeight :: BlockHeight -> m [BlockPointerType m]
    -- |Get a block's state.
    queryBlockState :: BlockPointerType m -> m (BlockState m)
    -- |Get the outcomes of a transaction.
    queryTransactionStatus :: TransactionHash -> m (Maybe TransactionStatus)
    -- |Get non-finalized transactions for an account, ordered by increasing nonce.
    queryNonFinalizedTransactions :: AccountAddress -> m [TransactionHash]
    -- |Get best guess for next account nonce.
    -- The second argument is 'True' if and only if all transactions from this account are finalized.
    queryNextAccountNonce :: AccountAddress -> m (Nonce, Bool)
    -- |Get the finalization index of a block's last finalized block.
    blockLastFinalizedIndex :: BlockPointerType m -> m FinalizationIndex
    -- |Get a catch-up status message. The flag indicates if the
    -- message should be a catch-up request.
    getCatchUpStatus :: Bool -> m CatchUpStatus

    getRuntimeParameters :: m RuntimeParameters
    default getRuntimeParameters :: (TS.TreeStateMonad m) => m RuntimeParameters
    getRuntimeParameters = TS.getRuntimeParameters

data MessageType = MessageBlock | MessageFinalizationRecord
    deriving (Eq, Show)

class (SkovQueryMonad m, TimeMonad m, MonadLogger m) => SkovMonad m where
    -- |Store a block in the block table and add it to the tree
    -- if possible.
    storeBlock :: PendingBlock -> m UpdateResult
    -- |Add a transaction to the transaction table.
    receiveTransaction :: BlockItem -> m UpdateResult
    -- |Finalize a block where the finalization record is known to be for the
    -- next finalization index and have a valid finalization proof.  This
    -- checks that the block being finalized is live.
    --  * If the block being finalized is live, it is finalized and the block pointer is returned.
    --  * If the block is already finalized or dead, 'ResultInvalid' is returned
    --  * If the block is unknown or pending, 'ResultUnverifiable' is returned.
    -- Note that this function is indended to be called by the finalization implemention,
    -- and will not call the finalization implementation itself.
    trustedFinalize :: FinalizationRecord -> m (Either UpdateResult (BlockPointerType m))
    -- |Handle a catch-up status message.
    handleCatchUpStatus :: CatchUpStatus -> Int -> m (Maybe ([(MessageType, ByteString)], CatchUpStatus), UpdateResult)


instance (Monad (t m), MonadTrans t, SkovQueryMonad m) => SkovQueryMonad (MGSTrans t m) where
    resolveBlock = lift . resolveBlock
    isFinalized = lift . isFinalized
    lastFinalizedBlock = lift lastFinalizedBlock
    lastFinalizedBlockWithRecord = lift lastFinalizedBlockWithRecord
    blockAtFinIndex = lift . blockAtFinIndex
    recordAtFinIndex = lift . recordAtFinIndex
    nextFinalizationIndex = lift nextFinalizationIndex
    getGenesisData = lift getGenesisData
    genesisBlock = lift genesisBlock
    getCurrentHeight = lift getCurrentHeight
    branchesFromTop = lift branchesFromTop
    getBlocksAtHeight = lift . getBlocksAtHeight
    queryBlockState = lift . queryBlockState
    queryTransactionStatus = lift . queryTransactionStatus
    queryNonFinalizedTransactions = lift . queryNonFinalizedTransactions
    queryNextAccountNonce = lift . queryNextAccountNonce
    blockLastFinalizedIndex = lift . blockLastFinalizedIndex
    getCatchUpStatus = lift . getCatchUpStatus
    getRuntimeParameters = lift getRuntimeParameters
    {- - INLINE resolveBlock - -}
    {- - INLINE isFinalized - -}
    {- - INLINE lastFinalizedBlock - -}
    {- - INLINE lastFinalizedBlockWithRecord - -}
    {- - INLINE blockAtFinIndex - -}
    {- - INLINE recordAtFinIndex - -}
    {- - INLINE getBirkParameters - -}
    {- - INLINE getGenesisData - -}
    {- - INLINE genesisBlock - -}
    {- - INLINE getCurrentHeight - -}
    {- - INLINE branchesFromTop - -}
    {- - INLINE getBlocksAtHeight - -}
    {- - INLINE queryBlockState - -}
    {- - INLINE queryTransactionStatus - -}
    {- - INLINE queryNonFinalizedTransactions - -}
    {- - INLINE queryNextAccountNonce - -}
    {- - INLINE blockLastFinalizedIndex - -}
    {- - INLINE getCatchUpStatus - -}
    {- - INLINE getRuntimeParameters - -}

deriving via (MGSTrans MaybeT m) instance SkovQueryMonad m => SkovQueryMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovQueryMonad m => SkovQueryMonad (ExceptT e m)

instance (MonadLogger (t m), MonadTrans t, SkovMonad m) => SkovMonad (MGSTrans t m) where
    storeBlock b = lift $ storeBlock b
    receiveTransaction = lift . receiveTransaction
    trustedFinalize = lift . trustedFinalize
    handleCatchUpStatus peerCUS = lift . handleCatchUpStatus peerCUS
    {- - INLINE storeBlock - -}
    {- - INLINE receiveTransaction - -}
    {- - INLINE trustedFinalize - -}
    {- - INLINE handleCatchUpStatus - -}

deriving via (MGSTrans MaybeT m) instance SkovMonad m => SkovMonad (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance SkovMonad m => SkovMonad (ExceptT e m)

getGenesisTime :: (SkovQueryMonad m) => m Timestamp
getGenesisTime = genesisTime <$> getGenesisData

getFinalizationParameters :: (SkovQueryMonad m) => m FinalizationParameters
getFinalizationParameters = genesisFinalizationParameters <$> getGenesisData

getSlotTime :: (SkovQueryMonad m) => Slot -> m UTCTime
getSlotTime s = do
        genData <- getGenesisData
        return $ posixSecondsToUTCTime $ 0.001 * (fromIntegral (tsMillis $ genesisTime genData) + fromIntegral (durationMillis $ genesisSlotDuration genData) * fromIntegral s)

-- * Generic instance of SkovQueryMonad based on a TreeStateMonad.

newtype SkovQueryMonadT m a = SkovQueryMonadT { runSkovQueryMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans SkovQueryMonadT where
  {- - INLINE lift - -}
  lift = SkovQueryMonadT

deriving via (MGSTrans SkovQueryMonadT m) instance ATITypes m => ATITypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance GlobalStateTypes m => GlobalStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateTypes (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance AccountOperations m => AccountOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateQuery m => BlockStateQuery (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockPointerMonad m => BlockPointerMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance TS.TreeStateMonad m => TS.TreeStateMonad (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateStorage m => BlockStateStorage (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance PerAccountDBOperations m => PerAccountDBOperations (SkovQueryMonadT m)
deriving via (MGSTrans SkovQueryMonadT m) instance BlockStateOperations m => BlockStateOperations (SkovQueryMonadT m)

instance (Monad m,
          BlockStateQuery m,
          TS.TreeStateMonad m)
          => SkovQueryMonad (SkovQueryMonadT m) where
    {- - INLINE resolveBlock - -}
    resolveBlock = lift . doResolveBlock
    {- - INLINE isFinalized - -}
    isFinalized = lift . doIsFinalized
    {- - INLINE blockAtFinIndex - -}
    blockAtFinIndex = lift . TS.getFinalizedAtIndex
    {- - INLINE recordAtFinIndex - -}
    recordAtFinIndex = lift . TS.getRecordAtIndex
    {- - INLINE lastFinalizedBlock - -}
    lastFinalizedBlock = lift (fst <$> TS.getLastFinalized)
    {- - INLINE lastFinalizedBlockWithRecord - -}
    lastFinalizedBlockWithRecord = lift TS.getLastFinalized
    {- - INLINE nextFinalizationIndex - -}
    nextFinalizationIndex = lift TS.getNextFinalizationIndex
    {- - INLINE getGenesisData - -}
    getGenesisData = lift TS.getGenesisData
    {- - INLINE genesisBlock - -}
    genesisBlock = lift TS.getGenesisBlockPointer
    {- - INLINE getCurrentHeight - -}
    getCurrentHeight = lift doGetCurrentHeight
    {- - INLINE branchesFromTop - -}
    branchesFromTop = lift doBranchesFromTop
    {- - INLINE getBlocksAtHeight - -}
    getBlocksAtHeight = lift . doGetBlocksAtHeight
    {- - INLINE queryBlockState - -}
    queryBlockState = lift . blockState
    {- - INLINE blockLastFinalizedIndex - -}
    blockLastFinalizedIndex = lift . doBlockLastFinalizedIndex
    {- - INLINE getCatchUpStatus - -}
    getCatchUpStatus = doGetCatchUpStatus
    {- - INLINE queryTransactionStatus - -}
    queryTransactionStatus = lift . TS.lookupTransaction
    {- - INLINE queryNonFinalizedTransactions - -}
    queryNonFinalizedTransactions addr = lift $ do
      txs <- TS.getAccountNonFinalized addr minNonce
      return $! map getHash . concatMap (toList . snd) $ txs

    {- - INLINE queryNextAccountNonce - -}
    queryNextAccountNonce = lift . TS.getNextAccountNonce

deriving via SkovQueryMonadT (GlobalStateM db c r g s m)
      instance (Monad m,
                BlockStateQuery (BlockStateM c r g s m),
                BlockStateStorage (BlockStateM c r g s m),
                TS.TreeStateMonad (TreeStateBlockStateM g c r s m)) => SkovQueryMonad (GlobalStateM db c r g s m)
