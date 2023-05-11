{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.TreeState.StartUp where

import Control.Monad.Catch
import Control.Monad.IO.Class

import Concordium.Types
import Concordium.Types.Parameters

import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Construct the epoch bakers for a given last finalized block based on the low level tree state
-- store.
makeEpochBakers ::
    ( BlockStateQuery m,
      GSTypes.BlockState m ~ PBS.HashedPersistentBlockState pv,
      IsConsensusV1 pv,
      MPV m ~ pv,
      MonadThrow m,
      LowLevel.MonadTreeStateStore m,
      MonadIO m
    ) =>
    BlockPointer pv ->
    m EpochBakers
makeEpochBakers lastFinBlock = do
    let lfbState = bpState lastFinBlock
    curFullBakers <- getCurrentEpochBakers lfbState
    curFinParams <- getCurrentEpochFinalizationCommitteeParameters lfbState
    let _currentEpochBakers = computeBakersAndFinalizers curFullBakers curFinParams
    nextFullBakers <- getNextEpochBakers lfbState
    nextFinParams <- getNextEpochFinalizationCommitteeParameters lfbState
    let _nextEpochBakers = computeBakersAndFinalizers nextFullBakers nextFinParams
    _nextPayday <- getPaydayEpoch lfbState
    _previousEpochBakers <-
        if blockEpoch lastFinBlock == 0
            then return _currentEpochBakers
            else backTo (blockEpoch lastFinBlock - 1) (0, 0) (blockEpoch lastFinBlock, blockHeight lastFinBlock)
    return $! EpochBakers{..}
  where
    -- INVARIANTS:
    --  * lowEpoch <= targetEpoch < highEpoch
    --  * lowHeight < highHeight
    --  * The block at lowHeight has epoch lowEpoch
    --  * The block at highEpoch has epoch highEpoch
    --  * If blockHeight x <= blockHeight y then blockEpoch x <= blockEpoch y.
    --  * There is at least one block of each epoch up to highEpoch.
    backTo targetEpoch (lowEpoch, lowHeight) (highEpoch, highHeight) = do
        -- We split the height interval in proportion to where the target epoch falls in the
        -- epoch interval. This guarantees:
        --  * curHeight < highHeight, since targetEpoch < highEpoch
        --  * lowHeight <= curHeight, with lowHeight == curHeight only if lowEpoch == targetEpoch,
        --    since (highHeight - lowHeight) >= (highEpoch - lowEpoch).
        let curHeight =
                lowHeight
                    + fromIntegral
                        ( toInteger (highHeight - lowHeight)
                            * toInteger (targetEpoch - lowEpoch)
                            `div` toInteger (highEpoch - lowEpoch)
                        )
        LowLevel.lookupBlockByHeight curHeight >>= \case
            Nothing ->
                throwM . TreeStateInvariantViolation $
                    "Missing block at height " ++ show curHeight
            Just stb -> case compare (blockEpoch stb) targetEpoch of
                EQ -> do
                    blockState <- bpState <$> mkBlockPointer stb
                    fullBakers <- getCurrentEpochBakers blockState
                    finParams <- getCurrentEpochFinalizationCommitteeParameters blockState
                    return $ computeBakersAndFinalizers fullBakers finParams
                LT -> do
                    backTo targetEpoch (blockEpoch stb, curHeight) (highEpoch, highHeight)
                GT -> do
                    backTo targetEpoch (lowEpoch, lowHeight) (blockEpoch stb, curHeight)
