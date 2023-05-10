{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus.Timeout.Internal where

import Control.Monad.State
import Data.Ratio
import Data.Word
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Parameters hiding (getChainParameters)
import Concordium.Utils

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types

-- |Helper function for growing the @currentTimeout@ by a given @timeoutFactor@.
-- (The factor should be positive, as shrinking should be bounded by the timeout base.)
updateCurrentTimeout ::
    -- |@timeoutFactor@
    Ratio Word64 ->
    -- |@currentTimeout@
    Duration ->
    Duration
updateCurrentTimeout timeoutFactor oldCurrentTimeout =
    let timeoutIncreaseRational = toRational timeoutFactor
        currentTimeOutRational = toRational oldCurrentTimeout
        newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational
        newCurrentTimeout = floor newCurrentTimeoutRational
    in  Duration newCurrentTimeout

-- |Grow the current timeout duration in response to an elapsed timeout.
-- This updates the timeout to @timeoutIncrease * oldTimeout@.
growTimeout ::
    ( BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      BlockStateQuery m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |Block to take the timeout parameters from
    BlockPointer (MPV m) ->
    m ()
growTimeout blockPtr = do
    chainParams <- getChainParameters $ bpState blockPtr
    let timeoutIncrease =
            chainParams
                ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
    roundStatus . rsCurrentTimeout %=! updateCurrentTimeout timeoutIncrease

-- |Shrink the current timeout duration in response to a successful QC for a round.
-- This updates the current timeout to @max timeoutBase (timeoutDecrease * oldTimeout)@, where
-- @timeoutBase@ and @timeoutDecrease@ are taken from the chain parameters of the supplied block.
shrinkTimeout ::
    ( BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      BlockStateQuery m,
      MonadState (SkovData (MPV m)) m
    ) =>
    -- |Block to take the timeout parameters from
    BlockPointer (MPV m) ->
    m ()
shrinkTimeout blockPtr = do
    chainParams <- getChainParameters (bpState blockPtr)
    let timeoutParams = chainParams ^. cpConsensusParameters . cpTimeoutParameters
        updateTimeout cur = max (timeoutParams ^. tpTimeoutBase) grow
          where
            grow =
                Duration . ceiling $
                    toRational (timeoutParams ^. tpTimeoutDecrease) * toRational cur
    roundStatus . rsCurrentTimeout %=! updateTimeout
