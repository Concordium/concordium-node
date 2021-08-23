{-# LANGUAGE NumericUnderscores #-}

-- |Hard-coded constants for various timings.
module Concordium.Constants.Time where

import Data.Time

import Concordium.Types

-- |Default value for early block threshold.
-- Set to 30 seconds.
defaultEarlyBlockThreshold :: Duration
defaultEarlyBlockThreshold = 30_000

-- |Default value for maximum baking delay.
-- Set to 10 seconds.
defaultMaxBakingDelay :: Duration
defaultMaxBakingDelay = 10_000

-- |This sets the base time for triggering finalization replay.
finalizationReplayBaseDelay :: NominalDiffTime
finalizationReplayBaseDelay = 300

-- |This sets the per-party additional delay for finalization replay.
finalizationReplayStaggerDelay :: NominalDiffTime
finalizationReplayStaggerDelay = 5
