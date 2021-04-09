{-# LANGUAGE NumericUnderscores #-}

-- |Hard-coded constants for various purposes.
module Concordium.Constants where

import Concordium.Types

-- |Default value for early block threshold.
-- Set to 30 seconds.
defaultEarlyBlockThreshold :: Duration
defaultEarlyBlockThreshold = 30_000

-- |Default value for maximum baking delay.
-- Set to 10 seconds.
defaultMaxBakingDelay :: Duration
defaultMaxBakingDelay = 10_000