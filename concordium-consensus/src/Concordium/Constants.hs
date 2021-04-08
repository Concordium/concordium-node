-- |Hard-coded constants for various purposes.
module Concordium.Constants where

import Concordium.Types

-- |Default value for early block threshold.
-- Set to 30 seconds.
defaultEarlyBlockThreshold :: Duration
defaultEarlyBlockThreshold = 30000