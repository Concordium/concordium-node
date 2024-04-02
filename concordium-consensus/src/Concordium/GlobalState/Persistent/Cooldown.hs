-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cooldown where

import Concordium.GlobalState.Persistent.ReleaseSchedule
import Concordium.Types

data AccountsInCooldown = AccountsInCooldown
    { cooldown :: NewReleaseSchedule,
      preCooldown :: [AccountIndex],
      prePreCooldown :: [AccountIndex]
    }
