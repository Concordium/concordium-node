-- {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cooldown where

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.ReleaseSchedule
import Concordium.Types
import Concordium.Types.Conditionally

data AccountListItem = AccountListItem
    { accountListEntry :: !AccountIndex,
      accountListTail :: !AccountList
    }

type AccountList = Nullable (UnbufferedRef AccountListItem)

-- | This is an indexing structure and therefore does not need to be hashed. FIXME: add more docs
data AccountsInCooldown = AccountsInCooldown
    { cooldown :: NewReleaseSchedule,
      preCooldown :: [AccountIndex],
      prePreCooldown :: [AccountIndex]
    }

type AccountsInCooldownForPV pv = (Conditionally (SupportsFlexibleCooldown (AccountVersionFor pv)) AccountsInCooldown)

instance (MonadBlobStore m) => BlobStorable m AccountsInCooldown where
    load = undefined

    -- do
    --   cooldown <- load
    --   preCooldown <- load
    --   return ()
    storeUpdate = undefined
