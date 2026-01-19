{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Persistent.Account.CooldownQueue where

import Data.Bool.Singletons
import Data.Functor
import qualified Data.Map.Strict as Map

import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Types.Option
import Concordium.Utils

import Concordium.GlobalState.Account
import Concordium.GlobalState.CooldownQueue as Cooldowns
import Concordium.GlobalState.Persistent.BlobStore

-- | A 'CooldownQueue' records the inactive stake amounts that are due to be released in future.
--  Note that prior to account version 3 (protocol version 7), the only value is the empty cooldown
--  queue.
data CooldownQueue store (av :: AccountVersion) where
    -- | The empty cooldown queue.
    EmptyCooldownQueue :: CooldownQueue store av
    -- | A non-empty cooldown queue.
    --  INVARIANT: The 'Cooldowns' must not satisfy 'isEmptyCooldowns'.
    CooldownQueue ::
        (AVSupportsFlexibleCooldown av) =>
        !(EagerBufferedRef store Cooldowns) ->
        CooldownQueue store av

deriving instance Show (CooldownQueue store av)

instance forall m store av. (MonadBlobStore m, store ~ MBSStore m, IsAccountVersion av) => BlobStorable m (CooldownQueue store av) where
    load = case sSupportsFlexibleCooldown (accountVersion @av) of
        SFalse -> return $ return EmptyCooldownQueue
        STrue -> do
            mRef <- load
            return $!
                mRef <&> \case
                    Null -> EmptyCooldownQueue
                    Some cooldowns -> CooldownQueue cooldowns
    storeUpdate = case sSupportsFlexibleCooldown (accountVersion @av) of
        SFalse -> \queue -> return (return (), queue)
        STrue -> \queue -> do
            (putter, nRef) <- storeUpdate (asNullable queue)
            return $!! (putter, ofNullable nRef)
      where
        asNullable :: CooldownQueue store av -> Nullable (EagerBufferedRef store Cooldowns)
        asNullable EmptyCooldownQueue = Null
        asNullable (CooldownQueue queue) = Some queue
        ofNullable Null = EmptyCooldownQueue
        ofNullable (Some queue) = CooldownQueue queue

-- | The hash of 'EmptyCooldownQueue'.
emptyCooldownQueueHash :: CooldownQueueHash av
{-# NOINLINE emptyCooldownQueueHash #-}
emptyCooldownQueueHash = CooldownQueueHash (getHash emptyCooldowns)

instance (MonadBlobStore m, store ~ MBSStore m) => MHashableTo m (CooldownQueueHash av) (CooldownQueue store av) where
    getHashM EmptyCooldownQueue = return emptyCooldownQueueHash
    getHashM (CooldownQueue ref) = CooldownQueueHash . getHash <$> refLoad ref

-- | The empty 'CooldownQueue'.
emptyCooldownQueue :: CooldownQueue store av
emptyCooldownQueue = EmptyCooldownQueue

-- | Check if a 'CooldownQueue' is empty.
isCooldownQueueEmpty :: CooldownQueue store av -> Bool
isCooldownQueueEmpty EmptyCooldownQueue = True
isCooldownQueueEmpty _ = False

-- | Construct a 'CooldownQueue' from a 'Cooldowns', which may be empty.
makeCooldownQueue ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    Cooldowns ->
    m (CooldownQueue (MBSStore m) av)
makeCooldownQueue cooldowns
    | isEmptyCooldowns cooldowns = return EmptyCooldownQueue
    | otherwise = CooldownQueue <$> refMake cooldowns

-- | Construct a 'CooldownQueue' from the representation used for transient accounts.
makePersistentCooldownQueue ::
    (MonadBlobStore m) =>
    Conditionally (SupportsFlexibleCooldown av) Cooldowns ->
    m (CooldownQueue (MBSStore m) av)
makePersistentCooldownQueue CFalse = return EmptyCooldownQueue
makePersistentCooldownQueue (CTrue cooldowns) = makeCooldownQueue cooldowns

-- | Convert a 'CooldownQueue' to representation used for transient accounts.
toTransientCooldownQueue ::
    forall av store.
    (IsAccountVersion av) =>
    CooldownQueue store av ->
    Conditionally (SupportsFlexibleCooldown av) Cooldowns
toTransientCooldownQueue = case sSupportsFlexibleCooldown (accountVersion @av) of
    SFalse -> const CFalse
    STrue ->
        CTrue . \case
            EmptyCooldownQueue -> emptyCooldowns
            CooldownQueue ref -> eagerBufferedDeref ref

-- | Create an initial 'CooldownQueue' with only the given amount set in pre-pre-cooldown.
initialPrePreCooldownQueue ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    -- | Initial amount in pre-pre-cooldown.
    Amount ->
    m (CooldownQueue (MBSStore m) av)
initialPrePreCooldownQueue target =
    CooldownQueue
        <$> refMake
            Cooldowns
                { inCooldown = Map.empty,
                  preCooldown = Absent,
                  prePreCooldown = Present target
                }

-- | Migrate a cooldown queue unchanged.
migrateCooldownQueue :: forall m t av1 av2. (SupportMigration m t, AVSupportsFlexibleCooldown av2) => CooldownQueue (MBSStore m) av1 -> t m (CooldownQueue (MBSStore (t m)) av2)
migrateCooldownQueue EmptyCooldownQueue = return EmptyCooldownQueue
migrateCooldownQueue (CooldownQueue queueRef) =
    CooldownQueue <$> migrateEagerBufferedRef return queueRef

-- | Get the total stake in cooldown, pre-cooldown and pre-pre-cooldown.
cooldownStake :: CooldownQueue store av -> Amount
cooldownStake EmptyCooldownQueue = 0
cooldownStake (CooldownQueue queueRef) = cooldownTotal $ eagerBufferedDeref queueRef

-- | Add the given amount to the pre-pre-cooldown.
addPrePreCooldown ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    -- | The amount to add to the pre-pre-cooldown.
    Amount ->
    CooldownQueue (MBSStore m) av ->
    m (CooldownQueue (MBSStore m) av)
addPrePreCooldown amount EmptyCooldownQueue = initialPrePreCooldownQueue amount
addPrePreCooldown amount (CooldownQueue queueRef) = do
    let oldCooldowns = eagerBufferedDeref queueRef
    let !newCooldowns = Cooldowns.addPrePreCooldown amount oldCooldowns
    makeCooldownQueue newCooldowns

-- | Remove up to the given amount from the cooldowns, starting with pre-pre-cooldown, then
--  pre-cooldown, and finally from the amounts in cooldown, in decreasing order of timestamp.
reactivateCooldownAmount ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    -- | The amount to reactivate.
    Amount ->
    CooldownQueue (MBSStore m) av ->
    m (CooldownQueue (MBSStore m) av)
reactivateCooldownAmount _ EmptyCooldownQueue = return EmptyCooldownQueue
reactivateCooldownAmount amount (CooldownQueue queueRef) = do
    let oldCooldowns = eagerBufferedDeref queueRef
    let !newCooldowns = Cooldowns.reactivateCooldownAmount amount oldCooldowns
    makeCooldownQueue newCooldowns

-- | Process all cooldowns that expire at or before the given timestamp.
--   This returns the next timestamp at which a cooldown expires, if any.
processCooldownsUntil ::
    (MonadBlobStore m) =>
    -- | Release all cooldowns up to and including this timestamp.
    Timestamp ->
    CooldownQueue (MBSStore m) av ->
    m (Maybe Timestamp, CooldownQueue (MBSStore m) av)
processCooldownsUntil _ EmptyCooldownQueue = return (Nothing, EmptyCooldownQueue)
processCooldownsUntil ts (CooldownQueue queueRef) = do
    let !newCooldowns = processCooldowns ts $ eagerBufferedDeref queueRef
    let !nextTimestamp = firstCooldownTimestamp newCooldowns
    newQueue <- makeCooldownQueue newCooldowns
    return (nextTimestamp, newQueue)

-- | The change to the next cooldown time as a result of processing the pre-cooldown.
data NextCooldownChange
    = -- | The previous next cooldown time was @oldNextCooldown@, but the new next cooldown
      -- time is earlier.
      EarlierNextCooldown {oldNextCooldown :: !Timestamp}
    | -- | There was no cooldown but now there is.
      NewNextCooldown
    | -- | There was no change.
      NextCooldownUnchanged

-- | Move the pre-cooldown amount on into cooldown with the specified release time.
--  This returns @EarlierNextCooldown ts@ if the previous next cooldown time was @ts@, but the new
--  next cooldown (i.e. the supplied timestamp) time is earlier. It returns @NewNextCooldown@ if
--  there was no cooldown but now there is. Otherwise, it returns @NextCooldownUnchanged@.
processPreCooldown ::
    (MonadBlobStore m) =>
    -- | The timestamp at which the pre-cooldown should be released.
    Timestamp ->
    CooldownQueue (MBSStore m) av ->
    m (NextCooldownChange, CooldownQueue (MBSStore m) av)
processPreCooldown _ EmptyCooldownQueue = return (NextCooldownUnchanged, EmptyCooldownQueue)
processPreCooldown ts (CooldownQueue queueRef) = do
    let oldCooldowns = eagerBufferedDeref queueRef
    let !newCooldowns = Cooldowns.processPreCooldown ts oldCooldowns
    let oldNextTimestamp = firstCooldownTimestamp oldCooldowns
    let nextTimestamp = firstCooldownTimestamp newCooldowns
    let !res
            | Just oldTS <- oldNextTimestamp,
              Just nextTS <- nextTimestamp,
              nextTS < oldTS =
                EarlierNextCooldown{oldNextCooldown = oldTS}
            | Nothing <- oldNextTimestamp,
              Just _ <- nextTimestamp =
                NewNextCooldown
            | otherwise = NextCooldownUnchanged
    newQueue <- makeCooldownQueue newCooldowns
    return (res, newQueue)

-- | Move the pre-pre-cooldown amount on into pre-cooldown.
--  It should be the case that there is a pre-pre-cooldown amount and no pre-cooldown amount.
--  However, if there is no pre-pre-cooldown amount, this will do nothing, and if there is already
--  a pre-cooldown amount, the pre-pre-cooldown amount will be added to it.
processPrePreCooldown :: (MonadBlobStore m) => CooldownQueue (MBSStore m) av -> m (CooldownQueue (MBSStore m) av)
processPrePreCooldown EmptyCooldownQueue = return EmptyCooldownQueue
processPrePreCooldown (CooldownQueue queueRef) = do
    let oldCooldowns = eagerBufferedDeref queueRef
    let !newCooldowns = Cooldowns.processPrePreCooldown oldCooldowns
    makeCooldownQueue newCooldowns
