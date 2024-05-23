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
import Concordium.Types.HashableTo
import Concordium.Utils

import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.Basic.BlockState.CooldownQueue as Transient
import Concordium.GlobalState.CooldownQueue
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.Option

-- | A 'CooldownQueue' records the inactive stake amounts that are due to be released in future.
--  Note that prior to account version 3 (protocol version 7), the only value is the empty cooldown
--  queue.
data CooldownQueue (av :: AccountVersion) where
    -- | The empty cooldown queue.
    EmptyCooldownQueue :: CooldownQueue av
    -- | A non-empty cooldown queue.
    --  INVARIANT: The 'Cooldowns' must not satisfy 'isEmptyCooldowns'.
    CooldownQueue ::
        (SupportsFlexibleCooldown av ~ 'True) =>
        !(EagerBufferedRef Cooldowns) ->
        CooldownQueue av

deriving instance Show (CooldownQueue av)

instance forall m av. (MonadBlobStore m, IsAccountVersion av) => BlobStorable m (CooldownQueue av) where
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
        asNullable :: CooldownQueue av -> Nullable (EagerBufferedRef Cooldowns)
        asNullable EmptyCooldownQueue = Null
        asNullable (CooldownQueue queue) = Some queue
        ofNullable Null = EmptyCooldownQueue
        ofNullable (Some queue) = CooldownQueue queue

-- | The has of 'EmptyCooldownQueue'.
emptyCooldownQueueHash :: CooldownQueueHash av
{-# NOINLINE emptyCooldownQueueHash #-}
emptyCooldownQueueHash = CooldownQueueHash (getHash emptyCooldowns)

instance (MonadBlobStore m) => MHashableTo m (CooldownQueueHash av) (CooldownQueue av) where
    getHashM EmptyCooldownQueue = return emptyCooldownQueueHash
    getHashM (CooldownQueue ref) = CooldownQueueHash . getHash <$> refLoad ref

-- | The empty 'CooldownQueue'.
emptyCooldownQueue :: CooldownQueue av
emptyCooldownQueue = EmptyCooldownQueue

-- | Check if a 'CooldownQueue' is empty.
isCooldownQueueEmpty :: CooldownQueue av -> Bool
isCooldownQueueEmpty EmptyCooldownQueue = True
isCooldownQueueEmpty _ = False

makePersistentCooldownQueue ::
    (MonadBlobStore m) =>
    Transient.CooldownQueue av ->
    m (CooldownQueue av)
makePersistentCooldownQueue Transient.EmptyCooldownQueue = return EmptyCooldownQueue
makePersistentCooldownQueue (Transient.CooldownQueue queue) = CooldownQueue <$> refMake queue

toTransientCooldownQueue :: CooldownQueue av -> Transient.CooldownQueue av
toTransientCooldownQueue EmptyCooldownQueue = Transient.EmptyCooldownQueue
toTransientCooldownQueue (CooldownQueue queueRef) =
    Transient.CooldownQueue (eagerBufferedDeref queueRef)

-- | Create an initial 'CooldownQueue' with only the given amount set in pre-pre-cooldown.
initialPrePreCooldownQueue ::
    (MonadBlobStore m, SupportsFlexibleCooldown av ~ True) =>
    -- | Initial amount in pre-pre-cooldown.
    Amount ->
    m (CooldownQueue av)
initialPrePreCooldownQueue target =
    CooldownQueue
        <$> refMake
            Cooldowns
                { inCooldown = Map.empty,
                  preCooldown = Absent,
                  prePreCooldown = Present target
                }

-- | Migrate a cooldown queue unchanged.
migrateCooldownQueue :: forall m t av. (SupportMigration m t) => CooldownQueue av -> t m (CooldownQueue av)
migrateCooldownQueue EmptyCooldownQueue = return EmptyCooldownQueue
migrateCooldownQueue (CooldownQueue queueRef) =
    CooldownQueue <$> migrateEagerBufferedRef return queueRef

-- | Get the total stake in cooldown, pre-cooldown and pre-pre-cooldown.
cooldownStake :: CooldownQueue av -> Amount
cooldownStake EmptyCooldownQueue = 0
cooldownStake (CooldownQueue queueRef) = cooldownTotal $ eagerBufferedDeref queueRef

{-
-- | Convert a 'Cooldowns' to a 'CooldownQueue', using 'EmptyCooldownQueue' for the case where
--  there are no cooldowns.
fromCooldowns :: (SupportsFlexibleCooldown av ~ True) => Cooldowns -> CooldownQueue av
fromCooldowns cooldowns
    | isEmptyCooldowns cooldowns = emptyCooldownQueue
    | otherwise = CooldownQueue cooldowns

-- | Process all cooldowns that expire at or before the given timestamp.
--  If there are no such cooldowns, then 'Nothing' is returned.
--  Otherwise, the total amount exiting cooldown and the remaining queue are returned.
processCooldowns :: Timestamp -> CooldownQueue av -> Maybe (Amount, CooldownQueue av)
processCooldowns _ EmptyCooldownQueue = Nothing
processCooldowns ts (CooldownQueue queue)
    | freeAmount == 0 = Nothing
    | otherwise = Just (freeAmount, remainder)
  where
    freeAmount = sum free + sum bonus
    (free, bonus, keep) = Map.splitLookup ts (inCooldown queue)
    remainder = fromCooldowns (queue{inCooldown = keep})

-- | Process the pre-cooldown (if any). The active stake is reduced to the new target stake, and
-- the remaining stake is added to the cooldown queue.
processPreCooldown ::
    -- | Timestamp at which the cooldown should expire.
    Timestamp ->
    -- | Current active stake.
    Amount ->
    -- | Current cooldown queue.
    CooldownQueue av ->
    -- | If a change is required, the new active stake and cooldown queue.
    Maybe (Amount, CooldownQueue av)
processPreCooldown _ _ EmptyCooldownQueue = Nothing
processPreCooldown ts stake (CooldownQueue cooldowns@Cooldowns{..}) =
    ofOption Nothing (Just . cooldownWithTarget) preCooldownTargetStake
  where
    cooldownsNoPre = cooldowns{preCooldownTargetStake = Absent}
    cooldownWithTarget targetStake
        | stake == 0 || targetStake >= stake = (stake, fromCooldowns cooldownsNoPre)
        | otherwise =
            ( targetStake,
              fromCooldowns
                cooldownsNoPre
                    { inCooldown = Map.alter (Just . (+ (stake - targetStake)) . fromMaybe 0) ts inCooldown
                    }
            )

-- | Move all pre-cooldowns into cooldown state. Where the pre-cooldown has a timestamp set, that
-- is used. Otherwise, the timestamp is used. This returns 'Nothing' if the queue would not be
-- changed, i.e. there are no pre-cooldowns.
-- Note, this will predominantly be used when there is at most one pre-cooldown, and it has no
-- timestamp set. Thus, this is not particularly optimized for other cases.
processPreCooldown :: Timestamp -> Amount -> CooldownQueue av -> Maybe (Amount, CooldownQueue av)
processPreCooldown _ _ EmptyCooldownQueue = Nothing
processPreCooldown ts stake (CooldownQueue queue)
    | null precooldowns = Nothing
    | tsMillis ts > theCooldownTimeCode maxCooldownTimestampCode = error "Timestamp out of bounds"
    | otherwise = Just (newStake, newQueue)
  where
    newQueue = CooldownQueue $ Map.unionsWith (+) [newCooldowns, preprecooldowns]
    (cooldowns, rest) = Map.spanAntitone (<= maxCooldownTimestampCode) queue
    (precooldowns, preprecooldowns) = Map.spanAntitone (<= encodeCooldownTime PreCooldown) rest
    (newStake, newCooldowns) = Map.foldlWithKey' ff (stake, cooldowns) precooldowns
    ff (staked, accCooldowns) tc amt
        | staked == 0 = (staked, accCooldowns)
        | staked < amt = (staked, accCooldowns)
        | otherwise = (amt, Map.alter (Just . (+ (staked - amt)) . fromMaybe 0) (f tc) accCooldowns)
    f c@(CooldownTimeCode code)
        | c == encodeCooldownTime PreCooldown = CooldownTimeCode $ tsMillis ts
        | otherwise = CooldownTimeCode (Bits.clearBit code 63)

-- | Get the next timestamp (if any) at which a cooldown is scheduled to elapse.
nextCooldownTime :: CooldownQueue av -> Maybe Timestamp
nextCooldownTime EmptyCooldownQueue = Nothing
nextCooldownTime (CooldownQueue queue) = case decodeCooldownTime minEntry of
    CooldownTimestamp ts -> Just ts
    _ -> Nothing
  where
    -- This is safe because 'CooldownQueue' requires @queue@ to be non-empty.
    (minEntry, _) = Map.findMin queue

-- | Check if a 'CooldownQueue' has any pre-cooldown entries.
hasPreCooldown :: CooldownQueue av -> Bool
hasPreCooldown EmptyCooldownQueue = False
hasPreCooldown (CooldownQueue queue) = case Map.lookupGT maxCooldownTimestampCode queue of
    Just (x, _) -> x <= encodeCooldownTime PreCooldown
    Nothing -> False

-- | Check if a 'CooldownQueue' has any pre-pre-cooldown entries.
hasPrePreCooldown :: CooldownQueue av -> Bool
hasPrePreCooldown EmptyCooldownQueue = False
hasPrePreCooldown (CooldownQueue queue) = isJust $ Map.lookupGT (encodeCooldownTime PreCooldown) queue
-}
