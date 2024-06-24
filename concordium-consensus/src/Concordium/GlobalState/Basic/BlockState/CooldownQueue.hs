{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Basic.BlockState.CooldownQueue where

import Data.Bool.Singletons
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Serialize

import Concordium.Types
import Concordium.Types.HashableTo

import Concordium.GlobalState.Account
import Concordium.GlobalState.CooldownQueue
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
        (AVSupportsFlexibleCooldown av) =>
        !Cooldowns ->
        CooldownQueue av

deriving instance Show (CooldownQueue av)
deriving instance Eq (CooldownQueue av)

instance forall av. (IsAccountVersion av) => Serialize (CooldownQueue av) where
    put = case sSupportsFlexibleCooldown (accountVersion @av) of
        SFalse -> const (return ())
        STrue -> \case
            EmptyCooldownQueue -> putWord64be 0
            CooldownQueue queue -> put queue
    get = case sSupportsFlexibleCooldown (accountVersion @av) of
        SFalse -> return EmptyCooldownQueue
        STrue -> do
            cooldowns <- get
            return $!
                if isEmptyCooldowns cooldowns
                    then EmptyCooldownQueue
                    else CooldownQueue cooldowns

instance HashableTo (CooldownQueueHash av) (CooldownQueue av) where
    getHash _ = undefined -- FIXME: Define

-- | The empty 'CooldownQueue'.
emptyCooldownQueue :: CooldownQueue av
emptyCooldownQueue = EmptyCooldownQueue

-- | Check if a 'CooldownQueue' is empty.
isCooldownQueueEmpty :: CooldownQueue av -> Bool
isCooldownQueueEmpty EmptyCooldownQueue = True
isCooldownQueueEmpty _ = False

-- | Convert a 'Cooldowns' to a 'CooldownQueue', using 'EmptyCooldownQueue' for the case where
--  there are no cooldowns.
fromCooldowns :: (AVSupportsFlexibleCooldown av) => Cooldowns -> CooldownQueue av
fromCooldowns cooldowns
    | isEmptyCooldowns cooldowns = emptyCooldownQueue
    | otherwise = CooldownQueue cooldowns

-- | Create an initial 'CooldownQueue' with only the given target amount set for pre-pre-cooldown.
initialPrePreCooldownQueue :: (AVSupportsFlexibleCooldown av) => Amount -> CooldownQueue av
initialPrePreCooldownQueue target =
    CooldownQueue $
        Cooldowns
            { inCooldown = Map.empty,
              preCooldown = Absent,
              prePreCooldown = Present target
            }

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

-- | Process the pre-cooldown (if any). Any pre-cooldown amount is added to the cooldown queue
--  with the specified expiry time.
processPreCooldown ::
    -- | Timestamp at which the cooldown should expire.
    Timestamp ->
    -- | Current cooldown queue.
    CooldownQueue av ->
    -- | If a change is required, the new cooldown queue.
    Maybe (CooldownQueue av)
processPreCooldown _ EmptyCooldownQueue = Nothing
processPreCooldown _ (CooldownQueue Cooldowns{preCooldown = Absent}) = Nothing
processPreCooldown ts (CooldownQueue cdns@Cooldowns{preCooldown = Present newCooldownAmt, ..}) =
    Just $
        CooldownQueue $
            cdns
                { preCooldown = Absent,
                  inCooldown = Map.alter (Just . (newCooldownAmt +) . fromMaybe 0) ts inCooldown
                }

{-}
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
