{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.CooldownQueue where

import qualified Data.Bits as Bits
import qualified Data.Map.Strict as Map
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Accounts (Cooldown (..), CooldownStatus (..))
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.Option
import Data.Foldable

-- | The amounts that are currently in cooldown and any pre-cooldown and pre-pre-cooldown target
-- balances.
data Cooldowns = Cooldowns
    { -- | Amounts currently in cooldown, indexed by their expiry timestamp.
      --  (Must have fewer than 2^62 entries.)
      inCooldown :: !(Map.Map Timestamp Amount),
      -- | The amount in pre-cooldown.
      --  This will enter cooldown at the next payday.
      --  If 'Nothing', there is no pre-cooldown.
      preCooldown :: !(Option Amount),
      -- | The amount in pre-pre-cooldown.
      --  This will enter pre-cooldown at the next epoch transition that is one epoch before a
      --  payday.
      --  If 'Nothing', there is no pre-pre-cooldown.
      prePreCooldown :: !(Option Amount)
    }
    deriving (Eq, Show)

instance Serialize Cooldowns where
    put Cooldowns{..} = do
        putWord64be tag
        putSafeSizedMapOf put put inCooldown
        mapM_ put preCooldown
        mapM_ put prePreCooldown
      where
        -- The two highest order bits encode whether there is a preCooldown and a
        -- prePreCooldown
        tag = fromIntegral (Map.size inCooldown) Bits..|. preCooldownBit Bits..|. prePreCooldownBit
        preCooldownBit
            | isPresent preCooldown = Bits.bit 62
            | otherwise = 0
        prePreCooldownBit
            | isPresent prePreCooldown = Bits.bit 63
            | otherwise = 0
    get :: Get Cooldowns
    get = do
        tag <- getWord64be
        inCooldown <- getSafeSizedMapOf (tag Bits..&. sizeMask) get get
        preCooldown <- if Bits.testBit tag 62 then Present <$> get else return Absent
        prePreCooldown <- if Bits.testBit tag 63 then Present <$> get else return Absent
        return Cooldowns{..}
      where
        sizeMask = Bits.bit 62 - 1

instance (MonadBlobStore m) => BlobStorable m Cooldowns

-- | Check if a 'Cooldowns' is empty (i.e. has no stake in cooldown, pre-cooldown or
--  pre-pre-cooldown).
isEmptyCooldowns :: Cooldowns -> Bool
isEmptyCooldowns Cooldowns{..} =
    Map.null inCooldown
        && null preCooldown
        && null prePreCooldown

-- | A 'Cooldowns' with no stake in cooldown, pre-cooldown or pre-pre-cooldown.
emptyCooldowns :: Cooldowns
emptyCooldowns =
    Cooldowns
        { inCooldown = Map.empty,
          preCooldown = Absent,
          prePreCooldown = Absent
        }

-- | The total amount in cooldown, pre-cooldown and pre-pre-cooldown.
cooldownTotal :: Cooldowns -> Amount
cooldownTotal Cooldowns{..} =
    sum (Map.elems inCooldown)
        + fromOption 0 preCooldown
        + fromOption 0 prePreCooldown

-- FIXME: Decide if we want to use the serialization for hashing.
instance HashableTo Hash.Hash Cooldowns where
    getHash = getHash . encode

-- | Add the given amount to the pre-pre-cooldown.
addPrePreCooldown :: Amount -> Cooldowns -> Cooldowns
addPrePreCooldown amt Cooldowns{..} =
    Cooldowns
        { prePreCooldown = Present (ofOption amt (+ amt) prePreCooldown),
          ..
        }

-- | Remove up to the given amount from the cooldowns, starting with pre-pre-cooldown, then
--  pre-cooldown, and finally from the amounts in cooldown, in decreasing order of timestamp.
reactivateCooldownAmount :: Amount -> Cooldowns -> Cooldowns
reactivateCooldownAmount = reactivatePrePre
  where
    reactivatePrePre amt cd = case prePreCooldown cd of
        Absent -> reactivatePre amt cd
        Present prePreAmt -> case prePreAmt `compare` amt of
            LT -> reactivatePre (amt - prePreAmt) cd{prePreCooldown = Absent}
            EQ -> cd{prePreCooldown = Absent}
            GT -> cd{prePreCooldown = Present (prePreAmt - amt)}
    reactivatePre amt cd = case preCooldown cd of
        Absent -> reactivateInCooldown amt cd
        Present preAmt -> case preAmt `compare` amt of
            LT -> reactivateInCooldown (amt - preAmt) cd{preCooldown = Absent}
            EQ -> cd{preCooldown = Absent}
            GT -> cd{preCooldown = Present (preAmt - amt)}
    reactivateInCooldown amt cd = reactivateCooldown (Map.toDescList $ inCooldown cd) amt cd
    reactivateCooldown [] _ cd = cd
    reactivateCooldown ((ts, availableAmount) : rest) amt cd =
        case availableAmount `compare` amt of
            LT -> reactivateCooldown rest (amt - availableAmount) cd{inCooldown = Map.delete ts (inCooldown cd)}
            EQ -> cd{inCooldown = Map.delete ts (inCooldown cd)}
            GT -> cd{inCooldown = Map.insert ts (availableAmount - amt) (inCooldown cd)}

-- | Remove any amounts in cooldown with timestamp before or equal to the given timestamp.
processCooldowns :: Timestamp -> Cooldowns -> Cooldowns
processCooldowns ts Cooldowns{..} =
    Cooldowns
        { inCooldown = snd $ Map.split ts inCooldown,
          ..
        }

-- | Transfer the pre-cooldown to cooldown with the specified expiry timestamp.
processPreCooldown :: Timestamp -> Cooldowns -> Cooldowns
processPreCooldown _ c@Cooldowns{preCooldown = Absent} = c
processPreCooldown expiry Cooldowns{preCooldown = Present preAmt, ..} =
    Cooldowns
        { inCooldown = Map.insertWith (+) expiry preAmt inCooldown,
          preCooldown = Absent,
          ..
        }

-- | Transfer the pre-pre-cooldown to pre-cooldown.
-- If there is already an amount in pre-cooldown, the two amounts are combined.
processPrePreCooldown :: Cooldowns -> Cooldowns
processPrePreCooldown c@Cooldowns{prePreCooldown = Absent} = c
processPrePreCooldown Cooldowns{preCooldown = Absent, ..} =
    Cooldowns
        { preCooldown = prePreCooldown,
          prePreCooldown = Absent,
          ..
        }
processPrePreCooldown Cooldowns{preCooldown = Present preAmt, prePreCooldown = Present prePreAmt, ..} =
    Cooldowns
        { preCooldown = Present (preAmt + prePreAmt),
          prePreCooldown = Absent,
          ..
        }

-- | Get the timestamp of the first cooldown that will expire, if any.
-- (This ignores pre-cooldown and pre-pre-cooldown.)
firstCooldownTimestamp :: Cooldowns -> Maybe Timestamp
firstCooldownTimestamp Cooldowns{..} = fst <$> Map.lookupMin inCooldown

-- | Parameters that are used to calculate the timestamps at which stake in pre-cooldown and
--  pre-pre-cooldown is expected to be released from cooldown.
data CooldownCalculationParameters = CooldownCalculationParameters
    { -- | The duration of an epoch.
      ccpEpochDuration :: Duration,
      -- | The current epoch number.
      ccpCurrentEpoch :: Epoch,
      -- | The time of the next epoch transition (i.e. trigger block time).
      ccpTriggerTime :: Timestamp,
      -- | The epoch number of the next payday. (Must be after the current epoch.)
      ccpNextPayday :: Epoch,
      -- | The length of a reward period in epochs.
      ccpRewardPeriodLength :: RewardPeriodLength,
      -- | The current duration for cooldowns.
      ccpCooldownDuration :: DurationSeconds
    }

-- | Calculate the timestamp at which stake in pre-cooldown is expected to be released from
--  cooldown. This is computed by adding the cooldown duration to the time of the next payday,
--  where the time of the next payday is the time of the next epoch transition (i.e trigger
--  block time) plus the duration of an epoch for each epoch between the current epoch and the
--  payday.
preCooldownTimestamp :: CooldownCalculationParameters -> Timestamp
preCooldownTimestamp CooldownCalculationParameters{..} =
    ccpTriggerTime
        `addDuration` (fromIntegral (ccpNextPayday - ccpCurrentEpoch - 1) * ccpEpochDuration)
        `addDurationSeconds` ccpCooldownDuration

-- | Calculate the timestamp at which stake in pre-pre-cooldown is expected to be released from
--  cooldown. If the next epoch is the next payday, the cooldown starts from the payday after
--  that. Otherwise, it starts from the next payday, as for 'preCooldownTimestamp'.
prePreCooldownTimestamp :: CooldownCalculationParameters -> Timestamp
prePreCooldownTimestamp ccp@CooldownCalculationParameters{..}
    | ccpNextPayday - ccpCurrentEpoch == 1 =
        ccpTriggerTime
            `addDuration` (ccpEpochDuration * fromIntegral ccpRewardPeriodLength)
            `addDurationSeconds` ccpCooldownDuration
    | otherwise = preCooldownTimestamp ccp

-- | Convert a 'Cooldowns' to a list of 'Cooldown's.
toCooldownList :: CooldownCalculationParameters -> Cooldowns -> [Cooldown]
toCooldownList ccp Cooldowns{..} = cooldowns ++ preCooldowns ++ prePreCooldowns
  where
    cooldowns = (\(ts, amt) -> Cooldown ts amt StatusCooldown) <$> Map.toAscList inCooldown
    preCooldowns =
        (\amt -> Cooldown (preCooldownTimestamp ccp) amt StatusPreCooldown) <$> toList preCooldown
    prePreCooldowns =
        (\amt -> Cooldown (prePreCooldownTimestamp ccp) amt StatusPrePreCooldown)
            <$> toList prePreCooldown
