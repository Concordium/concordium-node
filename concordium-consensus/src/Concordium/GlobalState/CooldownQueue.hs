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
    { -- | Amounts currently in cooldown.
      -- (Must have fewer than 2^62 entries.)
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

data CooldownCalculationParameters = CooldownCalculationParameters
    { ccpEpochDuration :: Duration,
      ccpCurrentEpoch :: Epoch,
      ccpTriggerTime :: Timestamp,
      ccpNextPayday :: Epoch,
      ccpRewardPeriodLength :: RewardPeriodLength,
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
--  cooldown. If the next epoch is the next payday, this is the time of the payday after that.
--  Otherwise, this is the same as the 'preCooldownTimestamp'.
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
