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

import Concordium.Types
import Concordium.Utils.Serialization

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.Option

-- | The amounts that are currently in cooldown and any pre-cooldown and pre-pre-cooldown target
-- balances.
data Cooldowns = Cooldowns
    { -- | Amounts currently in cooldown.
      -- (Must have fewer than 2^62 entries.)
      inCooldown :: !(Map.Map Timestamp Amount),
      -- | The target staked balance after the next payday.
      --  If 'Nothing', there is no change.
      preCooldownTargetStake :: !(Option Amount),
      -- | The target staked balance after the next payday after the next epoch transition.
      --  If 'Nothing', there is no change.
      prePreCooldownTargetStake :: !(Option Amount)
    }
    deriving (Eq, Show)

instance Serialize Cooldowns where
    put Cooldowns{..} = do
        putWord64be tag
        putSafeSizedMapOf put put inCooldown
        mapM_ put preCooldownTargetStake
        mapM_ put prePreCooldownTargetStake
      where
        -- The two highest order bits encode whether there is a preCooldownTargetStake and a
        -- prePreCooldownTargetStake
        tag = fromIntegral (Map.size inCooldown) Bits..|. preCooldownBit Bits..|. prePreCooldownBit
        preCooldownBit
            | isPresent preCooldownTargetStake = Bits.bit 62
            | otherwise = 0
        prePreCooldownBit
            | isPresent prePreCooldownTargetStake = Bits.bit 63
            | otherwise = 0
    get :: Get Cooldowns
    get = do
        tag <- getWord64be
        inCooldown <- getSafeSizedMapOf (tag Bits..&. sizeMask) get get
        preCooldownTargetStake <- if Bits.testBit tag 62 then Present <$> get else return Absent
        prePreCooldownTargetStake <- if Bits.testBit tag 63 then Present <$> get else return Absent
        return Cooldowns{..}
      where
        sizeMask = Bits.bit 62 - 1

instance (MonadBlobStore m) => BlobStorable m Cooldowns

-- | Check if a 'Cooldowns' is empty (i.e. has no stake in cooldown, pre-cooldown or
--  pre-pre-cooldown).
isEmptyCooldowns :: Cooldowns -> Bool
isEmptyCooldowns Cooldowns{..} =
    Map.null inCooldown
        && null preCooldownTargetStake
        && null prePreCooldownTargetStake
