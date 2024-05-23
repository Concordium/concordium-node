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
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types.Option

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
