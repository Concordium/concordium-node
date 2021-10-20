{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |This module defines types for blockchain parameters, including genesis data,
-- baker parameters and finalization parameters.
module Concordium.GlobalState.Parameters(
    module Concordium.GlobalState.Parameters,
    module Concordium.Types.Parameters,
    module Concordium.Genesis.Data,
    BakerInfo,
    MintDistribution(..),
    TransactionFeeDistribution(..),
    GASRewards(..)
) where

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Word

import Concordium.Types.Updates
import Concordium.Common.Version
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Genesis.Data


readIdentityProviders :: BSL.ByteString -> Maybe IdentityProviders
readIdentityProviders bs = do
  v <- AE.decode bs
   -- We only support Version 0 at this point for testing. When we support more
   -- versions we'll have to decode in a dependent manner, first reading the
   -- version, and then decoding based on that.
  guard (vVersion v == 0)
  return (vValue v)

readAnonymityRevokers :: BSL.ByteString -> Maybe AnonymityRevokers
readAnonymityRevokers bs = do
  v <- AE.decode bs
   -- We only support Version 0 at this point for testing. When we support more
   -- versions we'll have to decode in a dependent manner, first reading the
   -- version, and then decoding based on that.
  guard (vVersion v == 0)
  return (vValue v)

eitherReadIdentityProviders :: BSL.ByteString -> Either String IdentityProviders
eitherReadIdentityProviders bs = do
  v <- AE.eitherDecode bs
  unless (vVersion v == 0) $ Left $ "Incorrect version: " ++ show (vVersion v)
  return (vValue v)

eitherReadAnonymityRevokers :: BSL.ByteString -> Either String AnonymityRevokers
eitherReadAnonymityRevokers bs = do
  v <- AE.eitherDecode bs
  unless (vVersion v == 0) $ Left $ "Incorrect version: " ++ show (vVersion v)
  return (vValue v)

getExactVersionedCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
getExactVersionedCryptographicParameters bs = do
   v <- AE.decode bs
   -- We only support Version 0 at this point for testing. When we support more
   -- versions we'll have to decode in a dependent manner, first reading the
   -- version, and then decoding based on that.
   guard (vVersion v == 0)
   return (vValue v)

-- |Mode to use for flushing the blob store buffer.
-- Flushing to disk is more resilient, as it ensures that the data is written when a flush occurs.
-- However, it does increase the overhead of the flush operation.
data FlushMode
    = FlushToOS
    -- ^Flush the buffer to the operating system, but don't force a disk flush.
    | FlushToDisk
    -- ^Flush the buffer to the operating system and subsequently to disk.

-- |Decode a 'FlushMode' from a 'Word64'.
--
-- +--------+-------------+
-- |Encoding|'FlushMode'  |
-- +========+=============|
-- |0       |'FlushToOS'  |
-- +--------+-------------+
-- |1       |'FlushToDisk'|
-- +--------+-------------+
--
-- Any other value is interpreted as 'FlushToOS', but this behaviour should not be relied upon.
--
-- This should be kept in sync with the Rust type consensus_ffi::consensus::FlushMode.
flushModeFromWord64 :: Word64 -> FlushMode
flushModeFromWord64 1 = FlushToDisk
flushModeFromWord64 _ = FlushToOS

-- |Implementation-defined parameters, such as block size. They are not
-- protocol-level parameters hence do not fit into 'GenesisParameters'.
data RuntimeParameters = RuntimeParameters {
  -- |Maximum block size produced by the baker (in bytes). Note that this only
  -- applies to the blocks produced by this baker, we will still accept blocks
  -- of arbitrary size from other bakers.
  rpBlockSize :: !Int,
  -- |Timeout of block construction, i.e. the maximum time (in milliseconds) it
  -- may take to construct a block. After this amount of time, we will stop
  -- processing transaction groups in `filterTransactions` in Scheduler.hs
  -- and mark the rest as unprocessed. 
  rpBlockTimeout :: !Duration,
  -- |Threshold for how far into the future we accept blocks. Blocks with a slot
  -- time that exceeds our current time + this threshold are rejected and the p2p
  -- is told to not relay these blocks.  Setting this to 'maxBound' will disable the
  -- check.  Otherwise, the value should not be so large as to overflow when added
  -- to a timestamp within the operational life of the node.
  rpEarlyBlockThreshold :: !Duration,
  -- |Maximum number of milliseconds we can get behind before skipping to the current time
  -- when baking.
  rpMaxBakingDelay :: !Duration,
  -- |Number of insertions to be performed in the transaction table before running
  -- a purge to remove long living transactions that have not been executed for more
  -- than `rpTransactionsKeepAliveTime` seconds.
  rpInsertionsBeforeTransactionPurge :: !Int,
  -- |Number of seconds after receiving a transaction during which it is kept in the
  -- transaction table if a purge is executed.
  rpTransactionsKeepAliveTime :: !TransactionTime,
  -- |Number of seconds between automatic transaction table purging  runs.
  rpTransactionsPurgingDelay :: !Int,
  -- |The maximum allowed time difference between slot time and a transaction's expiry time.
  rpMaxTimeToExpiry :: !TransactionTime,
  -- |Flushing mode to use for the block state.
  rpBlockStateFlushMode :: !FlushMode
  }

-- |Default runtime parameters, block size = 10MB.
defaultRuntimeParameters :: RuntimeParameters
defaultRuntimeParameters = RuntimeParameters {
  rpBlockSize = 10 * 10^(6 :: Int), -- 10MB
  rpBlockTimeout = 3000, -- 3 seconds
  rpEarlyBlockThreshold = 30000, -- 30 seconds
  rpMaxBakingDelay = 10000, -- 10 seconds
  rpInsertionsBeforeTransactionPurge = 1000,
  rpTransactionsKeepAliveTime = 5 * 60, -- 5 min
  rpTransactionsPurgingDelay = 3 * 60, -- 3 min
  rpMaxTimeToExpiry = 60 * 60 * 2, -- 2 hours
  rpBlockStateFlushMode = FlushToOS
  }

-- |Values of updates that are stored in update queues.
-- These are slightly different to the 'UpdatePayload' type,
-- specifically in that for the foundation account we store
-- the account index rather than the account address.
data UpdateValue
    = -- |Protocol updates.
      UVProtocol !ProtocolUpdate
    -- |Updates to the election difficulty parameter.
    | UVElectionDifficulty !ElectionDifficulty
    -- |Updates to the euro:energy exchange rate.
    | UVEuroPerEnergy !ExchangeRate
    -- |Updates to the GTU:euro exchange rate.
    | UVMicroGTUPerEuro !ExchangeRate
    -- |Updates to the foundation account.
    | UVFoundationAccount !AccountIndex
    -- |Updates to the mint distribution.
    | UVMintDistribution !MintDistribution
    -- |Updates to the transaction fee distribution.
    | UVTransactionFeeDistribution !TransactionFeeDistribution
    -- |Updates to the GAS rewards.
    | UVGASRewards !GASRewards
    -- |Updates to the baker minimum threshold
    | UVBakerStakeThreshold !Amount
    -- |Adds a new anonymity revoker
    | UVAddAnonymityRevoker !ArInfo
    -- |Adds a new identity provider
    | UVAddIdentityProvider !IpInfo
    -- |Updates to root keys.
    | UVRootKeys !(HigherLevelKeys RootKeysKind)
    -- |Updates to level 1 keys.
    | UVLevel1Keys !(HigherLevelKeys Level1KeysKind)
    -- |Updates to level 2 keys.
    | UVLevel2Keys !Authorizations
    deriving (Eq, Show)
