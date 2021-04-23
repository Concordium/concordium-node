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
import Control.Monad.Fail
import Control.Monad hiding (fail)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), (.:), withObject)

import Concordium.Types.Updates
import Concordium.Common.Version
import Concordium.Types
import Concordium.Types.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Genesis.Data

import Concordium.GlobalState.BakerInfo

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
  -- |Treestate storage directory.
  rpTreeStateDir :: !FilePath,
  -- |BlockState storage file.
  rpBlockStateFile :: !FilePath,
  -- |Threshold for how far into the future we accept blocks. Blocks with a slot
  -- time that exceeds our current time + this threshold are rejected and the p2p
  -- is told to not relay these blocks.
  rpEarlyBlockThreshold :: !Timestamp,
  -- |Maximum number of milliseconds we can get behind before skipping to the current time
  -- when baking.
  rpMaxBakingDelay :: !Timestamp,
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
  rpMaxTimeToExpiry :: !TransactionTime
  }

-- |Default runtime parameters, block size = 10MB.
defaultRuntimeParameters :: RuntimeParameters
defaultRuntimeParameters = RuntimeParameters {
  rpBlockSize = 10 * 10^(6 :: Int), -- 10MB
  rpBlockTimeout = 3000, -- 3 seconds
  rpTreeStateDir = "treestate",
  rpBlockStateFile = "blockstate",
  rpEarlyBlockThreshold = 30000, -- 30 seconds
  rpMaxBakingDelay = 10000, -- 10 seconds
  rpInsertionsBeforeTransactionPurge = 1000,
  rpTransactionsKeepAliveTime = 5 * 60, -- 5 min
  rpTransactionsPurgingDelay = 3 * 60, -- 3 min
  rpMaxTimeToExpiry = 60 * 60 * 2 -- 2 hours
  }

instance FromJSON RuntimeParameters where
  parseJSON = withObject "RuntimeParameters" $ \v -> do
    rpBlockSize <- v .: "blockSize"
    rpBlockTimeout <- v .: "blockTimeout"
    rpTreeStateDir <- v .: "treeStateDir"
    rpBlockStateFile <- v .: "blockStateFile"
    rpEarlyBlockThreshold <- v .: "earlyBlockThreshold"
    rpMaxBakingDelay <- v .: "maxBakingDelay"
    rpInsertionsBeforeTransactionPurge <- v .: "insertionsBeforeTransactionPurge"
    rpTransactionsKeepAliveTime <- (fromIntegral :: Int -> TransactionTime) <$> v .: "transactionsKeepAliveTime"
    rpTransactionsPurgingDelay <- v .: "transactionsPurgingDelay"
    rpMaxTimeToExpiry <- v .: "maxTimeToExpiry"
    when (rpBlockSize <= 0) $
      fail "Block size must be a positive integer."
    when (rpEarlyBlockThreshold <= 0) $
      fail "The early block threshold must be a positive integer"
    return RuntimeParameters{..}

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
