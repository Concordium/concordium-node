{-# LANGUAGE DeriveGeneric #-}
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

import Concordium.GlobalState.BakerInfo
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

{-
-- 'GenesisBaker' is an abstraction of a baker at genesis.
-- It includes the minimal information for generating a baker.
data GenesisBaker = GenesisBaker {
    -- |The baker's public VRF key
    gbElectionVerifyKey :: BakerElectionVerifyKey,
    -- |The baker's public signature key
    gbSignatureVerifyKey :: BakerSignVerifyKey,
    -- |The baker's public key for aggregate signatures
    gbAggregationVerifyKey :: BakerAggregationVerifyKey,
    -- |The baker's initial stake
    gbStake :: Amount,
    -- |Whether to restake the baker's earnings from rewards
    gbRestakeEarnings :: Bool,
    -- |The baker ID is defined by the account, however we use it here
    -- so we can report an error if inconsistent data is used.
    gbBakerId :: BakerId
}

instance FromJSON GenesisBaker where
    parseJSON = withObject "GenesisBaker" $ \v -> do
            gbElectionVerifyKey <- v .: "electionVerifyKey"
            gbSignatureVerifyKey <- v .: "signatureVerifyKey"
            gbAggregationVerifyKey <- v .: "aggregationVerifyKey"
            gbStake <- v .: "stake"
            gbRestakeEarnings <- v .: "restakeEarnings"
            gbBakerId <- v .: "bakerId"
            return GenesisBaker{..}

-- |'GenesisAccount' are special account existing in the genesis block, in
-- addition to baker accounts which are defined by the 'GenesisBaker' structure.
data GenesisAccount = GenesisAccount {
  gaAddress :: !AccountAddress,
  gaVerifyKeys :: !ID.AccountKeys,
  gaBalance :: !Amount,
  -- |We only need the credential values. However when parsing we parse a full
  -- credential, due to some legacy format issues, and then extract the values.
  -- The legacy issues are that the commitments are part of the "proofs" object
  -- in the credential, which, in JSON, is represented just as a hex-string.
  -- This should be reworked at some point, so that it is more principled than
  -- the current, slighly hacky, solution.
  gaCredential :: !ID.AccountCredential,
  gaBaker :: !(Maybe GenesisBaker)
}

instance FromJSON GenesisAccount where
  parseJSON = withObject "GenesisAccount" $ \obj -> do
    gaAddress <- obj .: "address"
    gaVerifyKeys <- obj .: "accountKeys"
    gaBalance <- obj .: "balance"
    Versioned{..} <- obj .: "credential"
    unless (vVersion == 0) $ fail "Only V0 credentials supported in genesis."
    gaCredentialFull <- parseJSON vValue
    gaCredential <- case ID.values gaCredentialFull of
      Nothing -> fail "Account credential is malformed."
      Just gaCredential -> return gaCredential
    gaBaker <- obj .:? "baker"
    -- Check that bakers do not stake more than their balance.
    case gaBaker of
      Just gb | gbStake gb > gaBalance -> fail "Stake exceeds balance"
      _ -> return ()
    return GenesisAccount{..}

data GenesisChainParameters = GenesisChainParameters {
    -- |Election difficulty parameter.
    gcpElectionDifficulty :: !ElectionDifficulty,
    -- |Euro:Energy rate.
    gcpEuroPerEnergy :: !ExchangeRate,
    -- |uGTU:Euro rate.
    gcpMicroGTUPerEuro :: !ExchangeRate,
    -- |Number of additional epochs that bakers must cool down when
    -- removing stake. The cool-down will effectively be 2 epochs
    -- longer than this value, since at any given time, the bakers
    -- (and stakes) for the current and next epochs have already
    -- been determined.
    gcpBakerExtraCooldownEpochs :: !Epoch,
    -- |LimitAccountCreation: the maximum number of accounts
    -- that may be created in one block.
    gcpAccountCreationLimit :: !CredentialsPerBlockLimit,
    -- |Reward parameters.
    gcpRewardParameters :: !RewardParameters,
    -- |Foundation account address.
    gcpFoundationAccount :: !AccountAddress
}

instance FromJSON GenesisChainParameters where
  parseJSON = withObject "GenesisChainParameters" $ \v ->
    GenesisChainParameters
      <$> v .: "electionDifficulty"
      <*> v .: "euroPerEnergy"
      <*> v .: "microGTUPerEuro"
      <*> v .: "bakerCooldownEpochs"
      <*> v .: "accountCreationLimit"
      <*> v .: "rewardParameters"
      <*> v .: "foundationAccount"

instance ToJSON GenesisChainParameters where
  toJSON GenesisChainParameters{..} = object [
      "electionDifficulty" AE..= gcpElectionDifficulty,
      "euroPerEnergy" AE..= gcpEuroPerEnergy,
      "microGTUPerEuro" AE..= gcpMicroGTUPerEuro,
      "bakerCooldownEpochs" AE..= gcpBakerExtraCooldownEpochs,
      "accountCreationLimit" AE..= gcpAccountCreationLimit,
      "rewardParameters" AE..= gcpRewardParameters,
      "foundationAccount" AE..= gcpFoundationAccount
    ]

-- 'GenesisParameters' provides a convenient abstraction for
-- constructing 'GenesisData'.
data GenesisParametersV2 = GenesisParametersV2 {
    gpGenesisTime :: Timestamp,
    gpSlotDuration :: Duration,
    gpLeadershipElectionNonce :: LeadershipElectionNonce,
    gpEpochLength :: EpochLength,
    gpFinalizationParameters :: FinalizationParameters,
    gpCryptographicParameters :: CryptographicParameters,
    gpIdentityProviders :: IdentityProviders,
    gpAnonymityRevokers :: AnonymityRevokers,
    -- |Initial accounts. Since an account can be a baker, it is important that the
    -- order of the accounts matches the assigned baker ids.
    gpInitialAccounts :: [GenesisAccount],
    -- |Maximum total energy that can be consumed by the transactions in a block
    gpMaxBlockEnergy :: Energy,
    -- |The initial update authorizations
    gpAuthorizations :: Authorizations,
    -- |The initial (updatable) chain parameters
    gpChainParameters :: GenesisChainParameters
}

instance FromJSON GenesisParametersV2 where
    parseJSON = withObject "GenesisParameters" $ \v -> do
        gpGenesisTime <- v .: "genesisTime"
        gpSlotDuration <- v .: "slotDuration"
        gpLeadershipElectionNonce <- v .: "leadershipElectionNonce"
        gpEpochLength <- Slot <$> v .: "epochLength"
        when(gpEpochLength == 0) $ fail "Epoch length should be non-zero"
        gpFinalizationParameters <- v .: "finalizationParameters"
        gpCryptographicParameters <- v .: "cryptographicParameters"
        gpIdentityProviders <- v .:? "identityProviders" .!= emptyIdentityProviders
        gpAnonymityRevokers <- v .:? "anonymityRevokers" .!= emptyAnonymityRevokers
        gpInitialAccounts <- v .:? "initialAccounts" .!= []
        let hasBaker GenesisAccount{gaBaker=Nothing} = False
            hasBaker _ = True
        unless (any hasBaker gpInitialAccounts) $ fail "Must have at least one baker at genesis"
        gpMaxBlockEnergy <- v .: "maxBlockEnergy"
        gpAuthorizations <- v .: "updateAuthorizations"
        gpChainParameters <- v .: "chainParameters"
        let facct = gcpFoundationAccount gpChainParameters
        unless (any ((facct ==) . gaAddress) gpInitialAccounts) $
          fail $ "Foundation account (" ++ show facct ++ ") is not in initialAccounts"
        return GenesisParametersV2{..}

type GenesisParameters = GenesisParametersV2

genesisParametersVersion :: Version
genesisParametersVersion = 2
-}

-- |Implementation-defined parameters, such as block size. They are not
-- protocol-level parameters hence do not fit into 'GenesisParameters'.
data RuntimeParameters = RuntimeParameters {
  -- |Maximum block size produced by the baker (in bytes). Note that this only
  -- applies to the blocks produced by this baker, we will still accept blocks
  -- of arbitrary size from other bakers.
  rpBlockSize :: !Int,
  -- |Treestate storage directory.
  rpTreeStateDir :: !FilePath,
  -- |BlockState storage file.
  rpBlockStateFile :: !FilePath,
  -- |Threshold for how far into the future we accept blocks. Blocks with a slot
  -- time that exceeds our current time + this threshold are rejected and the p2p
  -- is told to not relay these blocks.
  rpEarlyBlockThreshold :: !Timestamp,
  -- |Number of insertions to be performed in the transaction table before running
  -- a purge to remove long living transactions that have not been executed for more
  -- than `rpTransactionsKeepAliveTime` seconds.
  rpInsertionsBeforeTransactionPurge :: !Int,
  -- |Number of seconds after receiving a transaction during which it is kept in the
  -- transaction table if a purge is executed.
  rpTransactionsKeepAliveTime :: !TransactionTime,
  -- |Number of seconds between automatic transaction table purging  runs.
  rpTransactionsPurgingDelay :: !Int
  }

-- |Default runtime parameters, block size = 10MB.
defaultRuntimeParameters :: RuntimeParameters
defaultRuntimeParameters = RuntimeParameters {
  rpBlockSize = 10 * 10^(6 :: Int), -- 10MB
  rpTreeStateDir = "treestate",
  rpBlockStateFile = "blockstate",
  rpEarlyBlockThreshold = 30, -- 30 seconds
  rpInsertionsBeforeTransactionPurge = 1000,
  rpTransactionsKeepAliveTime = 5 * 60, -- 5 min
  rpTransactionsPurgingDelay = 3 * 60 -- 3 min
  }

instance FromJSON RuntimeParameters where
  parseJSON = withObject "RuntimeParameters" $ \v -> do
    rpBlockSize <- v .: "blockSize"
    rpTreeStateDir <- v .: "treeStateDir"
    rpBlockStateFile <- v .: "blockStateFile"
    rpEarlyBlockThreshold <- v .: "earlyBlockThreshold"
    rpInsertionsBeforeTransactionPurge <- v .: "insertionsBeforeTransactionPurge"
    rpTransactionsKeepAliveTime <- (fromIntegral :: Int -> TransactionTime) <$> v .: "transactionsKeepAliveTime"
    rpTransactionsPurgingDelay <- v .: "transactionsPurgingDelay"
    when (rpBlockSize <= 0) $
      fail "Block size must be a positive integer."
    when (rpEarlyBlockThreshold <= 0) $
      fail "The early block threshold must be a postitive integer"
    return RuntimeParameters{..}

-- |Values of updates that are stored in update queues.
-- These are slightly different to the 'UpdatePayload' type,
-- specifically in that for the foundation account we store
-- the account index rather than the account address.
data UpdateValue
    -- |Updates to authorized update keys.
    = UVAuthorization !Authorizations
    -- |Protocol updates.
    | UVProtocol !ProtocolUpdate
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
    deriving (Eq, Show)
