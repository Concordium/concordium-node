{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
-- |This module defines types for blockchain parameters, including genesis data,
-- baker parameters and finalization parameters.
module Concordium.GlobalState.Parameters(
    module Concordium.GlobalState.Parameters,
    BakerInfo,
    BakerCreationInfo(..)
) where

import Prelude hiding (fail)
import GHC.Generics
import Data.Serialize
import Control.Monad.Fail
import Control.Monad hiding (fail)
import Data.Ratio
import Data.Word

import Concordium.Types
import Concordium.ID.Parameters(GlobalContext)
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders
import qualified Concordium.GlobalState.SeedState as SeedState
import qualified Concordium.ID.Types as ID
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Data.PQueue.Prio.Max as Queue
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), (.:), (.:?), (.!=), withObject)
import Concordium.Types.Transactions

type CryptographicParameters = GlobalContext

data VoterInfo = VoterInfo {
    voterVerificationKey :: VoterVerificationKey,
    voterVRFKey :: VoterVRFPublicKey,
    voterPower :: VoterPower,
    voterBlsKey :: Bls.PublicKey
} deriving (Eq, Generic, Show)
instance Serialize VoterInfo where

data FinalizationParameters = FinalizationParameters {
    finalizationMinimumSkip :: BlockHeight,
    finalizationCommitteeMaxSize :: FinalizationCommitteeSize,
    finalizationWaitingTime :: Duration,
    finalizationIgnoreFirstWait :: Bool,
    finalizationOldStyleSkip :: Bool,
    finalizationSkipShrinkFactor :: Ratio Word64,
    finalizationSkipGrowFactor :: Ratio Word64,
    finalizationDelayShrinkFactor :: Ratio Word64,
    finalizationDelayGrowFactor :: Ratio Word64,
    finalizationAllowZeroDelay :: Bool
} deriving (Eq, Generic, Show)
instance Serialize FinalizationParameters where

instance FromJSON FinalizationParameters where
    parseJSON = withObject "FinalizationParameters" $ \v -> do
        finalizationMinimumSkip <- BlockHeight <$> v .: "minimumSkip"
        finalizationCommitteeMaxSize <- v .: "committeeMaxSize"
        finalizationWaitingTime <- v .: "waitingTime"
        finalizationIgnoreFirstWait <- v .:? "ignoreFirstWait" .!= False
        finalizationOldStyleSkip <- v .:? "oldStyleSkip" .!= False
        finalizationSkipShrinkFactor <- v .: "skipShrinkFactor"
        unless (finalizationSkipShrinkFactor > 0 && finalizationSkipShrinkFactor < 1) $
          fail "skipShrinkFactor must be strictly between 0 and 1"
        finalizationSkipGrowFactor <- v .: "skipGrowFactor"
        unless (finalizationSkipGrowFactor > 1) $
          fail "skipGrowFactor must be strictly greater than 1"
        finalizationDelayShrinkFactor <- v .: "delayShrinkFactor"
        unless (finalizationDelayShrinkFactor > 0 && finalizationDelayShrinkFactor < 1) $
          fail "delayShrinkFactor must be strictly between 0 and 1"
        finalizationDelayGrowFactor <- v .: "delayGrowFactor"
        unless (finalizationDelayGrowFactor > 1) $
          fail "delayGrowFactor must be strictly greater than 1"
        finalizationAllowZeroDelay <- v .:? "allowZeroDelay" .!= False
        return FinalizationParameters{..}

data GenesisData = GenesisData {
    genesisTime :: !Timestamp,
    genesisSlotDuration :: !Duration,
    genesisBakers :: !Bakers,
    genesisSeedState :: !SeedState.SeedState,
    genesisElectionDifficulty :: !ElectionDifficulty,
    genesisAccounts :: ![Account],
    -- |Special accounts that will have additional rights initially.
    genesisControlAccounts :: ![Account],
    genesisFinalizationParameters :: !FinalizationParameters,
    genesisCryptographicParameters :: !CryptographicParameters,
    genesisIdentityProviders :: ![IpInfo],
    genesisMintPerSlot :: !Amount,
    genesisMaxBlockEnergy :: !Energy
} deriving (Generic, Show, Eq)

instance Serialize GenesisData where

instance Serialize GenesisData where
    put GenesisData{..} = do
        put __versionGenesisData
        put genesisTime
        put genesisSlotDuration
        put genesisBakers
        put genesisSeedState
        put genesisElectionDifficulty
        put genesisAccounts
        put genesisControlAccounts
        put genesisFinalizationParameters
        put genesisCryptographicParameters
        put genesisIdentityProviders
        put genesisMintPerSlot
        put genesisMaxBlockEnergy

    get = do
      version <- Version <$> S.get
      if version /= __versionGenesisData then fail "Invalid genesis data version"
      else do
        genesisTime <- S.get
        genesisSlotDuration <- S.get
        genesisBakers <- S.get
        genesisSeedState <- S.get
        genesisElectionDifficulty <- S.get
        genesisAccounts <- S.get
        genesisControlAccounts <- S.get
        genesisFinalizationParameters <- S.get
        genesisCryptographicParameters <- S.get
        genesisIdentityProviders <- S.get
        genesisMintPerSlot <- S.get
        genesisMaxBlockEnergy <- S.get
        return $! GenesisData{..}




genesisTotalGTU :: GenesisData -> Amount
genesisTotalGTU GenesisData{..} =
  sum (_accountAmount <$> (genesisAccounts ++ genesisControlAccounts))

readIdentityProviders :: BSL.ByteString -> Maybe [IpInfo]
readIdentityProviders = AE.decode

eitherReadIdentityProviders :: BSL.ByteString -> Either String [IpInfo]
eitherReadIdentityProviders = AE.eitherDecode

readCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
readCryptographicParameters = AE.decode

-- 'GenesisBaker' is an abstraction of a baker at genesis.
-- It includes the minimal information for generating a
-- baker and its account.
data GenesisBaker = GenesisBaker {
    -- |The baker's public VRF key
    gbElectionVerifyKey :: BakerElectionVerifyKey,
    -- |The baker's public signature key
    gbSignatureVerifyKey :: BakerSignVerifyKey,
    -- |The baker's public key for aggregate signatures
    gbAggregationVerifyKey :: BakerAggregationVerifyKey,
    -- |Baker's account (public data only).
    gbAccount :: GenesisAccount,
    -- |Whether the baker should be included in the initial
    -- finalization committee.
    gbFinalizer :: Bool
}

instance FromJSON GenesisBaker where
    parseJSON = withObject "GenesisBaker" $ \v -> do
            gbElectionVerifyKey <- v .: "electionVerifyKey"
            gbSignatureVerifyKey <- v .: "signatureVerifyKey"
            gbAggregationVerifyKey <- v .: "aggregationVerifyKey"
            gbAccount <- v .: "account"
            gbFinalizer <- v .: "finalizer"
            return GenesisBaker{..}

-- |'GenesisAccount' are special account existing in the genesis block, in
-- addition to baker accounts which are defined by the 'GenesisBaker' structure.
data GenesisAccount = GenesisAccount {
  gaAddress :: !AccountAddress,
  gaVerifyKeys :: !ID.AccountKeys,
  gaBalance :: !Amount,
  gaCredential :: !ID.CredentialDeploymentInformation
}

instance FromJSON GenesisAccount where
  parseJSON = withObject "GenesisAccount" $ \obj -> do
    gaAddress <- obj .: "address"
    gaVerifyKeys <- obj .: "accountKeys"
    gaBalance <- Amount <$> obj .: "balance"
    gaCredential <- obj .: "credential"
    return GenesisAccount{..}

-- 'GenesisParameters' provides a convenient abstraction for
-- constructing 'GenesisData'.
-- FIXME: We should refactor this so that we have a single list of accounts
-- which can delegate to whoever they wish, and then we calculate initial balances
-- for bakers.
data GenesisParameters = GenesisParameters {
    gpGenesisTime :: Timestamp,
    gpSlotDuration :: Duration,
    gpLeadershipElectionNonce :: LeadershipElectionNonce,
    gpEpochLength :: EpochLength,
    gpElectionDifficulty :: ElectionDifficulty,
    gpFinalizationParameters :: FinalizationParameters,
    gpBakers :: [GenesisBaker],
    gpCryptographicParameters :: CryptographicParameters,
    gpIdentityProviders :: [IpInfo],
    -- |Additional accounts (not baker accounts and not control accounts).
    -- They cannot delegate to any bakers in genesis.
    gpInitialAccounts :: [GenesisAccount],
    -- |Control accounts which have additional rights to update parameters.
    -- They cannot delegate stake to any bakers in genesis.
    gpControlAccounts :: [GenesisAccount],
    gpMintPerSlot :: Amount,
    -- |Maximum total energy that can be consumed by the transactions in a block
    gpMaxBlockEnergy :: Energy
}

instance FromJSON GenesisParameters where
    parseJSON = withObject "GenesisParameters" $ \v -> do
        gpGenesisTime <- v .: "genesisTime"
        gpSlotDuration <- v .: "slotDuration"
        gpLeadershipElectionNonce <- v .: "leadershipElectionNonce"
        gpEpochLength <- Slot <$> v .: "epochLength"
        when(gpEpochLength == 0) $ fail "Epoch length should be non-zero"
        gpElectionDifficulty <- v .: "electionDifficulty"
        gpFinalizationParameters <- v .: "finalizationParameters"
        gpBakers <- v .: "bakers"
        when (null gpBakers) $ fail "There should be at least one baker."
        gpCryptographicParameters <- v .: "cryptographicParameters"
        gpIdentityProviders <- v .:? "identityProviders" .!= []
        gpInitialAccounts <- v .:? "initialAccounts" .!= []
        gpControlAccounts <- v .:? "controlAccounts" .!= []
        gpMintPerSlot <- Amount <$> v .: "mintPerSlot"
        gpMaxBlockEnergy <- v .: "maxBlockEnergy"
        return GenesisParameters{..}

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
  -- |Number of insertions to be performed in the trasnaction table before running
  -- a purge to remove long living transactions that have not been executed for more
  -- than `rpTransactionsKeepAliveTime` seconds.
  rpInsertionsBeforeTransactionPurge :: !Int,
  -- |Number of seconds after receiving a transction during which it is kept in the
  -- transaction table if a purge is executed.
  rpTransactionsKeepAliveTime :: !TransactionTime
  }

-- |Default runtime parameters, block size = 10MB.
defaultRuntimeParameters :: RuntimeParameters
defaultRuntimeParameters = RuntimeParameters {
  rpBlockSize = 10 * 10^(6 :: Int), -- 10MB
  rpTreeStateDir = "treestate",
  rpBlockStateFile = "blockstate",
  rpEarlyBlockThreshold = 30, -- 30 seconds
  rpInsertionsBeforeTransactionPurge = 1000,
  rpTransactionsKeepAliveTime = 5 * 60 -- 5 min
  }

instance FromJSON RuntimeParameters where
  parseJSON = withObject "RuntimeParameters" $ \v -> do
    rpBlockSize <- v .: "blockSize"
    rpTreeStateDir <- v .: "treeStateDir"
    rpBlockStateFile <- v .: "blockStateFile"
    rpEarlyBlockThreshold <- v .: "earlyBlockThreshold"
    rpInsertionsBeforeTransactionPurge <- v .: "insertionsBeforeTransactionPurge"
    rpTransactionsKeepAliveTime <- (fromIntegral :: Int -> TransactionTime) <$> v .: "transactionsKeepAliveTime"
    when (rpBlockSize <= 0) $
      fail "Block size must be a positive integer."
    when (rpEarlyBlockThreshold <= 0) $
      fail "The early block threshold must be a postitive integer"
    return RuntimeParameters{..}

-- |NB: This function will silently ignore bakers with duplicate signing keys.
parametersToGenesisData :: GenesisParameters -> GenesisData
parametersToGenesisData GenesisParameters{..} = GenesisData{..}
    where
        genesisMintPerSlot = gpMintPerSlot
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisBakers = fst (bakersFromList (mkBaker <$> gpBakers))
        genesisSeedState = SeedState.genesisSeedState gpLeadershipElectionNonce gpEpochLength
        genesisElectionDifficulty = gpElectionDifficulty
        mkBaker GenesisBaker{..} = BakerInfo
                gbElectionVerifyKey
                gbSignatureVerifyKey
                gbAggregationVerifyKey
                (gaBalance gbAccount)
                (gaAddress gbAccount)

        mkAccount GenesisAccount{..} =
          let cdv = ID.cdiValues gaCredential in
          (newAccount gaVerifyKeys gaAddress (ID.cdvRegId cdv))
                {_accountAmount = gaBalance,
                 _accountCredentials = Queue.singleton (ID.pValidTo (ID.cdvPolicy cdv)) cdv
                }
        -- special accounts will have some special privileges during beta.
        genesisControlAccounts = map mkAccount gpControlAccounts
        -- Baker accounts will have no special privileges.
        -- We ignore any specified delegation target.
        genesisAccounts = [(mkAccount gbAccount) {_accountStakeDelegate = Just bid }
                          | (GenesisBaker{..}, bid) <- zip gpBakers [0..]]
                          -- and add any other initial accounts.
                          ++ map mkAccount gpInitialAccounts
        genesisFinalizationParameters = gpFinalizationParameters
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders
        genesisMaxBlockEnergy = gpMaxBlockEnergy
