{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- |This module defines types for blockchain parameters, including genesis data,
-- baker parameters and finalization parameters.
module Concordium.GlobalState.Parameters(
    module Concordium.GlobalState.Parameters,
    BakerInfo
) where

import Prelude hiding (fail)
import GHC.Generics hiding (to)
import Data.Serialize
import Control.Monad.Fail
import Control.Monad hiding (fail)
import Data.Maybe
import Data.Ratio
import Data.Word
import Lens.Micro.Platform

import Concordium.Common.Version
import Concordium.Types
import Concordium.Utils
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.ID.Parameters(GlobalContext)
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import qualified Concordium.GlobalState.SeedState as SeedState
import qualified Concordium.ID.Types as ID
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), ToJSON(..), (.:), (.:?), (.!=), withObject, object)
import Data.Aeson.TH
import Concordium.Types.Updates

type CryptographicParameters = GlobalContext

-- |The distribution of newly-minted GTU among bakers, finalizers,
-- and the foundation account.  It must be the case that
-- @m_dBakingReward + _mdFinalizationReward <= 1@. The remaining
-- amount is the platform development charge.
data MintDistribution = MintDistribution {
    -- |BakingRewMintFrac: the fraction allocated to baker rewards
    _mdBakingReward :: !RewardFraction,
    -- |FinRewMintFrac: the fraction allocated to finalization rewards
    _mdFinalizationReward :: !RewardFraction
} deriving (Eq, Show)
makeClassy ''MintDistribution

instance ToJSON MintDistribution where
  toJSON MintDistribution{..} = object [
      "bakingReward" AE..= _mdBakingReward,
      "finalizationReward" AE..= _mdFinalizationReward
    ]
instance FromJSON MintDistribution where
  parseJSON = withObject "MintDistribution" $ \v -> do
    _mdBakingReward <- v .: "bakingReward"
    _mdFinalizationReward <- v .: "finalizationReward"
    unless (isJust (_mdBakingReward `addRewardFraction` _mdFinalizationReward)) $ fail "Reward fractions exceed 100%"
    return MintDistribution{..}

instance Serialize MintDistribution where
  put MintDistribution{..} = put _mdBakingReward >> put _mdFinalizationReward
  get = do
    _mdBakingReward <- get
    _mdFinalizationReward <- get
    unless (isJust (_mdBakingReward `addRewardFraction` _mdFinalizationReward)) $ fail "Reward fractions exceed 100%"
    return MintDistribution{..}

-- |The distribution of block transaction fees among the block
-- baker, the GAS account, and the foundation account.  It
-- must be the case that @_tfdBaker + _tfdGASAccount <= 1@.
-- The remaining amount is the TransChargeFrac (paid to the
-- foundation account).
data TransactionFeeDistribution = TransactionFeeDistribution {
    -- |BakerTransFrac: the fraction allocated to the baker
    _tfdBaker :: !RewardFraction,
    -- |The fraction allocated to the GAS account
    _tfdGASAccount :: !RewardFraction
} deriving (Eq, Show)
makeClassy ''TransactionFeeDistribution

instance ToJSON TransactionFeeDistribution where
  toJSON TransactionFeeDistribution{..} = object [
      "baker" AE..= _tfdBaker,
      "gasAccount" AE..= _tfdGASAccount
    ]
instance FromJSON TransactionFeeDistribution where
  parseJSON = withObject "TransactionFeeDistribution" $ \v -> do
    _tfdBaker <- v .: "baker"
    _tfdGASAccount <- v .: "gasAccount"
    unless (isJust (_tfdBaker `addRewardFraction` _tfdGASAccount)) $ fail "Transaction fee fractions exceed 100%"
    return TransactionFeeDistribution{..}

instance Serialize TransactionFeeDistribution where
  put TransactionFeeDistribution{..} = put _tfdBaker >> put _tfdGASAccount
  get = do
    _tfdBaker <- get
    _tfdGASAccount <- get
    unless (isJust (_tfdBaker `addRewardFraction` _tfdGASAccount)) $ fail "Transaction fee fractions exceed 100%"
    return TransactionFeeDistribution{..}

data GASRewards = GASRewards {
  -- |BakerPrevTransFrac: fraction paid to baker
  _gasBaker :: !RewardFraction,
  -- |FeeAddFinalisationProof: fraction paid for including a
  -- finalization proof in a block.
  _gasFinalizationProof :: !RewardFraction,
  -- |FeeAccountCreation: fraction paid for including each
  -- account creation transaction in a block.
  _gasAccountCreation :: !RewardFraction,
  -- |FeeUpdate: fraction paid for including an update
  -- transaction in a block.
  _gasChainUpdate :: !RewardFraction
} deriving (Eq, Show)
makeClassy ''GASRewards

$(deriveJSON AE.defaultOptions{AE.fieldLabelModifier = firstLower . drop 3} ''GASRewards)

instance Serialize GASRewards where
  put GASRewards{..} = do
    put _gasBaker
    put _gasFinalizationProof
    put _gasAccountCreation
    put _gasChainUpdate
  get = do
    _gasBaker <- get
    _gasFinalizationProof <- get
    _gasAccountCreation <- get
    _gasChainUpdate <- get
    return GASRewards{..}

-- |Parameters affecting rewards.
-- It must be that @rpBakingRewMintFrac + rpFinRewMintFrac < 1@
data RewardParameters = RewardParameters {
    -- |Per slot mint rate.
    _rpMintPerSlot :: !MintRate,
    -- |Distribution of newly-minted GTUs.
    _rpMintDistribution :: !MintDistribution,
    -- |Distribution of transaction fees.
    _rpTransactionFeeDistribution :: !TransactionFeeDistribution,
    -- |Rewards paid from the GAS account.
    _rpGASRewards :: !GASRewards
} deriving (Eq, Show)
makeClassy ''RewardParameters

instance HasMintDistribution RewardParameters where
  mintDistribution = rpMintDistribution

instance HasTransactionFeeDistribution RewardParameters where
  transactionFeeDistribution = rpTransactionFeeDistribution

instance HasGASRewards RewardParameters where
  gASRewards = rpGASRewards

$(deriveJSON AE.defaultOptions{AE.fieldLabelModifier = firstLower . drop 3} ''RewardParameters)

instance Serialize RewardParameters where
  put RewardParameters{..} = do
    put _rpMintPerSlot
    put _rpMintDistribution
    put _rpTransactionFeeDistribution
    put _rpGASRewards
  get = do
    _rpMintPerSlot <- get
    _rpMintDistribution <- get
    _rpTransactionFeeDistribution <- get
    _rpGASRewards <- get
    return RewardParameters{..}

-- |Updatable chain parameters.
data ChainParameters = ChainParameters {
    -- |Election difficulty parameter.
    _cpElectionDifficulty :: !ElectionDifficulty,
    -- |Euro:Energy rate.
    _cpEuroPerEnergy :: !ExchangeRate,
    -- |uGTU:Euro rate.
    _cpMicroGTUPerEuro :: !ExchangeRate,
    -- |uGTU:Energy rate.
    -- This is derived, but will be computed when the other
    -- rates are updated since it is more useful.
    _cpEnergyRate :: !EnergyRate,
    -- |Number of additional epochs that bakers must cool down when
    -- removing stake. The cool-down will effectively be 2 epochs
    -- longer than this value, since at any given time, the bakers
    -- (and stakes) for the current and next epochs have already
    -- been determined.
    _cpBakerExtraCooldownEpochs :: !Epoch,
    -- |LimitAccountCreation: the maximum number of accounts
    -- that may be created in one block.
    _cpAccountCreationLimit :: !CredentialsPerBlockLimit,
    -- |Reward parameters.
    _cpRewardParameters :: !RewardParameters,
    -- |Foundation account index.
    _cpFoundationAccount :: !AccountIndex
} deriving (Eq, Show)

makeChainParameters ::
    ElectionDifficulty
    -- ^Election difficulty
    -> ExchangeRate
    -- ^Euro:Energy rate
    -> ExchangeRate
    -- ^uGTU:Euro rate
    -> Epoch
    -- ^Baker cooldown
    -> CredentialsPerBlockLimit
    -- ^Account creation limit
    -> RewardParameters
    -- ^Reward parameters
    -> AccountIndex
    -- ^Foundation account
    -> ChainParameters
makeChainParameters
    _cpElectionDifficulty
    _cpEuroPerEnergy
    _cpMicroGTUPerEuro
    _cpBakerExtraCooldownEpochs
    _cpAccountCreationLimit
    _cpRewardParameters
    _cpFoundationAccount
      = ChainParameters{..}
  where
    _cpEnergyRate = computeEnergyRate _cpMicroGTUPerEuro _cpEuroPerEnergy

{-# INLINE cpElectionDifficulty #-}
cpElectionDifficulty :: Lens' ChainParameters ElectionDifficulty
cpElectionDifficulty = lens _cpElectionDifficulty (\cp ed -> cp {_cpElectionDifficulty = ed})

{-# INLINE cpEuroPerEnergy #-}
cpEuroPerEnergy :: Lens' ChainParameters ExchangeRate
cpEuroPerEnergy = lens _cpEuroPerEnergy (\cp epe -> cp {_cpEuroPerEnergy = epe, _cpEnergyRate = computeEnergyRate (_cpMicroGTUPerEuro cp) epe})

{-# INLINE cpMicroGTUPerEuro #-}
cpMicroGTUPerEuro :: Lens' ChainParameters ExchangeRate
cpMicroGTUPerEuro = lens _cpMicroGTUPerEuro (\cp mgtupe -> cp {_cpMicroGTUPerEuro = mgtupe, _cpEnergyRate = computeEnergyRate mgtupe (_cpEuroPerEnergy cp)})

{-# INLINE cpEnergyRate #-}
cpEnergyRate :: SimpleGetter ChainParameters EnergyRate
cpEnergyRate = to _cpEnergyRate

{-# INLINE cpBakerExtraCooldownEpochs #-}
cpBakerExtraCooldownEpochs :: Lens' ChainParameters Epoch
cpBakerExtraCooldownEpochs = lens _cpBakerExtraCooldownEpochs (\cp bce -> cp {_cpBakerExtraCooldownEpochs = bce})

{-# INLINE cpFoundationAccount #-}
cpFoundationAccount :: Lens' ChainParameters AccountIndex
cpFoundationAccount = lens _cpFoundationAccount (\cp fa -> cp {_cpFoundationAccount = fa})

{-# INLINE cpAccountCreationLimit #-}
cpAccountCreationLimit :: Lens' ChainParameters CredentialsPerBlockLimit
cpAccountCreationLimit = lens _cpAccountCreationLimit (\cp acl -> cp {_cpAccountCreationLimit = acl})

instance HasRewardParameters ChainParameters where
  rewardParameters = lens _cpRewardParameters (\cp rp -> cp {_cpRewardParameters = rp})

instance Serialize ChainParameters where
  put ChainParameters{..} = do
    put _cpElectionDifficulty
    put _cpEuroPerEnergy
    put _cpMicroGTUPerEuro
    put _cpBakerExtraCooldownEpochs
    put _cpAccountCreationLimit
    put _cpRewardParameters
    put _cpFoundationAccount
  get = makeChainParameters <$> get <*> get <*> get <*> get <*> get <*> get <*> get

instance HashableTo Hash.Hash ChainParameters where
  getHash = Hash.hash . encode

instance Monad m => MHashableTo m Hash.Hash ChainParameters

instance FromJSON ChainParameters where
  parseJSON = withObject "ChainParameters" $ \v ->
    makeChainParameters
      <$> v .: "electionDifficulty"
      <*> v .: "euroPerEnergy"
      <*> v .: "microGTUPerEuro"
      <*> v .: "bakerCooldownEpochs"
      <*> v .: "accountCreationLimit"
      <*> v .: "rewardParameters"
      <*> v .: "foundationAccountIndex"

instance ToJSON ChainParameters where
  toJSON ChainParameters{..} = object [
      "electionDifficulty" AE..= _cpElectionDifficulty,
      "euroPerEnergy" AE..= _cpEuroPerEnergy,
      "microGTUPerEuro" AE..= _cpMicroGTUPerEuro,
      "bakerCooldownEpochs" AE..= _cpBakerExtraCooldownEpochs,
      "accountCreationLimit" AE..= _cpAccountCreationLimit,
      "rewardParameters" AE..= _cpRewardParameters,
      "foundationAccountIndex" AE..= _cpFoundationAccount
    ]

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

data GenesisDataV2 = GenesisDataV2 {
    genesisTime :: !Timestamp,
    genesisSlotDuration :: !Duration,
    genesisSeedState :: !SeedState.SeedState,
    genesisAccounts :: ![Account],
    genesisFinalizationParameters :: !FinalizationParameters,
    genesisCryptographicParameters :: !CryptographicParameters,
    genesisIdentityProviders :: !IdentityProviders,
    genesisAnonymityRevokers :: !AnonymityRevokers,
    genesisMaxBlockEnergy :: !Energy,
    genesisAuthorizations :: !Authorizations,
    genesisChainParameters :: !ChainParameters
} deriving (Generic, Show, Eq)

getGenesisDataV2 :: Get GenesisDataV2
getGenesisDataV2 = do
    genesisTime <- get
    genesisSlotDuration <- get
    genesisSeedState <- get
    genesisAccounts <- get
    -- Verify that each baker account records the correct baker id
    forM_ (zip [0..] genesisAccounts) $ \(i, acct) -> case _accountBaker acct of
        Just ab | _bakerIdentity (_accountBakerInfo ab) /= i ->
          fail "BakerId does not match account index"
        _ -> return ()
    genesisFinalizationParameters <- get
    genesisCryptographicParameters <- get
    genesisIdentityProviders <- get
    genesisAnonymityRevokers <- get
    genesisMaxBlockEnergy <- get
    genesisAuthorizations <- get
    genesisChainParameters <- get
    return GenesisDataV2{..}

putGenesisDataV2 :: Putter GenesisDataV2
putGenesisDataV2 GenesisDataV2{..} = do
    put genesisTime
    put genesisSlotDuration
    put genesisSeedState
    put genesisAccounts
    put genesisFinalizationParameters
    put genesisCryptographicParameters
    put genesisIdentityProviders
    put genesisAnonymityRevokers
    put genesisMaxBlockEnergy
    put genesisAuthorizations
    put genesisChainParameters


instance Serialize GenesisDataV2 where
  get = getGenesisDataV2
  put = putGenesisDataV2

type GenesisData = GenesisDataV2
genesisDataVersion :: Version
genesisDataVersion = 2

-- |Deserialize genesis data.
-- Read the version and decide how to parse the remaining data based on the
-- version.
--
-- Currently only supports version 1
getExactVersionedGenesisData :: Get GenesisData
getExactVersionedGenesisData =
  getVersion >>= \case
    2 -> getGenesisDataV2
    n -> fail $ "Unsupported Genesis version: " ++ show n

-- |Serialize the genesis data with a version according to the V1 format.
-- In contrast to 'putGenesisDataV1' this function also prepends the version.
putVersionedGenesisDataV2 :: GenesisData -> Put
putVersionedGenesisDataV2 fpm = putVersion 2 <> putGenesisDataV2 fpm

-- |Get the total amount of GTU in genesis data.
genesisTotalGTU :: GenesisData -> Amount
genesisTotalGTU GenesisDataV2{..} =
  sum (_accountAmount <$> genesisAccounts)

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
    -- |The baker's initial stake
    gbStake :: Amount,
    -- |Whether to restake the baker's earnings from rewards
    gbRestakeEarnings :: Bool
}

instance FromJSON GenesisBaker where
    parseJSON = withObject "GenesisBaker" $ \v -> do
            gbElectionVerifyKey <- v .: "electionVerifyKey"
            gbSignatureVerifyKey <- v .: "signatureVerifyKey"
            gbAggregationVerifyKey <- v .: "aggregationVerifyKey"
            gbStake <- v .: "stake"
            gbRestakeEarnings <- v .: "restakeEarnings"
            return GenesisBaker{..}

-- |'GenesisAccount' are special account existing in the genesis block, in
-- addition to baker accounts which are defined by the 'GenesisBaker' structure.
data GenesisAccount = GenesisAccount {
  gaAddress :: !AccountAddress,
  gaVerifyKeys :: !ID.AccountKeys,
  gaBalance :: !Amount,
  gaCredential :: !ID.AccountCredentialWithProofs,
  gaBaker :: !(Maybe GenesisBaker)
}

instance FromJSON GenesisAccount where
  parseJSON = withObject "GenesisAccount" $ \obj -> do
    gaAddress <- obj .: "address"
    gaVerifyKeys <- obj .: "accountKeys"
    gaBalance <- obj .: "balance"
    Versioned{..} <- obj .: "credential"
    unless (vVersion == 0) $ fail "Only V0 credentials supported in genesis."
    gaCredential <- parseJSON vValue
    gaBaker <- obj .:? "baker"
    -- Check that bakers do not stake more than their balance.
    case gaBaker of
      Just gb | gbStake gb > gaBalance -> fail "Stake exceeds balance"
      _ -> return ()
    return GenesisAccount{..}

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
    -- |Initial accounts
    gpInitialAccounts :: [GenesisAccount],
    -- |Maximum total energy that can be consumed by the transactions in a block
    gpMaxBlockEnergy :: Energy,
    -- |The initial update authorizations
    gpAuthorizations :: Authorizations,
    -- |The initial (updatable) chain parameters
    gpChainParameters :: ChainParameters
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
        return GenesisParametersV2{..}

type GenesisParameters = GenesisParametersV2

genesisParametersVersion :: Version
genesisParametersVersion = 2

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

-- |NB: This function will silently ignore bakers with duplicate signing keys.
parametersToGenesisData :: GenesisParameters -> GenesisData
parametersToGenesisData GenesisParametersV2{..} = GenesisDataV2{..}
    where
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisSeedState = SeedState.genesisSeedState gpLeadershipElectionNonce gpEpochLength

        mkAccount GenesisAccount{..} bid =
          let cdv = ID.values gaCredential in
          newAccount genesisCryptographicParameters gaVerifyKeys gaAddress cdv
                & accountAmount .~ gaBalance
                & case gaBaker of
                    Nothing -> id
                    Just GenesisBaker{..} -> accountBaker ?~ AccountBaker {
                      _stakedAmount = gbStake,
                      _stakeEarnings = gbRestakeEarnings,
                      _accountBakerInfo = BakerInfo {
                        _bakerIdentity = bid,
                        _bakerSignatureVerifyKey = gbSignatureVerifyKey,
                        _bakerElectionVerifyKey = gbElectionVerifyKey,
                        _bakerAggregationVerifyKey = gbAggregationVerifyKey
                      },
                      _bakerPendingChange = NoChange
                    }
        genesisAccounts = zipWith mkAccount gpInitialAccounts [0..]
        genesisFinalizationParameters = gpFinalizationParameters
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders
        genesisAnonymityRevokers = gpAnonymityRevokers
        genesisMaxBlockEnergy = gpMaxBlockEnergy
        genesisAuthorizations = gpAuthorizations
        genesisChainParameters = gpChainParameters
