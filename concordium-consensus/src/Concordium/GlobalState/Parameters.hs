{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Ratio
import Data.Word
import Lens.Micro.Platform

import Concordium.Common.Version
import Concordium.Types
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
import Concordium.Types.Updates

type CryptographicParameters = GlobalContext

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
    _cpBakerExtraCooldownEpochs :: !Epoch
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
    -> ChainParameters
makeChainParameters _cpElectionDifficulty _cpEuroPerEnergy _cpMicroGTUPerEuro _cpBakerExtraCooldownEpochs = ChainParameters{..}
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

instance Serialize ChainParameters where
  put ChainParameters{..} = do
    put _cpElectionDifficulty
    put _cpEuroPerEnergy
    put _cpMicroGTUPerEuro
    put _cpBakerExtraCooldownEpochs
  get = makeChainParameters <$> get <*> get <*> get <*> get

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

instance ToJSON ChainParameters where
  toJSON ChainParameters{..} = object [
      "electionDifficulty" AE..= _cpElectionDifficulty,
      "euroPerEnergy" AE..= _cpEuroPerEnergy,
      "microGTUPerEuro" AE..= _cpMicroGTUPerEuro,
      "bakerCooldownEpochs" AE..= _cpBakerExtraCooldownEpochs
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
    genesisMintPerSlot :: !Amount,
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
    genesisMintPerSlot <- get
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
    put genesisMintPerSlot
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
    gpMintPerSlot :: Amount,
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
        gpMintPerSlot <-  v .: "mintPerSlot"
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
  -- |Number of insertions to be performed in the trasnaction table before running
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
        genesisMintPerSlot = gpMintPerSlot
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisSeedState = SeedState.genesisSeedState gpLeadershipElectionNonce gpEpochLength

        mkAccount (GenesisAccount{..}, bid) =
          newAccount genesisCryptographicParameters gaVerifyKeys gaAddress gaCredential
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
        genesisAccounts = map mkAccount (zip gpInitialAccounts [0..])
        genesisFinalizationParameters = gpFinalizationParameters
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders
        genesisAnonymityRevokers = gpAnonymityRevokers
        genesisMaxBlockEnergy = gpMaxBlockEnergy
        genesisAuthorizations = gpAuthorizations
        genesisChainParameters = gpChainParameters
