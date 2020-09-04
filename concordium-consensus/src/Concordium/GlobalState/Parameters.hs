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
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import qualified Concordium.GlobalState.SeedState as SeedState
import qualified Concordium.ID.Types as ID
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as AE
import Data.Aeson.Types (FromJSON(..), (.:), (.:?), (.!=), withObject)
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
    _cpEnergyRate :: !EnergyRate
} deriving (Eq, Show)

makeChainParameters ::
    ElectionDifficulty
    -- ^Election difficulty
    -> ExchangeRate
    -- ^Euro:Energy rate
    -> ExchangeRate
    -- ^uGTU:Euro rate
    -> ChainParameters
makeChainParameters _cpElectionDifficulty _cpEuroPerEnergy _cpMicroGTUPerEuro = ChainParameters{..}
  where
    _cpEnergyRate = computeEnergyRate _cpMicroGTUPerEuro _cpEuroPerEnergy

cpElectionDifficulty :: Lens' ChainParameters ElectionDifficulty
cpElectionDifficulty = lens _cpElectionDifficulty (\cp ed -> cp {_cpElectionDifficulty = ed})

cpEuroPerEnergy :: Lens' ChainParameters ExchangeRate
cpEuroPerEnergy = lens _cpEuroPerEnergy (\cp epe -> cp {_cpEuroPerEnergy = epe, _cpEnergyRate = computeEnergyRate (_cpMicroGTUPerEuro cp) epe})

cpMicroGTUPerEuro :: Lens' ChainParameters ExchangeRate
cpMicroGTUPerEuro = lens _cpMicroGTUPerEuro (\cp mgtupe -> cp {_cpMicroGTUPerEuro = mgtupe, _cpEnergyRate = computeEnergyRate mgtupe (_cpEuroPerEnergy cp)})

cpEnergyRate :: SimpleGetter ChainParameters EnergyRate
cpEnergyRate = to _cpEnergyRate

instance Serialize ChainParameters where
  put ChainParameters{..} = do
    put _cpElectionDifficulty
    put _cpEuroPerEnergy
    put _cpMicroGTUPerEuro
  get = makeChainParameters <$> get <*> get <*> get

instance HashableTo Hash.Hash ChainParameters where
  getHash = Hash.hash . encode

instance Monad m => MHashableTo m Hash.Hash ChainParameters

instance FromJSON ChainParameters where
  parseJSON = withObject "ChainParameters" $ \v ->
    makeChainParameters
      <$> v .: "electionDifficulty"
      <*> v .: "euroPerEnergy"
      <*> v .: "microGTUPerEnergy"

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

data GenesisDataV1 = GenesisDataV1 {
    genesisTime :: !Timestamp,
    genesisSlotDuration :: !Duration,
    genesisBakers :: !Bakers,
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

getGenesisDataV1 :: Get GenesisDataV1
getGenesisDataV1 = do
    genesisTime <- get
    genesisSlotDuration <- get
    genesisBakers <- get
    genesisSeedState <- get
    genesisAccounts <- get
    genesisFinalizationParameters <- get
    genesisCryptographicParameters <- get
    genesisIdentityProviders <- get
    genesisAnonymityRevokers <- get
    genesisMintPerSlot <- get
    genesisMaxBlockEnergy <- get
    genesisAuthorizations <- get
    genesisChainParameters <- get
    return GenesisDataV1{..}

putGenesisDataV1 :: Putter GenesisDataV1
putGenesisDataV1 GenesisDataV1{..} = do
    put genesisTime
    put genesisSlotDuration
    put genesisBakers
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


instance Serialize GenesisDataV1 where
  get = getGenesisDataV1
  put = putGenesisDataV1

type GenesisData = GenesisDataV1
genesisDataVersion :: Version
genesisDataVersion = 1

-- |Deserialize genesis data.
-- Read the version and decide how to parse the remaining data based on the
-- version.
--
-- Currently only supports version 1
getExactVersionedGenesisData :: Get GenesisData
getExactVersionedGenesisData =
  getVersion >>= \case
    1 -> getGenesisDataV1
    n -> fail $ "Unsupported Genesis version: " ++ show n

-- |Serialize the genesis data with a version according to the V1 format.
-- In contrast to 'putGenesisDataV1' this function also prepends the version.
putVersionedGenesisDataV1 :: GenesisData -> Put
putVersionedGenesisDataV1 fpm = putVersion 1 <> putGenesisDataV1 fpm

-- |Get the total amount of GTU in genesis data.
genesisTotalGTU :: GenesisData -> Amount
genesisTotalGTU GenesisDataV1{..} =
  sum (_accountAmount <$> genesisAccounts)

readIdentityProviders :: BSL.ByteString -> Maybe [IpInfo]
readIdentityProviders = AE.decode

readAnonymityRevokers :: BSL.ByteString -> Maybe AnonymityRevokers
readAnonymityRevokers = AE.decode

eitherReadIdentityProviders :: BSL.ByteString -> Either String [IpInfo]
eitherReadIdentityProviders = AE.eitherDecode

eitherReadAnonymityRevokers :: BSL.ByteString -> Either String AnonymityRevokers
eitherReadAnonymityRevokers = AE.eitherDecode

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
    Versioned{..} <- obj .: "credential"
    unless (vVersion == 0) $ fail "Only V0 credentials supported in genesis."
    gaCredential <- parseJSON vValue
    return GenesisAccount{..}

-- 'GenesisParameters' provides a convenient abstraction for
-- constructing 'GenesisData'.
-- FIXME: We should refactor this so that we have a single list of accounts
-- which can delegate to whoever they wish, and then we calculate initial balances
-- for bakers.
data GenesisParametersV1 = GenesisParametersV1 {
    gpGenesisTime :: Timestamp,
    gpSlotDuration :: Duration,
    gpLeadershipElectionNonce :: LeadershipElectionNonce,
    gpEpochLength :: EpochLength,
    gpFinalizationParameters :: FinalizationParameters,
    gpBakers :: [GenesisBaker],
    gpCryptographicParameters :: CryptographicParameters,
    gpIdentityProviders :: IdentityProviders,
    gpAnonymityRevokers :: AnonymityRevokers,
    -- |Additional accounts (not baker accounts).
    -- They cannot delegate to any bakers in genesis.
    gpInitialAccounts :: [GenesisAccount],
    gpMintPerSlot :: Amount,
    -- |Maximum total energy that can be consumed by the transactions in a block
    gpMaxBlockEnergy :: Energy,
    -- |The initial update authorizations
    gpAuthorizations :: Authorizations,
    -- |The initial (updatable) chain parameters
    gpChainParameters :: ChainParameters
}

instance FromJSON GenesisParametersV1 where
    parseJSON = withObject "GenesisParameters" $ \v -> do
        gpGenesisTime <- v .: "genesisTime"
        gpSlotDuration <- v .: "slotDuration"
        gpLeadershipElectionNonce <- v .: "leadershipElectionNonce"
        gpEpochLength <- Slot <$> v .: "epochLength"
        when(gpEpochLength == 0) $ fail "Epoch length should be non-zero"
        gpFinalizationParameters <- v .: "finalizationParameters"
        gpBakers <- v .: "bakers"
        when (null gpBakers) $ fail "There should be at least one baker."
        gpCryptographicParameters <- v .: "cryptographicParameters"
        gpIdentityProviders <- v .:? "identityProviders" .!= emptyIdentityProviders
        gpAnonymityRevokers <- v .:? "anonymityRevokers" .!= emptyAnonymityRevokers
        gpInitialAccounts <- v .:? "initialAccounts" .!= []
        gpMintPerSlot <- Amount <$> v .: "mintPerSlot"
        gpMaxBlockEnergy <- v .: "maxBlockEnergy"
        gpAuthorizations <- v .: "updateAuthorizations"
        gpChainParameters <- v .: "chainParameters"
        return GenesisParametersV1{..}

type GenesisParameters = GenesisParametersV1

genesisParametersVersion :: Version
genesisParametersVersion = 1

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
parametersToGenesisData GenesisParametersV1{..} = GenesisDataV1{..}
    where
        genesisMintPerSlot = gpMintPerSlot
        genesisTime = gpGenesisTime
        genesisSlotDuration = gpSlotDuration
        genesisBakers = fst (bakersFromList (mkBaker <$> gpBakers))
        genesisSeedState = SeedState.genesisSeedState gpLeadershipElectionNonce gpEpochLength
        mkBaker GenesisBaker{..} = FullBakerInfo
            (BakerInfo
                gbElectionVerifyKey
                gbSignatureVerifyKey
                gbAggregationVerifyKey
                (gaAddress gbAccount))
            (gaBalance gbAccount)

        mkAccount GenesisAccount{..} =
          let cdv = ID.cdiValues gaCredential in
          newAccount gaVerifyKeys gaAddress cdv
                & accountAmount .~ gaBalance
        -- Baker accounts will have no special privileges.
        -- We ignore any specified delegation target.
        genesisAccounts = [mkAccount gbAccount & accountPersisting . accountStakeDelegate ?~ bid
                          | (GenesisBaker{..}, bid) <- zip gpBakers [0..]]
                          -- and add any other initial accounts.
                          ++ map mkAccount gpInitialAccounts
        genesisFinalizationParameters = gpFinalizationParameters
        genesisCryptographicParameters = gpCryptographicParameters
        genesisIdentityProviders = gpIdentityProviders
        genesisAnonymityRevokers = gpAnonymityRevokers
        genesisMaxBlockEnergy = gpMaxBlockEnergy
        genesisAuthorizations = gpAuthorizations
        genesisChainParameters = gpChainParameters
