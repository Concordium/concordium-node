{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module tests processing of transactions for consensus V1.
-- The tests included here does not differentiate between the individual
-- verification results, the tests care about if a particular block item can
-- be deemed verifiable or not.
-- The module 'ConcordiumTests.ReceiveTransactionsTest' contains more fine grained tests
-- for each individual type of transction, this is ok since the two
-- consensus implementations share the same transaction verifier.
module ConcordiumTests.KonsensusV1.TransactionProcessingTest (tests) where

import qualified Concordium.Crypto.SHA256 as Hash
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as AE
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.Common.Version
import Concordium.Genesis.Data hiding (GenesisConfiguration)
import qualified Concordium.Genesis.Data.Base as Base
import Concordium.Genesis.Data.BaseV1
import Concordium.Genesis.Data.P6
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters (defaultRuntimeParameters)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.GlobalState.TransactionTable
import Concordium.Logger
import Concordium.Scheduler.DummyData
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.Types.Parameters
import Concordium.Types.Transactions

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Dummy epoch bakers. This is only suitable for when the actual value is not meaningfully used.
dummyEpochBakers :: EpochBakers
dummyEpochBakers = EpochBakers 0 dummyBakersAndFinalizers dummyBakersAndFinalizers 1

-- |Dummy bakers and finalizers with no bakers or finalizers.
-- This is only suitable for when the value is not meaningfully used.
dummyBakersAndFinalizers :: BakersAndFinalizers
dummyBakersAndFinalizers =
    BakersAndFinalizers
        { _bfBakers = FullBakers Vec.empty 0,
          _bfFinalizers = FinalizationCommittee Vec.empty 0
        }

dummyLeadershipElectionNonce :: LeadershipElectionNonce
dummyLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"

-- |A valid 'AccountCreation' with expiry 1596409020
validAccountCreation :: AccountCreation
validAccountCreation = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

myIdentityProviders :: IdentityProviders
myIdentityProviders = case readIps . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ips.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

myAnonymityRevokers :: AnonymityRevokers
myAnonymityRevokers = case readArs . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ars.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

myCryptographicParameters :: CryptographicParameters
myCryptographicParameters =
    case getExactVersionedCryptographicParameters (BSL.fromStrict $(makeRelativeToProject "testdata/transactionverification/verifiable-global.json" >>= embedFile)) of
        Nothing -> error "Could not read cryptographic parameters."
        Just params -> params

credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 1 validAccountCreation

dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

dummyCredentialDeploymentHash :: TransactionHash
dummyCredentialDeploymentHash = getHash dummyCredentialDeployment

newtype FixedTimeT (m :: Type -> Type) a = FixedTime {runDeterministic :: UTCTime -> m a}
    deriving (Functor, Applicative, Monad, MonadIO) via ReaderT UTCTime m
    deriving (MonadTrans) via ReaderT UTCTime

instance Monad m => TimeMonad (FixedTimeT m) where
    currentTime = FixedTime return

instance MonadReader r m => MonadReader r (FixedTimeT m) where
    ask = lift ask
    local f (FixedTime k) = FixedTime $ local f . k

newtype NoLoggerT m a = NoLoggerT {runNoLoggerT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, TimeMonad)

instance Monad m => MonadLogger (NoLoggerT m) where
    logEvent _ _ _ = return ()

-- |A test monad that implements @StateT (SkovData pv) m , MonadProtocolVersion m, BlockStateQuery m@
-- so we can use it for 'processBlockItem' and 'processBlockItems'.
type MyTestMonad =
    PersistentBlockStateMonad
        'P6
        (PersistentBlockStateContext 'P6)
        ( StateT
            (SkovData 'P6)
            (NoLoggerT (FixedTimeT (BlobStoreM' (PersistentBlockStateContext 'P6))))
        )

runMyTestMonad :: MyTestMonad a -> UTCTime -> IO (a, SkovData 'P6)
runMyTestMonad action time = do
    runBlobStoreTemp "." $
        withNewAccountCache 1_000 $ do
            initState <- runPersistentBlockStateMonad initialData
            runDeterministic (runNoLoggerT (runStateT (runPersistentBlockStateMonad action) initState)) time
  where
    initialData ::
        PersistentBlockStateMonad
            'P6
            (PersistentBlockStateContext 'P6)
            (BlobStoreM' (PersistentBlockStateContext 'P6))
            (SkovData 'P6)
    initialData = do
        (bs, _) <-
            genesisState makeTestingGenesisDataP6 >>= \case
                Left err -> error $ "Invalid genesis state: " ++ err
                Right x -> return x
        return $! initialSkovData bs

initialSkovData :: HashedPersistentBlockState pv -> SkovData pv
initialSkovData bs =
    mkInitialSkovData
        defaultRuntimeParameters
        (dummyGenesisConfiguration (getHash bs))
        bs
        10_000
        dummyLeadershipElectionNonce
        dummyEpochBakers

dummyGenesisConfiguration :: StateHash -> GenesisConfiguration
dummyGenesisConfiguration stHash =
    GenesisConfiguration
        { gcParameters = coreGenesisParams,
          gcCurrentGenesisHash = BlockHash $ Hash.hash "DummyGenesis",
          gcFirstGenesisHash = BlockHash $ Hash.hash "DummyGenesis",
          gcStateHash = stHash
        }

coreGenesisParams :: CoreGenesisParametersV1
coreGenesisParams = CoreGenesisParametersV1{genesisTime = 0, genesisEpochDuration = 3_600_000}

makeTestingGenesisDataP6 :: GenesisData 'P6
makeTestingGenesisDataP6 =
    let genesisCryptographicParameters = myCryptographicParameters
        genesisIdentityProviders = myIdentityProviders
        genesisAnonymityRevokers = myAnonymityRevokers
        genesisUpdateKeys = dummyKeyCollection
        genesisChainParameters = dummyChainParameters
        genesisLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"
        genesisAccounts = Vec.fromList $ makeFakeBakers 1
    in  GDP6
            GDP6Initial
                { genesisCore = coreGenesisParams,
                  genesisInitialState = Base.GenesisState{..}
                }

readIps :: BSL.ByteString -> Maybe IdentityProviders
readIps bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

readArs :: BSL.ByteString -> Maybe AnonymityRevokers
readArs bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

getExactVersionedCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
getExactVersionedCryptographicParameters bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

testProcessBlockItem :: Spec
testProcessBlockItem = describe "processBlockItem" $ do
    it "verifiable credential deployment" $ do
        (pbiRes, sd') <- runMyTestMonad (processBlockItem dummyCredentialDeployment) theTime
        -- The credential deployment is valid and so should the result reflect this.
        assertEqual
            "The credential deployment should be accepted"
            Accepted
            pbiRes
        -- The credential deployment must be in the pending transaction table at this point.
        assertBool
            "The credential deployment is recorded in the pending transaction table"
            (isJust $! sd' ^? pendingTransactions . pendingTransactionTable . pttDeployCredential)
        -- The credential deployment must be in the the transaction table at this point.
        assertEqual
            "The transaction table should yield the 'Received' credential deployment with a round 0 as commit point"
            (HM.fromList [(dummyCredentialDeploymentHash, (credentialDeployment credentialDeploymentWM, Received (commitPoint $! Round 0) (TVer.Ok TVer.CredentialDeploymentSuccess)))])
            (sd' ^. transactionTable . ttHashMap)
        -- The purge counter must be incremented at this point.
        assertEqual
            "transaction table purge counter is incremented"
            1
            (sd' ^. transactionTablePurgeCounter)
  where
    theTime :: UTCTime
    theTime = posixSecondsToUTCTime 1 -- after genesis

tests :: Spec
tests = describe "KonsensusV1.TransactionProcessing" $ do
    describe "Individual transaction processing" $ do
        testProcessBlockItem
