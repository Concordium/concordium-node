{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
-- |This module tests processing of transactions for consensus V1.
-- The tests included here does not differentiate between the individual
-- verification results, the tests care about if a particular block item can
-- be deemed verifiable or not.
-- The module 'ConcordiumTests.ReceiveTransactionsTest' contains more fine grained tests
-- for each individual type of transction, this is ok since the two
-- consensus implementations share the same transaction verifier.
module ConcordiumTests.KonsensusV1.TransactionProcessingTest (tests) where

import Data.Time.Clock.POSIX
import Control.Monad.Reader
import Data.Kind (Type)
import Test.HUnit
import Test.Hspec
import qualified Concordium.Crypto.SHA256 as Hash
import Data.IORef
import System.IO.Unsafe
import qualified Data.Vector as Vec
import Control.Monad.State
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Data.Time.Clock

import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.Genesis.Data.P6
import qualified Concordium.Genesis.Data.Base as Base
import Concordium.Genesis.Data.BaseV1
import Concordium.Logger
import Concordium.Genesis.Data.BaseV1
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Transactions
import Concordium.TimeMonad
import Concordium.Scheduler.DummyData
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Parameters (defaultRuntimeParameters)

import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.Types
import Concordium.KonsensusV1.Consensus

-- |The dummy genesis hash.
dummyGenesisBlockHash :: BlockHash
dummyGenesisBlockHash = BlockHash (Hash.hash "DummyGenesis")

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
dummyAccountCreation :: AccountCreation
dummyAccountCreation = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 0 dummyAccountCreation

dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

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
        (StateT
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
        (bs, genTT) <- genesisState makeTestingGenesisDataP6 >>= \case
                     Left err -> error $ "Invalid genesis state: " ++ err
                     Right x -> return x
        return dummyInitialSkovData
                     

-- |A dummy block state that has no meaningful content.
dummyBlockState :: HashedPersistentBlockState pv
dummyBlockState = HashedPersistentBlockState{..}
  where
    hpbsPointers = dummyPersistentBlockState
    hpbsHash = dummyStateHash

    
-- |A dummy block state that is just a @BlobRef 0@.
dummyPersistentBlockState :: PersistentBlockState pv
{-# NOINLINE dummyPersistentBlockState #-}
dummyPersistentBlockState = unsafePerformIO $ newIORef $ blobRefToBufferedRef (BlobRef 0)

dummyStateHash :: StateHash
dummyStateHash = StateHashV0 $ Hash.hash "DummyPersistentBlockState"

dummyInitialSkovData :: SkovData pv
dummyInitialSkovData =
    mkInitialSkovData
        defaultRuntimeParameters
        dummyGenesisConfiguration
        dummyBlockState
        10_000
        dummyLeadershipElectionNonce
        dummyEpochBakers

dummyGenesisConfiguration :: GenesisConfiguration
dummyGenesisConfiguration =
    GenesisConfiguration
        { gcParameters = coreGenesisParams,
          gcCurrentGenesisHash = dummyGenesisBlockHash,
          gcFirstGenesisHash = dummyGenesisBlockHash,
          gcStateHash = getHash dummyBlockState
        }

coreGenesisParams :: CoreGenesisParametersV1
coreGenesisParams = CoreGenesisParametersV1{genesisTime = 0, genesisEpochDuration = 3_600_000}

makeTestingGenesisDataP6 :: GenesisDataP6
makeTestingGenesisDataP6  =
    let genesisCryptographicParameters = undefined
        genesisIdentityProviders = undefined
        genesisAnonymityRevokers = undefined
        genesisUpdateKeys = undefined
        genesisChainParameters = undefined
        genesisLeadershipElectionNonce = undefined
        genesisAccounts = Vec.empty
  in GDP6Initial
  { genesisCore = coreGenesisParams,
    genesisInitialState = Base.GenesisState{..}
  }

-- |This test proccesses one verifiable block item and one
-- that is not.
-- The test then checks that the resulting state is expected.
testProcessBlockItem :: Spec
testProcessBlockItem = describe "processBlockItem" $ do
    it "verifiable credential deployment" $ do
            (pbiRes, sd') <- runMyTestMonad (processBlockItem dummyCredentialDeployment) theTime
            assertEqual
                "The credential deployment should be accepted"
                Accepted
                pbiRes
  where
      theTime :: UTCTime
      theTime = posixSecondsToUTCTime 1 -- after genesis 

tests :: Spec
tests = describe "KonsensusV1.TransactionProcessing" $ do
    describe "Individual transaction processing" $ do
        testProcessBlockItem
        
