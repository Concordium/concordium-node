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
import qualified Data.HashSet as HS
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import Concordium.Common.Version
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
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
import Concordium.ID.Types (randomAccountAddress)
import Concordium.KonsensusV1.Monad
import Concordium.Scheduler.DummyData
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.Types.Parameters
import Concordium.Types.Transactions

import Concordium.GlobalState.Transactions
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Dummy epoch bakers. This is only suitable for when the actual value is not meaningfully used.
dummyEpochBakers :: EpochBakers
dummyEpochBakers = EpochBakers 0 dummyBakersAndFinalizers dummyBakersAndFinalizers dummyBakersAndFinalizers 1

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

-- |The identity providers required for succesfully verifying 'validAccountCreation'.
myIdentityProviders :: IdentityProviders
myIdentityProviders = case readIps . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ips.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

-- |The anonymity revokers required for succesfully verifying 'validAccountCreation'.
myAnonymityRevokers :: AnonymityRevokers
myAnonymityRevokers = case readArs . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ars.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

-- |The cryptographic parameters required for succesfully verifying 'validAccountCreation'.
myCryptographicParameters :: CryptographicParameters
myCryptographicParameters =
    case getExactVersionedCryptographicParameters (BSL.fromStrict $(makeRelativeToProject "testdata/transactionverification/verifiable-global.json" >>= embedFile)) of
        Nothing -> error "Could not read cryptographic parameters."
        Just params -> params

-- |The valid credential deployment wrapped in 'WithMetadata' and @1@ for the transaction time.
credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 1 validAccountCreation

-- |The valid credential deployment wrapped in a 'BlockItem'.
dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

-- |A dummy credential deployment 'TransactionHash'.
dummyCredentialDeploymentHash :: TransactionHash
dummyCredentialDeploymentHash = getHash dummyCredentialDeployment

-- |A monad for deriving 'MonadTime' by means of a provided time.
newtype FixedTimeT (m :: Type -> Type) a = FixedTime {runDeterministic :: UTCTime -> m a}
    deriving (Functor, Applicative, Monad, MonadIO) via ReaderT UTCTime m
    deriving (MonadTrans) via ReaderT UTCTime

instance Monad m => TimeMonad (FixedTimeT m) where
    currentTime = FixedTime return

instance MonadReader r m => MonadReader r (FixedTimeT m) where
    ask = lift ask
    local f (FixedTime k) = FixedTime $ local f . k

-- |A test monad that is suitable for testing transaction processing
-- as it derives the required capabilities.
-- I.e. 'BlockStateQuery' is supported via the 'PersistentBlockStateMonad and a 'MonadState' over the 'SkovData pv'.
-- Further it makes use of the 'FixedTimeT' which has an instance for the 'TimeMonad'.
type MyTestMonad =
    AccountNonceQueryT
        ( PersistentBlockStateMonad
            'P6
            (PersistentBlockStateContext 'P6)
            ( StateT
                (SkovData 'P6)
                (FixedTimeT (BlobStoreM' (PersistentBlockStateContext 'P6)))
            )
        )

-- |Run an action within the 'MyTestMonad'.
-- The 'IdentityProviders' and 'UTCTime' is required to setup
-- the monad to run the action within.
-- In particular the @idps@ indicate which identity providers are registered
-- on the chain and the @time@ indicates the actual time that the action is running within.
-- The @time@ is used for transaction verification.
runMyTestMonad :: IdentityProviders -> UTCTime -> MyTestMonad a -> IO (a, SkovData 'P6)
runMyTestMonad idps time action = do
    runBlobStoreTemp "." $
        withNewAccountCache 1_000 $ do
            initState <- runPersistentBlockStateMonad initialData
            runDeterministic (runStateT (runPersistentBlockStateMonad (runAccountNonceQueryT action)) initState) time
  where
    initialData ::
        PersistentBlockStateMonad
            'P6
            (PersistentBlockStateContext 'P6)
            (BlobStoreM' (PersistentBlockStateContext 'P6))
            (SkovData 'P6)
    initialData = do
        (bs, _) <-
            genesisState (makeTestingGenesisDataP6 idps) >>= \case
                Left err -> error $ "Invalid genesis state: " ++ err
                Right x -> return x
        return $! initialSkovData bs

-- |Initialize a 'SkovData pv' with the provided block state.
initialSkovData :: HashedPersistentBlockState pv -> SkovData pv
initialSkovData bs =
    mkInitialSkovData
        defaultRuntimeParameters
        (dummyGenesisMetadata (getHash bs))
        bs
        10_000
        dummyLeadershipElectionNonce
        dummyEpochBakers

-- |A block hash for the genesis.
dummyGenesisBlockHash :: BlockHash
dummyGenesisBlockHash = BlockHash $ Hash.hash "DummyGenesis"

-- |A dummy 'GenesisMetadata'
dummyGenesisMetadata :: StateHash -> GenesisMetadata
dummyGenesisMetadata stHash =
    GenesisMetadata
        { gmParameters = coreGenesisParams,
          gmCurrentGenesisHash = dummyGenesisBlockHash,
          gmFirstGenesisHash = dummyGenesisBlockHash,
          gmStateHash = stHash
        }

coreGenesisParams :: CoreGenesisParametersV1
coreGenesisParams = CoreGenesisParametersV1{genesisTime = 0, genesisEpochDuration = 3_600_000}

-- |Genesis data for P6 suitable for testing transaction processing.
-- The identity providers should be passed in as it makes it easier
-- to test some scenarios for credential deployments.
-- See the tests for these scenarios.
makeTestingGenesisDataP6 :: IdentityProviders -> GenesisData 'P6
makeTestingGenesisDataP6 idps =
    let genesisCryptographicParameters = myCryptographicParameters
        genesisIdentityProviders = idps
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

-- |Utility function for parrsing identity providers.
readIps :: BSL.ByteString -> Maybe IdentityProviders
readIps bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- |Utility function for parsing anonymity revokers.
readArs :: BSL.ByteString -> Maybe AnonymityRevokers
readArs bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- |Utility function for parsing cryptographic parameters.
getExactVersionedCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
getExactVersionedCryptographicParameters bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- |An arbitrary generated pair of keys suitable for signing a transaction.
dummySigSchemeKeys :: SigScheme.KeyPair
{-# NOINLINE dummySigSchemeKeys #-}
dummySigSchemeKeys =
    let ((signKey, verifyKey), _) = randomEd25519KeyPair $ mkStdGen 42
    in  SigScheme.KeyPairEd25519{..}

-- |A transaction signtaure on "transaction" hence it
-- has no relation to any transaction.
dummyTransactionSignature :: TransactionSignature
dummyTransactionSignature = TransactionSignature $ Map.singleton 0 (Map.singleton 0 sig)
  where
    sig = SigScheme.sign dummySigSchemeKeys "transaction"

-- |An arbitrary chosen 'AccountAddress'
dummyAccountAddress :: AccountAddress
dummyAccountAddress = fst $ randomAccountAddress (mkStdGen 42)

-- |A Normal transfer transaction that has no
-- relation to the tree state in this test, hence
-- when processed it will fail on looking up the sender.
-- Note. that the signature is not correct either.
dummyTransaction :: Transaction
dummyTransaction =
    addMetadata NormalTransaction 0 $
        makeAccountTransaction
            dummyTransactionSignature
            hdr
            payload
  where
    hdr =
        TransactionHeader
            { thSender = dummyAccountAddress,
              thPayloadSize = payloadSize payload,
              thNonce = 42,
              thExpiry = 500,
              thEnergyAmount = 5_000_000
            }
    payload = encodePayload $ Transfer dummyAccountAddress 10

-- |The block item for 'dummyTransaction'.
dummyTransactionBI :: BlockItem
dummyTransactionBI = normalTransaction dummyTransaction

-- |Testing various cases for processing a block item individually.
testProcessBlockItem :: Spec
testProcessBlockItem = describe "processBlockItem" $ do
    -- Test that an 'Ok' transaction is accepted into the state when being received individually.
    it "Ok transaction" $ do
        (pbiRes, sd') <- runMyTestMonad myIdentityProviders theTime (processBlockItem dummyCredentialDeployment)
        -- The credential deployment is valid and so should the result reflect this.
        assertEqual
            "The credential deployment should be accepted"
            (Added dummyCredentialDeployment $ TVer.Ok TVer.CredentialDeploymentSuccess)
            pbiRes
        -- The credential deployment must be in the pending transaction table at this point.
        assertBool
            "The credential deployment should be recorded in the pending transaction table"
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
    -- Test that a 'MaybeOk' transaction is rejected when being received individually.
    it "MaybeOk transaction" $ do
        -- We use a normal transfer transaction here with an invalid sender as it will yield a
        -- 'MaybeOk' verification result.
        (pbiRes, sd') <- runMyTestMonad dummyIdentityProviders theTime (processBlockItem dummyTransactionBI)
        assertEqual
            "The credential deployment should be rejected (the identity provider has correct id but wrong keys used for the credential deployment)"
            (NotAdded $ TVer.MaybeOk $ TVer.NormalTransactionInvalidSender dummyAccountAddress)
            pbiRes
        assertEqual
            "The transfer should not be recorded in the pending transaction table"
            HM.empty
            (sd' ^. pendingTransactions . pendingTransactionTable . pttWithSender)
        assertEqual
            "The transaction table should yield the 'Received' transfer with a round 0 as commit point"
            HM.empty
            (sd' ^. transactionTable . ttHashMap)
        assertEqual
            "transaction table purge counter is incremented"
            0
            (sd' ^. transactionTablePurgeCounter)
    -- Test that a 'NotOk' transaction is rejected when being received individually.
    it "NotOk transaction" $ do
        (pbiRes, sd') <- runMyTestMonad dummyIdentityProviders theTime (processBlockItem dummyCredentialDeployment)
        assertEqual
            "The credential deployment should be rejected (the identity provider has correct id but wrong keys used for the credential deployment)"
            (NotAdded $ TVer.NotOk TVer.CredentialDeploymentInvalidSignatures)
            pbiRes
        assertEqual
            "The credential deployment should not be recorded in the pending transaction table"
            HS.empty
            (sd' ^. pendingTransactions . pendingTransactionTable . pttDeployCredential)
        assertEqual
            "The transaction table should yield the 'Received' credential deployment with a round 0 as commit point"
            HM.empty
            (sd' ^. transactionTable . ttHashMap)
        assertEqual
            "transaction table purge counter is not incremented"
            0
            (sd' ^. transactionTablePurgeCounter)
    it "No duplicates" $ do
        (pbiRes, sd') <- runMyTestMonad myIdentityProviders theTime (processBlockItem dummyCredentialDeployment >> processBlockItem dummyCredentialDeployment)
        assertEqual
            "We just added the same twice, so the latter one added should be recognized as a duplicate."
            (Duplicate dummyCredentialDeployment $ Just $ TVer.Ok TVer.CredentialDeploymentSuccess)
            pbiRes
        -- The credential deployment that was not deemed duplicate must be in the pending transaction table at this point.
        assertBool
            "The credential deployment should be recorded in the pending transaction table"
            (isJust $! sd' ^? pendingTransactions . pendingTransactionTable . pttDeployCredential)
        -- The credential deployment that was not deemed duplicate must be in the the transaction table at this point.
        assertEqual
            "The transaction table should yield the 'Received' credential deployment with a round 0 as commit point"
            (HM.fromList [(dummyCredentialDeploymentHash, (credentialDeployment credentialDeploymentWM, Received (commitPoint $! Round 0) (TVer.Ok TVer.CredentialDeploymentSuccess)))])
            (sd' ^. transactionTable . ttHashMap)
        -- The purge counter must be incremented only once at this point.
        assertEqual
            "transaction table purge counter is incremented"
            1
            (sd' ^. transactionTablePurgeCounter)
  where
    theTime :: UTCTime
    theTime = posixSecondsToUTCTime 1 -- after genesis

-- |Testing cases for processing the transactions of a block received.
testProcessBlockItems :: Spec
testProcessBlockItems = describe "processBlockItems" $ do
    it "A non verifiable transaction first in the block makes it fail and stop processing the rest" $ do
        (processed, sd') <-
            runMyTestMonad dummyIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyCredentialDeployment, dummyTransactionBI]) =<< _lastFinalized <$> get
        assertBool
            "Block should not have been successfully processed"
            (not processed)
        assertEqual
            "transaction table purge counter is 0 as the processing stopped because of the first transaction"
            0
            (sd' ^. transactionTablePurgeCounter)
    it "A non verifiable transaction last in the block makes it fail but the valid ones have been put into the state." $ do
        (processed, sd') <-
            runMyTestMonad dummyIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyTransactionBI, dummyCredentialDeployment]) =<< _lastFinalized <$> get
        assertBool
            "Block should not have been successfully processed"
            (not processed)
        assertEqual
            "transaction table purge counter is 1 because of the first transaction was successfully processed"
            1
            (sd' ^. transactionTablePurgeCounter)
    it "A block consisting of verifiable transactions only is accepted" $ do
        (processed, sd') <-
            runMyTestMonad myIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyTransactionBI, dummyCredentialDeployment]) =<< _lastFinalized <$> get
        assertBool
            "Block should have been successfully processed"
            processed
        assertEqual
            "transaction table purge counter should have been bumped twice"
            2
            (sd' ^. transactionTablePurgeCounter)
    it "A transaction received as part of a block bumps the round for it if it was a duplicate" $ do
        (processed, sd') <-
            runMyTestMonad myIdentityProviders theTime $
                processBlockItem dummyCredentialDeployment
                    >> ( processBlockItems (blockToProcess [dummyCredentialDeployment])
                            =<< _lastFinalized <$> get
                       )
        assertBool
            "Block should have been successfully processed"
            processed
        assertEqual
            "transaction table purge counter should have been incremented once as the latter insertion was a duplicate"
            1
            (sd' ^. transactionTablePurgeCounter)
        assertEqual
            "The transaction table should yield the 'Received' credential deployment with a round 1 as commit point"
            (HM.fromList [(dummyCredentialDeploymentHash, (credentialDeployment credentialDeploymentWM, Received (commitPoint $! Round 1) (TVer.Ok TVer.CredentialDeploymentSuccess)))])
            (sd' ^. transactionTable . ttHashMap)
  where
    theTime :: UTCTime
    theTime = posixSecondsToUTCTime 1 -- after genesis
    -- This block is not valid or makes much sense in the context
    -- of a chain. But it does have transactions and that is what we care
    -- about in this test.
    blockToProcess :: [BlockItem] -> BakedBlock
    blockToProcess txs =
        let bbRound = 1
            bbEpoch = 0
            bbTimestamp = 0
            bbBaker = 0
            bbTimeoutCertificate = Absent
            bbEpochFinalizationEntry = Absent
            bbNonce = VRF.prove (fst $ VRF.randomKeyPair (mkStdGen 42)) ""
            -- The first transfer is 'MaybeOk'.
            -- But second transaction is not verifiable (i.e. 'NotOk') because of the chosen set of identity providers,
            bbTransactions = Vec.fromList txs
            bbTransactionOutcomesHash = TransactionOutcomesHash minBound
            bbStateHash = StateHashV0 $ Hash.hash "DummyStateHash"
        in  BakedBlock
                { bbQuorumCertificate =
                    QuorumCertificate
                        { qcBlock = dummyGenesisBlockHash,
                          qcRound = 0,
                          qcEpoch = 0,
                          qcAggregateSignature = mempty,
                          qcSignatories = FinalizerSet 0
                        },
                  ..
                }

tests :: Spec
tests = describe "KonsensusV1.TransactionProcessing" $ do
    describe "Individual transaction processing" $ do
        testProcessBlockItem
    describe "Batch transaction processing" $ do
        testProcessBlockItems
