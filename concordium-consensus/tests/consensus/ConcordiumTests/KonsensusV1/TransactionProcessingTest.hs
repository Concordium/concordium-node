{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module tests processing of transactions for consensus V1.
--  The tests included here does not differentiate between the individual
--  verification results, the tests care about if a particular block item can
--  be deemed verifiable or not.
--  The module 'ConcordiumTests.ReceiveTransactionsTest' contains more fine grained tests
--  for each individual type of transaction, this is ok since the two
--  consensus implementations share the same transaction verifier.
module ConcordiumTests.KonsensusV1.TransactionProcessingTest where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Identity
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
import Data.Ratio
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Data.Void
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import Concordium.Common.Version
import Concordium.Constants
import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.VRF as VRF
import Concordium.Genesis.Data hiding (GenesisConfiguration)
import qualified Concordium.Genesis.Data.Base as Base
import Concordium.Genesis.Data.BaseV1
import Concordium.Genesis.Data.P10
import Concordium.Genesis.Data.P6
import Concordium.Genesis.Data.P7
import Concordium.Genesis.Data.P8
import Concordium.Genesis.Data.P9
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters (defaultRuntimeParameters)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Transactions
import qualified Concordium.GlobalState.Types as GSTypes
import Concordium.ID.Types (randomAccountAddress)
import qualified Concordium.ID.Types as ID
import Concordium.Logger
import Concordium.Scheduler.DummyData
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders
import Concordium.Types.Option
import Concordium.Types.Parameters
import Concordium.Types.TransactionOutcomes
import Concordium.Types.Transactions
import Concordium.Types.Updates
import Concordium.Utils
import Concordium.Wasm (WasmVersion)

import Concordium.KonsensusV1.Transactions
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import qualified ConcordiumTests.KonsensusV1.Common as Common

-- | Dummy epoch bakers. This is only suitable for when the actual value is not meaningfully used.
dummyEpochBakers :: EpochBakers
dummyEpochBakers = EpochBakers dummyBakersAndFinalizers dummyBakersAndFinalizers dummyBakersAndFinalizers 1

-- | Dummy bakers and finalizers with no bakers or finalizers.
--  This is only suitable for when the value is not meaningfully used.
dummyBakersAndFinalizers :: BakersAndFinalizers
dummyBakersAndFinalizers =
    BakersAndFinalizers
        { _bfBakers = FullBakers Vec.empty 0,
          _bfFinalizers = finalizers,
          _bfFinalizerHash = computeFinalizationCommitteeHash finalizers
        }
  where
    finalizers = FinalizationCommittee Vec.empty 0

-- | A valid 'AccountCreation' with expiry 1596409020
validAccountCreation :: AccountCreation
validAccountCreation = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

-- | The identity providers required for succesfully verifying 'validAccountCreation'.
myIdentityProviders :: IdentityProviders
myIdentityProviders = case readIps . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ips.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

-- | The anonymity revokers required for succesfully verifying 'validAccountCreation'.
myAnonymityRevokers :: AnonymityRevokers
myAnonymityRevokers = case readArs . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ars.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

-- | The cryptographic parameters required for succesfully verifying 'validAccountCreation'.
myCryptographicParameters :: CryptographicParameters
myCryptographicParameters =
    case getExactVersionedCryptographicParameters (BSL.fromStrict $(makeRelativeToProject "testdata/transactionverification/verifiable-global.json" >>= embedFile)) of
        Nothing -> error "Could not read cryptographic parameters."
        Just params -> params

-- | The valid credential deployment wrapped in 'WithMetadata' and @1@ for the transaction time.
credentialDeploymentWM :: WithMetadata AccountCreation
credentialDeploymentWM = addMetadata CredentialDeployment 1 validAccountCreation

-- | The valid credential deployment wrapped in a 'BlockItem'.
dummyCredentialDeployment :: BlockItem
dummyCredentialDeployment = credentialDeployment credentialDeploymentWM

-- | A dummy credential deployment 'TransactionHash'.
dummyCredentialDeploymentHash :: TransactionHash
dummyCredentialDeploymentHash = getHash dummyCredentialDeployment

-- | A monad for deriving 'MonadTime' by means of a provided time.
newtype FixedTimeT (m :: Type -> Type) a = FixedTime {runDeterministic :: UTCTime -> m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch) via ReaderT UTCTime m
    deriving (MonadTrans) via ReaderT UTCTime

instance (Monad m) => TimeMonad (FixedTimeT m) where
    currentTime = FixedTime return

instance (MonadReader r m) => MonadReader r (FixedTimeT m) where
    ask = lift ask
    local f (FixedTime k) = FixedTime $ local f . k

newtype NoLoggerT m a = NoLoggerT {runNoLoggerT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadFail, TimeMonad, MonadState s, MonadThrow, MonadCatch)

instance (Monad m) => MonadLogger (NoLoggerT m) where
    logEvent _ _ _ = return ()

-- | A test monad that is suitable for testing transaction processing
--  as it derives the required capabilities.
--  I.e. 'BlockStateQuery' is supported via the 'PersistentBlockStateMonad and a 'MonadState' over the 'SkovData pv'.
--  Further it makes use of the 'FixedTimeT' which has an instance for the 'TimeMonad'.
type MyTestMonad pv =
    AccountNonceQueryT
        ( PersistentBlockStateMonad
            pv
            (PersistentBlockStateContext pv)
            ( NoLoggerT
                ( StateT
                    (SkovData pv)
                    (FixedTimeT (BlobStoreM' (PersistentBlockStateContext pv)))
                )
            )
        )

-- | Run an action within the 'MyTestMonad'.
--  The 'IdentityProviders' and 'UTCTime' is required to setup
--  the monad to run the action within.
--  In particular the @idps@ indicate which identity providers are registered
--  on the chain and the @time@ indicates the actual time that the action is running within.
--  The @time@ is used for transaction verification.
runMyTestMonad ::
    forall pv a.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    IdentityProviders ->
    UTCTime ->
    MyTestMonad pv a ->
    IO (a, SkovData pv)
runMyTestMonad idps time action = do
    runBlobStoreTemp "." $
        withNewAccountCacheAndLMDBAccountMap 1_000 "accountmap" $ do
            initState <- runNoLoggerT $ runPersistentBlockStateMonad initialData
            flip runDeterministic time $
                flip runStateT initState $
                    runNoLoggerT $
                        runPersistentBlockStateMonad $
                            runAccountNonceQueryT action
  where
    initialData ::
        PersistentBlockStateMonad
            pv
            (PersistentBlockStateContext pv)
            (NoLoggerT (BlobStoreM' (PersistentBlockStateContext pv)))
            (SkovData pv)
    initialData = do
        (bs, _) <-
            genesisState (makeTestingGenesisData @pv idps) >>= \case
                Left err -> error $ "Invalid genesis state: " ++ err
                Right x -> return x
        return $! initialSkovData bs

-- | Initialize a 'SkovData pv' with the provided block state.
initialSkovData :: (IsProtocolVersion pv) => HashedPersistentBlockState pv -> SkovData pv
initialSkovData bs =
    mkInitialSkovData
        defaultRuntimeParameters
        (dummyGenesisMetadata (getHash bs))
        GenesisBlockHeightInfo
            { gbhiAbsoluteHeight = 0,
              gbhiGenesisIndex = 0
            }
        bs
        10_000
        dummyEpochBakers
        emptyTransactionTable
        emptyPendingTransactionTable

-- | A block hash for the genesis.
dummyGenesisBlockHash :: BlockHash
dummyGenesisBlockHash = BlockHash $ Hash.hash "DummyGenesis"

-- | A dummy 'GenesisMetadata'
dummyGenesisMetadata :: StateHash -> GenesisMetadata
dummyGenesisMetadata stHash =
    GenesisMetadata
        { gmParameters = coreGenesisParams,
          gmCurrentGenesisHash = dummyGenesisBlockHash,
          gmFirstGenesisHash = dummyGenesisBlockHash,
          gmStateHash = stHash
        }

coreGenesisParams :: CoreGenesisParametersV1
coreGenesisParams = CoreGenesisParametersV1{genesisTime = 0, genesisEpochDuration = 3_600_000, genesisSignatureThreshold = 2 % 3}

-- | Genesis data for a protocol version suitable for testing transaction processing.
--  The identity providers should be passed in as it makes it easier
--  to test some scenarios for credential deployments.
--  See the tests for these scenarios.
makeTestingGenesisData :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => IdentityProviders -> GenesisData pv
makeTestingGenesisData idps =
    let genesisCryptographicParameters = myCryptographicParameters
        genesisIdentityProviders = idps
        genesisAnonymityRevokers = myAnonymityRevokers
        genesisUpdateKeys =
            withIsAuthorizationsVersionFor
                (protocolVersion @pv)
                (dummyKeyCollection @(AuthorizationsVersionFor pv))
        genesisChainParameters = dummyChainParameters @(ChainParametersVersionFor pv)
        genesisLeadershipElectionNonce = Hash.hash "LeadershipElectionNonce"
        genesisAccounts = Vec.fromList $ makeFakeBakers 1
    in  case protocolVersion @pv of
            SP6 ->
                GDP6
                    GDP6Initial
                        { genesisCore = coreGenesisParams,
                          genesisInitialState = Base.GenesisState{..}
                        }
            SP7 ->
                GDP7
                    GDP7Initial
                        { genesisCore = coreGenesisParams,
                          genesisInitialState = Base.GenesisState{..}
                        }
            SP8 ->
                GDP8
                    GDP8Initial
                        { genesisCore = coreGenesisParams,
                          genesisInitialState = Base.GenesisState{..}
                        }
            SP9 ->
                GDP9
                    GDP9Initial
                        { genesisCore = coreGenesisParams,
                          genesisInitialState = Base.GenesisState{..}
                        }
            SP10 ->
                GDP10
                    GDP10Initial
                        { genesisCore = coreGenesisParams,
                          genesisInitialState = Base.GenesisState{..}
                        }

-- | Utility function for parrsing identity providers.
readIps :: BSL.ByteString -> Maybe IdentityProviders
readIps bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- | Utility function for parsing anonymity revokers.
readArs :: BSL.ByteString -> Maybe AnonymityRevokers
readArs bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- | Utility function for parsing cryptographic parameters.
getExactVersionedCryptographicParameters :: BSL.ByteString -> Maybe CryptographicParameters
getExactVersionedCryptographicParameters bs = do
    v <- AE.decode bs
    -- We only support Version 0 at this point for testing. When we support more
    -- versions we'll have to decode in a dependent manner, first reading the
    -- version, and then decoding based on that.
    guard (vVersion v == 0)
    return (vValue v)

-- | An arbitrary generated pair of keys suitable for signing a transaction.
dummySigSchemeKeys :: SigScheme.KeyPair
{-# NOINLINE dummySigSchemeKeys #-}
dummySigSchemeKeys =
    let ((signKey, verifyKey), _) = randomEd25519KeyPair $ mkStdGen 42
    in  SigScheme.KeyPairEd25519{..}

-- | A transaction signtaure on "transaction" hence it
--  has no relation to any transaction.
dummyTransactionSignature :: TransactionSignature
dummyTransactionSignature = TransactionSignature $ Map.singleton 0 (Map.singleton 0 sig)
  where
    sig = SigScheme.sign dummySigSchemeKeys "transaction"

-- | An arbitrary chosen 'AccountAddress'
dummyAccountAddress :: AccountAddress
dummyAccountAddress = fst $ randomAccountAddress (mkStdGen 42)

-- | A Normal transfer transaction that has no
--  relation to the tree state in this test, hence
--  when processed it will fail on looking up the sender.
--  Note. that the signature is not correct either.
dummyNormalTransaction :: Transaction
dummyNormalTransaction =
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

-- | A dummy update instruction.
dummyUpdateInstruction :: TransactionTime -> WithMetadata UpdateInstruction
dummyUpdateInstruction effTime =
    addMetadata ChainUpdate 0 $
        makeUpdateInstruction
            RawUpdateInstruction
                { ruiSeqNumber = 1,
                  ruiEffectiveTime = effTime,
                  ruiTimeout = 2,
                  ruiPayload = cplt
                }
            (Map.singleton 0 dummyAuthorizationKeyPair)
  where
    cplt =
        CreatePLTUpdatePayload $
            CreatePLT
                { _cpltTokenId = TokenId "dummyToken",
                  _cpltTokenModule = TokenModuleRef $ Hash.hash "dummyToken",
                  _cpltDecimals = 4,
                  _cpltInitializationParameters = TokenParameter ""
                }

-- | The block item for 'dummyNormalTransaction'.
dummyNormalTransactionBI :: BlockItem
dummyNormalTransactionBI = normalTransaction dummyNormalTransaction

-- | The block item for 'dummyUpdateInstruction'.
dummyChainUpdateBI :: TransactionTime -> BlockItem
dummyChainUpdateBI effTime = chainUpdate $ dummyUpdateInstruction effTime

-- | Test for transaction verification
testTransactionVerification ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv, PVSupportsPLT pv) =>
    SProtocolVersion pv ->
    Spec
testTransactionVerification _ = describe "transaction verification" $ do
    it "ChainUpdate: CreatePLT: Ok" $ do
        (verResult, _) <- runMyTestMonad @pv myIdentityProviders theTime $
            do
                t <- utcTimeToTimestamp <$> currentTime
                ctx <- getCtx
                verifyBlockItem t (dummyChainUpdateBI 0) ctx
        assertEqual
            "A correct transaction should be verified"
            (TVer.Ok TVer.ChainUpdateSuccess{keysHash = getHash (dummyKeyCollection @(AuthorizationsVersionFor pv)), seqNumber = 1})
            verResult
    it "ChainUpdate: CreatePLT: Non-zero effective time for CreatePLT" $ do
        (verResult, _) <- runMyTestMonad @pv myIdentityProviders theTime $
            do
                t <- utcTimeToTimestamp <$> currentTime
                ctx <- getCtx
                verifyBlockItem t (dummyChainUpdateBI 1) ctx
        assertEqual
            "A CreatePLT update with non-zero effective time should be rejected"
            (TVer.NotOk TVer.ChainUpdateEffectiveTimeNonZeroForCreatePLT)
            verResult
  where
    -- Create a context suitable for verifying a transaction within a 'Individual' context.
    getCtx = do
        _ctxBs <- bpState <$> gets' _lastFinalized
        let chainParams = dummyChainParameters @(ChainParametersVersionFor pv)
        let _ctxMaxBlockEnergy = chainParams ^. cpConsensusParameters . cpBlockEnergyLimit
        return $! Context{_ctxTransactionOrigin = TVer.Individual, ..}

    theTime :: UTCTime
    theTime = posixSecondsToUTCTime 1 -- after genesis

-- | Testing various cases for processing a block item individually.
testProcessBlockItem ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testProcessBlockItem _ = describe "processBlockItem" $ do
    -- Test that an 'Ok' transaction is accepted into the state when being received individually.
    it "Ok transaction" $ do
        (pbiRes, sd') <- runMyTestMonad @pv myIdentityProviders theTime (processBlockItem dummyCredentialDeployment)
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
        (pbiRes, sd') <- runMyTestMonad @pv dummyIdentityProviders theTime (processBlockItem dummyNormalTransactionBI)
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
        (pbiRes, sd') <- runMyTestMonad @pv dummyIdentityProviders theTime (processBlockItem dummyCredentialDeployment)
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
        (pbiRes, sd') <- runMyTestMonad @pv myIdentityProviders theTime (processBlockItem dummyCredentialDeployment >> processBlockItem dummyCredentialDeployment)
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

-- | Testing cases for processing the transactions of a block received.
testProcessBlockItems ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testProcessBlockItems sProtocolVersion = describe "processBlockItems" $ do
    it "A non verifiable transaction first in the block makes it fail and stop processing the rest" $ do
        (processed, sd') <-
            runMyTestMonad dummyIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyCredentialDeployment, dummyNormalTransactionBI]) =<< _lastFinalized <$> get
        assertBool
            "Block should not have been successfully processed"
            (null processed)
        assertEqual
            "transaction table purge counter is 0 as the processing stopped because of the first transaction"
            0
            (sd' ^. transactionTablePurgeCounter)
    it "A non verifiable transaction last in the block makes it fail but the valid ones have been put into the state." $ do
        (processed, sd') <-
            runMyTestMonad dummyIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyNormalTransactionBI, dummyCredentialDeployment]) =<< _lastFinalized <$> get
        assertBool
            "Block should not have been successfully processed"
            (null processed)
        assertEqual
            "transaction table purge counter is 1 because of the first transaction was successfully processed"
            1
            (sd' ^. transactionTablePurgeCounter)
    it "A block consisting of verifiable transactions only is accepted" $ do
        (processed, sd') <-
            runMyTestMonad myIdentityProviders theTime $
                processBlockItems (blockToProcess [dummyNormalTransactionBI, dummyCredentialDeployment]) =<< _lastFinalized <$> get
        assertBool
            "Block should have been successfully processed"
            (isJust processed)
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
            (isJust processed)
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
    blockToProcess :: [BlockItem] -> BakedBlock pv
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
            bbDerivableHashes = case sBlockHashVersionFor sProtocolVersion of
                SBlockHashVersion0 ->
                    DerivableBlockHashesV0
                        { dbhv0TransactionOutcomesHash = TransactionOutcomesHash minBound,
                          dbhv0BlockStateHash = StateHashV0 $ Hash.hash "DummyStateHash"
                        }
                SBlockHashVersion1 ->
                    DerivableBlockHashesV1
                        { dbhv1BlockResultHash = BlockResultHash $ Hash.hash "DummyBlockResultHash"
                        }
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

-- TransactionVerifier testing
------------------------------

-- | Test data used for testing transaction verification
data TransactionVerifierTestData (pv :: ProtocolVersion) = TransactionVerifierTestData
    { tvtdAccounts :: !(Map.Map (GSTypes.Account (TVTM pv)) (AccountTestData pv)),
      tvtdAccountsByAddress :: !(Map.Map AccountAddress (GSTypes.Account (TVTM pv))),
      tvtdEnergyRate :: !EnergyRate,
      tvtdCheckExactNonce :: !Bool
    }

data AccountTestData (pv :: ProtocolVersion) = AccountTestData
    { atdAccountAvailableAmount :: !Amount,
      atdAccountNonce :: !Nonce,
      atdAccountVerificationKeys :: ID.AccountInformation
    }

-- The transaction verifier test monad.
newtype TVTM (pv :: ProtocolVersion) a = TVTM
    { runTVTM :: TransactionVerifierT' (TransactionVerifierTestData pv) Identity a
    }
    deriving (Functor, Applicative, Monad, MonadReader (TransactionVerifierTestData pv))

instance
    forall (pv :: ProtocolVersion).
    (IsProtocolVersion pv, IsCompatibleAuthorizationsVersion (ChainParametersVersionFor pv) (AuthorizationsVersionFor pv) ~ 'True) =>
    MonadProtocolVersion (TVTM pv)
    where
    type MPV (TVTM pv) = pv

-- type family used to define the following void type instances for
-- `BlockStateTypes (TVTM pv)``
type family X m :: WasmVersion -> Type

instance forall (pv :: ProtocolVersion). GSTypes.BlockStateTypes (TVTM pv) where
    type Account (TVTM pv) = T.Text
    type BlockState (TVTM pv) = Void
    type UpdatableBlockState (TVTM pv) = Void
    type ContractState (TVTM pv) = X Void
    type BakerInfoRef (TVTM pv) = Void
    type InstrumentedModuleRef (TVTM pv) = X Void
    type MutableTokenState (TVTM pv) = Void

instance
    forall (pv :: ProtocolVersion).
    (IsProtocolVersion pv, IsCompatibleAuthorizationsVersion (ChainParametersVersionFor pv) (AuthorizationsVersionFor pv) ~ 'True) =>
    TVer.TransactionVerifier (TVTM pv)
    where
    getAccount addr = do
        testData <- ask
        return $ Map.lookup addr $ tvtdAccountsByAddress testData

    getAccountAvailableAmount acc = do
        testData <- tvtdAccounts <$> ask
        case Map.lookup acc testData of
            Nothing -> error "account not available in test data"
            Just atd -> return $ atdAccountAvailableAmount atd

    getNextAccountNonce acc = do
        testData <- tvtdAccounts <$> ask
        case Map.lookup acc testData of
            Nothing -> error "account not available in test data"
            Just atd -> return $ atdAccountNonce atd

    getAccountVerificationKeys acc = do
        testData <- tvtdAccounts <$> ask
        case Map.lookup acc testData of
            Nothing -> error "account not available in test data"
            Just atd -> return $ atdAccountVerificationKeys atd

    getMaxBlockEnergy = return 1000

    -- For testing we assume exact nonces.
    checkExactNonce = tvtdCheckExactNonce <$> ask

    energyToCcd nrg = do
        testData <- ask
        return $ computeCost (tvtdEnergyRate testData) nrg

    -- We currently don't use the following for testing.
    getIdentityProvider = error "Unexpected use of `getIdentityProvider` in TransactionVerifierTestM"
    getAnonymityRevokers = error "Unexpected use of `getAnonymityRevokers` in TransactionVerifierTestM"
    getCryptographicParameters = error "Unexpected use of `getCryptographicParameters` in TransactionVerifierTestM"
    registrationIdExists = error "Unexpected use of `registrationIdExists` in TransactionVerifierTestM"
    getNextUpdateSequenceNumber = error "Unexpected use of `getNextUpdateSequenceNumber` in TransactionVerifierTestM"
    getUpdateKeysCollection = error "Unexpected use of `getUpdateKeysCollection` in TransactionVerifierTestM"

-- | Tests for the transaction verification of extended transaction introduced in P10.
testExtendedTransactionVerification ::
    forall pv.
    (IsConsensusV1 pv, IsProtocolVersion pv) =>
    SProtocolVersion pv ->
    Spec
testExtendedTransactionVerification spv = do
    it "A well-formed extended transaction with sponsor should pass verification" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `Ok ExtendTransactionSuccess` result"
            ( TVer.Ok
                TVer.ExtendedTransactionSuccess
                    { senderKeysHash = getHash senderAccountVerificationKeys,
                      sponsorKeysHash = Present $ getHash sponsorAccountVerificationKeys,
                      nonce = 1
                    }
            )
            res
    it "A transaction with a too big payload size should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = (fromIntegral $ maxPayloadSize spv + 1),
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `NotOk InvalidPayloadSize` result"
            (TVer.NotOk TVer.InvalidPayloadSize)
            res

    it "A transaction with a sponsor address but without sponsor signature should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Nothing
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `NotOk SponsoredTransactionMissingSponsorSignature` result"
            (TVer.NotOk TVer.SponsoredTransactionMissingSponsorSignature)
            res
    it "A transaction with no sponsor address but a sponsor signature should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Nothing
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `NotOk SponsoredTransactionMissingSponsor` result"
            (TVer.NotOk TVer.SponsoredTransactionMissingSponsor)
            res
    it "An extended transaction with too little energy should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              -- cost: 302
                              -- V1 header: 2 (bitmap) + 60 (V0 header) + 32 (sponsor address) = 94
                              -- payload: 8
                              -- signatures: 2 * 100
                              thEnergyAmount = 301,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `NotOk NormalTransactionDepositInsufficient` result"
            (TVer.NotOk TVer.NormalTransactionDepositInsufficient)
            res
    it "An extended transaction with energy exceeding the maximally allowed energy should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              -- max energy is set by the TVTM mock to 1000
                              thEnergyAmount = fromIntegral $ (1000 + 1 :: Int),
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `MaybeOk NormalTransactionEnergyExceeded` result"
            (TVer.MaybeOk TVer.NormalTransactionEnergyExceeded)
            res

    it "An extended transaction with nonce < next expected nonce should not pass verification" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 0,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `NotOk NormalTransactionDuplicateNonce` result"
            (TVer.NotOk $ TVer.NormalTransactionDuplicateNonce 0)
            res
    it "An extended transaction with nonce > next expected nonce should not pass verification if `checkExactNonce == True`" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              -- the expected nonce is 1
                              thNonce = 2,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `MaybeOk NormalTransactionInvalidNonce` result"
            (TVer.MaybeOk $ TVer.NormalTransactionInvalidNonce 1)
            res
    it "An extended transaction with nonce > next expected nonce should pass verification if `checkExactNonce == False`" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              -- the expected nonce is 1
                              thNonce = 2,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testDataNotExactNonce
        assertEqual
            "The verification should yield the expected `Ok ExtendedTransactionSuccess` result"
            ( TVer.Ok
                TVer.ExtendedTransactionSuccess
                    { senderKeysHash = getHash senderAccountVerificationKeys,
                      sponsorKeysHash = Present $ getHash sponsorAccountVerificationKeys,
                      nonce = 2
                    }
            )
            res
    it "An extended transaction with sponsor but missing sponsor account should not pass verification" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testDataNoSponsor
        assertEqual
            "The verification should yield the expected `MaybeOk ExtendedTransactionInvalidSponsor` result"
            (TVer.MaybeOk $ TVer.ExtendedTransactionInvalidSponsor sponsorAccountAddress)
            res
    it "An extended transaction where the sponsor has too little funds should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testDataTooLittleFunding
        assertEqual
            "The verification should yield the expected `MaybeOk NormalTransactionInsufficientFunds` result"
            (TVer.MaybeOk $ TVer.NormalTransactionInsufficientFunds)
            res
    it "An extended transaction with invalid sender signature should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature =
                TransactionSignature $
                    Map.singleton
                        0
                        ( Map.singleton
                            0
                            (SigScheme.sign senderKeyPair "nice_try")
                        )
            sponsorTxSignature = makeTxSignature sponsorKeyPair txBodyHash
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `MaybeOk NormalTransactionInvalidSignatures` result"
            (TVer.MaybeOk $ TVer.NormalTransactionInvalidSignatures)
            res
    it "An extended transaction with invalid sponsor signature should be rejected" $ do
        let
            txHeader =
                TransactionHeaderV1
                    { thv1HeaderV0 =
                        TransactionHeader
                            { thSender = senderAccountAddress,
                              thNonce = 1,
                              thEnergyAmount = 302,
                              thPayloadSize = 8,
                              thExpiry = 0
                            },
                      thv1Sponsor = Just sponsorAccountAddress
                    }
            txPayload = EncodedPayload "deadbeef"
            txBodyHash = transactionV1SignHashFromHeaderPayload txHeader txPayload
            senderTxSignature = makeTxSignature senderKeyPair txBodyHash
            sponsorTxSignature =
                TransactionSignature $
                    Map.singleton
                        0
                        ( Map.singleton
                            0
                            (SigScheme.sign sponsorKeyPair "some_other_transaction")
                        )
            txSignatures =
                TransactionSignaturesV1
                    { tsv1Sender = senderTxSignature,
                      tsv1Sponsor = Just sponsorTxSignature
                    }
            tx = makeAccountTransactionV1 txSignatures txHeader txPayload
        let res =
                runIdentity $
                    (runTransactionVerifierT $ runTVTM $ TVer.verifyExtendedTransaction tx)
                        testData
        assertEqual
            "The verification should yield the expected `MaybeOk NormalTransactionInvalidSignatures` result"
            (TVer.MaybeOk $ TVer.NormalTransactionInvalidSignatures)
            res
  where
    senderAccountAddress = fst $ randomAccountAddress (mkStdGen 42)
    sponsorAccountAddress = fst $ randomAccountAddress (mkStdGen 43)
    ((senderSignKey, senderVerifyKey), _) = randomEd25519KeyPair $ mkStdGen 42
    senderKeyPair =
        SigScheme.KeyPairEd25519
            { signKey = senderSignKey,
              verifyKey = senderVerifyKey
            }
    ((sponsorSignKey, sponsorVerifyKey), _) = randomEd25519KeyPair $ mkStdGen 43
    sponsorKeyPair =
        SigScheme.KeyPairEd25519
            { signKey = sponsorSignKey,
              verifyKey = sponsorVerifyKey
            }
    mkAccountVerificationKeys key = 
        ID.AccountInformation
            { aiCredentials =
                Map.singleton
                    (ID.CredentialIndex 0)
                    ( ID.CredentialPublicKeys
                        { credKeys = Map.singleton 0 (SigScheme.VerifyKeyEd25519 key),
                          credThreshold = 0
                        }
                    ),
              aiThreshold = 0
            }
    senderAccountVerificationKeys = mkAccountVerificationKeys senderVerifyKey
    sponsorAccountVerificationKeys = mkAccountVerificationKeys sponsorVerifyKey
    makeTxSignature keyPair txBodyHash =
        TransactionSignature $
            Map.singleton
                0
                ( Map.singleton
                    0
                    (SigScheme.sign keyPair (transactionSignHashToByteString txBodyHash))
                )
    mkTestData :: Amount -> Amount -> Bool -> TransactionVerifierTestData pv
    mkTestData senderAvailableAmount sponsorAvailableAmount checkExactNonce = 
        TransactionVerifierTestData
            { tvtdAccounts =
                Map.fromList
                    [   ( "sender_account",
                          AccountTestData
                            { -- transasction cost is `computeCost 1/3 302` = 100+2/3
                              atdAccountAvailableAmount = senderAvailableAmount,
                              atdAccountNonce = 1,
                              atdAccountVerificationKeys = senderAccountVerificationKeys
                            }
                        ),
                        ( "sponsor_account",
                          AccountTestData
                            { atdAccountAvailableAmount = sponsorAvailableAmount,
                              atdAccountNonce = 1,
                              atdAccountVerificationKeys = sponsorAccountVerificationKeys
                            }
                        )
                    ],
              tvtdAccountsByAddress =
                Map.fromList
                    [ (senderAccountAddress, "sender_account"),
                      (sponsorAccountAddress, "sponsor_account")
                    ],
              tvtdEnergyRate = 1 % 3,
              tvtdCheckExactNonce = checkExactNonce
            }
    -- the minimal funding for the test transactions
    minFunding = 101
    testData = mkTestData minFunding minFunding True
    testDataNotExactNonce = mkTestData minFunding minFunding False
    testDataTooLittleFunding = mkTestData minFunding (minFunding - 1) True
    -- test data with missing sponsor account
    testDataNoSponsor :: TransactionVerifierTestData pv =
        TransactionVerifierTestData
            { tvtdAccounts =
                Map.fromList
                    [   ( "sender_account",
                          AccountTestData
                            { atdAccountAvailableAmount = minFunding,
                              atdAccountNonce = 1,
                              atdAccountVerificationKeys = senderAccountVerificationKeys
                            }
                        )
                    ],
              tvtdAccountsByAddress =
                Map.fromList
                    [ (senderAccountAddress, "sender_account")
                    ],
              tvtdEnergyRate = 1 % 3,
              tvtdCheckExactNonce = True
            } 

tests :: Spec
tests = describe "KonsensusV1.TransactionProcessing" $ do
    Common.forEveryProtocolVersionConsensusV1 $ \spv pvString ->
        describe pvString $ do
            describe "Individual transaction processing" $ do
                testProcessBlockItem spv
            describe "Batch transaction processing" $ do
                testProcessBlockItems spv
    describe "P9" $ do
        describe "Transaction verification" $
            testTransactionVerification SP9
    describe "P10" $ do
        describe "ExtendedTransaction verification" $
            testExtendedTransactionVerification SP10
