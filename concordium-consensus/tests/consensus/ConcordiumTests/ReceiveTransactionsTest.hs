{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- The ReceiveTransactionsTest is testing verification of transactions received
-- either individualy or via a block.
--
-- In particular this module tests `doReceiveTransaction` and `doReceiveTransactionInternal` of `Update.hs`.
--
-- The module tests that `CredentialDeployments`, `ChainUpdates` and `NormalTransactions` are being verified according to the protocol
-- specified by the `TransactionVerification` module.
module ConcordiumTests.ReceiveTransactionsTest (test) where

import Test.Hspec

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Aeson as AE
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Lens.Micro.Platform
import System.IO.Temp (withTempDirectory)
import System.Random

import Concordium.Common.Time
import Concordium.Common.Version

import qualified Concordium.Cost as Cost
import Concordium.Crypto.DummyData
import Concordium.Crypto.FFIDataTypes
import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import Concordium.Genesis.Data
import Concordium.GlobalState.Account
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Parameters hiding (getExactVersionedCryptographicParameters)
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.Genesis
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.TransactionTable
import Concordium.GlobalState.Transactions
import Concordium.ID.Parameters
import Concordium.ID.Types
import Concordium.Logger
import Concordium.Scheduler.DummyData
import Concordium.Skov.Monad
import Concordium.Skov.Update
import Concordium.TimeMonad
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Execution
import Concordium.Types.IdentityProviders
import Concordium.Types.Transactions
import Concordium.Types.Updates

-- |Tests of doReceiveTransaction and doReceiveTransactionInternal.
test :: Spec
test = do
    describe "Verification of receiving transactions" $ do
        parallel $
            specify "Transactions received individually" $ do
                let gCtx = dummyGlobalContext
                now <- currentTime
                let genesis = testGenesisData now dummyIdentityProviders dummyArs dummyCryptographicParameters
                    tt = utcTimeToTransactionTime now
                    txs = accountCreations gCtx tt ++ chainUpdates tt ++ normals tt True 1
                s <- runTransactions testDoReceiveTransaction txs now genesis
                let results = fst s
                    resultingState = snd s
                -- Credential deployments
                check resultingState results 0 ResultStale False
                check resultingState results 1 ResultCredentialDeploymentExpired False
                check resultingState results 2 ResultDuplicateAccountRegistrationID False
                check resultingState results 3 ResultCredentialDeploymentInvalidIP False
                check resultingState results 4 ResultCredentialDeploymentInvalidAR False
                check resultingState results 5 ResultCredentialDeploymentInvalidSignatures False
                -- the intial account creation which has an invalid signature
                check resultingState results 6 ResultCredentialDeploymentInvalidSignatures False
                -- Chain Updates
                check resultingState results 7 ResultStale False
                check resultingState results 8 ResultChainUpdateInvalidEffectiveTime False
                check resultingState results 9 ResultChainUpdateSequenceNumberTooOld False
                check resultingState results 10 ResultNonceTooLarge False
                check resultingState results 11 ResultChainUpdateInvalidSignatures False
                check resultingState results 12 ResultSuccess True
                -- Normal Transactions
                check resultingState results 13 ResultStale False
                check resultingState results 14 ResultTooLowEnergy False
                check resultingState results 15 ResultNonexistingSenderAccount False
                check resultingState results 16 ResultNonceTooLarge False
                check resultingState results 17 ResultVerificationFailed False
                check resultingState results 18 ResultSuccess True
                check resultingState results 19 ResultDuplicate True --- already present
                check resultingState results 20 ResultSuccess True -- here to check that the TransactionVerifier uses the `correct` nonce for checking validity.
                check resultingState results 21 ResultEnergyExceeded False
                check resultingState results 22 ResultDuplicateNonce False
                check resultingState results 23 ResultSuccess True -- exactly max block energy
        specify "Credential deployments received individually" $ do
            let credentialDeploymentExpiryTime = 1596409020
                now = posixSecondsToUTCTime $ credentialDeploymentExpiryTime - 1
                txArrivalTime = utcTimeToTransactionTime now
                genesis = testGenesisData now myips myars myCryptoParams
                txs = [toBlockItem txArrivalTime mycdi, toBlockItem txArrivalTime myicdi]
            s <- runTransactions testDoReceiveTransaction txs now genesis
            let results = fst s
                resultingState = snd s
            check resultingState results 0 ResultSuccess True
            check resultingState results 1 ResultSuccess True
        specify "Transactions received as part of a block" $ do
            let gCtx = dummyGlobalContext
            now <- currentTime
            let genesis = testGenesisData now dummyIdentityProviders dummyArs dummyCryptographicParameters
                tt = utcTimeToTransactionTime now
                txs = accountCreations gCtx tt ++ chainUpdates tt ++ normals tt False 4
            s <- runTransactions testDoReceiveTransactionInternal txs now genesis
            let results = fst s
                resultingState = snd s
            -- Credential deployments
            check resultingState results 0 ResultStale False
            check resultingState results 1 ResultCredentialDeploymentExpired False
            check resultingState results 2 ResultDuplicateAccountRegistrationID False
            check resultingState results 3 ResultCredentialDeploymentInvalidIP True
            check resultingState results 4 ResultCredentialDeploymentInvalidAR True
            check resultingState results 5 ResultCredentialDeploymentInvalidSignatures False
            -- the intial account creation which has an invalid signature
            check resultingState results 6 ResultCredentialDeploymentInvalidSignatures False
            -- now check that the cache is being cleared when we purge transactions
            -- Chain Updates
            check resultingState results 7 ResultStale False
            check resultingState results 8 ResultChainUpdateInvalidEffectiveTime False
            check resultingState results 9 ResultChainUpdateSequenceNumberTooOld False
            check resultingState results 10 ResultSuccess True -- update sequence number too large, but as the transaction is from a block so we accept it.
            check resultingState results 11 ResultChainUpdateInvalidSignatures True
            check resultingState results 12 ResultSuccess True
            -- Normal Transactions
            check resultingState results 13 ResultStale False
            check resultingState results 14 ResultTooLowEnergy False
            check resultingState results 15 ResultNonexistingSenderAccount True
            check resultingState results 16 ResultSuccess True -- nonce too large, but as the transaction is from a block so we accept it.
            check resultingState results 17 ResultVerificationFailed True
            check resultingState results 18 ResultSuccess True
            check resultingState results 19 ResultDuplicate True -- already present
            check resultingState results 20 ResultSuccess True
            check resultingState results 21 ResultEnergyExceeded True
            check resultingState results 22 ResultDuplicateNonce False
            check resultingState results 23 ResultSuccess True -- exactly max block energy
        specify "doReceiveTransactionInternal with valid credential deployment" $ do
            let credentialDeploymentExpiryTime = 1596409020
                now = posixSecondsToUTCTime $ credentialDeploymentExpiryTime - 1
                txArrivalTime = utcTimeToTransactionTime now
                txs = [toBlockItem txArrivalTime mycdi, toBlockItem txArrivalTime myicdi]
                genesis = testGenesisData now myips myars myCryptoParams
            s <- runTransactions testDoReceiveTransactionInternal txs now genesis
            let results = fst s
                resultingState = snd s
            check resultingState results 0 ResultSuccess True
            check resultingState results 1 ResultSuccess True
        specify "receive transactions via blocks out of order preserves invariant of ptt" $ do
            theTime <- currentTime
            let now = utcTimeToTransactionTime theTime
                genesis = testGenesisData theTime dummyIdentityProviders dummyArs dummyCryptographicParameters
                txs =
                    [ toBlockItem now $ mkAccountTransaction (now + 1) True 2 True TheCost,
                      toBlockItem now $ mkAccountTransaction (now + 1) True 1 True TheCost
                    ]
            s <- runTransactions testDoReceiveTransactionInternal txs theTime genesis
            let results = fst s
                resultingState = snd s
            check resultingState results 0 ResultSuccess True
            check resultingState results 1 ResultSuccess True
            let ptt = resultingState ^. (pendingTransactions . pttWithSender)
            let aaddr = accountAddressEmbed (dummyAccountAddress 1)
            fst <$> (HM.lookup aaddr ptt) `shouldBe` Just 1
  where
    check resultingState results idx expectedVerRes expectedInTransactionTable = do
        let result = results !! idx
        -- check that verification result is as expected
        snd result `shouldBe` expectedVerRes
        -- check whether the transaction is present in the transaction table.
        let tt = resultingState ^. (transactionTable . ttHashMap)
        HM.member (fst result) tt `shouldBe` expectedInTransactionTable

type TestFunction = [BlockItem] -> Slot -> TestSkovQueryMonad [(TransactionHash, UpdateResult)]

runTransactions :: TestFunction -> [BlockItem] -> UTCTime -> GenesisData PV -> IO ([(TransactionHash, UpdateResult)], TestSkovState)
runTransactions f txs now gData = do
    runTestSkovQueryMonad' (f txs slot) now gData
  where
    slot = 0

type PV = 'P5
type TestSkovState = SkovPersistentData PV

newtype FixedTimeT (m :: Type -> Type) a = FixedTime {runDeterministic :: UTCTime -> m a}
    deriving (Functor, Applicative, Monad, MonadIO) via ReaderT UTCTime m
    deriving (MonadTrans) via ReaderT UTCTime

instance MonadReader r m => MonadReader r (FixedTimeT m) where
    ask = lift ask
    local f (FixedTime k) = FixedTime $ local f . k

instance Monad m => MonadLogger (FixedTimeT m) where
    logEvent _ _ _ = return ()

instance Monad m => TimeMonad (FixedTimeT m) where
    currentTime = FixedTime return

type TestBlockStateMonad =
    BS.PersistentBlockStateMonad
        PV
        (BS.PersistentBlockStateContext PV)
        ( StateT
            TestSkovState
            (NoLoggerT (FixedTimeT (Blob.BlobStoreM' (BS.PersistentBlockStateContext PV))))
        )

-- |A composition that implements TreeStateMonad, TimeMonad (via FixedTime) and SkovQueryMonadT.
type TestSkovQueryMonad = SkovQueryMonadT (PersistentTreeStateMonad TestSkovState TestBlockStateMonad)

newtype NoLoggerT m a = NoLoggerT {runNoLoggerT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, TimeMonad)

instance Monad m => MonadLogger (NoLoggerT m) where
    logEvent _ _ _ = return ()

-- |Run the given computation in a state consisting of only the genesis block and the state determined by it.
runTestSkovQueryMonad' :: TestSkovQueryMonad a -> UTCTime -> GenesisData PV -> IO (a, TestSkovState)
runTestSkovQueryMonad' act time gd = do
    withTempDirectory "." "treestate" $ \tsDir -> do
        Blob.runBlobStoreTemp "." $
            BS.withNewAccountCache 1000 $ do
                initState <- runPersistentBlockStateMonad $ initialData tsDir
                runDeterministic (runNoLoggerT (runStateT (runPersistentBlockStateMonad . runPersistentTreeStateMonad . runSkovQueryMonad $ act) initState)) time
  where
    initialData ::
        FilePath ->
        BS.PersistentBlockStateMonad
            PV
            (BS.PersistentBlockStateContext PV)
            (Blob.BlobStoreM' (BS.PersistentBlockStateContext PV))
            TestSkovState
    initialData tsDir = do
        (bs, genTT) <-
            genesisState gd >>= \case
                Left err -> error $ "Invalid genesis state: " ++ err
                Right x -> return x
        cryptoParams <- getCryptographicParameters bs
        updBS <- thawBlockState bs
        let now = utcTimeToTransactionTime time
        bsWithUpdate <- bsoEnqueueUpdate updBS (now - 1) (UVAddIdentityProvider myipInfo)
        bsWithAccount <-
            bsoCreateAccount bsWithUpdate cryptoParams (accountAddressFromSeed 1) (makeTestCredentialFromSeed 1) >>= \case
                (Nothing, _) -> error "Account should not already exist."
                (Just _, newBS) -> do
                    newAccIdx <- fromIntegral . length <$> getAccountList bs
                    bsoModifyAccount newBS (emptyAccountUpdate newAccIdx & (auAmount ?~ amountToDelta 10 ^ (10 :: Int)))
        finalBS <- freezeBlockState bsWithAccount
        serializedState <- saveBlockState finalBS
        initialSkovPersistentDataDefault tsDir (genesisConfiguration gd) finalBS serializedState genTT Nothing

maxBlockEnergy :: Energy
maxBlockEnergy = 3_000_000

-- |Construct a genesis state with hardcoded values for parameters that should not affect this test.
-- Modify as you see fit.
testGenesisData :: UTCTime -> IdentityProviders -> AnonymityRevokers -> CryptographicParameters -> GenesisData PV
testGenesisData now ips ars cryptoParams = makeTestingGenesisDataP5 (utcTimeToTimestamp now) 1 1 1 dummyFinalizationCommitteeMaxSize cryptoParams ips ars maxBlockEnergy dummyKeyCollection dummyChainParameters

-- |Run the doReceiveTransaction function and obtain the results
testDoReceiveTransaction :: [BlockItem] -> Slot -> TestSkovQueryMonad [(TransactionHash, UpdateResult)]
testDoReceiveTransaction trs _ =
    mapM
        ( \tr ->
            doReceiveTransaction tr
                >>= (\res -> pure (wmdHash tr, res))
        )
        trs

-- |Run the doReceiveTransactionInternal function and obtain the results
testDoReceiveTransactionInternal :: [BlockItem] -> Slot -> TestSkovQueryMonad [(TransactionHash, UpdateResult)]
testDoReceiveTransactionInternal trs slot = do
    theTime <- currentTime
    bs <- queryBlockState =<< lastFinalizedBlock
    mapM
        ( \tr ->
            doReceiveTransactionInternal Block bs tr (utcTimeToTimestamp theTime) slot
                >>= (\res -> pure (wmdHash tr, snd res))
        )
        trs

accountCreations :: GlobalContext -> TransactionTime -> [BlockItem]
accountCreations gCtx now =
    [ expiredTransaction,
      expiredCredentialDeployment,
      credentialDeploymentWithDuplicateRegId,
      credentialWithInvalidIP,
      credentialWithInvalidAr,
      credentialWithInvalidSignatures,
      intialCredentialWithInvalidSignatures
    ]
  where
    expiry = now + 1
    expiredTransaction = toBlockItem now (mkAccountCreation (now - 1) (regId 1) 0 True True False)
    expiredCredentialDeployment = toBlockItem now (mkAccountCreation expiry (regId 1) 0 True True True)
    credentialDeploymentWithDuplicateRegId = toBlockItem now (mkAccountCreation expiry duplicateRegId 0 True True False)
    credentialWithInvalidIP = toBlockItem now (mkAccountCreation expiry (regId 1) 42 True True False)
    credentialWithInvalidAr = toBlockItem now (mkAccountCreation expiry (regId 1) 0 False True False)
    credentialWithInvalidSignatures = toBlockItem now (mkAccountCreation expiry (regId 1) 0 True True False)
    intialCredentialWithInvalidSignatures = toBlockItem now (mkInitialAccountCreationWithInvalidSignatures expiry (regId 42))
    regId seed = RegIdCred $ generateGroupElementFromSeed gCtx seed

chainUpdates :: TransactionTime -> [BlockItem]
chainUpdates now =
    [ expiredTimeout,
      invalidEffectiveTime,
      sequenceNumberTooOld,
      sequenceNumberTooLarge,
      invalidSignature,
      verifiable
    ]
  where
    expiredTimeout = toBlockItem now (mkChainUpdate (now - 1) (now - 1) getValidSequenceNumber True)
    invalidEffectiveTime = toBlockItem now (mkChainUpdate (now + 1) (now + 2) getValidSequenceNumber True)
    sequenceNumberTooOld = toBlockItem now (mkChainUpdate (now + 2) (now + 1) getTooOldSequenceNumber True)
    sequenceNumberTooLarge = toBlockItem now (mkChainUpdate (now + 2) (now + 1) getTooLargeSequenceNumber True)
    invalidSignature = toBlockItem now (mkChainUpdate (now + 2) (now + 1) getValidSequenceNumber False)
    verifiable = toBlockItem now (mkChainUpdate (now + 2) (now + 1) getValidSequenceNumber True)
    getValidSequenceNumber = minUpdateSequenceNumber + 1
    getTooOldSequenceNumber = minUpdateSequenceNumber
    getTooLargeSequenceNumber = minUpdateSequenceNumber + 2

-- The isSingle flag determines if the transaction should be perceived
-- as it has been received individually or as part of a block.
normals :: TransactionTime -> Bool -> Nonce -> [BlockItem]
normals now isSingle successNonce =
    [ expired,
      depositInsufficient,
      invalidSender,
      nonceTooLarge,
      invalidSignature,
      verifiable successNonce, -- should become part of the transaction table.
      verifiable successNonce, -- duplicate nonce
      verifiable $ 1 + successNonce,
      tooMuchEnergy,
      verifiable 0, -- duplicate nonce
      atMaxBlockEnergy
    ]
  where
    expired = toBlockItem now $ mkAccountTransaction (now - 1) True 1 True TheCost
    depositInsufficient = toBlockItem now $ mkAccountTransaction (now + 1) True 1 True TooLittle
    invalidSender = toBlockItem now $ mkAccountTransaction (now + 1) True 1 False TheCost
    -- 'invalidNonce' should be accepted for transactions received individually, but rejected if it was part of a block.
    nonceTooLarge = toBlockItem now $ mkAccountTransaction (now + 1) True 3 True TheCost
    -- since the one above was accepted because it was part of a block we must increment the nonce here,
    -- if the transaction  is part of a block
    invalidSignature =
        if isSingle
            then toBlockItem now $ mkAccountTransaction (now + 1) False 1 True TheCost
            else toBlockItem now $ mkAccountTransaction (now + 1) False 3 True TheCost
    -- This ones also needs a nonce depending of the `TransactionOrigin`
    verifiable nonce = toBlockItem now $ mkAccountTransaction (now + 1) True nonce True TheCost
    -- Also this one.
    tooMuchEnergy =
        if isSingle
            then toBlockItem now $ mkAccountTransaction (now + 1) True 2 True TooMuch
            else toBlockItem now $ mkAccountTransaction (now + 1) True 5 True TooMuch
    -- transactions which state exactly the max block energy bound should be accepted
    atMaxBlockEnergy = toBlockItem now $ mkAccountTransaction (now + 1) True (if isSingle then 3 else 6) True MaxBlockEnergy

toBlockItem :: TransactionTime -> BareBlockItem -> BlockItem
toBlockItem now bbi =
    case bbi of
        CredentialDeployment cred -> credentialDeployment $ addMetadata (\x -> CredentialDeployment{biCred = x}) now cred
        ChainUpdate ui -> chainUpdate $ addMetadata (\x -> ChainUpdate{biUpdate = x}) now ui
        NormalTransaction tx -> normalTransaction $ addMetadata (\x -> NormalTransaction{biTransaction = x}) now tx

duplicateRegId :: CredentialRegistrationID
duplicateRegId = credId (makeTestCredentialFromSeed 1)

-- |Specification of the supplied energy for a transaction
data TestEnergyParam
    = -- |The tranasction will be provided with a too little amount of energy for
      -- the particular transaction.
      TooLittle
    | -- |The tranasction will be provided with exactly maximum block energy amount.
      TooMuch
    | -- |The energy provided was more than what is allowed in a block,
      -- and as such the transaction will be rejected.
      MaxBlockEnergy
    | -- |The energy was set to the correct cost for the transaction.
      TheCost
    deriving (Eq)

mkAccountTransaction :: TransactionTime -> Bool -> Nonce -> Bool -> TestEnergyParam -> BareBlockItem
mkAccountTransaction expiry validSignature nonce validSender energyAmount =
    case validSignature of
        False ->
            NormalTransaction
                { biTransaction =
                    AccountTransaction
                        { atrSignature = TransactionSignature{tsSignatures = Map.empty},
                          atrHeader = mkHeader,
                          atrPayload = payload,
                          atrSignHash = TransactionSignHashV0{v0TransactionSignHash = dummyHash}
                        }
                }
        True -> NormalTransaction{biTransaction = signTransactionSingle (dummyKeyPair 1) mkHeader payload}
  where
    mkHeader =
        TransactionHeader
            { thSender = if validSender then dummyAccountAddress 1 else dummyAccountAddress 99,
              thNonce = nonce,
              thEnergyAmount = theCost,
              thPayloadSize = payloadSize payload,
              thExpiry = expiry
            }
    payload = encodePayload $ Transfer (dummyAccountAddress 1) 10
    theCost = case energyAmount of
        TooLittle -> 0
        TooMuch -> maxBound
        MaxBlockEnergy -> maxBlockEnergy
        TheCost -> Cost.baseCost (getTransactionHeaderPayloadSize mkHeaderWithoutCost) 1
    mkHeaderWithoutCost =
        TransactionHeader
            { thSender = if validSender then dummyAccountAddress 1 else dummyAccountAddress 99,
              thNonce = nonce,
              thEnergyAmount = 0,
              thPayloadSize = payloadSize payload,
              thExpiry = expiry
            }

mkChainUpdate :: TransactionTime -> TransactionTime -> UpdateSequenceNumber -> Bool -> BareBlockItem
mkChainUpdate effectTime timeout sequenceNumber validSignature =
    if validSignature
        then ChainUpdate ui
        else ChainUpdate dummyUi
  where
    ui = makeUpdateInstruction rawUi $ Map.singleton 0 dummyAuthorizationKeyPair
    rawUi =
        RawUpdateInstruction
            { ruiSeqNumber = sequenceNumber,
              ruiEffectiveTime = effectTime,
              ruiTimeout = timeout,
              ruiPayload = payload
            }
    dummyUi =
        UpdateInstruction
            { uiHeader = dummyHeader,
              uiPayload = payload,
              uiSignHash = mkDummySignHash,
              uiSignatures = mkDummySignature
            }
    dummyHeader =
        UpdateHeader
            { updateSeqNumber = sequenceNumber,
              updateEffectiveTime = effectTime,
              updateTimeout = timeout,
              updatePayloadSize = 42
            }
    payload = AddIdentityProviderUpdatePayload myipInfo
    mkDummySignHash = UpdateInstructionSignHashV0 dummyHash
    mkDummySignature = UpdateInstructionSignatures Map.empty

mkAccountCreation :: TransactionTime -> CredentialRegistrationID -> Word32 -> Bool -> Bool -> Bool -> BareBlockItem
mkAccountCreation expiry regId identityProviderId validAr validPubKeys credExpired =
    CredentialDeployment
        AccountCreation
            { messageExpiry = expiry,
              credential =
                NormalACWP
                    CredentialDeploymentInformation
                        { cdiValues =
                            CredentialDeploymentValues
                                { cdvPublicKeys = mkCredentialPublicKeys validPubKeys,
                                  cdvCredId = regId,
                                  cdvIpId = IP_ID identityProviderId,
                                  cdvThreshold = Threshold 1,
                                  cdvArData = mkArData validAr,
                                  cdvPolicy = mkPolicy credExpired
                                },
                          cdiProofs = Proofs "invalid proof"
                        }
            }

mkInitialAccountCreationWithInvalidSignatures :: TransactionTime -> CredentialRegistrationID -> BareBlockItem
mkInitialAccountCreationWithInvalidSignatures expiry regId =
    CredentialDeployment
        AccountCreation
            { messageExpiry = expiry,
              credential =
                InitialACWP
                    InitialCredentialDeploymentInfo
                        { icdiValues =
                            InitialCredentialDeploymentValues
                                { icdvAccount = mkCredentialPublicKeys True,
                                  icdvRegId = regId,
                                  icdvIpId = IP_ID 0,
                                  icdvPolicy = mkPolicy False
                                },
                          icdiSig =
                            IpCdiSignature
                                { theSignature = "invalid signature"
                                }
                        }
            }

mkCredentialPublicKeys :: Bool -> CredentialPublicKeys
mkCredentialPublicKeys validKeys = credKeys
  where
    credKeys =
        if validKeys
            then makeCredentialPublicKeys [key] 1
            else
                CredentialPublicKeys
                    { credKeys = Map.empty,
                      credThreshold = 0
                    }
    key = SigScheme.correspondingVerifyKey $ dummyKeyPair 1

dummyKeyPair :: Int -> SigScheme.KeyPair
dummyKeyPair = uncurry SigScheme.KeyPairEd25519 . fst . randomEd25519KeyPair . mkStdGen

mkPolicy :: Bool -> Policy
mkPolicy expired = mkDummyPolicy
  where
    mkDummyPolicy =
        Policy
            { pValidTo = _validTo,
              pCreatedAt =
                YearMonth
                    { ymYear = 2021,
                      ymMonth = 1
                    },
              pItems = Map.empty
            }
    _validTo =
        if expired
            then
                YearMonth
                    { ymYear = 1970,
                      ymMonth = 1
                    }
            else
                YearMonth
                    { ymYear = 2070,
                      ymMonth = 1
                    }

mkArData :: Bool -> Map.Map ArIdentity ChainArData
mkArData valid = if valid then validAr else Map.empty
  where
    validAr = Map.insert key arData Map.empty
    arData = ChainArData{ardIdCredPubShare = AREnc zeroElgamalCipher}
    key = head $ Map.keys $ arRevokers dummyArs

dummyHash :: SHA256.Hash
dummyHash = SHA256.hash B.empty

-- expiry time for below two credentials 1596409020
-- They are valid and should pass credential verification.
mycdi :: BareBlockItem
mycdi = CredentialDeployment $ readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

myicdi :: BareBlockItem
myicdi = CredentialDeployment $ readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-initial-credential.json" >>= embedFile)

myipInfo :: IpInfo
myipInfo = case Map.lookup (IP_ID 0) (idProviders myips) of
    Just x -> x
    Nothing -> error "oops"

myips :: IdentityProviders
myips = case readIps . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ips.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

myars :: AnonymityRevokers
myars = case readArs . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ars.json" >>= embedFile) of
    Just x -> x
    Nothing -> error "oops"

myCryptoParams :: CryptographicParameters
myCryptoParams =
    case getExactVersionedCryptographicParameters (BSL.fromStrict $(makeRelativeToProject "testdata/transactionverification/verifiable-global.json" >>= embedFile)) of
        Nothing -> error "Could not read cryptographic parameters."
        Just params -> params

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

dummyAccountAddress :: Int -> AccountAddress
dummyAccountAddress seed = fst $ randomAccountAddress (mkStdGen seed)
