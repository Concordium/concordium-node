{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.Word
import Data.Time.Clock
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Data.Time.Clock.POSIX
import qualified Data.Aeson as AE

import Concordium.Types
import Concordium.TimeMonad
import Concordium.Types.Transactions
import Concordium.ID.Types
import Concordium.Crypto.FFIDataTypes
import Concordium.Common.Time
import Concordium.Common.Version
import Concordium.ID.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Crypto.SignatureScheme

import Concordium.Genesis.Data
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Basic.BlockState hiding (initialState)
import Concordium.Skov.Monad
import qualified Concordium.TransactionVerification as TVer

import Concordium.GlobalState.DummyData
import Concordium.Crypto.DummyData
import Lens.Micro.Platform
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import System.Random
import Concordium.Skov.Update
import Concordium.Types.Parameters (CryptographicParameters)
import Concordium.GlobalState.TreeState (TreeStateMonad(finalizeTransactions))

-- |Tests of doReceiveTransaction and doReceiveTransactionInternal of the Updater.
test :: Spec
test = do
  describe "Verification of received acccount creations" $ do
    parallel $
      specify "Invalid account creations should fail with expected error codes" $ do
      let gCtx = dummyGlobalContext
      now <- currentTime
      let genesis = testGenesisData now dummyIdentityProviders dummyArs dummyCryptographicParameters
          txs = accountCreations gCtx $ utcTimeToTransactionTime now
      s <- runMkCredentialDeployments txs now genesis
      let results = fst s
          outState = snd s
          cache = outState ^. transactionVerificationResults
      check results cache 0 False TVer.TransactionExpired
      check results cache 1 False TVer.ExpiryTooLate
      check results cache 2 True TVer.CredentialDeploymentExpired
      check results cache 3 True $ TVer.DuplicateAccountRegistrationID duplicateRegId
      check results cache 4 True TVer.CredentialDeploymentInvalidIdentityProvider
      check results cache 5 True TVer.CredentialDeploymentInvalidKeys
      check results cache 6 True TVer.CredentialDeploymentInvalidAnonymityRevokers
      check results cache 7 True TVer.CredentialDeploymentInvalidSignatures
      -- the intial account creation which has an invalid signature
      check results cache 8 True TVer.CredentialDeploymentInvalidSignatures
      -- now check that the cache is being cleared when we purge transactions
      s' <- runPurgeTransactions (addUTCTime (secondsToNominalDiffTime 2) now) outState
      let cache' = snd s' ^. transactionVerificationResults
      cache' `shouldBe` HM.empty
    specify "Receive valid account creations should result in success" $ do
      let credentialDeploymentExpiryTime = 1596409020
          now = posixSecondsToUTCTime $ credentialDeploymentExpiryTime - 1
          txArrivalTime = utcTimeToTransactionTime now
          genesis = testGenesisData now myips myars myCryptoParams
          txs = [toBlockItem txArrivalTime mycdi, toBlockItem txArrivalTime myicdi]
      s <- runMkCredentialDeployments txs now genesis
      let results = fst s
          outState = snd s
          cache = outState ^. transactionVerificationResults
      check results cache 0 True TVer.Success
      check results cache 1 True TVer.Success
    specify "Finalized account creations should be expunged from the cache"  $ do
      let credentialDeploymentExpiryTime = 1596409020
          now = posixSecondsToUTCTime $ credentialDeploymentExpiryTime - 1
          txArrivalTime = utcTimeToTransactionTime now
          txs = [toBlockItem txArrivalTime mycdi]
          genesis = testGenesisData now myips myars myCryptoParams
      s <- runMkCredentialDeployments txs now genesis
      let bh = genesisBlockHash genesis
      s' <- runFinalizeTransactions now bh txs $ snd s
      let cache' = snd s' ^. transactionVerificationResults
      cache' `shouldBe` HM.empty
  where
    check results cache idx shouldBeInCache verRes = do
      checkVerificationResult (snd $ results !! idx) $ mapTransactionVerificationResult verRes
      -- result should be in cache
      if shouldBeInCache
        then checkCacheIsOK cache (fst $ results !! idx) (Just verRes)
        else checkCacheIsOK cache (fst $ results !! idx) Nothing
    checkVerificationResult actual expected = do
      actual `shouldBe` expected
    checkCacheIsOK c k expected = do
      let cacheResult = HM.lookup k c
      cacheResult `shouldBe` expected

runMkCredentialDeployments :: [BlockItem] -> UTCTime -> GenesisData PV -> IO ([(TransactionHash, UpdateResult)], MyState)
runMkCredentialDeployments txs now gData = do
  runMyMonad' (testDoReceiveTransactionAccountCreations txs slot) now gData
  where
    slot = 0

runPurgeTransactions :: UTCTime -> MyState -> IO ((), MyState)
runPurgeTransactions = runMyMonad doPurgeTransactions

runFinalizeTransactions :: UTCTime -> BlockHash -> [BlockItem] -> MyState -> IO ((), MyState)
runFinalizeTransactions now bh txs s = do
  runMyMonad (finalizeTransactions bh slot txs) now s
  where
    slot = 0

type PV = 'P1
type MyBlockState = HashedBlockState PV
type MyState = SkovData PV MyBlockState

newtype FixedTime a = FixedTime { runDeterministic :: ReaderT UTCTime IO a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance TimeMonad FixedTime where
    currentTime = FixedTime ask

-- |A composition that implements TreeStateMonad, TimeMonad (via FixedTime) and SkovQueryMonadT.
type MyMonad = SkovQueryMonadT (PureTreeStateMonad PV MyBlockState (PureBlockStateMonad PV (StateT MyState FixedTime)))

-- |Run the computation in the given initial state. All queries to
-- 'currentTimeStamp' will return the given time. The IO is unfortunate, but it
-- is needed since PureBlockStateMonad is otherwise not a BlockStateStorage.
-- This should probably be revised at some point.
runMyMonad :: MyMonad a -> UTCTime -> MyState -> IO (a, MyState)
runMyMonad act time initialState = runReaderT (runDeterministic (runStateT (runPureBlockStateMonad . runPureTreeStateMonad . runSkovQueryMonad $ act) initialState)) time

-- |Run the given computation in a state consisting of only the genesis block and the state determined by it.
runMyMonad' :: MyMonad a -> UTCTime -> GenesisData PV -> IO (a, MyState)
runMyMonad' act time gd = runPureBlockStateMonad (initialSkovDataDefault gd (hashBlockState bs)) >>= runMyMonad act time
  where
    bs = case genesisState gd of
               Left err -> error $ "Invalid genesis state: " ++ err
               Right x -> x

-- |Construct a genesis state with hardcoded values for parameters that should not affect this test.
-- Modify as you see fit.
testGenesisData :: UTCTime -> IdentityProviders -> AnonymityRevokers -> CryptographicParameters -> GenesisData PV
testGenesisData now ips ars cryptoParams = makeTestingGenesisDataP1 (utcTimeToTimestamp now) 1 1 1 dummyFinalizationCommitteeMaxSize cryptoParams ips ars maxBound dummyKeyCollection dummyChainParameters

testDoReceiveTransactionAccountCreations :: [BlockItem] -> Slot -> MyMonad [(TransactionHash, UpdateResult)]
testDoReceiveTransactionAccountCreations trs slot = mapM (\tr -> doReceiveTransaction tr slot
                                                           >>= (\res -> pure (wmdHash tr, res))) trs

accountCreations :: GlobalContext -> TransactionTime -> [BlockItem]
accountCreations gCtx now =
  [
    expiredTransaction,
    credentialDeploymentWithExpiryTooLate,
    expiredCredentialDeployment,
    credentialDeploymentWithDuplicateRegId,
    credentialWithInvalidIP,
    credentialWithInvalidKeys,
    credentialWithInvalidAr,
    credentialWithInvalidSignatures,
    intialCredentialWithInvalidSignatures
  ]
  where
    expiry = now + 1
    expiredTransaction = toBlockItem now (mkAccountCreation (now - 1) (regId 1) 0 True True False)
    credentialDeploymentWithExpiryTooLate =  toBlockItem now (mkAccountCreation (now + (60 * 60 * 2) + 1) (regId 0) 0 True True False)
    expiredCredentialDeployment = toBlockItem now (mkAccountCreation expiry (regId 1) 0 True True True)
    credentialDeploymentWithDuplicateRegId = toBlockItem now (mkAccountCreation expiry duplicateRegId 0 True True False)
    credentialWithInvalidIP = toBlockItem now (mkAccountCreation expiry (regId 1) 42 True True False)
    credentialWithInvalidKeys = toBlockItem now (mkAccountCreation expiry (regId 1) 0 True False False)
    credentialWithInvalidAr = toBlockItem now (mkAccountCreation expiry (regId 1) 0 False True False)
    credentialWithInvalidSignatures = toBlockItem now (mkAccountCreation expiry (regId 1) 0 True True False)
    intialCredentialWithInvalidSignatures = toBlockItem now (mkInitialAccountCreationWithInvalidSignatures expiry (regId 42))
    regId seed = RegIdCred $ generateGroupElementFromSeed gCtx seed

toBlockItem :: TransactionTime -> AccountCreation -> BlockItem
toBlockItem now acc = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now acc

duplicateRegId :: CredentialRegistrationID
duplicateRegId = cred
  where
    cred = maybe
      undefined credId
      (Map.lookup 0 (gaCredentials $ head (makeFakeBakers 1)))

mkAccountCreation :: TransactionTime -> CredentialRegistrationID -> Word32 -> Bool -> Bool ->  Bool -> AccountCreation
mkAccountCreation expiry regId identityProviderId validAr validPubKeys credExpired = AccountCreation
  {
    messageExpiry=expiry,
    credential= NormalACWP CredentialDeploymentInformation
                {
                  cdiValues=CredentialDeploymentValues
                  {
                    cdvPublicKeys=mkCredentialPublicKeys validPubKeys,
                    cdvCredId=regId,
                    cdvIpId=IP_ID identityProviderId,
                    cdvThreshold=Threshold 1,
                    cdvArData=mkArData validAr,
                    cdvPolicy=mkPolicy credExpired
                  },
                  cdiProofs=Proofs "invalid proof"
                }
  }

mkInitialAccountCreationWithInvalidSignatures :: TransactionTime -> CredentialRegistrationID -> AccountCreation
mkInitialAccountCreationWithInvalidSignatures expiry regId = AccountCreation
  {
    messageExpiry=expiry,
    credential=InitialACWP InitialCredentialDeploymentInfo
               {
                 icdiValues=InitialCredentialDeploymentValues
                 {
                   icdvAccount=mkCredentialPublicKeys True,
                   icdvRegId=regId,
                   icdvIpId=IP_ID 0,
                   icdvPolicy=mkPolicy False
                 },
                 icdiSig=IpCdiSignature
                 {
                   theSignature="invalid signature"
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
        {
          credKeys=Map.empty,
          credThreshold=0
        }
    key = SigScheme.correspondingVerifyKey $ dummyKeyPair 1

dummyKeyPair :: Int -> SigScheme.KeyPair
dummyKeyPair = uncurry SigScheme.KeyPairEd25519 . fst . randomEd25519KeyPair . mkStdGen

mkPolicy :: Bool -> Policy
mkPolicy expired = mkDummyPolicy where
  mkDummyPolicy = Policy
           {
             pValidTo=_validTo,
             pCreatedAt=YearMonth
                        {
                          ymYear=2021,
                          ymMonth=1
                        },
             pItems=Map.empty
           }
  _validTo = if expired
    then 
      YearMonth
           {
             ymYear=1970,
             ymMonth=1
           }
    else 
      YearMonth
            {
              ymYear=2070,
              ymMonth=1
            } 

mkArData :: Bool -> Map.Map ArIdentity ChainArData
mkArData valid = if valid then validAr else Map.empty
  where
    validAr = Map.insert key arData Map.empty
    arData = ChainArData {ardIdCredPubShare=AREnc zeroElgamalCipher}
    key = head $ Map.keys $ arRevokers dummyArs

mkCredentialKeyPair :: IO KeyPair
mkCredentialKeyPair = newKeyPair Ed25519

-- expiry time for credentials 1596409020
{-# WARNING mycdi "Do not use in production." #-}
mycdi :: AccountCreation
mycdi = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-credential.json" >>= embedFile)

{-# WARNING myicdi "Do not use in production." #-}
myicdi :: AccountCreation
myicdi = readAccountCreation . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-initial-credential.json" >>= embedFile)

{-# WARNING myips "Do not use in production." #-}
myips :: IdentityProviders
myips = case readIps . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ips.json" >>= embedFile) of
  Just x -> x
  Nothing -> error "oops"

{-# WARNING myars "Do not use in production." #-}
myars :: AnonymityRevokers
myars = case readArs . BSL.fromStrict $ $(makeRelativeToProject "testdata/transactionverification/verifiable-ars.json" >>= embedFile) of
  Just x -> x
  Nothing -> error "oops"

{-# WARNING myCryptoParams "Do not use in production" #-}
myCryptoParams :: CryptographicParameters
myCryptoParams =
  case getExactVersionedCryptographicParameters (BSL.fromStrict $(makeRelativeToProject "testdata/transactionverification/verifiable-global.json" >>= embedFile)) of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

readAccountCreation :: BSL.ByteString -> AccountCreation
readAccountCreation bs =
  case AE.eitherDecode bs of
    Left err -> error $ "Cannot read account creation " ++ err
    Right d -> if vVersion d == 0 then vValue d else error "Incorrect account creation version."

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
