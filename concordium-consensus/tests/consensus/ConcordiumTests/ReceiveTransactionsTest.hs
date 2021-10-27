{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.Word
import Data.Time.Clock
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM

import Concordium.Types
import Concordium.TimeMonad
import Concordium.Types.Transactions
import Concordium.ID.Types
import Concordium.Crypto.FFIDataTypes
import Concordium.Common.Time
import Concordium.ID.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Crypto.SignatureScheme

import Concordium.Genesis.Data
import Concordium.GlobalState.Types ()
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


-- |Tests of doReceiveTransaction and doReceiveTransactionInternal of the Updater.
test :: Spec
test = do
  describe "doReceiveTansaction" $ do
    parallel $
      specify "Receive normal (invalid) account creation should fail properly" $ do
      let gCtx = dummyGlobalContext
      now <- currentTime 
      s <- runMkNormalCredentialDeployments gCtx now
      let results = fst s
      let outState = snd s
      let cache = outState ^. transactionVerificationResults
      check results cache 0 False TVer.ResultExpiryTooLate
      check results cache 1 False TVer.ResultTransactionExpired
      check results cache 2 True $ TVer.ResultDuplicateAccountRegistrationID duplicateRegId
      check results cache 3 True TVer.ResultCredentialDeploymentInvalidIdentityProvider
      check results cache 4 True TVer.ResultCredentialDeploymentInvalidKeys
      check results cache 5 True TVer.ResultCredentialDeploymentInvalidAnonymityRevokers
      check results cache 6 True TVer.ResultCredentialDeploymentInvalidSignatures
      -- now check that the cache is being cleared when we purge transactions
      s' <- runPurgeTransactions (addUTCTime (secondsToNominalDiffTime 2) now) outState
      let cache' = snd s' ^. transactionVerificationResults
      cache' `shouldBe` HM.empty
    specify "Receive normal valid account creation should result in success" $ do
      makeFakeBakers 1 `shouldBe` makeFakeBakers 1
      -- todo
      1 `shouldBe` 1
    specify "Receive inital (invalid) account creation should fail properly" $ do
      -- todo
      1 `shouldBe` 1
    specify "Receive intial valid account creation should result in success" $ do
      -- todo
      1 `shouldBe` 1
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

runMkNormalCredentialDeployments :: GlobalContext -> UTCTime -> IO ([(TransactionHash, UpdateResult)], MyState)
runMkNormalCredentialDeployments gCtx now = do
  runMyMonad' (testDoReceiveTransactionAccountCreations txs slot) now (testGenesisData now dummyIdentityProviders dummyArs)
  where
    txs = accountCreations gCtx $ utcTimeToTransactionTime now
    slot = 0

runPurgeTransactions :: UTCTime -> MyState -> IO ((), MyState)
runPurgeTransactions = runMyMonad doPurgeTransactions
                         
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
testGenesisData :: UTCTime -> IdentityProviders -> AnonymityRevokers -> GenesisData PV
testGenesisData now ips ars = makeTestingGenesisDataP1 (utcTimeToTimestamp now) 1 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters ips ars maxBound dummyKeyCollection dummyChainParameters

testDoReceiveTransactionAccountCreations :: [BlockItem] -> Slot -> MyMonad [(TransactionHash, UpdateResult)]
testDoReceiveTransactionAccountCreations trs slot = mapM (\tr -> doReceiveTransaction tr slot
                                                           >>= (\res -> pure (wmdHash tr, res))) trs

accountCreations :: GlobalContext -> TransactionTime -> [BlockItem]
accountCreations gCtx now =
  [
    credentialDeploymentWithExpiryTooLate,
    expiredCredentialDeployment,
    credentialDeploymentWithDuplicateRegId,
    credentialWithInvalidIP,
    credentialWithInvalidKeys,
    credentialWithInvalidAr,
    credentialWithInvalidSignatures,
    verifiableCredential
  ]
  where
    expiry = now + 1
    credentialDeploymentWithExpiryTooLate = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation (now + (60 * 60 * 2) + 1) (regId 0) 0 True True)
    expiredCredentialDeployment = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation (now - 1) (regId 1) 0 True True)
    credentialDeploymentWithDuplicateRegId = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry duplicateRegId 0 True True)
    credentialWithInvalidIP = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry (regId 1) 42 True True)
    credentialWithInvalidKeys = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry (regId 1) 0 True False)
    credentialWithInvalidAr = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry (regId 1) 0 False True)
    credentialWithInvalidSignatures = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry (regId 1) 0 True True)
    verifiableCredential = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation expiry (regId 1) 0 True True)
    regId seed = RegIdCred $ generateGroupElementFromSeed gCtx seed

duplicateRegId :: CredentialRegistrationID
duplicateRegId = cred
  where
    cred = case Map.lookup 0 (gaCredentials $  head (makeFakeBakers 1)) of
              Nothing -> undefined
              Just x -> credId x
  

--testDoReceiveTransactionInternalAccountCreations :: [BlockItem] -> Slot -> MyMonad [UpdateResult]
--testDoReceiveTransactionInternalAccountCreations trs slot = do
--        mapM (\tr -> snd <$> doReceiveTransactionInternal tr slot) trs


mkAccountCreation :: TransactionTime -> CredentialRegistrationID -> Word32 -> Bool ->  Bool -> AccountCreation
mkAccountCreation expiry regId identityProviderId validAr validPubKeys = AccountCreation
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
                    cdvPolicy=mkPolicy
                  },
                  cdiProofs=Proofs "invalid proof"
                }
  }

mkInitialAccountCreation :: TransactionTime -> CredentialRegistrationID -> AccountCreation
mkInitialAccountCreation expiry regId = AccountCreation
  {
    messageExpiry=expiry,
    credential=InitialACWP InitialCredentialDeploymentInfo
               {
                 icdiValues=InitialCredentialDeploymentValues
                 {
                   icdvAccount=mkCredentialPublicKeys True,
                   icdvRegId=regId,
                   icdvIpId=IP_ID 0,
                   icdvPolicy=mkPolicy
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

mkPolicy :: Policy
mkPolicy = Policy
        {
          pValidTo=YearMonth
                   {
                     ymYear=2070,
                     ymMonth=1
                   },
          pCreatedAt=YearMonth
                   {
                     ymYear=2021,
                     ymMonth=1
                   },
          pItems=Map.empty
        }

mkArData :: Bool -> Map.Map ArIdentity ChainArData
mkArData valid = if valid then validAr else Map.empty
  where
    validAr = Map.insert key arData Map.empty
    arData = ChainArData {ardIdCredPubShare=AREnc zeroElgamalCipher}
    key = head $ Map.keys $ arRevokers dummyArs

mkCredentialKeyPair :: IO KeyPair
mkCredentialKeyPair = newKeyPair Ed25519

