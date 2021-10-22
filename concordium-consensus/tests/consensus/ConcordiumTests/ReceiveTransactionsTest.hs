{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.Word
import Data.Time.Clock
import Data.ByteString.Short(ShortByteString)
import Control.Monad.State
import Control.Monad.Reader

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

import Concordium.Skov.Update (doReceiveTransaction, doReceiveTransactionInternal)
import Concordium.Types.Transactions (BlockItem)
import Concordium.Genesis.Data
import Concordium.GlobalState
import Concordium.GlobalState.Types ()
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.TreeState (TreeStateMonad)
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Basic.BlockState hiding (initialState)
import Concordium.TimeMonad (TimeMonad)
import Concordium.Skov.Monad

import Concordium.GlobalState.DummyData
import Concordium.Crypto.DummyData
import Concordium.Types.DummyData
import Concordium.ID.Parameters
import Concordium.ID.DummyData
import Concordium.GlobalState.Persistent.Accounts (emptyAccounts)
import Lens.Micro.Platform


-- |Tests of doReceiveTransaction and doReceiveTransactionInternal of the Updater.
test :: Spec
test = do
  describe "doReceiveTansaction" $ do
    parallel $
      specify "Receive normal account creation should fail with expected types" $ do
      s <- runMkNormalCredentialDeployments
      let results = fst s
      -- First should yield that expiry was set as too distant in the future
      let expectedExpiryTooLate = head results
      expectedExpiryTooLate `shouldBe` ResultExpiryTooLate
      -- Second should yield an expired result
      let expectedExpired = results !! 1
      expectedExpired `shouldBe` ResultTransactionExpired

runMkNormalCredentialDeployments :: IO ([UpdateResult], MyState)
runMkNormalCredentialDeployments = do
  now <- currentTime
  runMyMonad' (testDoReceiveTransactionAccountCreations (txs now) slot) now (testGenesisData now dummyIdentityProviders dummyArs)
  where
    txs now =  accountCreations mkGlobalContext $ utcTimeToTransactionTime now
    slot = genesisSlot + 1

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
runMyMonad' act time gd = runPureBlockStateMonad (setupState $ initialSkovDataDefault gd (hashBlockState bs)) >>= runMyMonad act time
  where
    bs = case genesisState gd of
               Left err -> error $ "Invalid genesis state: " ++ err
               Right x -> x
    setupState s = do
      s

-- |Construct a genesis state with hardcoded values for parameters that should not affect this test.
-- Modify as you see fit.
testGenesisData :: UTCTime -> IdentityProviders -> AnonymityRevokers -> GenesisData PV
testGenesisData now ips ars = makeTestingGenesisDataP1 (utcTimeToTimestamp now) 1 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters ips ars maxBound dummyKeyCollection dummyChainParameters

testDoReceiveTransactionAccountCreations :: [BlockItem] -> Slot -> MyMonad [UpdateResult]
testDoReceiveTransactionAccountCreations trs slot = mapM (\tr -> doReceiveTransaction tr slot) trs

accountCreations :: GlobalContext -> TransactionTime -> [BlockItem]
accountCreations gCtx now = [
  credentialDeploymentWithExpiryTooLate,
  expiredCredentialDeployment,
  credentialDeploymentWithDuplicateRegId
  ]
  where
    credentialDeploymentWithExpiryTooLate = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation (now + (60 * 60 * 2) + 1) (regId 0))
    expiredCredentialDeployment = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation (now - 1) (regId 0))
    credentialDeploymentWithDuplicateRegId = credentialDeployment $
      addMetadata (\x -> CredentialDeployment {biCred=x}) now (mkAccountCreation (now - 1) (regId 0))
    regId seed = RegIdCred $ mkGroupElement gCtx seed

--testDoReceiveTransactionInternalAccountCreations :: [BlockItem] -> Slot -> MyMonad [UpdateResult]
--testDoReceiveTransactionInternalAccountCreations trs slot = do
--        mapM (\tr -> snd <$> doReceiveTransactionInternal tr slot) trs


mkAccountCreation :: TransactionTime -> CredentialRegistrationID -> AccountCreation
mkAccountCreation expiry regId = AccountCreation
  {
    messageExpiry=expiry,
    credential= NormalACWP CredentialDeploymentInformation
                {
                  cdiValues=CredentialDeploymentValues
                  {
                    cdvPublicKeys=mkCredentialPublicKeys,
                    cdvCredId=regId,
                    cdvIpId=IP_ID 0,
                    cdvThreshold=Threshold 1,
                    cdvArData=mkArData,
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
                   icdvAccount=mkCredentialPublicKeys,
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

mkCredentialPublicKeys :: CredentialPublicKeys
mkCredentialPublicKeys = CredentialPublicKeys
                         {
                           credKeys=Map.empty, credThreshold=SignatureThreshold 1
                         }

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

mkArData :: Map.Map ArIdentity ChainArData
mkArData = Map.empty

mkGroupElement :: GlobalContext -> Word64 -> GroupElement
mkGroupElement = generateGroupElementFromSeed

mkCredentialKeyPair :: IO KeyPair
mkCredentialKeyPair = newKeyPair Ed25519

mkGlobalContext :: GlobalContext
mkGlobalContext = dummyGlobalContext

