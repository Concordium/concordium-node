{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

import qualified Data.Map.Strict as Map
import Data.Word
import Data.ByteString.Short(ShortByteString)

import Concordium.Types
import Concordium.Types.Transactions
import Concordium.ID.Types
import Concordium.Crypto.FFIDataTypes
import Concordium.Common.Time
import Concordium.ID.Parameters

import Concordium.Skov.Update (doReceiveTransaction, doReceiveTransactionInternal)
import Concordium.Types.Transactions (BlockItem)
import Concordium.GlobalState.TreeState (TreeStateMonad)
import Concordium.TimeMonad (TimeMonad)
import Concordium.Skov.Monad


-- Tests of doReceiveTransaction and doReceiveTransactionInternal of the Updater.

type PV = 'P1

accountCreations :: [BlockItem]
accountCreations = undefined

slot :: Slot
slot = undefined

testDoReceiveTransactionAccountCreations :: (TreeStateMonad pv m, TimeMonad m, SkovQueryMonad pv m) => [BlockItem] -> Slot -> m [UpdateResult]
testDoReceiveTransactionAccountCreations trs slot = mapM (\tr -> doReceiveTransaction tr slot) trs

testDoReceiveTransactionInternalAccountCreations :: (TreeStateMonad pv m, TimeMonad m, SkovQueryMonad pv m) => [BlockItem] -> Slot -> m [UpdateResult]
testDoReceiveTransactionInternalAccountCreations trs slot = do
  mapM (\tr -> snd <$> doReceiveTransactionInternal tr slot) trs

test :: Spec
test = do
  describe "doReceiveTansaction" $ do
    parallel $
      specify "Receive invalid AccountCreation fails " $ do
      let accs = accountCreations
      1 `shouldBe` 1


mkBlockItem :: AccountCreation ->  BlockItem
mkBlockItem = addMetadata CredentialDeployment

mkInitialAccountCreation :: GlobalContext -> AccountCreation
mkInitialAccountCreation globalContext = AccountCreation TransactionTime{ttsSeconds=10} $
  InitialACWP InitialCredentialDeploymentInfo
  {
  icdiValues=InitialCredentialDeploymentValues
    {
      icdvAccount=mkCredentialPublicKeys,
      icdvRegId=RegIdCred $ mkGroupElement globalContext,
      icdvIpId=IP_ID 0,
      icdvPolicy=mkPolicy
      },
    icdiSig=IpCdiSignature
    {
      theSignature="invalid signature"
    }
  }

mkAccountCreation :: GlobalContext -> AccountCreation
mkAccountCreation globalContext = AccountCreation TransactionTime{ttsSeconds=10} $
  NormalACWP CredentialDeploymentInformation
  {
    cdiValues=CredentialDeploymentValues
              {
                cdvPublicKeys=mkCredentialPublicKeys,
                cdvCredId=RegIdCred $ mkGroupElement globalContext,
                cdvIpId=IP_ID 0,
                cdvThreshold=Threshold 1,
                cdvArData=mkArData,
                cdvPolicy=mkPolicy
              },
    cdiProofs=Proofs "invalid proof"
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
        
mkGroupElement :: GlobalContext -> GroupElement
mkGroupElement ctx = generateGroupElementFromSeed ctx 10

mkGlobalContext :: GlobalContext
mkGlobalContext = dummyGlobalContext

