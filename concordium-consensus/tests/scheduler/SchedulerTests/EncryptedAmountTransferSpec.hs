{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module SchedulerTests.EncryptedAmountTransferSpec where

import qualified Test.HUnit as HUnit
import Test.Hspec

import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.Runner as Runner

import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData
import Concordium.Crypto.EncryptedTransfers

import SchedulerTests.TestUtils

import Data.Aeson
import Data.Sequence as Seq

import Lens.Micro.Platform

import Debug.Trace
import Concordium.Crypto.FFIDataTypes (ElgamalSecond, ElgamalSecondSecret)
import Data.Word (Word64)
import System.IO.Unsafe (unsafePerformIO)
import Concordium.ID.DummyData (dummyEncryptionSecretKey)
import Data.Maybe (fromJust)
import Concordium.ID.Types (AccountEncryptionKey(..),CredentialRegistrationID(RegIdCred))
import Concordium.Scheduler.Types (unhashed)


initialBlockState :: BlockState
initialBlockState = blockStateWithAlesAccount
    10000000000
    (Acc.putAccountWithRegIds (mkAccount thomasVK thomasAccount 10000000000) Acc.emptyAccounts)

alesEncryptionSecretKey :: ElgamalSecondSecret
alesEncryptionSecretKey = dummyEncryptionSecretKey alesAccount

alesEncryptionPublicKey :: ElgamalSecond
alesEncryptionPublicKey = let AccountEncryptionKey (RegIdCred s) = (fromJust $ Acc.getAccount alesAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey in s

thomasEncryptionSecretKey :: ElgamalSecondSecret
thomasEncryptionSecretKey = dummyEncryptionSecretKey thomasAccount

thomasEncryptionPublicKey :: ElgamalSecond
thomasEncryptionPublicKey = let AccountEncryptionKey (RegIdCred s) = (fromJust $ Acc.getAccount thomasAccount (initialBlockState ^. blockAccounts)) ^. accountPersisting . accountEncryptionKey in s

encryptedAmount1000 :: EncryptedAmount
encryptedAmount1000 = encryptAmount (initialBlockState ^. blockCryptographicParameters . unhashed) 1000

createEncryptedTransferData :: ElgamalSecond -> ElgamalSecondSecret -> AggregatedDecryptedAmount -> Word64 -> Maybe EncryptedAmountTransferData
createEncryptedTransferData second secret aggDecAmount amount = unsafePerformIO $ makeEncryptedAmountTransferData (initialBlockState ^. blockCryptographicParameters . unhashed) second secret aggDecAmount amount

testCases :: [TestCase]
testCases =
  [ TestCase
    { tcName = "Makes an encrypted transfer"
    , tcParameters = defaultParams { tpInitialBlockState = initialBlockState }
    , tcTransactions =
      [ ( Runner.TJSON { payload = Runner.TransferToEncrypted 1000
                         , metadata = makeDummyHeader alesAccount 1 100000
                         , keys = [(0, alesKP)]
                         }
          , (SuccessE [Types.EncryptedSelfAmountAdded {
                          eaaAccount = alesAccount,
                          eaaNewAmount = encryptedAmount1000,
                          eaaAmount = 1000
                          }
                      ], checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = encryptedAmount1000} alesAccount
            )
          ),
        let aggregatedDecryptedAmount = makeAggregatedDecryptedAmount encryptedAmount1000 1000 0
            encryptedTransferData@EncryptedAmountTransferData{..} = fromJust $ createEncryptedTransferData thomasEncryptionPublicKey alesEncryptionSecretKey aggregatedDecryptedAmount 100
        in
        ( Runner.TJSON { payload = Runner.EncryptedAmountTransfer thomasAccount encryptedTransferData
                       , metadata = makeDummyHeader alesAccount 2 100000
                       , keys = [(0, alesKP)]
                       }
        , (SuccessE [Types.EncryptedAmountsRemoved {
                        earAccount = alesAccount,
                        earUpToIndex = 0,
                        earNewAmount = eatdRemainingAmount
                        },
                      Types.NewEncryptedAmount {
                        neaAccount = thomasAccount,
                        neaNewIndex = 0,
                        neaEncryptedAmount = eatdTransferAmount
                        }
                    ], emptySpec -- \bs -> do
                       --   checkEncryptedBalance initialAccountEncryptedAmount{_selfAmount = eatdRemainingAmount} alesAccount bs
                       --   checkEncryptedBalance initialAccountEncryptedAmount{_startIndex = 1,
                       --                                                       _incomingEncryptedAmounts = Seq.singleton eatdTransferAmount} thomasAccount bs
          )
        )
      ]
     }
  ]
  where checkEncryptedBalance accEncAmount acc = (\bs -> specify "Correct final balance" $
           case Acc.getAccount acc (bs ^. blockAccounts) of
             Nothing -> HUnit.assertFailure $ "Account with id '" ++ show acc ++ "' not found"
             Just account -> HUnit.assertEqual "Expected encrypted amount matches"  accEncAmount (account ^. accountEncryptedAmount))

tests :: Spec
tests = describe "TransferToEncrypted." $
  mkSpecs testCases
