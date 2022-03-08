{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module SchedulerTests.StakedAmountLocked where

import Test.Hspec
import Test.HUnit

import Control.Monad
import System.Random
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.Types.Accounts


import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Scheduler.Types hiding (Transfer)
import Concordium.TransactionVerification

import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

-- |Protocol version
type PV1 = 'Types.P1

keyPair :: Int -> SigScheme.KeyPair
keyPair = uncurry SigScheme.KeyPairEd25519 . fst . randomEd25519KeyPair . mkStdGen

account :: Int -> Types.AccountAddress
account = accountAddressFrom

baker0 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker0 = mkFullBaker 0 0

initialBlockState :: BlockState PV1
initialBlockState = createBlockState $ foldr putAccountWithRegIds Acc.emptyAccounts [acc, accWithLockup]
  where acc = mkAccount @'AccountV0 (SigScheme.correspondingVerifyKey (keyPair 0)) (account 0) 10_000_058 & accountStaking .~ AccountStakeBaker baker
        baker = AccountBaker {
          _stakedAmount = 10_000_000,
          _stakeEarnings = False,
          _accountBakerInfo = BakerInfoExV0 $ baker0 ^. _1 . bakerInfo,
          _bakerPendingChange = NoChange
          }
        accWithLockup = mkAccount (SigScheme.correspondingVerifyKey (keyPair 1)) (account 1) 10_000_033 & accountReleaseSchedule .~ lockup
        lockup = addReleases releases emptyAccountReleaseSchedule
        releases = ([(1, 10_000_000)], TransactionHashV0 $ Hash.hash "")

baker1 :: (FullBakerInfo, VRF.SecretKey, BlockSig.SignKey, Bls.SecretKey)
baker1 = mkFullBaker 1 1

transactionsInput :: [TransactionJSON]
transactionsInput =
    [
      TJSON { payload = Transfer (account 0) 0
           , metadata = makeDummyHeader (account 0) 1 10000
           , keys = [(0,[(0, keyPair 0)])]
           },
      TJSON { payload = Transfer (account 1) 0
           , metadata = makeDummyHeader (account 1) 1 10000
           , keys = [(0,[(0, keyPair 1)])]
           }
    ]

type TestResult = ([(BlockItemWithStatus, Types.TransactionSummary)], [(TransactionWithStatus, FailureKind)])

runTransactions :: IO TestResult
runTransactions = do
  txs <- processUngroupedTransactions transactionsInput
  let (Sch.FilteredTransactions{..}, _) =
          Types.runSI
          (Sch.filterTransactions dummyBlockSize dummyBlockTimeout txs)
          dummyChainMeta
          maxBound
          maxBound
          initialBlockState
  return (ftAdded, ftFailed)

tests :: Spec
tests = do
  (valid, invalid) <- runIO runTransactions
  describe "Insufficient available amount." $ do
    specify "Correct number of valid transactions" $
        assertEqual "There are some valid transactions" [] valid
    specify "Invalid transactions fail for the right reason" $ do
        assertEqual "Incorrect number of invalid transactions" 2 (length invalid)
        zipWithM_ (\(_, invalidReason) idx ->
                     assertEqual ("Incorrect failure reason for transaction " ++ show idx) Types.InsufficientFunds invalidReason)
                  invalid
                  [0 :: Int ..]
