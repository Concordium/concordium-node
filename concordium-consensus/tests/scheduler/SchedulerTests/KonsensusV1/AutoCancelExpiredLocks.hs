{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.KonsensusV1.AutoCancelExpiredLocks (tests) where

import Control.Monad.IO.Class
import Data.Foldable (toList)
import Test.HUnit
import Test.Hspec hiding (after, before, pending)

import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.TransactionTable as TT
import qualified Concordium.KonsensusV1.Scheduler as Block
import Concordium.Scheduler.Types
import Concordium.Types.Tokens (TokenAmount (..))
import qualified SchedulerTests.Helpers as Helpers
import SchedulerTests.KonsensusV1.AutoCancelExpiredLocksFixture

tests :: Spec
tests =
    describe "Automatic lock cancellation" $
        it "production and verification unlock expired funds using post-transaction capacity" $
            Helpers.runTestBlockState @'P11 $ do
                let oneReferenceCancellationCost = emptyCancellationCost + perReferenceCancellationCost
                (fixture, ids, nonce) <- makeFixture 3 True
                (firstId, remainingIds) <- case ids of
                    first : rest -> return (first, rest)
                    [] -> liftIO $ assertFailure "Expected funded locks" >> fail "Empty fixture"
                before <- observeLockBalance $ Block.bedParentState fixture
                liftIO $ assertEqual "Funded locks before block execution" (ids, TokenAmount 750 0, TokenAmount 0 0) before
                (unrestricted, energy) <- requireExecuted =<< Block.executeBlockState fixture []
                after <- observeLockBalance unrestricted
                liftIO $ do
                    assertEqual "Expired locks are destroyed; total holdings stay unchanged" ([], TokenAmount 750 0, TokenAmount 750 0) after
                    assertEqual "Cleanup reports energy for three references" (3 * oneReferenceCancellationCost) energy
                -- The 1200-NRG limit fits two cancellations; prioritizing the 501-NRG
                -- transfer leaves 699 NRG, so only one 600-NRG cancellation fits.
                let blockEnergyLimit = 2 * oneReferenceCancellationCost
                    remainingCapacity = blockEnergyLimit - Helpers.simpleTransferCost
                executionData <- withEnergyLimit blockEnergyLimit fixture
                tx <- liftIO $ makeTransfer nonce
                let table = snd $ TT.addTransaction tx 0 verification TT.emptyTransactionTable
                    pending = case wmdData tx of
                        NormalTransaction accountTx -> TT.addPendingTransaction nonce accountTx TT.emptyPendingTransactionTable
                        _ -> error "Expected normal transaction"
                -- Distinct production/verification entry points must derive identical cleanup state, events, and energy.
                (included, produced, producedEnergy) <- Block.constructBlockState defaultRuntimeParameters table pending executionData
                let added = ftAdded included
                liftIO $ do
                    assertEqual "Included transaction is not displaced by cleanup" [tx] (fst . fst <$> added)
                    assertEqual "Individual transaction energy excludes cleanup" [Helpers.simpleTransferCost] (tsEnergyCost . snd <$> added)
                    assertBool
                        "Remaining capacity fits one cancellation while the full limit fits two"
                        ( remainingCapacity >= oneReferenceCancellationCost
                            && remainingCapacity < 2 * oneReferenceCancellationCost
                            && blockEnergyLimit >= 2 * oneReferenceCancellationCost
                        )
                    assertBool "Full block limit is below the unrestricted cleanup cost" (blockEnergyLimit < 3 * oneReferenceCancellationCost)
                (verified, verificationEnergy) <- requireExecuted =<< Block.executeBlockState executionData [(fst $ fst item, verification) | item <- added]
                producedBalance <- observeLockBalance produced
                verifiedBalance <- observeLockBalance verified
                producedOutcomes <- BS.getSpecialOutcomes produced
                verifiedOutcomes <- BS.getSpecialOutcomes verified
                producedHash <- BS.getStateHash produced
                verifiedHash <- BS.getStateHash verified
                producedOutcomeHash <- BS.getTransactionOutcomesHash produced
                verifiedOutcomeHash <- BS.getTransactionOutcomesHash verified
                let expectedCleanup = [SpecialUnlockAmount (HolderAccount ownerAddress) firstId token (TokenAmount 250 0), SpecialLockDestroyed firstId]
                    outcomes = toList producedOutcomes
                liftIO $ do
                    assertEqual "Only one lock fits post-transaction capacity" (remainingIds, TokenAmount 750 0, TokenAmount 250 0) producedBalance
                    assertEqual "Production/verification balances" producedBalance verifiedBalance
                    assertEqual "Reported energy includes cleanup" (Helpers.simpleTransferCost + oneReferenceCancellationCost) producedEnergy
                    assertEqual "Production/verification energy" producedEnergy verificationEnergy
                    assertEqual "Decoded cleanup outcomes precede rewards" expectedCleanup (take 2 outcomes)
                    assertBool "Reward outcomes follow cleanup" (not $ null $ drop 2 outcomes)
                    assertEqual "Production/verification special outcomes" producedOutcomes verifiedOutcomes
                    assertEqual "Production/verification state hashes" producedHash verifiedHash
                    assertEqual "Production/verification outcome hashes" producedOutcomeHash verifiedOutcomeHash
