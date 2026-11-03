{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SchedulerTests.RustScheduler where

import qualified Concordium.GlobalState.BlockState as BlockState
import qualified Concordium.Scheduler.Environment as EI
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler as RustPLTScheduler
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Execution
import qualified Control.Exception as Exception
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Short as ByteString
import qualified SchedulerTests.Helpers as Helpers
import Test.HUnit
import Test.Hspec

testExecuteTransaction :: Spec
testExecuteTransaction = describe "executeTransaction" $ do
    it "throws when called with a non-supported payload type" $ do
        assertions <- Helpers.runTestBlockState @'Types.P11 $ do
            account0 <- Helpers.makeTestAccountFromSeed 2_000_000 0
            blockStateBefore <- Helpers.createTestBlockStateWithAccounts [account0]
            blockStateBeforeThawed <- BlockState.thawBlockState blockStateBefore
            let schedulerState = EI.makeInitialSchedulerState @(Helpers.PersistentBSM 'Types.P11) blockStateBeforeThawed
            let depositContext :: EI.WithDepositContext (Helpers.PersistentBSM 'Types.P11) =
                    EI.WithDepositContext
                        { _wtcSenderAccount = (0, account0),
                          _wtcPayerAccount = (0, account0),
                          _wtcTransactionType = Execution.TTRegisterData,
                          _wtcTransactionHash = read "0000000000000000000000000000000000000000000000000000000000000000",
                          _wtcSenderAddress = Helpers.accountAddressFromSeed 0,
                          _wtcSponsorAddress = Nothing,
                          _wtcEnergyAmount = 500_000,
                          _wtcTransactionCheckHeaderCost = 1_000,
                          _wtcCurrentlyUsedBlockEnergy = 0,
                          _wtcTransactionIndex = 0,
                          _wtcTransactionSequenceNumber = 1
                        }
            let schedulerComputation =
                    RustPLTScheduler.executeTransaction
                        @(Helpers.PersistentBSM 'Types.P11)
                        depositContext
                        (Execution.RegisterData $ Types.RegisteredData ByteString.empty)
            let contextState =
                    EI.ContextState
                        { _chainMetadata =
                            Types.ChainMetadata
                                { slotTime = 500
                                },
                          _maxBlockEnergy = 500_000_000,
                          _accountCreationLimit = 500
                        }
            errorMessage <-
                Catch.catch
                    @_
                    @Exception.ErrorCall
                    (EI.runSchedulerT schedulerComputation contextState schedulerState >> return Nothing)
                    (\message -> return $ Just message)
            return $ do
                case errorMessage of
                    Nothing -> assertFailure "Expected an exception to be thrown"
                    Just (Exception.ErrorCall message) -> assertEqual "Expected error message" "Call to 'ffiExecuteTransaction' resulted in panic with message: \"Unexpected failure during transaction execution: UnexpectedPayload\"" message
        assertions

tests :: Spec
tests = describe "RustScheduler" $ do
    testExecuteTransaction
