module ConcordiumTests.KonsensusV1.TransactionProcessingTest (tests) where

import Concordium.KonsensusV1.Consensus

testProcessBlockItem :: Spec
testProcessBlockItem = describe "processBlockItem" $ do
    it "normal transaction processed" $ do
        ()

tests :: Spec
tests = describe "KonsensusV1.TransactionProcessing" $ do
    describe "Individual transaction processing" $ do
        testProcessBlockItem
