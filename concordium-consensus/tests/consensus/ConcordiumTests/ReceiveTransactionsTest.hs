module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

-- todo
-- receiveTransactionreceiveTransaction and storeBlock are the two functions that needs to be tested
-- with invalid and valid credential deployments.
test :: Spec
test = do
  describe "Receive transaction CredentialDeployment" $ do
    parallel $
      specify "Receive invalid AccountCreation fails " $ do
      1 `shouldBe` 1
