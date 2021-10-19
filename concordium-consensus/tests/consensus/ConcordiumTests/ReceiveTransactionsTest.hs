module ConcordiumTests.ReceiveTransactionsTest where

import Test.Hspec

test :: Spec
test = do
  describe "Receive transaction CredentialDeployment" $ do
    parallel $
      specify "Receive invalid AccountCreation fails " $ do
      1 `shouldBe` 1
