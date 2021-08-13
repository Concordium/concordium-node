{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module CommonTests.VerifyCredentialDeploymentTest where


import qualified Concordium.GlobalState.DummyData as DD
import Concordium.Types.Transactions
import qualified Concordium.Scheduler.DummyData as DD
import qualified Concordium.TransactionVerification as TVer
import qualified Concordium.ID.Types as IDTypes
import qualified Concordium.Types as Types
import qualified Concordium.ID.IdentityProvider as IP
import qualified Data.Map.Strict as Map
import qualified Concordium.Types.IdentityProviders as TIP
import qualified Concordium.ID.AnonymityRevoker as AR
import qualified Concordium.Types.AnonymityRevokers as ARS

import Test.Hspec

tests :: Spec
tests = do
  describe "Testing 'CredentialDeployment' verification" $ do
    parallel $ do
      specify "Duplicate 'CredentialRegistrationID' results in error" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi1
        res `shouldBe` TVer.ResultDuplicateAccountRegistrationID (regId DD.cdi1)
      specify "Duplicate account address results in error" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi2
        res `shouldBe` TVer.ResultDuplicateAccountRegistrationID (regId DD.cdi2)
      specify "Invalid 'IdentityProvider' results in error" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi14
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidIdentityProvider
      specify "Invalid 'AnonymityRevoker' results in error" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi15
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidAnonymityRevokers
      specify "Malformed keys results in error" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi16
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidKeys
      specify "OK 'CredentialDeployment' should pass verification" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi3
        res `shouldBe` TVer.ResultSuccess
      specify "OK initial 'CredentialDeployment' should pass verification" $ do
        res <- TVer.verifyCredentialDeployment DD.icdi1
        res `shouldBe` TVer.ResultSuccess

--  describe "Testing initial 'CredentialDeployment' verification" $ do
--    parallel $ do
--      specify "Testing initial credential deployment signature verification" $ do
--        res <- TVer.verifyCredentialDeployment DD.icdi1
--        res `shouldBe` TVer.ResultCredentialDeploymentInvalidSignatures
        
-- |A mocked TransactionVerifier for testing purposes
instance Monad m => TVer.TransactionVerifier m where
  {-# INLINE getIdentityProvider #-}
  getIdentityProvider ipId = return $ getIP ipId
  {-# INLINE getAnonymityRevokers #-}
  getAnonymityRevokers arrIds = return $ getARs arrIds
  {-# INLINE getCryptographicParameters #-}
  getCryptographicParameters = return DD.dummyCryptographicParameters
  {-# INLINE registrationIdExists #-}
  registrationIdExists rId = return $ regIdExists rId
  {-# INLINE accountExists #-}
  accountExists aaddr = return $ aaddrExists aaddr

-- Helper functions for mocking 
regId :: AccountCreation -> IDTypes.CredentialRegistrationID
regId AccountCreation{..} = IDTypes.credId credential

regIdExists :: IDTypes.CredentialRegistrationID -> Bool
regIdExists rId = if rId == (regId DD.cdi1) then True else False

aaddrExists :: Types.AccountAddress -> Bool
aaddrExists aaddr = if aaddr == (IDTypes.addressFromRegId $ regId DD.cdi2) then True else False

getIP :: IDTypes.IdentityProviderIdentity -> Maybe IP.IpInfo
getIP ipId = Map.lookup ipId (TIP.idProviders DD.dummyIdentityProviders)

getARs :: [IDTypes.ArIdentity] ->  Maybe [AR.ArInfo]
getARs arrIds = case res of
                  [] -> Nothing
                  _ -> Just res
  where res = Map.elems (Map.filterWithKey (\k _ -> k `elem` arrIds) (ARS.arRevokers DD.dummyArs))
