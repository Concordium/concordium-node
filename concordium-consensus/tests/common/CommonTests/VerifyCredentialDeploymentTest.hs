{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module CommonTests.VerifyCredentialDeploymentTest where

import Concordium.Types.Transactions
import Concordium.ID.Types

import qualified Concordium.TransactionVerification as TVer
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
        res <- TVer.verifyCredentialDeployment mkNormalDuplicateRegIdAC
        res `shouldBe` TVer.ResultDuplicateAccountRegistrationID (regId mkNormalDuplicateRegIdAC)
      specify "Duplicate account address results in error" $ do
        res <- TVer.verifyCredentialDeployment mkNormalDuplicateAccAddrAC
        res `shouldBe` TVer.ResultDuplicateAccountRegistrationID (regId mkNormalDuplicateAccAddrAC)
      specify "Invalid 'IdentityProvider' results in error" $ do
        res <- TVer.verifyCredentialDeployment mkNormalInvalidIPAC
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidIdentityProvider
      specify "Invalid 'AnonymityRevoker' results in error" $ do
        res <- TVer.verifyCredentialDeployment mkNormalInvalidARAC
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidAnonymityRevokers
      specify "Malformed keys results in error" $ do
        res <- TVer.verifyCredentialDeployment mkNormalMalformedKeysAC
        res `shouldBe` TVer.ResultCredentialDeploymentInvalidKeys
      specify "OK 'CredentialDeployment' should pass verification" $ do
        res <- TVer.verifyCredentialDeployment DD.cdi1
        res `shouldBe` TVer.ResultSuccess
      specify "OK initial 'CredentialDeployment' should pass verification" $ do
        res <- TVer.verifyCredentialDeployment dd.icdi1
        res `shouldBe` TVer.ResultSuccess
        
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


-- Account creations
mkNormalDuplicateRegIdAC :: AccountCreation
mkNormalDuplicateRegIdAC = mkNormalAC vals
  where
    vals = CredentialDeploymentValues{
      
    }

mkNormalDuplicateAccAddrAC :: AccountCreation
mkNormalDuplicateAccAddrAC = mkNormalCredDeploymentInfo vals
  where
    vals = CredentialDeploymentValues{}

mkNormalInvalidIPAC :: AccountCreation
mkNormalInvalidIPAC = undefined

mkNormalInvalidARAC :: AccountCreation
mkNormalInvalidARAC = undefined

mkNormalMalformedKeysAC :: AccountCreation
mkNormalMalformedKeysAC = undefined

mkNormalVerifiableAC :: AccountCreation
mkNormalVerifiableAC = undefined

mkInitialVerifiableAC :: AccountCreation
mkInitialVerifiableAC = undefined

mkNormalAC :: CredentialDeploymentInformation -> AccountCreation
mkNormalAC = undefined

mkNormalCredDeploymentInfo :: CredentialDeploymentValues -> CredentialDeploymentInformation
mkNormalCredDeploymentInfo vals = CredentialDeploymentInformation{cdiValues: vals}

mkIntialCredDeploymentInfo :: InitialCredentialDeploymentValues -> InitialCredentialDeploymentInfo
mkIntialCredDeploymentInfo = undefined

mkInitialAC :: InitialCredentialDeploymentInfo -> AccountCreation
mkInitialAC = undefined  

-- Helper functions for mocking 
regId :: AccountCreation -> CredentialRegistrationID
regId AccountCreation{..} = credId credential

regIdExists :: CredentialRegistrationID -> Bool
regIdExists rId = if rId == (regId mkNormalDuplicateRegIdAC) then True else False

aaddrExists :: Types.AccountAddress -> Bool
aaddrExists aaddr = if aaddr == (addressFromRegId $ regId mkNormalDuplicateAccAddrAC) then True else False

getIP :: IdentityProviderIdentity -> Maybe IP.IpInfo
getIP ipId = Map.lookup ipId (TIP.idProviders DD.dummyIdentityProviders)

getARs :: [ArIdentity] ->  Maybe [AR.ArInfo]
getARs arrIds = case res of
                  [] -> Nothing
                  _ -> Just res
  where res = Map.elems (Map.filterWithKey (\k _ -> k `elem` arrIds) (ARS.arRevokers DD.dummyArs))
