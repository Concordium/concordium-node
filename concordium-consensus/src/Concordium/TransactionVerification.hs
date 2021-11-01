{-# LANGUAGE TypeFamilies #-}
module Concordium.TransactionVerification
  where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.Serialize as S
import qualified Concordium.Types.Transactions as Tx
import qualified Concordium.ID.AnonymityRevoker as AR
import qualified Concordium.ID.IdentityProvider as IP
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.Types.Parameters as Params
import qualified Concordium.Types as Types
import qualified Concordium.ID.Account as A
import qualified Data.Map.Strict as OrdMap
import qualified Concordium.ID.Types as ID
import Data.Maybe (isJust)

-- |The 'VerificationResult' type serves as an intermediate `result type` between the 'TxResult' and 'UpdateResult' types.
-- VerificationResult's contains possible verification errors that may have occurred when verifying a 'AccountCreation' type.
data VerificationResult
  = Success
    -- ^The verification passed
    | TransactionExpired
    -- ^The transaction was expired.
    | ExpiryTooLate
    -- ^The transaction had an expiry too distant in the future
    | DuplicateAccountRegistrationID !ID.CredentialRegistrationID
    -- ^The 'CredentialDeployment' contained an invalid registration id.
    -- There already exists an account with the registration id.
    | CredentialDeploymentInvalidIdentityProvider
    -- ^The IdentityProvider does not exist for this 'CredentialDeployment'.
    | CredentialDeploymentInvalidAnonymityRevokers
    -- ^The anonymity revokers does not exist for this 'CredentialDeployment'.
    | CredentialDeploymentInvalidKeys
    -- ^The 'AccountCreation' contained invalid keys.
    | CredentialDeploymentInvalidSignatures
    -- ^The 'AccountCreation' contained invalid identity provider signatures.
    | CredentialDeploymentExpired
    -- ^The 'AccountCreation' contained an expired 'validTo'
    deriving (Eq, Show)

-- |Type which can verify transactions in a monadic context. 
-- The type is responsible for retrieving the necessary information
-- in order to deem a transaction valid or 'unverifiable'.
-- Unverifiable transactions are transactions which are and never will be valid transactions
-- e.g., due to erroneous signatures, invalid 'IdentityProvider's, invalid 'AnonymityRevoker's etc.
class Monad m => TransactionVerifier m where
  -- |Get the provider identity data for the given identity provider, or Nothing if
  -- the identity provider with given ID does not exist.
  getIdentityProvider :: ID.IdentityProviderIdentity -> m (Maybe IP.IpInfo)
  -- |Get the anonymity revokers with given ids. Returns 'Nothing' if any of the
  -- anonymity revokers are not found.
  getAnonymityRevokers :: [ID.ArIdentity] -> m (Maybe [AR.ArInfo])
  -- |Get cryptographic parameters for the current state.
  getCryptographicParameters :: m Params.CryptographicParameters
  -- |Check whether the given credential registration ID exists
  registrationIdExists :: ID.CredentialRegistrationID -> m Bool
  -- |Check whether the account address corresponds to an existing account.
  accountExists :: Types.AccountAddress -> m Bool


-- |Verifies that a transaction is not yet expired.
verifyTransactionNotExpired :: TransactionVerifier m => Tx.BlockItem -> Types.Timestamp -> m VerificationResult
verifyTransactionNotExpired tx now = do
  let expired = Types.transactionExpired (Tx.msgExpiry tx) now
  if expired then return TransactionExpired else return Success

-- |Verifies a 'CredentialDeployment'
-- That is:
-- * Checks that the 'CredentialDeployment' is not expired
-- * Making sure that an registration id does not already exist and also that 
-- a corresponding account does not exist.
-- * Validity of the 'IdentityProvider' and 'AnonymityRevokers' provided.
-- * Key sizes for the 'CredentialDeployment'
-- * Valid signatures on the 'CredentialDeployment'
verifyCredentialDeployment :: TransactionVerifier m => Types.Timestamp -> Tx.AccountCreation -> m VerificationResult
verifyCredentialDeployment now accountCreation@Tx.AccountCreation{..} = do
  -- check that the credential deployment is not yet expired
  let expiry = ID.validTo credential
  if not (Types.isTimestampBefore now expiry) then return CredentialDeploymentExpired
  else do
    -- check that the credential deployment is not a duplicate
    unique <- verifyCredentialUniqueness accountCreation
    if unique /= Success
    then return unique
    else do
      let credIpId = ID.ipId accountCreation
      mIpInfo <- getIdentityProvider credIpId
      case mIpInfo of
      -- check that the identity provider exists
        Nothing -> return CredentialDeploymentInvalidIdentityProvider
        Just ipInfo ->
         case credential of
           ID.InitialACWP icdi ->
            -- check signatures for an initial credential deployment
            if not (A.verifyInitialAccountCreation ipInfo messageExpiry (S.encode icdi))
            then return CredentialDeploymentInvalidSignatures
            else return Success
           ID.NormalACWP ncdi -> do
             cryptoParams <- getCryptographicParameters
             let ncdv = ID.cdiValues ncdi
             case ID.cdvPublicKeys ncdv of
               ID.CredentialPublicKeys keys _ -> do
                 -- check that the keys are well sized
                 if null keys || (length keys > 255) then return CredentialDeploymentInvalidKeys
                 else do
                   mArsInfos <- getAnonymityRevokers (OrdMap.keys (ID.cdvArData ncdv))
                   case mArsInfos of
                     -- check that the anonymity revokers exists
                     Nothing -> return CredentialDeploymentInvalidAnonymityRevokers
                     Just arsInfos ->
                       -- if the credential deployment contained an empty map of 'ChainArData' then the result will be 'Just empty'.
                       if null arsInfos then return CredentialDeploymentInvalidAnonymityRevokers
                       else do
                        -- check signatures for a normal credential deployment
                        if not (A.verifyCredential cryptoParams ipInfo arsInfos (S.encode ncdi) (Left messageExpiry))
                        then return CredentialDeploymentInvalidSignatures
                        else return Success


-- |Verifies that a credential is unique
verifyCredentialUniqueness :: TransactionVerifier m => Tx.AccountCreation -> m VerificationResult
verifyCredentialUniqueness accountCreation = do
  -- check that the registration id does not already exist
  regIdExists <- registrationIdExists (ID.credId accountCreation)
  -- check that the account does not already exist (very unlikely to happen but we check it for good measure)
  accExists <- accountExists (ID.addressFromRegId (ID.credId accountCreation))
  if regIdExists || accExists
  then return $ DuplicateAccountRegistrationID (ID.credId accountCreation)
  else return Success 

instance (Monad m, r ~ GSTypes.BlockState m, BS.BlockStateQuery m) => TransactionVerifier (ReaderT r m) where
  {-# INLINE getIdentityProvider #-}
  getIdentityProvider ipId = do
    state <- ask
    lift (BS.getIdentityProvider state ipId)
  {-# INLINE getAnonymityRevokers #-}
  getAnonymityRevokers arrIds = do
    state <- ask
    lift (BS.getAnonymityRevokers state arrIds)
  {-# INLINE getCryptographicParameters #-}
  getCryptographicParameters = do
    state <- ask
    lift (BS.getCryptographicParameters state)
  {-# INLINE registrationIdExists #-}
  registrationIdExists regId = do
    state <- ask
    let res = lift (BS.regIdExists state regId)
    fmap isJust res
  {-# INLINE accountExists #-}
  accountExists aaddr = do
    state <- ask
    let res = lift (BS.getAccount state aaddr)
    fmap isJust res

