{-# LANGUAGE TypeFamilies #-}
module Concordium.TransactionVerification
  where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as OrdMap
import qualified Data.Serialize as S

import qualified Concordium.Types.Transactions as Tx
import qualified Concordium.ID.AnonymityRevoker as AR
import qualified Concordium.ID.IdentityProvider as IP
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.Types.Parameters as Params
import qualified Concordium.Types.Updates as Updates
import qualified Concordium.Types as Types
import qualified Concordium.ID.Account as A
import qualified Concordium.ID.Types as ID
import qualified Concordium.Crypto.SHA256 as Sha256
import qualified Concordium.Scheduler.Types as ST
import Concordium.Types.HashableTo (getHash)

import Data.Maybe (isJust)
import Control.Monad.Except

-- |The 'VerificationResult' type serves as an intermediate `result type` between the 'TxResult' and 'UpdateResult' types.
-- VerificationResult's contains possible verification errors that may have occurred when verifying a 'AccountCreation' type.
data VerificationResult
  = Success
  -- ^The verification passed
  | DuplicateAccountRegistrationID !ID.CredentialRegistrationID
  -- ^The 'CredentialDeployment' contained an invalid registration id.
  -- There already exists an account with the registration id.
  | CredentialDeploymentInvalidIdentityProvider
  -- ^The IdentityProvider does not exist for this 'CredentialDeployment'.
  | CredentialDeploymentInvalidAnonymityRevokers
  -- ^The anonymity revokers does not exist for this 'CredentialDeployment'.
  | CredentialDeploymentInvalidSignatures
  -- ^The 'AccountCreation' contained invalid identity provider signatures.
  | CredentialDeploymentExpired
  -- ^The 'AccountCreation' contained an expired 'validTo'
  | ChainUpdateInvalidSignatures
  -- ^The 'ChainUpdate' contained invalid signatures.
  | ChainUpdateEffectiveTimeBeforeTimeout
  -- ^The 'ChainUpdate' had an expiry set too late.
  | ChainUpdateSuccess !Sha256.Hash
  deriving (Eq, Show)

-- |Type which can verify transactions in a monadic context. 
-- The type is responsible for retrieving the necessary information
-- in order to deem a transaction valid or 'unverifiable'.
-- Unverifiable transactions are transactions which are and never will be valid transactions
-- e.g., due to erroneous signatures, invalid expiry etc.
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
  -- |Get the UpdateKeysCollection
  getUpdateKeysCollection :: m Updates.UpdateKeysCollection

-- |Verifies a 'CredentialDeployment' transaction.
--
-- This function verifies the following:
-- * Checks that the 'CredentialDeployment' is not expired
-- * Making sure that an registration id does not already exist and also that 
-- a corresponding account does not exist.
-- * Validity of the 'IdentityProvider' and 'AnonymityRevokers' provided.
-- * That the 'CredentialDeployment' contains valid signatures.
verifyCredentialDeployment :: TransactionVerifier m => Types.Timestamp -> Tx.AccountCreation -> m VerificationResult
verifyCredentialDeployment now accountCreation@Tx.AccountCreation{..} =
  either id id <$> runExceptT (do
    -- check that the credential deployment is not yet expired
    let expiry = ID.validTo credential
    unless (Types.isTimestampBefore now expiry) $ throwError CredentialDeploymentExpired
    -- check that the credential deployment is not a duplicate
    exists <- lift (registrationIdExists (ID.credId accountCreation))
    when exists $ throwError $ DuplicateAccountRegistrationID (ID.credId accountCreation)
    let credIpId = ID.ipId accountCreation
    mIpInfo <- lift (getIdentityProvider credIpId)
    case mIpInfo of
      -- check that the identity provider exists
      Nothing -> throwError CredentialDeploymentInvalidIdentityProvider
      Just ipInfo ->
        case credential of
          ID.InitialACWP icdi ->
            -- check signatures for an initial credential deployment
            unless (A.verifyInitialAccountCreation ipInfo messageExpiry (S.encode icdi)) $ throwError CredentialDeploymentInvalidSignatures
          ID.NormalACWP ncdi -> do
            cryptoParams <- lift getCryptographicParameters
            let ncdv = ID.cdiValues ncdi
            mArsInfos <- lift (getAnonymityRevokers (OrdMap.keys (ID.cdvArData ncdv)))
            case mArsInfos of
              -- check that the anonymity revokers exist
              Nothing -> throwError CredentialDeploymentInvalidAnonymityRevokers
              Just arsInfos -> do
                -- if the credential deployment contained an empty map of 'ChainArData' then the result will be 'Just empty'.
                when (null arsInfos) $ throwError CredentialDeploymentInvalidAnonymityRevokers
                -- check signatures for a normal credential deployment
                unless (A.verifyCredential cryptoParams ipInfo arsInfos (S.encode ncdi) (Left messageExpiry)) $ throwError CredentialDeploymentInvalidSignatures
    return Success)
  
-- |Verifies a 'ChainUpdate' transaction.
-- This function verifies the following:
-- * Checks that the effective time is no later than the timeout of the chain update.
-- * Checks that the 'ChainUpdate' is correctly signed.
verifyChainUpdate :: TransactionVerifier m => ST.UpdateInstruction -> m VerificationResult
verifyChainUpdate ui@ST.UpdateInstruction{..} =
  either id id <$> runExceptT (do
    -- check that the effective time is not after the timeout of the chain update.
    when (ST.updateTimeout uiHeader >= ST.updateEffectiveTime uiHeader && ST.updateEffectiveTime uiHeader /= 0) $
      throwError ChainUpdateEffectiveTimeBeforeTimeout
    -- check the signature is valid
    keys <- lift getUpdateKeysCollection
    unless (Updates.checkAuthorizedUpdate keys ui) $ throwError ChainUpdateInvalidSignatures
    return (ChainUpdateSuccess (getHash keys)))

-- |Verifies a 'NormalTransaction' transaction.
-- This function verifies the following:
-- * Checks that enough energy is supplied for the transaction
-- * Checks that the sender is a valid account
-- * Checks that the nonce is correct
-- * Checks that 
verifyNormalTransaction :: TransactionVerifier m => TransactionData -> m VerificationResult
verifyNormalTransaction meta = either id id <$> runExceptT (do
  return ChainUpdateSuccess)
  
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
    lift $ isJust <$> BS.getAccountByCredId state regId
  {-# INLINE accountExists #-}
  accountExists aaddr = do
    state <- ask
    let res = lift (BS.getAccount state aaddr)
    fmap isJust res
  {-# INLINE getUpdateKeysCollection #-}
  getUpdateKeysCollection = do
    state <- ask
    lift (BS.getUpdateKeysCollection state)
