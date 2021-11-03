{-# LANGUAGE TypeFamilies #-}
module Concordium.TransactionVerification
  where

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as OrdMap
import qualified Data.HashMap.Strict as HM
import qualified Data.Serialize as S

import qualified Concordium.Types.Transactions as Tx
import qualified Concordium.ID.AnonymityRevoker as AR
import qualified Concordium.ID.IdentityProvider as IP
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.Types.Parameters as Params
import qualified Concordium.Types as Types
import qualified Concordium.ID.Account as A
import qualified Concordium.ID.Types as ID
import Data.Maybe (isJust)
import Control.Monad.Except

-- |The 'VerificationResult' type serves as an intermediate `result type` between the 'TxResult' and 'UpdateResult' types.
-- VerificationResult's contains possible verification errors that may have occurred when verifying a 'AccountCreation' type.
data VerificationResult
  = Success
  -- ^The verification passed
  | Stale
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

-- |Returns `True` if the `VerificationResult` should be stored in the cache.
-- That is, verification results which are not immediately rejectable and could be valid in the future.
isVerifiable :: VerificationResult -> Bool
isVerifiable Success = True
isVerifiable CredentialDeploymentInvalidIdentityProvider = True
isVerifiable CredentialDeploymentInvalidAnonymityRevokers = True
isVerifiable _ = False

-- |The transaction verification cache stores transaction 'VerificationResult's associated with 'TransactionHash'es.
-- New entries are being put into the cache when receiving new transasactions (either as a single transaction or within a block).
-- The cached verification results are used by the Scheduler to short-cut verification
-- during block execution.
-- Entries in the cache are removed when the associated transaction is either
-- finalized or purged.
type TransactionVerificationCache = HM.HashMap Types.TransactionHash VerificationResult

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

-- |Verifies a 'CredentialDeployment' transaction.
-- Use this verification function if the transaction has not been received via a block.
-- 
-- In addition to the checks of `verifyCredentialDeployment` this function also carries out the
-- following checks:
-- * Check the transaction is not expired
-- * Checks that the 'CredentialDeployment' is not expired
verifyCredentialDeploymentFull :: TransactionVerifier m => Types.Timestamp -> Tx.AccountCreation -> m VerificationResult
verifyCredentialDeploymentFull now tx@Tx.AccountCreation{..} =
  either id id <$> runExceptT (do
    -- check that the transaction is not yet expired
    let expired = Types.transactionExpired (Tx.msgExpiry tx) now
    when expired $ throwError Stale
    -- check that the credential deployment is not yet expired
    let expiry = ID.validTo credential
    unless (Types.isTimestampBefore now expiry) $ throwError CredentialDeploymentExpired
    lift (verifyCredentialDeployment tx))

-- |Verifies a 'CredentialDeployment' transaction which origins from a block
-- Note. The caller must make sure to only use this verification function if the
-- transaction stems from a block.
-- If the transaction does not come from a block, but as a single transaction, then
-- use `verifyCredentialDeploymentFull`.
--
-- This function verifies the following:
-- * Making sure that an registration id does not already exist and also that 
-- a corresponding account does not exist.
-- * Validity of the 'IdentityProvider' and 'AnonymityRevokers' provided.
-- * Key sizes for the 'CredentialDeployment'
-- * Valid signatures on the 'CredentialDeployment'
verifyCredentialDeployment :: TransactionVerifier m => Tx.AccountCreation -> m VerificationResult
verifyCredentialDeployment accountCreation@Tx.AccountCreation{..} =
  either id id <$> runExceptT (do
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
            case ID.cdvPublicKeys ncdv of
              ID.CredentialPublicKeys keys _ -> do
                -- check that the keys are well sized
                when (null keys || (length keys > 255)) $ throwError CredentialDeploymentInvalidKeys
                mArsInfos <- lift (getAnonymityRevokers (OrdMap.keys (ID.cdvArData ncdv)))
                case mArsInfos of
                  -- check that the anonymity revokers exists
                  Nothing -> throwError CredentialDeploymentInvalidAnonymityRevokers
                  Just arsInfos -> do
                    -- if the credential deployment contained an empty map of 'ChainArData' then the result will be 'Just empty'.
                    when (null arsInfos) $ throwError CredentialDeploymentInvalidAnonymityRevokers
                    -- check signatures for a normal credential deployment
                    unless (A.verifyCredential cryptoParams ipInfo arsInfos (S.encode ncdi) (Left messageExpiry)) $ throwError CredentialDeploymentInvalidSignatures
    return Success)

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

