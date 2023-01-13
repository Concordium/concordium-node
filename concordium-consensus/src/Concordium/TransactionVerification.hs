{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.TransactionVerification where

import Control.Monad.Trans
import qualified Data.Map.Strict as OrdMap
import qualified Data.Serialize as S

import qualified Concordium.Cost as Cost
import qualified Concordium.Crypto.SHA256 as Sha256
import qualified Concordium.GlobalState.Types as GSTypes
import qualified Concordium.ID.Account as A
import qualified Concordium.ID.AnonymityRevoker as AR
import qualified Concordium.ID.IdentityProvider as IP
import qualified Concordium.ID.Types as ID
import qualified Concordium.Types as Types
import Concordium.Types.HashableTo (getHash)
import qualified Concordium.Types.Parameters as Params
import qualified Concordium.Types.Transactions as Tx
import Concordium.Types.Updates (UpdateSequenceNumber)
import qualified Concordium.Types.Updates as Updates
import Control.Monad.Except

-- |VerificationResults of transactions.
-- A verification result can either be 'Ok', 'MaybeOk' or 'NotOk'.
--
-- This type exists as an intermediate type for decoupling the internal verification process from the outcome of processing a transaction.
data VerificationResult
    = -- |The transaction successfully passed the verification check.
      Ok !OkResult
    | -- |The transaction did not pass the verification check, but it might
      -- be valid at a later point in time. As such if the transaction was received individually
      -- it must be rejected at once. However if the transaction was received as part of a block,
      -- then it must be accepted and re-verified in the 'Scheduler'.
      MaybeOk !MaybeOkResult
    | -- |The transaction is definitely not valid and must be rejected.
      NotOk !NotOkResult
    deriving (Eq, Show, Ord)

-- |The transaction was verified successfully and was valid at the point of receiving it.
-- The OkResult's should contain enough information for the scheduler to verify if it is still valid
-- or if it needs a new verification before executing.
--
-- 'CredentialDeploymentSuccess' as ip's or ar's cannot be removed then nothing needs to be checked before executing
-- 'ChainUpdateSuccess' it should be checked that the authorization keys match the current ones before executing
-- 'NormalTransactionSuccess' it should be checked that the account information can still verify before executing
data OkResult
    = -- |The 'CredentialDeployment' passed verification.
      CredentialDeploymentSuccess
    | -- |The 'ChainUpdate' passed verification.
      -- The result contains the hash of the authorization keys and the `UpdateSequenceNumber`
      -- It must be checked that this hash corresponds to the hash of the current authorization keys before
      -- executing the transaction.
      ChainUpdateSuccess
        { keysHash :: !Sha256.Hash,
          seqNumber :: !Updates.UpdateSequenceNumber
        }
    | -- |The 'NormalTransaction' passed verification.
      -- The result contains the hash of the keys and the transaction nonce.
      -- The `keysHash` can be used to short-circuit the signature verification when executing the transaction.
      -- Note. If in the meantime the keys have changed for the account then the signature has to be verified again.
      NormalTransactionSuccess
        { keysHash :: !Sha256.Hash,
          nonce :: !Types.Nonce
        }
    deriving (Eq, Show, Ord)

-- |Verification results which are rejectable if the transaction was received individually.
-- However if the transaction was received as part of a block, then the transaction is considered maybe valid.
-- Note. It could be the case that transactions received individually and failing verification with
-- one of the below status codes could be valid for a future block. However we choose not to accept these
-- in order to protect the network.
data MaybeOkResult
    = -- |The IdentityProvider does not exist for this 'CredentialDeployment'.
      -- Reason for 'MaybeOk': the IP could've been added at a later point in time.
      CredentialDeploymentInvalidIdentityProvider !ID.IdentityProviderIdentity
    | -- |The anonymity revokers does not exist for this 'CredentialDeployment'.
      -- Reason for 'MaybeOk': the AR could've been added at a later point in time.
      CredentialDeploymentInvalidAnonymityRevokers
    | -- |The sequence number was not sequential.
      -- Reason for `MaybeOk`: it could be that former chain update was simply not received yet.
      ChainUpdateInvalidNonce !Updates.UpdateSequenceNumber
    | -- |The 'ChainUpdate' contained invalid signatures.
      -- Reason for 'MaybeOk': the keys might have been updated in a later block resulting in a valid signature
      ChainUpdateInvalidSignatures
    | -- |The 'NormalTransaction' contained an invalid sender
      -- Reason for 'MaybeOk': the sender could exist at a later point in time.
      NormalTransactionInvalidSender !Types.AccountAddress
    | -- |The 'NormalTransaction' contained an invalid nonce.
      -- The result contains the next nonce.
      -- Reason for 'MaybeOk': the nonce could be valid at a later point in time.
      NormalTransactionInvalidNonce !Types.Nonce
    | -- |The sender does not have enough funds to cover the transfer.
      -- Reason for 'MaybeOk': the sender could have enough funds at a later point in time.
      NormalTransactionInsufficientFunds
    | -- |The 'NormalTransaction' contained invalid signatures.
      -- Reason for 'MaybeOk': the sender could've changed account information at a later point in time.
      NormalTransactionInvalidSignatures
    deriving (Eq, Show, Ord)

-- |Verification results which always should result in a transaction being rejected.
data NotOkResult
    = -- |The 'AccountCreation' contained an expired 'validTo'
      CredentialDeploymentExpired
    | -- |The 'CredentialDeployment' contained an invalid registration id.
      -- There already exists an account with the registration id.
      -- Reason for 'NotOk': As we verify against the last finalized block or the parent block of which it was received within then
      -- under no circumstances must there be duplicate registration ids.
      CredentialDeploymentDuplicateAccountRegistrationID !ID.CredentialRegistrationID
    | -- |The 'AccountCreation' contained invalid identity provider and/or anonymity revoker signatures.
      -- Reason for 'NotOk': IdentityProvider and AnonymityRevoker keys cannot be updated.
      CredentialDeploymentInvalidSignatures
    | -- |The 'ChainUpdate' had an expiry set too late.
      ChainUpdateEffectiveTimeBeforeTimeout
    | -- |The `UpdateSequenceNumber` of the 'ChainUpdate' was too old.
      -- Reason for 'NotOk': the UpdateSequenceNumber can never be valid in a later block if the
      -- nonce is already used.
      ChainUpdateSequenceNumberTooOld !UpdateSequenceNumber
    | -- |Not enough energy was supplied for the transaction.
      NormalTransactionDepositInsufficient
    | -- |The energy requirement of the transaction exceeds the maximum allowed for a block.
      -- The transaction can never be part of a block so it is rejected.
      NormalTransactionEnergyExceeded
    | -- |The 'NormalTransaction' contained an already used nonce.
      NormalTransactionDuplicateNonce !Types.Nonce
    | -- |The transaction was expired
      Expired
    | -- |Transaction payload size exceeds protocol limit.
      InvalidPayloadSize
    deriving (Eq, Show, Ord)

-- |Type which can verify transactions in a monadic context.
-- The type is responsible for retrieving the necessary information
-- in order to deem a transaction `Ok`, `MaybeOk` or `NotOk`.
-- See above for explanations of the distinction between these types.
class (Monad m, GSTypes.MonadProtocolVersion m) => TransactionVerifier m where
    -- |Get the provider identity data for the given identity provider, or Nothing if
    -- the identity provider with given ID does not exist.
    getIdentityProvider :: ID.IdentityProviderIdentity -> m (Maybe IP.IpInfo)

    -- |Get the anonymity revokers with given ids. Returns 'Nothing' if any of the
    -- anonymity revokers are not found.
    getAnonymityRevokers :: [ID.ArIdentity] -> m (Maybe [AR.ArInfo])

    -- |Get cryptographic parameters for the current state.
    getCryptographicParameters :: m Params.CryptographicParameters

    -- |Check whether the given credential registration ID exists.
    registrationIdExists :: ID.CredentialRegistrationID -> m Bool

    -- |Get the account associated for the given account address.
    -- Returns 'Nothing' if no such account exists.
    getAccount :: Types.AccountAddress -> m (Maybe (GSTypes.Account m))

    -- |Get the next 'SequenceNumber' given the 'UpdateType'.
    getNextUpdateSequenceNumber :: Updates.UpdateType -> m Updates.UpdateSequenceNumber

    -- |Get the UpdateKeysCollection
    getUpdateKeysCollection :: m (Updates.UpdateKeysCollection (Params.AuthorizationsVersionForPV (GSTypes.MPV m)))

    -- |Get the current available amount for the specified account.
    getAccountAvailableAmount :: GSTypes.Account m -> m Types.Amount

    -- |Get the next account nonce.
    getNextAccountNonce :: GSTypes.Account m -> m Types.Nonce

    -- |Get the verification keys associated with an Account.
    getAccountVerificationKeys :: GSTypes.Account m -> m ID.AccountInformation

    -- |Convert the given energy to CCD at the current block.
    energyToCcd :: Types.Energy -> m Types.Amount

    -- |Get the maximum energy for a block.
    getMaxBlockEnergy :: m Types.Energy

    -- |If the nonce should be checked if it is exactly the next one for the given account
    -- or not (from the perspective of the verification context).
    -- When transactions are received initially via a block we allow some slack with regard to exactness of the
    -- provided nonce. The node may just not have received a parent block.
    -- On the other hand when a transaction is received individually we must check that the nonce is exactly the next one for
    -- the associated account and vice versa before executing the transaction in the `Scheduler`.
    checkExactNonce :: m Bool

-- |Convenience function for verifying a transaction.
-- This function takes the time of verification and the actual `BlockItem`.
-- If the transaction was received individually then @time@ is 'now'.
-- Otherwise if the transaction was received as part of a block then @now@ refers the timestamp of the `Slot` associated with the `Block`.
--
-- See @verifyCredentialDeployment@, @verifyChainUpdate@ and @verifyNormalTransaction@.
verify :: TransactionVerifier m => Types.Timestamp -> Tx.BlockItem -> m VerificationResult
verify now bi = do
    if Types.transactionExpired (Tx.msgExpiry bi) now
        then return (NotOk Expired)
        else case bi of
            Tx.WithMetadata{wmdData = Tx.CredentialDeployment cred} -> do
                verifyCredentialDeployment now cred
            Tx.WithMetadata{wmdData = Tx.ChainUpdate ui} -> do
                verifyChainUpdate ui
            Tx.WithMetadata{wmdData = Tx.NormalTransaction tx} -> do
                verifyNormalTransaction tx

-- |Verifies a 'CredentialDeployment' transaction.
--
-- This function verifies the following:
-- * Checks that the 'CredentialDeployment' is not expired
-- * Making sure that an registration id does not already exist
--   The `Scheduler` ensures that an account address doesn't clash with an existing one on the chain.
-- * Validity of the 'IdentityProvider' and 'AnonymityRevokers' provided.
-- * That the 'CredentialDeployment' contains valid signatures.
verifyCredentialDeployment :: TransactionVerifier m => Types.Timestamp -> Tx.AccountCreation -> m VerificationResult
verifyCredentialDeployment now accountCreation@Tx.AccountCreation{..} =
    either id id
        <$> runExceptT
            ( do
                -- check that the credential deployment is not yet expired
                let expiry = ID.validTo credential
                unless (Types.isTimestampBefore now expiry) $ throwError $ NotOk CredentialDeploymentExpired
                -- check that the credential deployment is not a duplicate
                exists <- lift (registrationIdExists (ID.credId accountCreation))
                when exists $ throwError $ NotOk $ CredentialDeploymentDuplicateAccountRegistrationID (ID.credId accountCreation)
                let credIpId = ID.ipId accountCreation
                mIpInfo <- lift (getIdentityProvider credIpId)
                case mIpInfo of
                    -- check that the identity provider exists
                    Nothing -> throwError $ MaybeOk (CredentialDeploymentInvalidIdentityProvider credIpId)
                    Just ipInfo ->
                        case credential of
                            ID.InitialACWP icdi ->
                                -- check signatures for an initial credential deployment
                                unless (A.verifyInitialAccountCreation ipInfo messageExpiry (S.encode icdi)) $ throwError $ NotOk CredentialDeploymentInvalidSignatures
                            ID.NormalACWP ncdi -> do
                                cryptoParams <- lift getCryptographicParameters
                                let ncdv = ID.cdiValues ncdi
                                mArsInfos <- lift (getAnonymityRevokers (OrdMap.keys (ID.cdvArData ncdv)))
                                case mArsInfos of
                                    -- check that the anonymity revokers exist
                                    Nothing -> throwError $ MaybeOk CredentialDeploymentInvalidAnonymityRevokers
                                    Just arsInfos -> do
                                        -- if the credential deployment contained an empty map of 'ChainArData' then the result will be 'Just empty'.
                                        when (null arsInfos) $ throwError $ MaybeOk CredentialDeploymentInvalidAnonymityRevokers
                                        -- check signatures for a normal credential deployment
                                        unless (A.verifyCredential cryptoParams ipInfo arsInfos (S.encode ncdi) (Left messageExpiry)) $ throwError $ NotOk CredentialDeploymentInvalidSignatures
                return $ Ok CredentialDeploymentSuccess
            )

-- |Verifies a 'ChainUpdate' transaction.
-- This function verifies the following:
-- * Checks that the effective time is no later than the timeout of the chain update.
-- * Checks that provided sequence number is sequential.
-- * Checks that the 'ChainUpdate' is correctly signed.
verifyChainUpdate :: forall m. TransactionVerifier m => Updates.UpdateInstruction -> m VerificationResult
verifyChainUpdate ui@Updates.UpdateInstruction{..} =
    either id id
        <$> runExceptT
            ( do
                unless (Types.validatePayloadSize (Types.protocolVersion @(GSTypes.MPV m)) (Updates.updatePayloadSize uiHeader)) $
                    throwError $
                        NotOk InvalidPayloadSize
                -- Check that the timeout is no later than the effective time,
                -- or the update is immediate
                when (Updates.updateTimeout uiHeader >= Updates.updateEffectiveTime uiHeader && Updates.updateEffectiveTime uiHeader /= 0) $
                    throwError $
                        NotOk ChainUpdateEffectiveTimeBeforeTimeout
                -- check that the sequence number is not too old.
                nextSN <- lift $ getNextUpdateSequenceNumber (Updates.updateType (Updates.uiPayload ui))
                let nonce = Updates.updateSeqNumber (Updates.uiHeader ui)
                when (nextSN > nonce) $ throwError $ NotOk $ ChainUpdateSequenceNumberTooOld nextSN
                -- if this transaction was received individually then we also verify that the sequence
                -- number is the next one.
                exactSequenceNumber <- lift checkExactNonce
                when (exactSequenceNumber && nonce /= nextSN) $ throwError (MaybeOk $ ChainUpdateInvalidNonce nextSN)
                -- check the signature is valid
                keys <- lift getUpdateKeysCollection
                unless (Updates.checkAuthorizedUpdate keys ui) $ throwError $ MaybeOk ChainUpdateInvalidSignatures
                return $ Ok $ ChainUpdateSuccess (getHash keys) nonce
            )

-- |Verifies a 'NormalTransaction' transaction.
-- This function verifies the following:
-- * Checks that enough energy is supplied for the transaction.
-- * Checks that the sender is a valid account.
-- * Checks that the nonce is correct.
-- * Checks that the 'NormalTransaction' is correctly signed.
verifyNormalTransaction ::
    forall m msg.
    (TransactionVerifier m, Tx.TransactionData msg) =>
    msg ->
    m VerificationResult
verifyNormalTransaction meta =
    either id id
        <$> runExceptT
            ( do
                unless (Types.validatePayloadSize (Types.protocolVersion @(GSTypes.MPV m)) (Tx.thPayloadSize (Tx.transactionHeader meta))) $
                    throwError $
                        NotOk InvalidPayloadSize
                -- Check that enough energy is supplied
                let cost = Cost.baseCost (Tx.getTransactionHeaderPayloadSize $ Tx.transactionHeader meta) (Tx.getTransactionNumSigs (Tx.transactionSignature meta))
                unless (Tx.transactionGasAmount meta >= cost) $ throwError $ NotOk NormalTransactionDepositInsufficient
                -- Check that the required energy does not exceed the maximum allowed for a block
                maxEnergy <- lift getMaxBlockEnergy
                when (Tx.transactionGasAmount meta > maxEnergy) $ throwError $ NotOk NormalTransactionEnergyExceeded
                -- Check that the sender account exists
                let addr = Tx.transactionSender meta
                macc <- lift (getAccount addr)
                case macc of
                    Nothing -> throwError (MaybeOk $ NormalTransactionInvalidSender addr)
                    Just acc -> do
                        -- Check that the nonce of the transaction is correct.
                        nextNonce <- lift (getNextAccountNonce acc)
                        let nonce = Tx.transactionNonce meta
                        when (nonce < nextNonce) $ throwError (NotOk $ NormalTransactionDuplicateNonce nonce)
                        -- For transactions received as part of a `Block` we only check that the `Nonce`
                        -- is not too old with respect to the 'last finalized block' or the 'parent block'.
                        -- In the `Scheduler` we check that the `Nonce` is actually the next one.
                        -- The reason for this is that otherwise when transactions are received via a block
                        -- we don't know what the next nonce is as we can't verify in the context of the 'block' which the
                        -- transactions were received with. Hence if there are multiple transactions from the same account within the same block,
                        -- which would lead us to rejecting the valid transaction(s).
                        exactNonce <- lift checkExactNonce
                        when (exactNonce && nonce /= nextNonce) $ throwError (MaybeOk $ NormalTransactionInvalidNonce nextNonce)
                        -- check that the sender account has enough funds to cover the transfer
                        amnt <- lift $ getAccountAvailableAmount acc
                        depositedAmount <- lift (energyToCcd (Tx.transactionGasAmount meta))
                        unless (depositedAmount <= amnt) $ throwError $ MaybeOk NormalTransactionInsufficientFunds
                        -- Check the signature
                        keys <- lift (getAccountVerificationKeys acc)
                        let sigCheck = Tx.verifyTransaction keys meta
                        unless sigCheck $ throwError $ MaybeOk NormalTransactionInvalidSignatures
                        return $ Ok $ NormalTransactionSuccess (getHash keys) nonce
            )

-- |Wrapper types for pairing a transaction with its verification result (if it has one).
-- The purpose of these types is to provide a uniform api between the 'TransactionTable' and the 'TreeState'.
--
-- The verification result is computed when the transaction is received either by 'doReceiveTransaction' or 'doReceiveTransactionInternal'.
-- The verification results are used by the scheduler when executing transactions, hence finalized transactions
-- are not required to store the verification results.
-- But it happens that a finalized transaction is included in multiple blocks while finalized in a parent block.
-- This will ultimately result in a duplicate transaction and the child block will fail execution.
-- But to accommodate the above mentioned scenario the verification results are wrapped in a `Maybe`.

-- |A 'BlockItem' with its associated 'VerificationResult'
type BlockItemWithStatus = (Tx.BlockItem, Maybe VerificationResult)

-- |A 'Transaction' with its associated 'VerificationResult'
type TransactionWithStatus = (Tx.Transaction, Maybe VerificationResult)

-- |A 'CredentialDeployment' with its associated 'VerificationResult'
type CredentialDeploymentWithStatus = (Tx.CredentialDeploymentWithMeta, Maybe VerificationResult)

-- |A 'ChainUpdate' with its associated 'VerificationResult'
type ChainUpdateWithStatus = (Tx.WithMetadata Updates.UpdateInstruction, Maybe VerificationResult)

-- |Type for describing the origin of the transaction.
-- The transaction can either arrive at the consensus as a single transaction,
-- or the transaction can be received as part of a block.
-- The ´Block´ additionally contains a ´BlockState´ of either the parent block (iff. it's 'alive') or the last finalized block.
data TransactionOrigin m = Single | Block (GSTypes.BlockState m)

-- |Determines if a transaction definitely cannot be valid now or in a future block.
-- Transactions received individually must be verified successfully.
-- However there is a looser requirement for transactions received
-- as part of a block. See the 'TransactionVerification' module for more details.
definitelyNotValid :: VerificationResult -> Bool -> Bool
definitelyNotValid verificationResult fromBlock =
    case fromBlock of
        False ->
            case verificationResult of
                Ok _ -> False
                MaybeOk _ -> True
                NotOk _ -> True
        True ->
            case verificationResult of
                NotOk _ -> True
                Ok _ -> False
                MaybeOk _ -> False
