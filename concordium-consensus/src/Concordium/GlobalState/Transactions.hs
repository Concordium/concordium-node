{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module combines the following transaction related functionalites
-- across the consensus protocols (v0 and v1)
--
-- * The 'AddTransactionResult' is the result of inserting a transaction into
--   a supported tree state.
--
-- * 'TransactionOrigin' indicates whether a transaction stems from a block or
--   it arrived at the consensus layer individually. These are the only ways
--   a transaction can ever be inserted into an underlying tree state.
--
-- * The 'TransactionVerifierT' is defined here. This is the monad transformer
--   that is used for verifying transactions.
--
-- * The 'AccountNonceQuery' class which is responsible for retrieving the
--   "next available" account nonce from the underlying 'TreeState'.
--   Note that this is required in order to share the same transaction verifier implementation
--   accros the consensus protocol versions as this is used by the 'TransactionVerifier'.
module Concordium.GlobalState.Transactions where

import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Kind (Type)
import Data.Maybe (isJust)
import Lens.Micro.Platform

-- Base package dependencies
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.Types.Parameters
import Concordium.Types.Transactions

-- Consensus package dependencies
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Types
import qualified Concordium.TransactionVerification as TVer

-- |Result of trying to add a transaction to the transaction table.
data AddTransactionResult
    = -- |Transaction is a duplicate of the given transaction.
      -- Contains the duplicate `BlockItem` and the cached `VerificationResult` only if
      -- the transaction has status `Received` or ´Committed´.
      Duplicate !BlockItem (Maybe TVer.VerificationResult)
    | -- |The transaction was newly added.
      -- Contains the `BlockItem` that was added and the cached `VerificationResult`.
      Added !BlockItem !TVer.VerificationResult
    | -- |The nonce of the transaction is not later than the last finalized transaction for the sender.
      -- The transaction is not added to the table.
      ObsoleteNonce
    | -- |The transaction was not added as it could not be deemed verifiable.
      -- The `NotAdded` contains the `VerificationResult`
      NotAdded !TVer.VerificationResult
    deriving (Eq, Show)

-- |Type for describing the origin of the transaction.
-- The transaction can either arrive at the consensus individually,
-- or the transaction can be received as part of a block.
-- The ´Block´ additionally contains a ´BlockState´ of either the parent block (iff. it's 'alive') or the last finalized block.
data TransactionOrigin = Individual | Block
    deriving (Eq, Show)

-- |The Context that a transaction is verified within
-- in the reader based instance.
-- The `Context` contains the `BlockState`, the maximum energy of a block and
-- also whether the transaction was received individually or as part of a block.
--
-- The `Context` is used for verifying the transaction in a deferred manner.
-- That is, the verification process will only take place if the transaction is not already contained
-- in the `TransactionTable`.
-- Note. The `Context` is created when the transaction is received by `doReceiveTransactionInternal` and
-- the actual verification is carried out within `addCommitTransaction` when it has been checked
-- that the transaction does not already exist in the `TransactionTable`.
data Context t = Context
    { _ctxBs :: t,
      _ctxMaxBlockEnergy :: !Energy,
      -- |Whether the transaction was received from a block or individually.
      _ctxTransactionOrigin :: !TransactionOrigin
    }

makeLenses ''Context

-- |Monad for acquiring the next available nonce for an account.
class Monad m => AccountNonceQuery m where
    -- |Get the successor of the largest known account for the given account
    -- The function should return 'True' in the second component if and only if
    -- all (known) transactions from this account are finalized.
    getNextAccountNonce :: AccountAddressEq -> m (Nonce, Bool)

instance (Monad (t m), MonadTrans t, AccountNonceQuery m) => AccountNonceQuery (MGSTrans t m) where
    getNextAccountNonce = lift . getNextAccountNonce
    {-# INLINE getNextAccountNonce #-}

deriving via (MGSTrans MaybeT m) instance AccountNonceQuery m => AccountNonceQuery (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance AccountNonceQuery m => AccountNonceQuery (ExceptT e m)

-- |Helper type for defining 'TransactionVerifierT'. While we only instantiate @r@ with
-- @Context (BlockState m)@, it is simpler to derive the 'MonadTrans' instance using the present
-- definition.
newtype TransactionVerifierT' (r :: Type) (m :: Type -> Type) (a :: Type) = TransactionVerifierT {runTransactionVerifierT :: r -> m a}
    deriving (Functor, Applicative, Monad, MonadReader r) via (ReaderT r m)
    deriving (MonadTrans) via (ReaderT r)

deriving via (ReaderT r) m instance (MonadProtocolVersion m) => MonadProtocolVersion (TransactionVerifierT' r m)
deriving via (ReaderT r) m instance BlockStateTypes (TransactionVerifierT' r m)

type TransactionVerifierT m = TransactionVerifierT' (Context (BlockState m)) m

instance
    ( BlockStateQuery m,
      MonadProtocolVersion m,
      AccountNonceQuery m,
      r ~ Context (BlockState m)
    ) =>
    TVer.TransactionVerifier (TransactionVerifierT' r m)
    where
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider ipId = do
        bs <- view ctxBs
        lift $ getIdentityProvider bs ipId
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers arrIds = do
        bs <- view ctxBs
        lift $ getAnonymityRevokers bs arrIds
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = do
        bs <- view ctxBs
        lift $ getCryptographicParameters bs
    {-# INLINE registrationIdExists #-}
    registrationIdExists regId = do
        bs <- view ctxBs
        lift $ isJust <$> getAccountByCredId bs (ID.toRawCredRegId regId)
    {-# INLINE getAccount #-}
    getAccount aaddr = do
        bs <- view ctxBs
        fmap snd <$> lift (getAccount bs aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = do
        bs <- view ctxBs
        lift $ getNextUpdateSequenceNumber bs uType
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = do
        bs <- view ctxBs
        lift $ getUpdateKeysCollection bs
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce acc = do
        -- If the transaction was received as part of a block
        -- then we check the account nonce from the `BlockState` in the context
        -- Otherwise if the transaction was received individually then we
        -- check the transaction table for the nonce.
        view ctxTransactionOrigin >>= \case
            Individual -> do
                aaddr <- lift $ getAccountCanonicalAddress acc
                lift $ fst <$> getNextAccountNonce (accountAddressEmbed aaddr)
            Block -> lift $ getAccountNonce acc
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        bs <- view ctxBs
        rate <- lift $ _erEnergyRate <$> getExchangeRates bs
        return $! computeCost rate v
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = view ctxMaxBlockEnergy
    {-# INLINE checkExactNonce #-}
    checkExactNonce = (== Individual) <$> view ctxTransactionOrigin
