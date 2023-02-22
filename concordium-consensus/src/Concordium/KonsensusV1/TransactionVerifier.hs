{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
-- |This module implements the 'TransactionVerifier' for consensus protocol V1.
module Concordium.KonsensusV1.TransactionVerifier where

import Data.Kind (Type)
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Maybe (isJust)

import Concordium.Types
import qualified Concordium.ID.Types as ID
import Concordium.Types.Parameters hiding (getChainParameters)

import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.TransactionVerification as TVer
import Concordium.KonsensusV1.TreeState.Implementation

-- |Where a received transaction stems from.
-- A transaction is either received as part of a block or it
-- has been submitted individually to the consensus.
data TransactionOrigin = Block | Individual
    deriving (Eq, Show)

-- |Context for verifying a transaction.
data Context (m :: Type -> Type) = Context
    { -- |The 'SkovData' to use for verifying a transaction.
      _ctxSkovData :: !(SkovData (MPV m)),
      -- |The blockstate
      _ctxBs :: BlockState m,
      -- |Whether the transaction was received from a block or individually.
      _transactionOrigin :: !TransactionOrigin
    }
makeLenses ''Context

-- |Helper type for defining 'TransactionVerifierT'. While we only instantiate @r@ with
-- @Context (BlockState m)@, it is simpler to derive the 'MonadTrans' instance using the present
-- definition.
newtype TransactionVerifierT' (r :: Type) (m :: Type -> Type) (a :: Type) = TransactionVerifierT {runTransactionVerifierT :: r -> m a}
    deriving (Functor, Applicative, Monad, MonadReader r) via (ReaderT r m)
    deriving (MonadTrans) via (ReaderT r)

deriving via (ReaderT r) m instance (MonadProtocolVersion m) => MonadProtocolVersion (TransactionVerifierT' r m)
deriving via (ReaderT r) m instance BlockStateTypes (TransactionVerifierT' r m)

type TransactionVerifierT m = TransactionVerifierT' (Context m) m

instance
  ( IsConsensusV1 (MPV m),
    MonadProtocolVersion m,
    BS.BlockStateQuery m,
    r ~ Context m
  ) => TVer.TransactionVerifier (TransactionVerifierT' r m) where
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider ipId = do
        bs <- asks _ctxBs
        lift $! BS.getIdentityProvider bs ipId
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers arrIds = do
        bs <- asks _ctxBs
        lift $! BS.getAnonymityRevokers bs arrIds
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = do
        bs <- asks _ctxBs
        lift $! BS.getCryptographicParameters bs
    {-# INLINE registrationIdExists #-}
    registrationIdExists regId = do
        bs <- asks _ctxBs
        lift $ isJust <$> BS.getAccountByCredId bs (ID.toRawCredRegId regId)
    {-# INLINE getAccount #-}
    getAccount aaddr = do
        bs <- asks _ctxBs
        fmap snd <$> lift (BS.getAccount bs aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = do
        bs <- asks _ctxBs
        lift $! BS.getNextUpdateSequenceNumber bs uType
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = do
        bs <- asks _ctxBs
        lift $! BS.getUpdateKeysCollection bs
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . BS.getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce acc = do
        ctx <- ask
        -- If the transaction was received as part of a block
        -- then we check the account nonce from the `BlockState` in the context
        -- Otherwise if the transaction was received individually then we
        -- check the transaction table for the nonce.
        asks _transactionOrigin >>= \case
            Block -> lift (BS.getAccountNonce acc)
            Individual -> do
                aaddr <- lift $! BS.getAccountCanonicalAddress acc
                return $! fst $! doGetNextAccountNonce (accountAddressEmbed aaddr) (ctx ^. ctxSkovData)
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . BS.getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        bs <- asks _ctxBs
        rate <- lift $! _erEnergyRate <$> BS.getExchangeRates bs
        return $! computeCost rate v
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        bs <- asks _ctxBs
        chainParams <- lift $! BS.getChainParameters bs
        return $! chainParams ^. cpConsensusParameters . cpBlockEnergyLimit
    {-# INLINE checkExactNonce #-}
    checkExactNonce = do
        asks _transactionOrigin >>= \case
            Block -> return False
            Individual -> return True

