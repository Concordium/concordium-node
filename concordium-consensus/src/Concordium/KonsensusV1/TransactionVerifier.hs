{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Concordium.KonsensusV1.TransactionVerifier where

import Data.Kind (Type)
import Control.Monad.Reader
import Lens.Micro.Platform
import Data.Maybe (isJust)

import Concordium.Types
import qualified Concordium.ID.Types as ID
import Concordium.Types.Parameters

import Concordium.GlobalState.Types
import Concordium.GlobalState.BlockState
import qualified Concordium.TransactionVerification as TVer
import Concordium.KonsensusV1.TreeState.Implementation

-- |The context required for the transaction verifier in order to verify a transaction.
data Context (m :: Type -> Type) = Context
    { -- |The 'SkovData' to use for verifying a transaction.
      _ctxSkovData :: !(SkovData (MPV m)),
      -- |The blockstate
      _ctxBs :: BlockState m,
      -- |Whether the transaction was received from a block or individually.
      _isTransactionFromBlock :: Bool
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
  ( Monad m,
    MonadProtocolVersion m,
    r ~ Context m
  ) => TVer.TransactionVerifier (TransactionVerifierT' r m) where
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider ipId = do
        ctx <- ask
        lift $! getIdentityProvider (ctx ^. ctxBs) ipId
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers arrIds = do
        ctx <- ask
        lift $! getAnonymityRevokers (ctx ^. ctxBs) arrIds
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = do
        ctx <- ask
        lift $! getCryptographicParameters (ctx ^. ctxBs)
    {-# INLINE registrationIdExists #-}
    registrationIdExists regId = do
        ctx <- ask
        lift $ isJust <$> getAccountByCredId (ctx ^. ctxBs) (ID.toRawCredRegId regId)
    {-# INLINE getAccount #-}
    getAccount aaddr = do
        ctx <- ask
        fmap snd <$> lift (getAccount (ctx ^. ctxBs) aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = do
        ctx <- ask
        lift $! getNextUpdateSequenceNumber (ctx ^. ctxBs) uType
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = do
        ctx <- ask
        lift $! getUpdateKeysCollection (ctx ^. ctxBs)
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce acc = do
        ctx <- ask
        -- If the transaction was received as part of a block
        -- then we check the account nonce from the `BlockState` in the context
        -- Otherwise if the transaction was received individually then we
        -- check the transaction table for the nonce.
        if ctx ^. isTransactionFromBlock
            then lift (getAccountNonce acc)
            else do
                aaddr <- lift $! getAccountCanonicalAddress acc
                return $! fst $! doGetNextAccountNonce (accountAddressEmbed aaddr) (ctx ^. ctxSkovData)
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        ctx <- ask
        rate <- lift $! _erEnergyRate <$> getExchangeRates (ctx ^. ctxBs)
        return $! computeCost rate v
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        ctx <- ask
        lift $! getChainParameters (ctx ^. ctxBs)
    {-# INLINE checkExactNonce #-}
    checkExactNonce = do
        ctx <- ask
        return $! not (ctx ^. isTransactionFromBlock)

