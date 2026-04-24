{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.Scheduler.ProtocolLevelTokens.Queries (
    QueryTokenModuleError (..),
    QueryLockModuleError (..),
    queryTokenInfo,
    queryAccountTokens,
    queryPLTList,
    queryTokenAuthorizations,
    queryLockList,
    queryLockInfo,
) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Void

import Concordium.Types
import qualified Concordium.Types.Locks as Locks
import qualified Concordium.Types.Queries.Locks as LockQueries
import Concordium.Types.Queries.Tokens

import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens (TokenAccountState (tasBalance))
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens (PLTConfiguration (..), TokenIndex)
import Concordium.GlobalState.Types
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.ProtocolLevelTokens.Module
import qualified Concordium.Scheduler.ProtocolLevelTokens.RustPLTScheduler.Queries as RustQ

-- | The 'QueryContext' provides the context to run 'PLTKernelQuery' operations against a
--  particular token index and block state.
data QueryContext m = QueryContext
    { -- | Index of the token. This must be valid in the context of the block state.
      qcTokenIndex :: !TokenIndex,
      -- | The block state.
      qcBlockState :: !(BlockState m),
      -- | Reference to the token state.
      qcTokenState :: !(MutableTokenState m)
    }

-- | @QueryT fail ret@ is a monad transformer that supports 'PLTKernelQuery' and 'PLTKernelFail'
--  on top of an underlying monad that implements 'BS.BlockStateQuery'.
newtype QueryT fail ret m a = QueryT
    { runQueryT' :: ReaderT (QueryContext m) (ContT (Either fail ret) m) a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadReader (QueryContext m)
        )

-- | Run a @QueryT fail a m a@ monadic action in the underlying monad @m@, given the
--  'QueryContext', and returning either the failure reason or the result.
runQueryT :: (Monad m) => QueryT fail a m a -> QueryContext m -> m (Either fail a)
runQueryT a ctx = runContT (runReaderT (runQueryT' a) ctx) (return . Right)

-- | Run a @QueryT Void ret m a@ monadic action in the underlying monad @m@, given the
--  'QueryContext', and returning the result. This is a variant of 'runQueryT' that does not
--  allow for failure, and thus the failure case is impossible.
runQueryTNoFail :: (Monad m) => QueryT Void a m a -> QueryContext m -> m a
runQueryTNoFail a ctx =
    runQueryT a ctx >>= \case
        Right r -> return r
        Left v -> case v of {}

instance MonadTrans (QueryT fail ret) where
    lift = QueryT . lift . lift

instance (BS.BlockStateQuery m, PVSupportsHaskellManagedPLT (MPV m)) => PLTKernelQuery (QueryT fail ret m) where
    type PLTAccount (QueryT fail ret m) = IndexedAccount m
    getTokenState key = do
        QueryContext{..} <- ask
        lift $ BS.lookupTokenState key qcTokenState
    getAccount addr = do
        QueryContext{..} <- ask
        lift $ BS.getAccount qcBlockState addr
    getAccountIndex (ix, _acct) = return ix
    getAccountByIndex accountIndex = do
        QueryContext{..} <- ask
        lift $ BS.getAccountByIndex qcBlockState accountIndex
    getAccountBalance acct = do
        QueryContext{..} <- ask
        lift $ BS.getAccountTokenBalance (snd acct) qcTokenIndex
    getAccountCanonicalAddress acct = do
        lift $ BS.getAccountCanonicalAddress (snd acct)
    getCirculatingSupply = do
        QueryContext{..} <- ask
        lift $ BS.getTokenCirculatingSupply qcBlockState qcTokenIndex
    getDecimals = do
        QueryContext{..} <- ask
        lift $ _pltDecimals <$> BS.getTokenConfiguration qcBlockState qcTokenIndex

instance (Monad m) => PLTKernelFail fail (QueryT fail ret m) where
    -- To abort, we simply drop the continuation and return the error.
    pltError err = QueryT $ ReaderT $ \_ -> ContT $ \_ -> return (Left err)

-- | An error that may occur as a result of 'queryTokenInfo' or 'queryTokenAuthorizations'.
data QueryTokenModuleError
    = -- | The requested token does not exist in the block.
      QTMEUnknownToken
    | -- | An error occurred in the token module.
      QTMEInternal !QueryTokenError
    | -- | The protocol version does not support the query.
      QTMEUnavailable
    deriving (Eq)

instance Show QueryTokenModuleError where
    show QTMEUnknownToken = "unknown token"
    show (QTMEInternal e) = show e
    show QTMEUnavailable = "The information is not available for this block"

-- | Get the 'TokenInfo' associated with a 'TokenId' in the given 'BlockState'.
queryTokenInfo ::
    forall m.
    (BS.BlockStateQuery m) =>
    TokenId ->
    BlockState m ->
    m (Either QueryTokenModuleError TokenInfo)
queryTokenInfo tokenId bs = case sPltStateVersionFor (protocolVersion @(MPV m)) of
    SPLTStateNone -> return (Left QTMEUnknownToken)
    SPLTStateV0 -> do
        mTokenIx <- BS.getTokenIndex bs tokenId
        case mTokenIx of
            Nothing -> return (Left QTMEUnknownToken)
            Just tokenIx -> do
                PLTConfiguration{..} <- BS.getTokenConfiguration bs tokenIx
                totalSupply <- BS.getTokenCirculatingSupply bs tokenIx
                tokenState <- BS.getMutableTokenState bs tokenIx
                let ctx = QueryContext{qcTokenIndex = tokenIx, qcBlockState = bs, qcTokenState = tokenState}
                runQueryT queryTokenModuleState ctx >>= \case
                    Left e -> return (Left (QTMEInternal e))
                    Right tms -> do
                        let ts =
                                TokenState
                                    { tsTokenModuleRef = _pltModule,
                                      tsDecimals = _pltDecimals,
                                      tsTotalSupply = toTokenAmount _pltDecimals totalSupply,
                                      tsModuleState = tms
                                    }
                        return $ Right TokenInfo{tiTokenId = _pltTokenId, tiTokenState = ts}
    SPLTStateV1 ->
        RustQ.queryTokenInfo bs tokenId <&> \case
            Just tokenInfo -> Right tokenInfo
            Nothing -> Left QTMEUnknownToken

-- | Get the 'TokenAuthorizations' associated with a 'TokenId' in the given 'BlockState'.
queryTokenAuthorizations ::
    forall m.
    (BS.BlockStateQuery m) =>
    TokenId ->
    BlockState m ->
    m (Either QueryTokenModuleError TokenAuthorizations)
queryTokenAuthorizations tokenId bs = case sPltStateVersionFor (protocolVersion @(MPV m)) of
    SPLTStateNone -> return (Left QTMEUnknownToken)
    SPLTStateV0 -> return (Left QTMEUnavailable)
    SPLTStateV1 ->
        RustQ.queryTokenAuthorizations bs tokenId <&> \case
            Just out -> Right out
            Nothing -> Left QTMEUnknownToken

-- | Get the list of 'Token's on an account.
queryAccountTokens ::
    forall m.
    (PVSupportsPLT (MPV m), BS.BlockStateQuery m) =>
    IndexedAccount m -> BlockState m -> m [Token]
queryAccountTokens acc bs = case sPltStateVersionFor (protocolVersion @(MPV m)) of
    SPLTStateV0 ->
        do
            tokenStatesMap <- BS.getAccountTokens (snd acc)
            forM (Map.toList tokenStatesMap) $ \(tokenIndex, tokenAccountState) -> do
                pltConfiguration <- BS.getTokenConfiguration @_ @_ @m bs tokenIndex
                let accountBalance =
                        TokenAmount
                            { taValue = tasBalance tokenAccountState,
                              taDecimals = _pltDecimals pltConfiguration
                            }
                tokenState <- BS.getMutableTokenState bs tokenIndex
                let ctx = QueryContext{qcTokenIndex = tokenIndex, qcBlockState = bs, qcTokenState = tokenState}
                accountState <- runQueryTNoFail (queryAccountState acc) ctx
                return
                    Token
                        { tokenId = _pltTokenId pltConfiguration,
                          tokenAccountState =
                            TokenAccountState
                                { balance = accountBalance,
                                  moduleAccountState = accountState
                                }
                        }
    SPLTStateV1 ->
        RustQ.queryTokenAccountInfos bs (fst acc)

-- | Get the list all tokens
queryPLTList ::
    forall m.
    (BS.BlockStateQuery m) =>
    BlockState m ->
    m [TokenId]
queryPLTList bs =
    case sPltStateVersionFor (protocolVersion @(MPV m)) of
        SPLTStateNone -> return []
        SPLTStateV0 -> BS.getPLTList bs
        SPLTStateV1 -> RustQ.queryPLTList bs

-- | An error that may occur as a result of 'queryLockInfo'.
data QueryLockModuleError
    = -- | The requested lock does not exist in the block.
      QLMEUnknownLock
    | -- | An internal error occurred during the lock query (currently unused; kept for parity
      --   with 'QueryTokenModuleError' so future failure modes can be reported without
      --   changing the public API).
      QLMEInternal
    | -- | The protocol version does not support the query.
      QLMEUnavailable
    deriving (Eq)

instance Show QueryLockModuleError where
    show QLMEUnknownLock = "unknown lock"
    show QLMEInternal = "internal error processing lock query"
    show QLMEUnavailable = "The information is not available for this block"

-- | Get the list of all PLT lock ids that exist in the given block. Pre-V1 PLT state versions
-- (including no PLT support) return the empty list, mirroring 'queryPLTList'. The returned ids
-- are forwarded verbatim from the Rust scheduler.
queryLockList ::
    forall m.
    (BS.BlockStateQuery m) =>
    BlockState m ->
    m [Locks.LockId]
queryLockList bs =
    case sPltStateVersionFor (protocolVersion @(MPV m)) of
        SPLTStateNone -> return []
        SPLTStateV0 -> return []
        SPLTStateV1 -> RustQ.queryLockList bs

-- | Get the CBOR-encoded `lock-info` payload for a given lock id, wrapped in the opaque
-- 'LockQueries.LockInfo' newtype. The Haskell layer treats the payload as opaque bytes —
-- it never parses, validates, or re-encodes the CBOR. Pre-V1 PLT state versions return
-- 'QLMEUnknownLock' (this matches the precedent set by 'queryTokenAuthorizations' for
-- the V0 / no-PLT cases).
queryLockInfo ::
    forall m.
    (BS.BlockStateQuery m) =>
    Locks.LockId ->
    BlockState m ->
    m (Either QueryLockModuleError LockQueries.LockInfo)
queryLockInfo lockId bs = case sPltStateVersionFor (protocolVersion @(MPV m)) of
    SPLTStateNone -> return (Left QLMEUnknownLock)
    SPLTStateV0 -> return (Left QLMEUnknownLock)
    SPLTStateV1 ->
        RustQ.queryLockInfo bs lockId <&> \case
            Just lockInfo -> Right lockInfo
            Nothing -> Left QLMEUnknownLock
