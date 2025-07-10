{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.Scheduler.ProtocolLevelTokens.Queries where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Bool.Singletons
import qualified Data.Map.Strict as Map
import Data.Void

import Concordium.Types
import Concordium.Types.Queries.Tokens

import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens (TokenAccountState (tasBalance))
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens (PLTConfiguration (..), TokenIndex)
import Concordium.GlobalState.Types
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.ProtocolLevelTokens.Module

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

instance (BS.BlockStateQuery m, PVSupportsPLT (MPV m)) => PLTKernelQuery (QueryT fail ret m) where
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

-- | An error that may occur as a result of 'queryTokenInfo'.
data QueryTokenInfoError
    = -- | The requested token does not exist in the block.
      QTIEUnknownToken
    | -- | An error occurred in the token module.
      QTIEInternal !QueryTokenError

instance Show QueryTokenInfoError where
    show QTIEUnknownToken = "unknown token"
    show (QTIEInternal e) = show e

-- | Get the 'TokenInfo' associated with a 'TokenId' in the given 'BlockState'.
queryTokenInfo ::
    forall m.
    (BS.BlockStateQuery m) =>
    TokenId ->
    BlockState m ->
    m (Either QueryTokenInfoError TokenInfo)
queryTokenInfo tokenId bs = case sSupportsPLT (accountVersion @(AccountVersionFor (MPV m))) of
    SFalse -> return (Left QTIEUnknownToken)
    STrue -> do
        mTokenIx <- BS.getTokenIndex bs tokenId
        case mTokenIx of
            Nothing -> return (Left QTIEUnknownToken)
            Just tokenIx -> do
                PLTConfiguration{..} <- BS.getTokenConfiguration bs tokenIx
                totalSupply <- BS.getTokenCirculatingSupply bs tokenIx
                tokenState <- BS.getMutableTokenState bs tokenIx
                let ctx = QueryContext{qcTokenIndex = tokenIx, qcBlockState = bs, qcTokenState = tokenState}
                runQueryT queryTokenModuleState ctx >>= \case
                    Left e -> return (Left (QTIEInternal e))
                    Right tms -> do
                        let ts =
                                TokenState
                                    { tsTokenModuleRef = _pltModule,
                                      tsDecimals = _pltDecimals,
                                      tsTotalSupply = toTokenAmount _pltDecimals totalSupply,
                                      tsModuleState = tms
                                    }
                        return $ Right TokenInfo{tiTokenId = tokenId, tiTokenState = ts}

-- | Get the list of 'Token's on an account.
queryAccountTokens :: forall m. (PVSupportsPLT (MPV m), BS.BlockStateQuery m) => IndexedAccount m -> BlockState m -> m [Token]
queryAccountTokens acc bs = do
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
