{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.Scheduler.ProtocolLevelTokens.Queries where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.Reader
import Data.Bool.Singletons
import Data.Functor
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
      qcBlockState :: !(BlockState m)
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

instance MonadTrans (QueryT fail ret) where
    lift = QueryT . lift . lift

instance (BS.BlockStateQuery m, PVSupportsPLT (MPV m)) => PLTKernelQuery (QueryT fail ret m) where
    type PLTAccount (QueryT fail ret m) = IndexedAccount m
    getTokenState key = do
        QueryContext{..} <- ask
        lift $ BS.getTokenState qcBlockState qcTokenIndex key
    getAccount addr = do
        QueryContext{..} <- ask
        lift $ BS.getAccount qcBlockState addr
    getAccountBalance acct = do
        QueryContext{..} <- ask
        lift $ BS.getAccountTokenBalance (snd acct) qcTokenIndex
    getAccountState acct key = do
        QueryContext{..} <- ask
        lift $ BS.getAccountTokenState (snd acct) qcTokenIndex key
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
                let ctx = QueryContext{qcTokenIndex = tokenIx, qcBlockState = bs}
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

queryAccountTokens :: forall m. (PVSupportsPLT (MPV m), BS.BlockStateQuery m) => IndexedAccount m -> BlockState m -> m [Token]
queryAccountTokens acc bs = do
    tokenStatesMap <- BS.getAccountTokens (snd acc)
    forM (Map.toList tokenStatesMap) $ \(tokenIndex, tokenState) -> do
        pltConfiguration <- BS.getTokenConfiguration @_ @m bs tokenIndex
        let accountBalance =
                TokenAmount
                    { taValue = tasBalance tokenState,
                      taDecimals = _pltDecimals pltConfiguration
                    }
        let ctx = QueryContext{qcTokenIndex = tokenIndex, qcBlockState = bs}
        (isAllow, isDeny) <-
            runQueryT @_ @Void (queryAccountListStatus acc) ctx <&> \case
                Right (isAllow, isDeny) -> (isAllow, isDeny)
                Left v -> case v of {}
        return
            Token
                { tokenId = _pltTokenId pltConfiguration,
                  tokenAccountState =
                    TokenAccountState
                        { balance = accountBalance,
                          -- TODO Support allow/deny list state in account info query (Issue https://linear.app/concordium/issue/COR-1349)
                          memberAllowList = isAllow,
                          memberDenyList = isDeny
                        }
                }
