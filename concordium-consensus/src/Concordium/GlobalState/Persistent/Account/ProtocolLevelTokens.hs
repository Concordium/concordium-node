{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens where

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Serialize

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types.HashableTo
import Concordium.Types.Tokens
import Concordium.Utils.Serialization

import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.LFMBTree (hashAsLFMBTV1)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens

-- | The table of PLT account states. The table is indexed by the token index
--  into the global token table.
newtype TokenAccountStateTable = TokenAccountStateTable
    { tokenAccountStateTable :: Map.Map TokenIndex (HashedBufferedRef TokenAccountState)
    }
    deriving newtype (Show)

instance (MonadBlobStore m) => MHashableTo m TokenStateTableHash TokenAccountStateTable where
    getHashM (TokenAccountStateTable tast) = do
        hashes <-
            mapM
                ( \(tokIx, ref) -> do
                    h :: Hash.Hash <- getHashM ref
                    return (Hash.hashLazy . runPutLazy $ put tokIx >> put h)
                )
                $ Map.toAscList tast
        return $ TokenStateTableHash $ hashAsLFMBTV1 emptyTokenAccountStateTableHash hashes

instance (MonadBlobStore m) => BlobStorable m TokenAccountStateTable where
    storeUpdate (TokenAccountStateTable tast) = do
        storeUpdatedMap <- mapM storeUpdate tast
        let putter = do
                putLength (Map.size tast)
                void $ Map.traverseWithKey (\tokIx (p, _) -> put tokIx >> p) storeUpdatedMap
        return (putter, TokenAccountStateTable $ snd <$> storeUpdatedMap)

    load = do
        count <- getLength
        l <- replicateM count $ do
            tokIx <- get
            ref <- load
            return (tokIx, ref)
        return $ TokenAccountStateTable <$> sequenceA (Map.fromList l)

-- | The empty token account state table.
emptyTokenAccountStateTable :: TokenAccountStateTable
emptyTokenAccountStateTable = TokenAccountStateTable{tokenAccountStateTable = Map.empty}

-- | The empty token account state.
emptyTokenAccountState :: TokenAccountState
emptyTokenAccountState =
    TokenAccountState
        { tasBalance = TokenRawAmount 0
        }

-- | Token state at the account level
newtype TokenAccountState = TokenAccountState
    { -- | The available balance for the account.
      tasBalance :: TokenRawAmount
    }
    deriving (Eq, Show, Ord)

instance Serialize TokenAccountState where
    put TokenAccountState{..} = do
        put tasBalance
    get = do
        tasBalance <- get
        return TokenAccountState{..}

instance HashableTo Hash.Hash TokenAccountState where
    getHash = Hash.hashLazy . runPutLazy . put

instance (Monad m) => MHashableTo m Hash.Hash TokenAccountState

instance (MonadBlobStore m) => BlobStorable m TokenAccountState

-- | A change in a 'TokenRawAmount'.
newtype TokenAmountDelta = TokenAmountDelta {tokenAmountDelta :: Integer} deriving (Eq, Show)

-- | Convert a 'TokenRawAmount' to a positive 'TokenAmountDelta' corresponding to that amount.
toTokenAmountDelta :: TokenRawAmount -> TokenAmountDelta
toTokenAmountDelta = TokenAmountDelta . fromIntegral

-- | Convert a 'TokenRawAmount' to a positive 'TokenAmountDelta' corresponding to the deduction
--  of the given amount.
negativeTokenAmountDelta :: TokenRawAmount -> TokenAmountDelta
negativeTokenAmountDelta = TokenAmountDelta . negate . fromIntegral
