{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens where

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.LFMBTree (hashAsLFMBTV1)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Serialize

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
        { tasBalance = TokenRawAmount 0,
          tasModuleState = Map.empty
        }

-- | Token state at the account level
data TokenAccountState = TokenAccountState
    { -- | The available balance for the account.
      tasBalance :: !TokenRawAmount,
      -- | The token module state for the account, represented as a key-value map.
      tasModuleState :: !(Map.Map TokenStateKey TokenStateValue)
    }
    deriving (Eq, Show, Ord)

instance Serialize TokenAccountState where
    put TokenAccountState{..} = do
        put tasBalance
        put tasModuleState
    get = do
        tasBalance <- get
        tasModuleState <- get
        return TokenAccountState{..}

instance HashableTo Hash.Hash TokenAccountState where
    getHash = Hash.hashLazy . runPutLazy . put

instance (Monad m) => MHashableTo m Hash.Hash TokenAccountState

instance (MonadBlobStore m) => BlobStorable m TokenAccountState

-- | An update to the token account state.
data TokenAccountStateDelta = TokenAccountStateDelta
    { -- | A change to the token balance.
      tasBalanceDelta :: !(Maybe TokenAmountDelta),
      -- | A change to the token module state.
      tasModuleStateDelta :: ![(TokenStateKey, TokenAccountStateValueDelta)]
    }
    deriving (Eq, Show)

-- | A change in a 'TokenRawAmount'.
newtype TokenAmountDelta = TokenAmountDelta {tokenAmountDelta :: Integer} deriving (Eq, Show)

-- | Convert a 'TokenRawAmount' to a positive 'TokenAmountDelta' corresponding to that amount.
toTokenAmountDelta :: TokenRawAmount -> TokenAmountDelta
toTokenAmountDelta = TokenAmountDelta . fromIntegral

-- | Convert a 'TokenRawAmount' to a positive 'TokenAmountDelta' corresponding to the deduction
--  of the given amount.
negativeTokenAmountDelta :: TokenRawAmount -> TokenAmountDelta
negativeTokenAmountDelta = TokenAmountDelta . negate . fromIntegral

-- | The possible update actions of a token module state.
data TokenAccountStateValueDelta
    = -- | Delete the state.
      TASVDelete
    | -- | Update the state to a new value or create it if it doesn't already exist.
      TASVUpdate TokenStateValue
    deriving (Eq, Show)
