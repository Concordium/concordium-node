{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Utils
import Concordium.Utils.Serialization

import Concordium.GlobalState.Basic.BlockState.LFMBTree (LFMBTreeHash' (..))
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBTree

-- | Represents the raw amount of a token. This is the amount of the token in its smallest unit.
newtype TokenRawAmount = TokenRawAmount {theTokenRawAmount :: Word64}
    deriving newtype (Eq, Ord, Show, Num, Real)

-- | Serialization of 'TokenRawAmount' is as a variable length quantity (VLQ). We disallow
--  0-padding to enforce canonical serialization.
--
--  The VLQ encoding represents a value in big-endian base 128. Each byte of the encoding uses
--  the high-order bit to indicate if further bytes follow (when set). The remaining bits represent
--  the positional value in base 128.  See https://en.wikipedia.org/wiki/Variable-length_quantity
instance Serialize TokenRawAmount where
    put (TokenRawAmount amt) = do
        mapM_ putWord8 (chunk amt [])
      where
        chunk num []
            | num == 0 = [0]
            | otherwise = chunk (num `shiftR` 7) [fromIntegral $ num .&. 0x7f]
        chunk num l
            | num == 0 = l
            | otherwise = chunk (num `shiftR` 7) (fromIntegral (0x80 .|. (num .&. 0x7f)) : l)
    get = TokenRawAmount <$> loop 0
      where
        loop accum = do
            b <- getWord8
            when (b == 0x80 && accum == 0) $
                fail "Padding bytes are not allowed"
            -- The following test ensures that @accum * 128 <= maxBound@, i.e. the shift in
            -- computing @accum'@ will not overflow.
            when (accum > maxBound `shiftR` 7) $
                fail "Value out of range"
            let accum' = accum `shiftL` 7 .|. fromIntegral (b .&. 0x7f)
            if testBit b 7
                then loop accum'
                else return accum'

instance (MonadBlobStore m) => BlobStorable m TokenRawAmount

-- | A token index is the index of a token in the 'ProtocolLevelTokens' table.
newtype TokenIndex = TokenIndex {theTokenIndex :: Word64}
    deriving newtype (Eq, Ord, Serialize, Show, Num, Real, Enum, Integral, Bounded, Bits)

-- | A hash that identifies the specific implementation to use for a token.
newtype TokenModuleRef = TokenModuleRef {theTokenModuleRef :: SHA256.Hash}
    deriving newtype (Eq, Ord, Serialize, Show)

-- | The configuration of a protocol-level token that is generally not expected to change.
data PLTConfiguration = PLTConfiguration
    { -- | The token ID.
      _pltTokenId :: !TokenId,
      -- | The token module reference.
      _pltModule :: !TokenModuleRef,
      -- | The number of decimal places used in the representation of the token.
      _pltDecimals :: !Word8
    }
    deriving (Eq, Ord, Show)

instance Serialize PLTConfiguration where
    put PLTConfiguration{..} = do
        put _pltTokenId
        put _pltModule
        put _pltDecimals
    get = do
        _pltTokenId <- get
        _pltModule <- get
        _pltDecimals <- get
        return PLTConfiguration{..}

instance (MonadBlobStore m) => BlobStorable m PLTConfiguration

instance (MonadBlobStore m) => Cacheable m PLTConfiguration

-- | The hash of a 'PLTConfiguration'.
newtype PLTConfigurationHash = PLTConfigurationHash SHA256.Hash
    deriving newtype (Eq, Ord, Show, Serialize)

instance HashableTo PLTConfigurationHash PLTConfiguration where
    getHash = PLTConfigurationHash . SHA256.hashLazy . runPutLazy . put

instance (Monad m) => MHashableTo m PLTConfigurationHash PLTConfiguration

-- | The type of keys in the token state key-value map.
type TokenStateKey = SBS.ShortByteString

-- | The type of values in the token state key-value map.
type TokenStateValue = BS.ByteString

-- | The state of a particular protocol-level token.
data PLT = PLT
    { -- | The token configuration.
      _pltConfiguration :: !(HashedBufferedRef' PLTConfigurationHash PLTConfiguration),
      -- | The token-level state of the PLT.
      -- TODO: Replace with trie-based state. https://linear.app/concordium/issue/NOD-700/switch-plt-key-value-maps-to-trie-implementation
      _pltState :: !(Map.Map TokenStateKey TokenStateValue),
      -- | The total amount of the token that exists in circulation.
      _pltCirculatingSupply :: !TokenRawAmount
    }

instance (MonadBlobStore m) => BlobStorable m PLT where
    load = do
        configRef <- load
        _pltState <- getSafeMapOf get get
        _pltCirculatingSupply <- get
        return $ do
            _pltConfiguration <- configRef
            return PLT{..}
    storeUpdate plt = do
        (putConfig, newConfig) <- storeUpdate (_pltConfiguration plt)
        let thePutter = do
                putConfig
                putSafeMapOf put put (_pltState plt)
                put (_pltCirculatingSupply plt)
        return $!! (thePutter, plt{_pltConfiguration = newConfig})

instance (MonadBlobStore m) => Cacheable m PLT where
    cache plt = do
        cachedConfiguration <- cache (_pltConfiguration plt)
        return plt{_pltConfiguration = cachedConfiguration}

instance (MonadBlobStore m) => MHashableTo m SHA256.Hash PLT where
    getHashM PLT{..} = do
        (PLTConfigurationHash configHash) <- getHashM _pltConfiguration

        let stateHash = SHA256.hashLazy $ runPutLazy $ do
                putSafeMapOf put put _pltState
                put _pltCirculatingSupply

        return $! SHA256.hashOfHashes configHash stateHash

type TokenRef = HashedBufferedRef PLT

-- | The table holding the protocol level token state.
data ProtocolLevelTokens = ProtocolLevelTokens
    { -- | The table of PLTs.
      _pltTable :: !(LFMBTree.LFMBTree' TokenIndex HashedBufferedRef TokenRef),
      -- | A map from 'TokenId's to 'TokenIndex'es. This is constructed, rather than stored.
      -- TODO: In future it would likely make sense to handle this with a difference map and store
      -- the finalized map in the LMDB database. (As, for instance, for modules.)
      -- This is not necessary if the number of tokens remains small.
      _pltMap :: !(Map.Map TokenId TokenIndex)
    }

instance (MonadBlobStore m) => BlobStorable m ProtocolLevelTokens where
    load = do
        loadTable <- load
        return $ do
            _pltTable <- loadTable
            -- We construct the map by simply iterating over the LFMBTree.
            let f (nxtIndex, m) v = do
                    config <- refLoad (_pltConfiguration v)
                    return $!! (nxtIndex + 1, Map.insert (_pltTokenId config) nxtIndex m)
            (_, _pltMap) <- LFMBTree.mfold f (0, Map.empty) _pltTable

            return ProtocolLevelTokens{..}
    storeUpdate plts = do
        (putTable, newTable) <- storeUpdate (_pltTable plts)
        return (putTable, plts{_pltTable = newTable})

instance (MonadBlobStore m) => Cacheable m ProtocolLevelTokens where
    cache ProtocolLevelTokens{..} = do
        pltTable' <- cache _pltTable
        return ProtocolLevelTokens{_pltTable = pltTable', ..}

-- | The hash of a 'ProtocolLevelTokens'. This is the hash of the LFMBTree holding the
--  'PLT's. The hash is computed using the 'BlockHashVersion1' algorithm.
newtype ProtocolLevelTokensHash = ProtocolLevelTokensHash {theProtocolLevelTokensHash :: SHA256.Hash}
    deriving newtype (Eq, Ord, Show, Serialize)

instance (MonadBlobStore m) => MHashableTo m ProtocolLevelTokensHash ProtocolLevelTokens where
    getHashM ProtocolLevelTokens{..} =
        ProtocolLevelTokensHash . theLFMBTreeHash @BlockHashVersion1
            <$> getHashM _pltTable

-- | An empty 'ProtocolLevelTokens' structure.
emptyProtocolLevelTokens :: ProtocolLevelTokens
emptyProtocolLevelTokens =
    ProtocolLevelTokens
        { _pltTable = LFMBTree.empty,
          _pltMap = Map.empty
        }

-- | Get the 'TokenIndex' for a 'TokenId'. Returns @Nothing@ if there is no token with the given
-- 'TokenId'.
getTokenIndex :: (MonadBlobStore m) => TokenId -> ProtocolLevelTokens -> m (Maybe TokenIndex)
getTokenIndex tokId plts = return $ Map.lookup tokId (_pltMap plts)

-- | Get the 'PLT' with the given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
lookupPLT :: (MonadBlobStore m) => TokenIndex -> ProtocolLevelTokens -> m PLT
lookupPLT index plts = do
    mPLT <- LFMBTree.lookup index (_pltTable plts)
    case mPLT of
        Just plt -> return plt
        Nothing ->
            error $ "lookupPLT: TokenIndex (" ++ show index ++ ") not found in ProtocolLevelTokens"

-- | Get the state of a token for a given 'TokenStateKey'. Returns @Nothing@ if the token does not
--  have a state for the given key.
--
-- PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
getTokenState ::
    (MonadBlobStore m) =>
    TokenIndex ->
    TokenStateKey ->
    ProtocolLevelTokens ->
    m (Maybe TokenStateValue)
getTokenState index key plts = do
    plt <- lookupPLT index plts
    return $ Map.lookup key (_pltState plt)

-- | Set the state of a token for a given 'TokenStateKey'. If the value is @Nothing@, the key is
--  removed from the token state. Otherwise, the key is set to the given value.
--  Returns the updated 'ProtocolLevelTokens'.
--
-- PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
setTokenState ::
    (MonadBlobStore m) =>
    TokenIndex ->
    TokenStateKey ->
    Maybe TokenStateValue ->
    ProtocolLevelTokens ->
    m ProtocolLevelTokens
setTokenState index key mValue plts = do
    LFMBTree.update upd index (_pltTable plts) >>= \case
        Nothing ->
            error $
                "setTokenState: TokenIndex (" ++ show index ++ ") not found in ProtocolLevelTokens"
        Just (_, newTable) -> return plts{_pltTable = newTable}
  where
    upd plt = do
        let !newState = case mValue of
                Nothing -> Map.delete key (_pltState plt)
                Just v -> Map.insert key v (_pltState plt)
        return ((), plt{_pltState = newState})

-- | Get the configuration data of a token for a given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
getTokenConfiguration ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokens ->
    m PLTConfiguration
getTokenConfiguration index plts = do
    plt <- lookupPLT index plts
    refLoad (_pltConfiguration plt)

-- | Get the circulating supply of a token for a given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
getTokenCirculatingSupply ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokens ->
    m TokenRawAmount
getTokenCirculatingSupply index plts = do
    plt <- lookupPLT index plts
    return $ _pltCirculatingSupply plt

-- | Set the circulating supply of a token for a given 'TokenIndex'.
--  Returns the updated 'ProtocolLevelTokens'.
--
-- PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
setTokenCirculatingSupply ::
    (MonadBlobStore m) =>
    TokenIndex ->
    TokenRawAmount ->
    ProtocolLevelTokens ->
    m ProtocolLevelTokens
setTokenCirculatingSupply index newSupply plts = do
    LFMBTree.update upd index (_pltTable plts) >>= \case
        Nothing ->
            error $
                "setTokenCirculatingSupply: TokenIndex ("
                    ++ show index
                    ++ ") not found in ProtocolLevelTokens"
        Just (_, newTable) -> return plts{_pltTable = newTable}
  where
    upd plt = return ((), plt{_pltCirculatingSupply = newSupply})

-- | Create a new token with the given configuration. The initial state will be empty and the
--  initial supply will be 0. Returns the token index and the updated 'ProtocolLevelTokens'.
--
--  PRECONDITION: The 'TokenId' of the given configuration MUST NOT already exist in the
--  'ProtocolLevelTokens'.
createToken ::
    (MonadBlobStore m) =>
    PLTConfiguration ->
    ProtocolLevelTokens ->
    m (TokenIndex, ProtocolLevelTokens)
createToken config plts = do
    newConfigRef <- refMake config
    let plt =
            PLT
                { _pltConfiguration = newConfigRef,
                  _pltState = Map.empty,
                  _pltCirculatingSupply = TokenRawAmount 0
                }
    (tokIndex, newTable) <- LFMBTree.append plt (_pltTable plts)
    let newMap = Map.insert (_pltTokenId config) tokIndex (_pltMap plts)
    return (tokIndex, ProtocolLevelTokens{_pltTable = newTable, _pltMap = newMap})
