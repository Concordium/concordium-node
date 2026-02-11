{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- We suppress redundant constraint warnings since GHC does not detect when a constraint is used
-- for pattern matching. (See: https://gitlab.haskell.org/ghc/ghc/-/issues/20896)
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Implementation of protocol-level tokens block state. This is both the Haskell implementation (V0)
-- and the Rust implementation (V1). The latter is using bindings to the Rust library which implements the PLT block state.
module Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens where

import Control.Arrow
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Short as SBS
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Word

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.Genesis.Data
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.HashableTo
import Concordium.Types.Tokens
import Concordium.Utils

import Concordium.GlobalState.Basic.BlockState.LFMBTree (LFMBTreeHash' (..))
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens.RustPLTBlockState as RustBS
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBTree

-- | A token index is the index of a token in the 'ProtocolLevelTokens' table.
newtype TokenIndex = TokenIndex {theTokenIndex :: Word64}
    deriving newtype (Eq, Ord, Serialize, Show, Num, Real, Enum, Integral, Bounded, Bits)

instance (MonadBlobStore m) => BlobStorable m TokenIndex

instance HashableTo SHA256.Hash TokenIndex where
    getHash = SHA256.hash . runPut . put . theTokenIndex

-- | The configuration of a protocol-level token that is generally not expected to change.
data PLTConfiguration = PLTConfiguration
    { -- | The token ID.
      _pltTokenId :: !TokenId,
      -- | The token module reference.
      _pltModule :: !TokenModuleRef,
      -- | The number of decimal places used in the representation of the token.
      _pltDecimals :: !Word8
    }
    deriving (Eq, Show)

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
type TokenStateKey = BS.ByteString

-- | The type of values in the token state key-value map.
type TokenStateValue = BS.ByteString

-- | The state of a particular protocol-level token.
data PLT = PLT
    { -- | The token configuration.
      _pltConfiguration :: !(HashedBufferedRef' PLTConfigurationHash PLTConfiguration),
      -- | The token-level state of the PLT.
      _pltState :: !StateV1.PersistentState,
      -- | The total amount of the token that exists in circulation.
      _pltCirculatingSupply :: !TokenRawAmount
    }

instance (MonadBlobStore m) => BlobStorable m PLT where
    load = do
        configRef <- load
        stateRef <- load
        _pltCirculatingSupply <- get
        return $ do
            _pltConfiguration <- configRef
            _pltState <- stateRef
            return PLT{..}
    storeUpdate plt = do
        (putConfig, newConfig) <- storeUpdate (_pltConfiguration plt)
        (putState, newState) <- storeUpdate (_pltState plt)
        let thePutter = do
                putConfig
                putState
                put (_pltCirculatingSupply plt)
        return $!! (thePutter, plt{_pltConfiguration = newConfig, _pltState = newState})

instance (MonadBlobStore m) => Cacheable m PLT where
    cache plt = do
        cachedConfiguration <- cache (_pltConfiguration plt)
        return plt{_pltConfiguration = cachedConfiguration}

instance (MonadBlobStore m) => MHashableTo m SHA256.Hash PLT where
    getHashM PLT{..} = do
        (PLTConfigurationHash configHash) <- getHashM _pltConfiguration
        tokenStateHash :: SHA256.Hash <- getHashM _pltState
        let stateHash = SHA256.hashLazy $ runPutLazy $ do
                put tokenStateHash
                put _pltCirculatingSupply
        return $! SHA256.hashOfHashes configHash stateHash

type TokenRef = HashedBufferedRef PLT

-- Normalized version of 'TokenId'. Normalized here means with all letters capitalized.
-- The 'TokenIndex' is stored under a 'NormalizedTokenId', so when looking up a token, it is first
-- normalized, so that a token can be looked up by any capitalization.
newtype NormalizedTokenId = NormalizedTokenId BS.ByteString
    deriving (Eq, Ord)

-- Normalize a 'TokenId', making all letters upper case.
normalizeTokenId :: TokenId -> NormalizedTokenId
normalizeTokenId (TokenId tid) = NormalizedTokenId $ BSC.map toUpper $ SBS.fromShort tid

-- | The table holding the protocol level token state.
data ProtocolLevelTokens = ProtocolLevelTokens
    { -- | The table of PLTs.
      _pltTable :: !(LFMBTree.LFMBTree' TokenIndex HashedBufferedRef TokenRef),
      -- | A map from 'TokenId's to 'TokenIndex'es. This is constructed, rather than stored.
      -- TODO: In future it would likely make sense to handle this with a difference map and store
      -- the finalized map in the LMDB database. (As, for instance, for modules.)
      -- This is not necessary if the number of tokens remains small.
      _pltMap :: !(Map.Map NormalizedTokenId TokenIndex)
    }

instance (MonadBlobStore m) => BlobStorable m ProtocolLevelTokens where
    load = do
        loadTable <- load
        return $ do
            _pltTable <- loadTable
            -- We construct the map by simply iterating over the LFMBTree.
            let f (nxtIndex, m) v = do
                    config <- refLoad (_pltConfiguration v)
                    return $!! (nxtIndex + 1, Map.insert (normalizeTokenId (_pltTokenId config)) nxtIndex m)
            (_, _pltMap) <- LFMBTree.mfold f (0, Map.empty) _pltTable

            return ProtocolLevelTokens{..}
    storeUpdate plts = do
        (putTable, newTable) <- storeUpdate (_pltTable plts)
        return (putTable, plts{_pltTable = newTable})

instance (MonadBlobStore m) => Cacheable m ProtocolLevelTokens where
    cache ProtocolLevelTokens{..} = do
        pltTable' <- cache _pltTable
        return ProtocolLevelTokens{_pltTable = pltTable', ..}

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

-- | Protocol level tokens depending on PLT state version.
--
-- * 'PLTStateNone': No PLT state
-- * 'PLTStateV0': Managed in Haskell
-- * 'PLTStateV1': Managed in Rust
data ProtocolLevelTokensForStateVersion (pltsv :: PLTStateVersion) where
    ProtocolLevelTokensNone :: ProtocolLevelTokensForStateVersion 'PLTStateNone
    ProtocolLevelTokensV0 :: (HashedBufferedRef' ProtocolLevelTokensHash ProtocolLevelTokens) -> ProtocolLevelTokensForStateVersion 'PLTStateV0
    ProtocolLevelTokensV1 :: RustBS.ForeignPLTBlockStatePtr -> ProtocolLevelTokensForStateVersion 'PLTStateV1

instance (MonadBlobStore m, IsPLTStateVersion pltsv) => BlobStorable m (ProtocolLevelTokensForStateVersion pltsv) where
    load = case pltStateVersion @pltsv of
        SPLTStateNone -> return $ return ProtocolLevelTokensNone
        SPLTStateV0 -> fmap ProtocolLevelTokensV0 <$> load
        SPLTStateV1 -> fmap ProtocolLevelTokensV1 <$> load
    storeUpdate ProtocolLevelTokensNone = return (return (), ProtocolLevelTokensNone)
    storeUpdate (ProtocolLevelTokensV0 hbref) = second ProtocolLevelTokensV0 <$> storeUpdate hbref
    storeUpdate (ProtocolLevelTokensV1 fstate) = second ProtocolLevelTokensV1 <$> storeUpdate fstate

instance (MonadBlobStore m) => Cacheable m (ProtocolLevelTokensForStateVersion pltsv) where
    cache = \case
        ProtocolLevelTokensNone -> return ProtocolLevelTokensNone
        ProtocolLevelTokensV0 hbref -> ProtocolLevelTokensV0 <$> cache hbref
        ProtocolLevelTokensV1 fstate -> ProtocolLevelTokensV1 <$> cache fstate

instance
    (MonadBlobStore m, b ~ PltStatePresent (pltsv)) =>
    MHashableTo m (Conditionally b ProtocolLevelTokensHash) (ProtocolLevelTokensForStateVersion pltsv)
    where
    getHashM = \case
        ProtocolLevelTokensNone -> return CFalse
        ProtocolLevelTokensV0 hbref -> CTrue <$> getHashM hbref
        ProtocolLevelTokensV1 fstate -> CTrue <$> getHashM fstate

-- | Load a 'ProtocolLevelTokens' in the Haskell managed version of 'ProtocolLevelTokensForStateVersion'.
loadPLTs ::
    (MonadBlobStore m) =>
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m ProtocolLevelTokens
loadPLTs (ProtocolLevelTokensV0 hbref) = refLoad hbref

-- | Store a 'ProtocolLevelTokens' in the Haskell managed version of 'ProtocolLevelTokensForStateVersion'.
storePLTs ::
    (MonadBlobStore m) =>
    ProtocolLevelTokens ->
    m (ProtocolLevelTokensForStateVersion 'PLTStateV0)
storePLTs = fmap ProtocolLevelTokensV0 . refMake

-- | An empty 'ProtocolLevelTokensForStateVersion' with no tokens.
emptyProtocolLevelTokensForStateVersion ::
    forall m pltsv.
    (IsPLTStateVersion pltsv, MonadBlobStore m) =>
    m (ProtocolLevelTokensForStateVersion pltsv)
emptyProtocolLevelTokensForStateVersion = case pltStateVersion @pltsv of
    SPLTStateNone -> return $ ProtocolLevelTokensNone
    SPLTStateV0 -> storePLTs emptyProtocolLevelTokens
    SPLTStateV1 -> ProtocolLevelTokensV1 <$> RustBS.empty

-- | Get the list of all existing protocol-level tokens by their 'TokenId's.
--  This returns the empty list when the protocol version does not support PLTs.
--
-- This implementation is for the Haskell managed state version V0.
getPLTList :: (MonadBlobStore m) => ProtocolLevelTokensForStateVersion 'PLTStateV0 -> m [TokenId]
getPLTList pltsV0 = do
    plts <- loadPLTs pltsV0
    LFMBTree.mfold step [] (_pltTable plts)
  where
    step acc v = do
        tid <- _pltTokenId <$> refLoad (_pltConfiguration v)
        return (tid : acc)

-- | Get the 'TokenIndex' for a 'TokenId'. Returns @Nothing@ if there is no token with the given
-- 'TokenId'.
--
-- This implementation is for the Haskell managed state version V0.
getTokenIndex ::
    (MonadBlobStore m) =>
    TokenId ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m (Maybe TokenIndex)
getTokenIndex tokId = fmap (Map.lookup ntid . _pltMap) . loadPLTs
  where
    ntid = normalizeTokenId tokId

-- | Get the 'PLT' with the given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
lookupPLT ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m PLT
lookupPLT index pltsV0 = do
    plts <- loadPLTs pltsV0
    mPLT <- LFMBTree.lookup index (_pltTable plts)
    case mPLT of
        Just plt -> return plt
        Nothing ->
            error $ "lookupPLT: TokenIndex (" ++ show index ++ ") not found in ProtocolLevelTokens"

-- | Create a mutable state from a persistent one. This is generative, it creates independent
-- mutable states in different calls.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
getMutableTokenState ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m StateV1.MutableState
getMutableTokenState index pltsV0 = do
    plt <- lookupPLT index pltsV0
    loadCallback <- fst <$> getCallbacks
    liftIO $ StateV1.thaw loadCallback (_pltState plt)

-- | Convert the mutable state to a persistent one, setting it as the state of the provided token
-- index.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
setTokenState ::
    (MonadBlobStore m) =>
    TokenIndex ->
    StateV1.MutableState ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m (ProtocolLevelTokensForStateVersion 'PLTStateV0)
setTokenState index mutableState pltsV0 = do
    plts <- loadPLTs pltsV0
    LFMBTree.update upd index (_pltTable plts) >>= \case
        Nothing ->
            error $
                "setTokenState: TokenIndex (" ++ show index ++ ") not found in ProtocolLevelTokens"
        Just (_, newTable) -> storePLTs plts{_pltTable = newTable}
  where
    upd plt = do
        loadCallback <- fst <$> getCallbacks
        (_hash, persistentState) <- liftIO $ StateV1.freeze loadCallback mutableState
        return ((), plt{_pltState = persistentState})

-- | Get the state of a token for a given 'TokenStateKey'. Returns @Nothing@ if the token does not
--  have a state for the given key.
lookupTokenState ::
    (MonadBlobStore m) =>
    TokenStateKey ->
    StateV1.MutableState ->
    m (Maybe TokenStateValue)
lookupTokenState key mutableState = liftIO $ StateV1.lookupMutableState key mutableState

-- | Insert entry into the mutable state, overwriting the value if already present.
-- If the provided value is @Nothing@ the entry gets deleted.
--
-- Returns
-- * @Just True@ signals an entry was present in the state.
-- * @Just False@ if no entry was present.
-- * @Nothing@ signals error due to the entry being locked.
updateTokenState ::
    (MonadBlobStore m) =>
    TokenStateKey ->
    Maybe TokenStateValue ->
    StateV1.MutableState ->
    m (Maybe Bool)
updateTokenState key maybeValue mutableState =
    liftIO $ case maybeValue of
        Just value -> StateV1.insertMutableState key value mutableState
        Nothing -> StateV1.deleteEntryMutableState key mutableState

-- | Get the configuration data of a token for a given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
getTokenConfiguration ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m PLTConfiguration
getTokenConfiguration index pltsV0 = do
    plt <- lookupPLT index pltsV0
    refLoad (_pltConfiguration plt)

-- | Get the circulating supply of a token for a given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
getTokenCirculatingSupply ::
    (MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m TokenRawAmount
getTokenCirculatingSupply index pltsV0 = do
    plt <- lookupPLT index pltsV0
    return $ _pltCirculatingSupply plt

-- | Set the circulating supply of a token for a given 'TokenIndex'.
--  Returns the updated 'ProtocolLevelTokensForStateVersion'.
--
-- PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
setTokenCirculatingSupply ::
    (MonadBlobStore m) =>
    TokenIndex ->
    TokenRawAmount ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m (ProtocolLevelTokensForStateVersion 'PLTStateV0)
setTokenCirculatingSupply index newSupply pltsV0 = do
    plts <- loadPLTs pltsV0
    LFMBTree.update upd index (_pltTable plts) >>= \case
        Nothing ->
            error $
                "setTokenCirculatingSupply: TokenIndex ("
                    ++ show index
                    ++ ") not found in ProtocolLevelTokens"
        Just (_, newTable) -> storePLTs plts{_pltTable = newTable}
  where
    upd plt = return ((), plt{_pltCirculatingSupply = newSupply})

-- | Create a new token with the given configuration. The initial state will be empty and the
--  initial supply will be 0. Returns the token index and the updated 'ProtocolLevelTokensForStateVersion'.
--
--  PRECONDITION: The 'TokenId' of the given configuration MUST NOT already exist in the
--  'ProtocolLevelTokens'.
--
-- This implementation is for the Haskell managed state version V0.
createToken ::
    (MonadBlobStore m) =>
    PLTConfiguration ->
    ProtocolLevelTokensForStateVersion 'PLTStateV0 ->
    m (TokenIndex, ProtocolLevelTokensForStateVersion 'PLTStateV0)
createToken config pltsV0 = do
    plts <- loadPLTs pltsV0
    newConfigRef <- refMake config
    state <- liftIO StateV1.emptyPersistentState
    let plt =
            PLT
                { _pltConfiguration = newConfigRef,
                  _pltState = state,
                  _pltCirculatingSupply = TokenRawAmount 0
                }
    (tokIndex, newTable) <- LFMBTree.append plt (_pltTable plts)
    let newMap = Map.insert (normalizeTokenId (_pltTokenId config)) tokIndex (_pltMap plts)
    pltsV0' <- storePLTs ProtocolLevelTokens{_pltTable = newTable, _pltMap = newMap}
    return (tokIndex, pltsV0')

-- | Migrate 'ProtocolLevelTokens' unchanged.
migrateProtocolLevelTokens ::
    forall t m.
    (SupportMigration m t) =>
    ProtocolLevelTokens ->
    t m ProtocolLevelTokens
migrateProtocolLevelTokens ProtocolLevelTokens{..} = do
    newTable <- LFMBTree.migrateLFMBTree (migrateHashedBufferedRef migratePLT) _pltTable
    return ProtocolLevelTokens{_pltTable = newTable, _pltMap = _pltMap}
  where
    migratePLT PLT{..} = do
        newConfig <- migrateHashedBufferedRefKeepHash _pltConfiguration
        (oldLoadCallback, _) <- lift getCallbacks
        (_, newStoreCallback) <- getCallbacks
        newState <-
            liftIO $
                StateV1.migratePersistentState oldLoadCallback newStoreCallback _pltState
        return
            PLT
                { _pltConfiguration = newConfig,
                  _pltState = newState,
                  _pltCirculatingSupply = _pltCirculatingSupply
                }

-- | Migrate 'ProtocolLevelTokensForStateVersion'. Where the old protocol version did not support PLTs, and
--  the new protocol version does, this initializes the empty 'ProtocolLevelTokensForStateVersion'. Otherwise,
--  the PLTs are unchanged in the migration.
migrateProtocolLevelTokensForStateVersion ::
    forall t m.
    ( SupportMigration m t,
      MonadProtocolVersion m,
      MonadProtocolVersion (t m)
    ) =>
    StateMigrationParameters (MPV m) (MPV (t m)) ->
    ProtocolLevelTokensForStateVersion (PltStateVersionFor (MPV m)) ->
    t m (ProtocolLevelTokensForStateVersion (PltStateVersionFor (MPV (t m))))
migrateProtocolLevelTokensForStateVersion _ ProtocolLevelTokensNone = do
    -- When migrating from a version where there are no protocol-level tokens, we use the
    -- empty protocol level tokens (if the new state supports LTS).
    emptyProtocolLevelTokensForStateVersion
migrateProtocolLevelTokensForStateVersion migration oldPLTsV0@(ProtocolLevelTokensV0 _) =
    case pltStateVersion @(PltStateVersionFor (MPV (t m))) of
        SPLTStateNone -> case migration of {}
        SPLTStateV0 -> do
            oldPLTs <- lift $ loadPLTs oldPLTsV0
            newPLTs <- migrateProtocolLevelTokens oldPLTs
            storePLTs newPLTs
        -- todo implement P10 to P11 migration in https://linear.app/concordium/issue/COR-2218/implement-p10-to-p11-migration-for-plt-state
        SPLTStateV1 -> undefined
migrateProtocolLevelTokensForStateVersion migration (ProtocolLevelTokensV1 fstate) =
    case pltStateVersion @(PltStateVersionFor (MPV (t m))) of
        SPLTStateNone -> case migration of {}
        SPLTStateV0 -> case migration of {}
        SPLTStateV1 -> ProtocolLevelTokensV1 <$> RustBS.migrate fstate
