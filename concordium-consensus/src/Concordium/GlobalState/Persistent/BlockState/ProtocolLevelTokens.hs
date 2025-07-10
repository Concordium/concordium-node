{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens where

import Control.Monad.Trans.Class
import Data.Bits
import Data.Bool.Singletons
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
import qualified Concordium.GlobalState.Persistent.LFMBTree as LFMBTree
import Control.Monad.IO.Class (liftIO)

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
    deriving (Show)

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

newtype NormalizedTokenId = NormalizedTokenId BS.ByteString
    deriving (Eq, Ord)

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

-- | Protocol level tokens where supported by the protocol version.
--  The 'ProtocolLevelTokens' structure is stored under a 'HashedBufferedRef''.
newtype ProtocolLevelTokensForPV (pv :: ProtocolVersion) = ProtocolLevelTokensForPV
    { theProtocolLevelTokensForPV ::
        (Conditionally (SupportsPLT (AccountVersionFor pv)))
            (HashedBufferedRef' ProtocolLevelTokensHash ProtocolLevelTokens)
    }

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (ProtocolLevelTokensForPV pv) where
    load = case sSupportsPLT (accountVersion @(AccountVersionFor pv)) of
        SFalse -> return (return (ProtocolLevelTokensForPV CFalse))
        STrue -> fmap (ProtocolLevelTokensForPV . CTrue) <$> load
    storeUpdate pltPV@(ProtocolLevelTokensForPV CFalse) = do
        return (return (), pltPV)
    storeUpdate (ProtocolLevelTokensForPV (CTrue pltRef)) = do
        (ppltRef, pltRef') <- storeUpdate pltRef
        return (ppltRef, ProtocolLevelTokensForPV (CTrue pltRef'))

instance
    (MonadBlobStore m, b ~ SupportsPLT (AccountVersionFor pv)) =>
    MHashableTo m (Conditionally b ProtocolLevelTokensHash) (ProtocolLevelTokensForPV pv)
    where
    getHashM (ProtocolLevelTokensForPV CFalse) = return CFalse
    getHashM (ProtocolLevelTokensForPV (CTrue ref)) = CTrue <$> getHashM ref

instance
    (MonadBlobStore m, PVSupportsPLT pv) =>
    MHashableTo m ProtocolLevelTokensHash (ProtocolLevelTokensForPV pv)
    where
    getHashM (ProtocolLevelTokensForPV (CTrue ref)) = getHashM ref

instance (MonadBlobStore m) => Cacheable m (ProtocolLevelTokensForPV pv) where
    cache (ProtocolLevelTokensForPV (CTrue pltsRef)) =
        ProtocolLevelTokensForPV . CTrue <$> cache pltsRef
    cache pvPLTs = pure pvPLTs

-- | Load a 'ProtocolLevelTokens' from a 'ProtocolLevelTokensForPV'.
loadPLTs ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    ProtocolLevelTokensForPV pv ->
    m ProtocolLevelTokens
loadPLTs = refLoad . uncond . theProtocolLevelTokensForPV

-- | Store a 'ProtocolLevelTokens' in a 'ProtocolLevelTokensForPV'.
storePLTs ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    ProtocolLevelTokens ->
    m (ProtocolLevelTokensForPV pv)
storePLTs = fmap (ProtocolLevelTokensForPV . CTrue) . refMake

-- | Store a 'ProtocolLevelTokens' in a 'ProtocolLevelTokensForPV' if the protocol version supports
--  protocol-level tokens.
conditionallyStorePLTs ::
    forall m pv.
    (IsProtocolVersion pv, MonadBlobStore m) =>
    ProtocolLevelTokens ->
    m (ProtocolLevelTokensForPV pv)
conditionallyStorePLTs = case sSupportsPLT (accountVersion @(AccountVersionFor pv)) of
    STrue -> storePLTs
    SFalse -> const (return $ ProtocolLevelTokensForPV CFalse)

-- | An empty 'ProtocolLevelTokensForPV' with no tokens.
emptyProtocolLevelTokensForPV ::
    (IsProtocolVersion pv, MonadBlobStore m) =>
    m (ProtocolLevelTokensForPV pv)
emptyProtocolLevelTokensForPV = conditionallyStorePLTs emptyProtocolLevelTokens

-- | Get the list of all existing protocol-level tokens by their 'TokenId's.
--  This returns the empty list when the protocol version does not support PLTs.
getPLTList :: (MonadBlobStore m) => ProtocolLevelTokensForPV pv -> m [TokenId]
getPLTList (ProtocolLevelTokensForPV CFalse) = return []
getPLTList (ProtocolLevelTokensForPV (CTrue plts)) = do
    table <- _pltTable <$> refLoad plts
    LFMBTree.mfold step [] table
  where
    step acc v = do
        tid <- _pltTokenId <$> refLoad (_pltConfiguration v)
        return (tid : acc)

-- | Get the 'TokenIndex' for a 'TokenId'. Returns @Nothing@ if there is no token with the given
-- 'TokenId'.
getTokenIndex ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenId ->
    ProtocolLevelTokensForPV pv ->
    m (Maybe TokenIndex)
getTokenIndex tokId = fmap (Map.lookup ntid . _pltMap) . loadPLTs
  where
    ntid = normalizeTokenId tokId

-- | Get the 'PLT' with the given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
lookupPLT ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForPV pv ->
    m PLT
lookupPLT index pvPLTs = do
    plts <- loadPLTs pvPLTs
    mPLT <- LFMBTree.lookup index (_pltTable plts)
    case mPLT of
        Just plt -> return plt
        Nothing ->
            error $ "lookupPLT: TokenIndex (" ++ show index ++ ") not found in ProtocolLevelTokens"

-- | Create a mutable state from a persistent one. This is generative, it creates independent
-- mutable states in different calls.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
getMutableTokenState ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForPV pv ->
    m StateV1.MutableState
getMutableTokenState index pvPLTs = do
    plt <- lookupPLT index pvPLTs
    loadCallback <- fst <$> getCallbacks
    liftIO $ StateV1.thaw loadCallback (_pltState plt)

-- | Convert the mutable state to a persistent one, setting it as the state of the provided token
-- index.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
setTokenState ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    StateV1.MutableState ->
    ProtocolLevelTokensForPV pv ->
    m (ProtocolLevelTokensForPV pv)
setTokenState index mutableState pvPLTs = do
    plts <- loadPLTs pvPLTs
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
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
getTokenConfiguration ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForPV pv ->
    m PLTConfiguration
getTokenConfiguration index pvPLTs = do
    plt <- lookupPLT index pvPLTs
    refLoad (_pltConfiguration plt)

-- | Get the circulating supply of a token for a given 'TokenIndex'.
--
--  PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
getTokenCirculatingSupply ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    ProtocolLevelTokensForPV pv ->
    m TokenRawAmount
getTokenCirculatingSupply index pvPLTs = do
    plt <- lookupPLT index pvPLTs
    return $ _pltCirculatingSupply plt

-- | Set the circulating supply of a token for a given 'TokenIndex'.
--  Returns the updated 'ProtocolLevelTokensForPV'.
--
-- PRECONDITION: The 'TokenIndex' MUST exist in the given 'ProtocolLevelTokensForPV'.
setTokenCirculatingSupply ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    TokenIndex ->
    TokenRawAmount ->
    ProtocolLevelTokensForPV pv ->
    m (ProtocolLevelTokensForPV pv)
setTokenCirculatingSupply index newSupply pvPLTs = do
    plts <- loadPLTs pvPLTs
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
--  initial supply will be 0. Returns the token index and the updated 'ProtocolLevelTokensForPV'.
--
--  PRECONDITION: The 'TokenId' of the given configuration MUST NOT already exist in the
--  'ProtocolLevelTokensForPV'.
createToken ::
    (PVSupportsPLT pv, MonadBlobStore m) =>
    PLTConfiguration ->
    ProtocolLevelTokensForPV pv ->
    m (TokenIndex, ProtocolLevelTokensForPV pv)
createToken config pvPLTs = do
    plts <- loadPLTs pvPLTs
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
    pvPLTs' <- storePLTs ProtocolLevelTokens{_pltTable = newTable, _pltMap = newMap}
    return (tokIndex, pvPLTs')

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
        return PLT{_pltConfiguration = newConfig, ..}

-- | Migrate 'ProtocolLevelTokensForPV'. Where the old protocol version did not support PLTs, and
--  the new protocol version does, this initializes the empty 'ProtocolLevelTokens'. Otherwise,
--  the PLTs are unchanged in the migration.
migrateProtocolLevelTokensForPV ::
    forall t m.
    ( SupportMigration m t,
      MonadProtocolVersion (t m)
    ) =>
    StateMigrationParameters (MPV m) (MPV (t m)) ->
    ProtocolLevelTokensForPV (MPV m) ->
    t m (ProtocolLevelTokensForPV (MPV (t m)))
migrateProtocolLevelTokensForPV _ (ProtocolLevelTokensForPV CFalse) = do
    -- When migrating from a version where there are no protocol-level tokens, we use the
    -- empty protocol level tokens (if the new state supports LTS).
    emptyProtocolLevelTokensForPV
migrateProtocolLevelTokensForPV migration oldPLTsPV@(ProtocolLevelTokensForPV (CTrue _)) =
    case sSupportsPLT (accountVersion @(AccountVersionFor (MPV (t m)))) of
        STrue -> do
            oldPLTs <- lift $ loadPLTs oldPLTsPV
            newPLTs <- migrateProtocolLevelTokens oldPLTs
            storePLTs newPLTs
        SFalse ->
            -- There are no migrations that remove PLTs altogether.
            case migration of {}
