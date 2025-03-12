{-# LANGUAGE BangPatterns #-}

-- | The 'DifferenceMap' stores accounts or modules that have been created in a non-finalized block.
--  When a block is finalized then the associated 'DifferenceMap' must be written
--  to disk via 'Concordium.GlobalState.AccountMap.LMDB.insertAccounts' or
--  'Concordium.GlobalState.AccountMap.ModuleMap.insertModules'.
--
--  The difference map is never written to the blob store. Typically, the blob store contains
--  finalized blocks, and so the difference map for such blocks will be empty (i.e. the
--  'DifferenceMapReference' will hold the value 'Absent'). Certified blocks, however, may
--  have a non-empty difference map and be written to disk. It is thus important that after loading
--  a certified block that the difference map is correctly reconstructed.
module Concordium.GlobalState.AccountMap.DifferenceMap (
    -- * Definitions

    -- The difference map definition.
    DifferenceMap (..),
    -- A mutable reference to a 'DifferenceMap'.
    DifferenceMapReference,

    -- * Auxiliary functions
    newEmptyReference,
    flatten,
    empty,
    newChildReference,
    lookup,
    refLookup,
    insertFresh,
    refInsertFresh,
    fromList,
    clearReferences,

    -- * Account specific
    AccountDifferenceMap,
    AccountDifferenceMapReference,
    lookupAccountEquiv,
    lookupAccountExact,
    flattenAccounts,
    insertFreshAccount,
    fromAccountList,
) where

import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.IORef
import Data.Tuple (swap)
import Data.Word
import Prelude hiding (lookup)

import Concordium.Types
import Concordium.Types.Option (Option (..))

-- | A mutable reference to a 'DiffMap.DifferenceMap'.
--  This is an 'IORef' since the parent map may belong
--  to multiple blocks if they have not yet been persisted.
--
--  The 'IORef' enables us to clear any child difference maps
--  when a block is finalized.
type DifferenceMapReference k v = IORef (Option (DifferenceMap k v))

-- | Create a new empty reference.
newEmptyReference :: (MonadIO m) => m (DifferenceMapReference k v)
newEmptyReference = liftIO $ newIORef Absent

-- | A difference map that indicates newly added map entries for
--  a block that has not yet been finalized.
--  @dmMap@ only contains entries that were added since the 'dmParentMapRef'.
data DifferenceMap k v = DifferenceMap
    { -- | The map of entries added in the current block.
      dmMap :: !(HM.HashMap k v),
      -- | The size of 'dmMap'. This is maintained as computing @HM.size@ is O(n).
      --
      --  prop> dmMapSize = HM.size dmMap
      dmMapSize :: !Word64,
      -- | Parent map of non-finalized blocks.
      --  In other words, if the parent block is finalized,
      --  then the parent map is @Absent@ as the persistent (LMDB) map should be consulted instead.
      dmParentMapRef :: !(DifferenceMapReference k v)
    }
    deriving (Eq)

-- | An updatable reference to an account difference map.
type AccountDifferenceMapReference = DifferenceMapReference AccountAddressEq (AccountIndex, AccountAddress)

-- | The difference map for accounts maps account addresses (modulo equivalence) to account indices
--  and the canonical account address.
type AccountDifferenceMap = DifferenceMap AccountAddressEq (AccountIndex, AccountAddress)

-- | Gather all entries from the provided 'DifferenceMap' and its parent maps.
--
--  Note. This function does not guarantee the order of the returned pairs.
flatten :: (MonadIO m) => DifferenceMap k v -> m [(k, v)]
flatten dmap = go dmap []
  where
    go diffMap !accum = do
        mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
        case mParentMap of
            Absent -> return collectedAccounts
            Present parentMap -> go parentMap collectedAccounts
      where
        collectedAccounts = HM.toList (dmMap diffMap) <> accum

-- | Gather all accounts from the provided 'AccountDifferenceMap' and its parent maps.
--
--  Note. This function does not guarantee the order of the returned pairs.
flattenAccounts :: (MonadIO m) => AccountDifferenceMap -> m [(AccountAddress, AccountIndex)]
flattenAccounts = fmap (map (swap . snd)) . flatten

-- | Create a new empty 'DifferenceMap' potentially based on the difference map of
-- the parent.
empty :: DifferenceMapReference k v -> DifferenceMap k v
empty mParentDifferenceMap =
    DifferenceMap
        { dmMap = HM.empty,
          dmMapSize = 0,
          dmParentMapRef = mParentDifferenceMap
        }

-- | Create a new 'DifferenceMapReference' that is a child of the provided difference map reference.
--  The new child difference map is empty, but inherits via reference from the parent
--  difference map.
newChildReference :: (MonadIO m) => DifferenceMapReference k v -> m (DifferenceMapReference k v)
newChildReference = liftIO . newIORef . Present . empty

-- | Lookup an entry in the difference map or any of the parent difference maps.
--  Returns @Right v@ if the account could be looked up, and otherwise @Left n@,
--  where @n@ is the number of entries in the difference map and any parent difference maps.
lookup :: (MonadIO m, Hashable k) => k -> DifferenceMap k v -> m (Either Word64 v)
lookup addr = check 0
  where
    check !accum diffMap = case HM.lookup addr (dmMap diffMap) of
        Nothing -> do
            mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
            let !accum' = accum + dmMapSize diffMap
            case mParentMap of
                Absent -> return $ Left accum'
                Present parentMap -> check accum' parentMap
        Just res -> return $ Right res

-- | Lookup an entry in the difference map reference or any of the parent difference maps.
--  Returns @Right v@ if the account could be looked up, and otherwise @Left n@,
--  where @n@ is the number of entries in the difference map and any parent difference maps.
refLookup :: (MonadIO m, Hashable k) => k -> DifferenceMapReference k v -> m (Either Word64 v)
refLookup addr ref = do
    oDiffMap <- liftIO $ readIORef ref
    case oDiffMap of
        Absent -> return $ Left 0
        Present diffMap -> lookup addr diffMap

-- | Lookup an account in the difference map or any of the parent
--  difference maps using the account address equivalence class.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Left Word64@ indicating how many accounts there are present in the
--  difference map(s).
--  Precondition: As this implementation uses the 'AccountAddressEq' equivalence
--  class for looking up an 'AccountIndex', then it MUST only be used
--  when account aliases are supported.
lookupAccountEquiv :: (MonadIO m) => AccountAddressEq -> AccountDifferenceMap -> m (Either Word64 AccountIndex)
lookupAccountEquiv addr dm = fmap fst <$> lookup addr dm

-- | Lookup an account in the difference map or any of the parent
--  difference maps via an exactness check.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Left Word64@ indicating how many accounts there are present in the
--  difference map(s).
--  Note that this function also returns @Nothing@ if the provided 'AccountAddress'
--  is an alias but not the canonical address.
lookupAccountExact :: (MonadIO m) => AccountAddress -> AccountDifferenceMap -> m (Either Word64 AccountIndex)
lookupAccountExact addr diffMap =
    lookup (accountAddressEmbed addr) diffMap >>= \case
        Left noAccounts -> return $ Left noAccounts
        Right (accIdx, actualAddr) ->
            if actualAddr == addr
                then return $ Right accIdx
                else do
                    -- This extra flatten is really not ideal, but it should also really never happen,
                    -- hence the extra flatten here justifies the simpler implementation and optimization
                    -- towards the normal use case.
                    size <- length <$> flatten diffMap
                    return $ Left $ fromIntegral size

-- | Insert a fresh key-value pair into the difference map.
--
--  PRECONDITION: The provided key MUST NOT be present in the difference map.
insertFresh :: (Hashable k) => k -> v -> DifferenceMap k v -> DifferenceMap k v
insertFresh k v m =
    m
        { dmMap = HM.insert k v $ dmMap m,
          dmMapSize = dmMapSize m + 1
        }

-- | Insert a fresh key-value pair into the difference map reference.
--
--  PRECONDITION: The provided key MUST NOT be present in the difference map.
refInsertFresh :: (Hashable k, MonadIO m) => k -> v -> DifferenceMapReference k v -> m ()
refInsertFresh k v ref = liftIO $ do
    readIORef ref >>= \case
        Absent -> do
            newRef <- newIORef Absent
            atomicWriteIORef ref $ Present $ insertFresh k v (empty newRef)
        Present diffMap -> atomicWriteIORef ref $ Present $ insertFresh k v diffMap

-- | Insert an account into the difference map.
--  Note that it is up to the caller to ensure only the canonical 'AccountAddress' is inserted.
--
--  PRECONDITION: The provided 'AccountAddress' MUST be the canonical address, and MUST NOT
--  be present in the difference map.
insertFreshAccount :: AccountAddress -> AccountIndex -> AccountDifferenceMap -> AccountDifferenceMap
insertFreshAccount addr accIndex = insertFresh (accountAddressEmbed addr) (accIndex, addr)

-- | Create an 'AccountDifferenceMap' with the provided parent and list of account addresses and account indices.
fromAccountList :: IORef (Option AccountDifferenceMap) -> [(AccountAddress, AccountIndex)] -> AccountDifferenceMap
fromAccountList parentRef = fromList parentRef . map mkKeyVal
  where
    -- Make a key value pair to put in the @dmAccounts@.
    mkKeyVal (accAddr, accIdx) = (accountAddressEmbed accAddr, (accIdx, accAddr))

-- | Create a 'DifferenceMap' with the provided parent and list of key-value pairs.
fromList :: (Hashable k) => DifferenceMapReference k v -> [(k, v)] -> DifferenceMap k v
fromList parentRef keyValList =
    DifferenceMap
        { dmMap = theMap,
          dmMapSize = fromIntegral $ HM.size theMap,
          dmParentMapRef = parentRef
        }
  where
    theMap = HM.fromList keyValList

-- | Clear the reference to the parent difference map (if any).
--  Note that if there is a parent map then this function clears the remaining parent references
--  recursively.
clearReferences :: (MonadIO m) => DifferenceMap k v -> m ()
clearReferences DifferenceMap{..} = do
    oParentDiffMap <- liftIO $ readIORef dmParentMapRef
    case oParentDiffMap of
        Absent -> return ()
        Present diffMap -> do
            -- Clear this parent reference.
            liftIO $ atomicWriteIORef dmParentMapRef Absent
            -- Continue and check if the parent should have cleared it parent(s).
            clearReferences diffMap
