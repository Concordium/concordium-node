{-# LANGUAGE BangPatterns #-}

-- | The 'DifferenceMap' stores accounts that have been created in a non-finalized block.
--  When a block is finalized then the associated 'DifferenceMap' must be written
--  to disk via 'Concordium.GlobalState.AccountMap.LMDB.insertAccounts'.
module Concordium.GlobalState.AccountMap.DifferenceMap (
    -- * Definitions

    -- The difference map definition.
    DifferenceMap (..),
    -- A mutable reference to a 'DifferenceMap'.
    DifferenceMapReference,

    -- * Auxiliary functions

    -- Create a new empty mutable reference.
    newEmptyReference,
    -- Get a list of all @(AccountAddress, AccountIndex)@ pairs for the
    --  provided 'DifferenceMap' and all parent maps.
    flatten,
    -- Create an empty 'DifferenceMap'
    empty,
    -- Set the accounts int he 'DifferenceMap'.
    fromList,
    -- Insert an account into the 'DifferenceMap'.
    insert,
    -- Lookup in a difference map (and potential parent maps) whether
    -- it yields the 'AccountIndex' for the provided 'AccountAddress' or any
    -- alias of it.
    lookupViaEquivalenceClass,
    -- Lookup in a difference map (and potential parent maps) whether
    -- it yields the 'AccountIndex' for the provided 'AccountAddress'.
    lookupExact,
) where

import Control.Monad.IO.Class
import Data.Tuple (swap)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Prelude hiding (lookup)

import Concordium.Types
import Concordium.Types.Option (Option (..))

-- | A mutable reference to a 'DiffMap.DifferenceMap'.
--  This is an 'IORef' since the parent map may belong
--  to multiple blocks if they have not yet been persisted.
--
--  The 'IORef' enables us to clear any child difference maps
--  when a block is finalized.
type DifferenceMapReference = IORef (Option DifferenceMap)

-- | Create a new empty reference.
newEmptyReference :: (MonadIO m) => m DifferenceMapReference
newEmptyReference = liftIO $ newIORef Absent

-- | A difference map that indicates newly added accounts for
--  a block identified by a 'BlockHash' and its associated 'BlockHeight'.
--  The difference map only contains accounts that were added since the '_dmParentMapRef'.
data DifferenceMap = DifferenceMap
    { -- | Accounts added in a block keyed by their equivalence class and
      --  the @AccountIndex@ and canonical account adddress as values.
      dmAccounts :: !(HM.HashMap AccountAddressEq (AccountIndex, AccountAddress)),
      -- | Parent map of non-finalized blocks.
      --  In other words, if the parent block is finalized,
      --  then the parent map is @Absent@ as the LMDB account map
      --  should be consulted instead.
      dmParentMapRef :: !DifferenceMapReference
    }
    deriving (Eq)

-- | Gather all accounts from the provided 'DifferenceMap' and its parent maps.
--  Accounts are returned in ascending order of their 'AccountAddress'.
--
--  Note. This function does not guarantee the order of the returned pairs.
flatten :: (MonadIO m) => DifferenceMap -> m [(AccountAddress, AccountIndex)]
flatten dmap = go dmap []
  where
    go diffMap !accum = do
        mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
        case mParentMap of
            Absent -> return collectedAccounts
            Present parentMap -> go parentMap collectedAccounts
      where
        collectedAccounts = map swap (HM.elems $ dmAccounts diffMap) <> accum

-- | Create a new empty 'DifferenceMap' potentially based on the difference map of
-- the parent.
empty :: DifferenceMapReference -> DifferenceMap
empty mParentDifferenceMap =
    DifferenceMap
        { dmAccounts = HM.empty,
          dmParentMapRef = mParentDifferenceMap
        }

-- | Internal helper function for looking up an entry in @dmAccounts@.
lookupViaEquivalenceClass' :: (MonadIO m) => AccountAddressEq -> DifferenceMap -> m (Maybe (AccountIndex, AccountAddress))
lookupViaEquivalenceClass' addr = check
  where
    check diffMap = case HM.lookup addr (dmAccounts diffMap) of
        Nothing -> do
            mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
            case mParentMap of
                Absent -> return Nothing
                Present parentMap -> check parentMap
        Just accIdx -> return $ Just accIdx

-- | Lookup an account in the difference map or any of the parent
--  difference maps using the account address equivalence class.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
--  Precondition: As this implementation uses the 'AccountAddressEq' equivalence
--  class for looking up an 'AccountIndex', then it MUST only be used
--  when account aliases are supported.
lookupViaEquivalenceClass :: (MonadIO m) => AccountAddressEq -> DifferenceMap -> m (Maybe AccountIndex)
lookupViaEquivalenceClass addr dm = lookupViaEquivalenceClass' addr dm >>= \case
    Nothing -> return Nothing
    Just (accIdx, _) -> return $ Just accIdx

-- | Lookup an account in the difference map or any of the parent
--  difference maps via an exactness check.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
--  Precondition: As this implementation checks for exactness of the provided
--  @AccountAddress@ then it MUST only be used when account aliases are NOT supported.
--  Note that this implementation is very inefficient for large difference maps and thus should be revised
--  if the credential deployments limit gets revised significantly.
lookupExact :: (MonadIO m) => AccountAddress -> DifferenceMap -> m (Maybe AccountIndex)
lookupExact addr diffMap = lookupViaEquivalenceClass' (accountAddressEmbed addr) diffMap >>= \case
    Nothing -> return Nothing
    Just (accIdx, actualAddr) -> if actualAddr == addr then return $ Just accIdx else return Nothing

-- | Insert an account into the difference map.
--  Note that it is up to the caller to ensure only the canonical 'AccountAddress' is being inserted.
insert :: AccountAddress -> AccountIndex -> DifferenceMap -> DifferenceMap
insert addr accIndex m = m{dmAccounts = HM.insert (accountAddressEmbed addr) (accIndex, addr) $ dmAccounts m}

-- | Create a 'DifferenceMap' with the provided parent and list of account addresses and account indices.
fromList :: IORef (Option DifferenceMap) -> [(AccountAddress, AccountIndex)] -> DifferenceMap
fromList parentRef listOfAccountsAndIndices =
    DifferenceMap
        { dmAccounts = HM.fromList $ map mkKeyVal listOfAccountsAndIndices,
          dmParentMapRef = parentRef
        }
  where
    -- Make a key value pair to put in the @dmAccounts@.
    mkKeyVal (accAddr, accIdx) = (accountAddressEmbed accAddr, (accIdx, accAddr))
