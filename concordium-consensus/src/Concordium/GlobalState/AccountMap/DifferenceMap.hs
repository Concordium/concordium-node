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

    -- The empty reference
    emptyReference,
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
import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Prelude hiding (lookup)

import Concordium.Types
import Concordium.Types.Option (Option (..))

-- | A mutable reference to a 'DiffMap.DifferenceMap'.
type DifferenceMapReference = IORef (Option DifferenceMap)

-- | The empty reference
emptyReference :: (MonadIO m) => m (IORef (Option DifferenceMap))
emptyReference = liftIO $ newIORef Absent

-- | A difference map that indicates newly added accounts for
--  a block identified by a 'BlockHash' and its associated 'BlockHeight'.
--  The difference map only contains accounts that were added since the '_dmParentMapRef'.
data DifferenceMap = DifferenceMap
    { -- | Accounts added in a block.
      dmAccounts :: !(HM.HashMap AccountAddressEq AccountIndex),
      -- | Parent map of non-finalized blocks.
      --  In other words, if the parent block is finalized,
      --  then the parent map is @Absent@ as the LMDB account map
      --  should be consulted instead.
      --  This is an 'IORef' since the parent map may belong
      --  to multiple blocks if they have not yet been persisted.
      --  So the 'IORef' enables us to when persisting a block,
      --  then we also clear the 'DifferenceMap' for the child block.
      dmParentMapRef :: !DifferenceMapReference
    }
    deriving (Eq)

-- | Gather all accounts from the provided 'DifferenceMap' and its parent maps.
--  Accounts are returned in ascending order of their 'AccountAddress'.
flatten :: (MonadIO m) => DifferenceMap -> m [(AccountAddress, AccountIndex)]
flatten dmap = map (first aaeAddress) <$> go dmap []
  where
    go diffMap !accum = do
        mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
        case mParentMap of
            Absent -> return collectedAccounts
            Present parentMap -> go parentMap collectedAccounts
      where
        collectedAccounts = HM.toList (dmAccounts diffMap) <> accum

-- | Create a new empty 'DifferenceMap' potentially based on the difference map of
-- the parent.
empty :: IORef (Option DifferenceMap) -> DifferenceMap
empty mParentDifferenceMap =
    DifferenceMap
        { dmAccounts = HM.empty,
          dmParentMapRef = mParentDifferenceMap
        }

-- | Lookup an account in the difference map or any of the parent
--  difference maps using the account address equivalence class.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
--  Precondition: As this implementation uses the 'AccountAddressEq' equivalence
--  class for looking up an 'AccountIndex', then it MUST only be used
--  when account aliases are supported.
lookupViaEquivalenceClass :: (MonadIO m) => AccountAddressEq -> DifferenceMap -> m (Maybe AccountIndex)
lookupViaEquivalenceClass addr = check
  where
    check diffMap = case HM.lookup addr (dmAccounts diffMap) of
        Nothing -> do
            mParentMap <- liftIO $ readIORef (dmParentMapRef diffMap)
            case mParentMap of
                Absent -> return Nothing
                Present parentMap -> check parentMap
        Just accIdx -> return $ Just accIdx

-- | Lookup an account in the difference map or any of the parent
--  difference maps via an exactness check.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
--  Precondition: As this implementation checks for exactness of the provided
--  @AccountAddress@ then it MUST only be used when account aliases are NOT supported.
--
--  Implementation note: It is not as sufficient as 'lookupViaEquivalenceClass' as it folds over the accounts,
--  but this should be fine as the maps are generally very small.
lookupExact :: (MonadIO m) => AccountAddress -> DifferenceMap -> m (Maybe AccountIndex)
lookupExact addr diffMap =
    foldl'
        ( \_ (accAddr, accIdx) ->
            if addr == accAddr
                then return $ Just accIdx
                else return Nothing
        )
        (pure Nothing)
        =<< flatten diffMap

-- | Insert an account into the difference map.
--  Note that it is up to the caller to ensure only the canonical 'AccountAddress' is being inserted.
insert :: AccountAddress -> AccountIndex -> DifferenceMap -> DifferenceMap
insert addr accIndex m = m{dmAccounts = HM.insert (accountAddressEmbed addr) accIndex $ dmAccounts m}

-- | Create a 'DifferenceMap' with the provided parent and list of account addresses and account indices.
fromList :: IORef (Option DifferenceMap) -> [(AccountAddress, AccountIndex)] -> DifferenceMap
fromList parentRef listOfAccountsAndIndices =
    DifferenceMap
        { dmAccounts = HM.fromList $ map (first accountAddressEmbed) listOfAccountsAndIndices,
          dmParentMapRef = parentRef
        }
