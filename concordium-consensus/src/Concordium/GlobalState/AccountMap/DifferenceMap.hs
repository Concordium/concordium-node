{-# LANGUAGE BangPatterns #-}
-- | The 'DifferenceMap' stores accounts have been created in a non-finalized block.
--  When a block is being finalized then the assoicated 'DifferenceMap' must be written
--  to disk via 'Concordium.GlobalState.AccountMap.LMDB.insert'.
module Concordium.GlobalState.AccountMap.DifferenceMap where

import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

import Concordium.Types

-- | A difference map that indicates newly added accounts for
--  a block identified by a 'BlockHash' and its associated 'BlockHeight'.
--  The difference map only contains accounds that was added since the '_dmParentMap'.
data DifferenceMap = DifferenceMap
    { -- | Accounts added in a block.
      dmAccounts :: !(Map.Map AccountAddress AccountIndex),
      -- | Parent map of non-finalized blocks.
      --  In other words, if the parent block is finalized,
      --  then the parent map is @Notnhing@ as the LMDB account map
      --  should be consulted instead.
      dmParentMap :: !(Maybe DifferenceMap)
    }
    deriving (Eq, Show)

-- | Gather all accounts from the provided 'DifferenceMap' and its parent maps.
--  Accounts are returned in ascending order of their 'AccountIndex'.
flatten :: DifferenceMap -> Map.Map AccountAddress AccountIndex
flatten dmap = go (Just dmap) Map.empty
  where
    go :: Maybe DifferenceMap -> Map.Map AccountAddress AccountIndex -> Map.Map AccountAddress AccountIndex
    go Nothing accum = accum
    go (Just DifferenceMap{..}) !accum = go dmParentMap $ dmAccounts `Map.union` accum

-- | Create a new empty 'DifferenceMap' based on the difference map of
-- the parent.
empty :: Maybe DifferenceMap -> DifferenceMap
empty mParentDifferenceMap =
    DifferenceMap
        { dmAccounts = Map.empty,
          dmParentMap = mParentDifferenceMap
        }

-- | Lookup an account in the difference map or any of the parent
--  difference maps.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
lookup :: AccountAddress -> DifferenceMap -> Maybe AccountIndex
lookup addr = check
  where
    check DifferenceMap{..} = case Map.lookupGE k dmAccounts of
        Nothing -> check =<< dmParentMap
        Just (foundAccAddr, accIdx) ->
            if checkEquivalence foundAccAddr
                then Just accIdx
                else Nothing
    k = createAlias addr 0
    checkEquivalence found = accountAddressEmbed k == accountAddressEmbed found

-- | Insert an account into the difference map.
--  Note that it is up to the caller to ensure only the canonical 'AccountAddress' is added.
insert :: AccountAddress -> AccountIndex -> DifferenceMap -> DifferenceMap
insert addr accIndex m = m{dmAccounts = Map.insert addr accIndex $ dmAccounts m}
