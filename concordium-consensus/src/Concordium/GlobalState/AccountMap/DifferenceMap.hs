{-# LANGUAGE TemplateHaskell #-}
-- | The 'DifferenceMap' stores accounts have been created in a non-finalized block.
--  When a block is being finalized then the assoicated 'DifferenceMap' must be written
--  to disk via 'Concordium.GlobalState.AccountMap.LMDB.insert'.
module Concordium.GlobalState.AccountMap.DifferenceMap where

import qualified Data.List as List
import Prelude hiding (lookup)

import Concordium.Types

-- | A difference map that indicates newly added accounts for
--  a block identified by a 'BlockHash' and its associated 'BlockHeight'.
--  The difference map only contains accounds that was added since the '_dmParentMap'.
data DifferenceMap = DifferenceMap
    {
      -- | Accounts added to the chain in the
      --  block 'amdmLfbHash'.
      --  Note. The list is in descending order of the 'AccountIndex'.
      --  TODO: Use Ordered set or a sequence instead?
      dmAccounts :: ![(AccountAddress, AccountIndex)],
      -- | Next available account index.
      dmNextAccountIndex :: !AccountIndex,
      -- | Parent map of non-finalized blocks.
      --  In other words, if the parent block is finalized,
      --  then the parent map is @Nothing@ as the LMDB account map
      --  should be consulted instead.
      dmParentMap :: !(Maybe DifferenceMap)
    }

-- | Create a new empty 'DifferenceMap' based on either a finalized block (in which case
--  the @dmNextAccountIndex@ must be provided explicitly or in case that the parent block is
--  not yet finalized then that map is supplied.
empty :: AccountIndex -> DifferenceMap
empty nextAccountIndex =
    DifferenceMap
        { dmAccounts = [],
          dmNextAccountIndex = nextAccountIndex,
          dmParentMap = Nothing
        }

-- | Check if an account exists in the difference map or any of the parent
--  difference maps.
--  Returns @Just AccountIndex@ if the account is present and
--  otherwise @Nothing@.
--  Note. It is up to the caller to check whether the account exists in the last finalized block.
lookup :: AccountAddress -> DifferenceMap -> Maybe AccountIndex
lookup addr DifferenceMap{..} =
    case List.lookup addr dmAccounts of
        Nothing -> case dmParentMap of
            Nothing -> Nothing
            Just parentMap -> lookup addr parentMap
        Just idx -> Just idx

-- | Insert an account into the difference and return @Just AccountIndex@ if the
--  account was added and @Nothing@ if it was already present.
--
--  If a an account was succesfully added the 'dmNextAccountIndex' is being incremented by one.
addAccount :: AccountAddress -> AccountIndex -> DifferenceMap -> DifferenceMap
addAccount addr accIndex diffMap =
    diffMap {
        dmAccounts = (addr,accIndex) : dmAccounts diffMap,
        dmNextAccountIndex = 1 + dmNextAccountIndex diffMap
    }
    
    

