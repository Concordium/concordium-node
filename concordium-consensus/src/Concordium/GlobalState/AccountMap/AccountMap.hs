-- | This module exposes an account map backed by a LMDB database.
--  The ‘AccountMap’ is a simple key/value store where the keys consists
--  of the first 29 bytes of an ‘AccountAddress’ and the values are the
--  associated ‘AccountIndex’.
--
-- The account map is integrated with the block state “on-the-fly” meaning that
-- whenver the node starts up and the ‘AccountMap’ is not populated, then it will be
-- initialized on startup via the existing ‘PersistentAccountMap’.
--
--  Invariants:
-- * Only finalized accounts may be added to the ‘AccountMap’
--  The following operations are applicable to the ‘AccountMap’.
--
module Concordium.GlobalState.AccountMap.AccountMap (
  -- | The account map.
  AccountMap(..),
  -- | Initialize the account map.
  initialize,
  -- | Check whether the account map is initialized.
  isInitialized,
  -- | Add an account to the ‘AccountMap’.
  addAccount,
  -- | Look up an ‘AccountIndex’ by the supplied ‘AccountAddress’.
  lookupAccount
) where

import Concordium.Types
import Concordium.GlobalState.AccountMap.LMDB
import qualified Concordium.GlobalState.AccountMap as GSA

data AccountMap = AccountMap

-- | Create and initialize the ‘AccountMap’ via the supplied
--  ‘GSA.PersistentAccountMap’.
--
--  Depending on the protocol version, then the ‘GSA.PersistentAccountMap’ may
--  contain keys which refer to the same ‘AccountIndex’.
--  The constructed ‘AccountMap’ will only retain one entry per account.
initialize :: GSA.PersistentAccountMap pv -> AccountMap
initialize _ = undefined

-- | Check whether the ‘AccountMap’ is initialized.
--  Returns “Just BlockHash” if the ‘AccountMap’ is initialized,
--  the ‘BlockHash’ indicates the last finalized block for the ‘AccountMap’.
--  Returns @Nothing@ if the account map is not initialized.
isInitialized :: Monad m => m (Maybe BlockHash)
isInitialized = return Nothing

-- | Adds an account to the ‘AccountMap’.
addAccount :: Monad m => BlockHash -> AccountAddress -> AccountIndex -> m ()
addAccount _ = undefined

-- | Looks up the ‘AccountIndex’ for the provided ‘AccountAddress’.
--  Returns @Just AccountIndex@ if the account is present in the ‘AccountMap’
--  and returns @Nothing@ if the account was not present.
lookupAccount :: Monad m => AccountAddress -> m (Maybe AccountIndex)
lookupAccount _ = undefined
