{-# LANGUAGE DerivingVia, StandaloneDeriving, DefaultSignatures, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}
module Concordium.GlobalState.AccountTransactionIndex where

import Control.Monad.Except
-- import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Execution
import Concordium.GlobalState.Classes

-- |A typeclass that abstract the ability to record the footprint.
-- If a = () we don't record anything, and if a = HashSet we record the set of addresses
-- that were affected by execution.
class Monoid a => CanRecordFootprint a where
  -- Log an individual account address.
  logAccount :: AccountAddress -> a

  -- Traverse the outcomes.
  traverseOutcomes :: Monad m => a -> (AccountAddress -> m ()) -> m ()

instance CanRecordFootprint () where
  {-# INLINE logAccount #-}
  logAccount = const ()

  {-# INLINE traverseOutcomes #-}
  traverseOutcomes = \() _ -> return ()

instance CanRecordFootprint (HS.HashSet AccountAddress) where
  {-# INLINE logAccount #-}
  logAccount = HS.singleton

  {-# INLINE traverseOutcomes #-}
  traverseOutcomes = forM_

-- |Mapping from account addresses to a list of transaction affecting this account.
-- The transactions are ordered in reverse (i.e., head is the most recent transaction
-- affecting the account).
type AccountTransactionIndex = HM.HashMap AccountAddress [TransactionSummary]

class CanExtend a where
  defaultValue :: a
  extendRecord :: AccountAddress -> TransactionSummary -> a -> a

instance a ~ () => CanExtend a where
  defaultValue = ()
  extendRecord = \_ _ -> id
  {-# INLINE defaultValue #-}
  {-# INLINE extendRecord #-}

instance CanExtend AccountTransactionIndex where
  defaultValue = HM.empty
  extendRecord addr summary = at addr . non [] %~ (summary:)
  {-# INLINE defaultValue #-}
  {-# INLINE extendRecord #-}

-- |Footprint record recorded during scheduler execution.
type family Footprint a

type instance Footprint () = ()
type instance Footprint AccountTransactionIndex = HS.HashSet AccountAddress

class (CanExtend (ATIStorage m), CanRecordFootprint (Footprint (ATIStorage m))) => ATITypes (m :: * -> *) where
  -- |Type of values stored in the block pointer, e.g., a map Address -> Summary
  type ATIStorage m

class (Monad m, ATITypes m) => PerAccountDBOperations m where
  flushBlockSummaries :: BlockHash -> ATIStorage m -> m ()

  default flushBlockSummaries :: (ATIStorage m ~ ()) => BlockHash -> ATIStorage m -> m ()
  flushBlockSummaries = \_ () -> return ()
  {-# INLINE flushBlockSummaries #-}

instance ATITypes m => ATITypes (MGSTrans t m) where
  type ATIStorage (MGSTrans t m) = ATIStorage m

instance (MonadTrans t, Monad (t m), PerAccountDBOperations m) => PerAccountDBOperations (MGSTrans t m) where
  {-# INLINE flushBlockSummaries #-}
  flushBlockSummaries bh ati = lift (flushBlockSummaries bh ati)

deriving via (MGSTrans MaybeT m) instance ATITypes m => ATITypes (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance ATITypes m => ATITypes (ExceptT e m)

deriving via (MGSTrans MaybeT m) instance PerAccountDBOperations m => PerAccountDBOperations (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance PerAccountDBOperations m => PerAccountDBOperations (ExceptT e m)

-- -- Only have the default instance since RWST might be used with genuine ati otherwise.
-- instance (Monoid w, Monad m) => PerAccountDBOperations (RWST c w s m) where
--   type (ATIStorage (RWST c w s m)) = ()
--   -- default instance

