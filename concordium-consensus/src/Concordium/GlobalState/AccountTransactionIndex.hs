{-# LANGUAGE DerivingVia, StandaloneDeriving, DefaultSignatures, TypeFamilies, UndecidableInstances, ScopedTypeVariables #-}
module Concordium.GlobalState.AccountTransactionIndex where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HS
import Lens.Micro.Platform
import Control.Monad.Identity
import qualified Data.ByteString.Char8 as BS

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
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
-- The transactions are ordered in reverse (i.e., head is the most recent transaction)
type AccountTransactionIndex = [(AccountAddress, TransactionSummary)]

class CanExtend a where
  defaultValue :: a
  extendRecord :: AccountAddress -> TransactionSummary -> a -> a

instance CanExtend () where
  defaultValue = ()
  extendRecord = \_ _ -> id
  {-# INLINE defaultValue #-}
  {-# INLINE extendRecord #-}

instance CanExtend AccountTransactionIndex where
  defaultValue = []
  extendRecord addr summary = ((addr,summary) :)
  {-# INLINE defaultValue #-}
  {-# INLINE extendRecord #-}

-- |Footprint record recorded during scheduler execution.
type family Footprint a

type instance Footprint () = ()
type instance Footprint AccountTransactionIndex = HS.HashSet AccountAddress

class (CanExtend (ATIStorage m), CanRecordFootprint (Footprint (ATIStorage m))) => ATITypes (m :: * -> *) where
  -- |Type of values stored in the block pointer, e.g., a map Address -> Summary
  type ATIStorage m

data BlockContext = BlockContext {
  bcHash :: BlockHash,
  bcHeight :: BlockHeight,
  bcTime :: Timestamp
  }

class (Monad m, ATITypes m) => PerAccountDBOperations m where
  flushBlockSummaries :: BlockContext -> ATIStorage m -> [SpecialTransactionOutcome] -> m ()

  default flushBlockSummaries :: (ATIStorage m ~ ()) => BlockContext -> ATIStorage m -> [SpecialTransactionOutcome] -> m ()
  flushBlockSummaries = \_ () _ -> return ()
  {-# INLINE flushBlockSummaries #-}

instance ATITypes m => ATITypes (MGSTrans t m) where
  type ATIStorage (MGSTrans t m) = ATIStorage m

instance (MonadTrans t, Monad (t m), PerAccountDBOperations m) => PerAccountDBOperations (MGSTrans t m) where
  {-# INLINE flushBlockSummaries #-}
  flushBlockSummaries bh ati = lift . flushBlockSummaries bh ati

deriving via (MGSTrans MaybeT m) instance ATITypes m => ATITypes (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance ATITypes m => ATITypes (ExceptT e m)

deriving via (MGSTrans MaybeT m) instance PerAccountDBOperations m => PerAccountDBOperations (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance PerAccountDBOperations m => PerAccountDBOperations (ExceptT e m)

-- -- Only have the default instance since RWST might be used with genuine ati otherwise.
-- instance (Monoid w, Monad m) => PerAccountDBOperations (RWST c w s m) where
--   type (ATIStorage (RWST c w s m)) = ()
--   -- default instance


-- auxiliary classes to derive instances

class HasLogContext ctx r | r -> ctx where
  logContext :: Lens' r ctx

instance HasLogContext g (Identity g) where
    logContext = lens runIdentity (const Identity)

-- |Additional logs produced by execution
data NoLogContext = NoLogContext

type family ATIValues ati
type family ATIContext ati

-- instance used when we want no logging of transactions
type instance ATIValues () = ()
type instance ATIContext () = NoLogContext

-- * Sqlite log instance.

data PerAccountAffectIndex = PAAIConfig BS.ByteString
-- When we want to dump data to disk.
data DiskDump
type instance ATIValues DiskDump = AccountTransactionIndex
type instance ATIContext DiskDump = PerAccountAffectIndex
