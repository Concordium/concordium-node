{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.AccountTransactionIndex where

import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashSet as HS
import Lens.Micro.Platform
import Control.Monad.Identity
import Data.Kind
import qualified Data.Sequence as Seq

import Data.Pool
import Database.Persist.Sql

import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.GlobalState.Classes

-- |A typeclass that abstract the ability to record the footprint.
-- If a = () we don't record anything, and if a = RecordAffectedAccountsAndContracts
-- we record the set of account addresses and contracts addresses that were affected
-- by execution.
--
-- This particularly means that we will output information when a transaction modifies
-- either an instance (creation or update) or an account following the schema defined
-- in Concordium/GlobalState/SQL/AccountTransactionIndex.hs
class Monoid a => CanRecordFootprint a where
  -- Log an individual account address.
  logAccount :: AccountAddress -> a

  -- Log an individual contract address.
  logContract :: ContractAddress -> a

  -- Traverse the account outcomes.
  traverseAccountOutcomes :: Monad m => a -> (AccountAddress -> m ()) -> m ()

  -- Traverse the contract outcomes.
  traverseContractOutcomes :: Monad m => a -> (ContractAddress -> m ()) -> m ()

instance CanRecordFootprint () where
  {-# INLINE logAccount #-}
  logAccount = const ()

  {-# INLINE logContract #-}
  logContract = const ()

  {-# INLINE traverseAccountOutcomes #-}
  traverseAccountOutcomes = \() _ -> return ()

  {-# INLINE traverseContractOutcomes #-}
  traverseContractOutcomes = \() _ -> return ()

type RecordAffectedAccountsAndContracts = (HS.HashSet AccountAddress, HS.HashSet ContractAddress)

instance CanRecordFootprint RecordAffectedAccountsAndContracts where
  {-# INLINE logAccount #-}
  logAccount x = (HS.singleton x, HS.empty)

  {-# INLINE logContract #-}
  logContract x = (HS.empty, HS.singleton x)

  {-# INLINE traverseAccountOutcomes #-}
  traverseAccountOutcomes (a, _) = forM_ a

  {-# INLINE traverseContractOutcomes #-}
  traverseContractOutcomes (_, c) = forM_ c

-- |Mappings from account addresses to a list of transaction affecting this account and
-- from contract addresses to a list of transactions affecting this contract.
-- The transactions are ordered in reverse (i.e., head is the most recent transaction)
data AccountTransactionIndex = AccountTransactionIndex {
  accountLogIndex :: [(AccountAddress, TransactionSummary)],
  contractLogIndex :: [(ContractAddress, TransactionSummary)]
  }

class CanExtend a where
  defaultValue :: a
  extendAccountRecord :: AccountAddress -> TransactionSummary -> a -> a
  extendContractRecord :: ContractAddress -> TransactionSummary -> a -> a

instance CanExtend () where
  defaultValue = ()
  extendAccountRecord = \_ _ -> id
  extendContractRecord = \_ _ -> id
  {-# INLINE defaultValue #-}
  {-# INLINE extendAccountRecord #-}
  {-# INLINE extendContractRecord #-}

instance CanExtend AccountTransactionIndex where
  defaultValue = AccountTransactionIndex [] []
  extendAccountRecord addr summary ati@AccountTransactionIndex{..} = ati { accountLogIndex =  (addr,summary) : accountLogIndex }
  extendContractRecord addr summary ati@AccountTransactionIndex{..} = ati { contractLogIndex = (addr, summary) : contractLogIndex }
  {-# INLINE defaultValue #-}
  {-# INLINE extendAccountRecord #-}
  {-# INLINE extendContractRecord #-}

-- |Footprint record recorded during scheduler execution.
type family Footprint a

type instance Footprint () = ()
type instance Footprint AccountTransactionIndex = RecordAffectedAccountsAndContracts

class (CanExtend (ATIStorage m), CanRecordFootprint (Footprint (ATIStorage m))) => ATITypes (m :: Type -> Type) where
  -- |Type of values stored in the block pointer, e.g., a map Address -> Summary
  type ATIStorage m

data BlockContext = BlockContext {
  bcHash :: BlockHash,
  bcHeight :: BlockHeight,
  bcTime :: Timestamp
  }

class (Monad m, ATITypes m) => PerAccountDBOperations m where
  flushBlockSummaries :: BlockContext -> ATIStorage m -> Seq.Seq SpecialTransactionOutcome -> m ()

  default flushBlockSummaries :: (ATIStorage m ~ ()) => BlockContext -> ATIStorage m -> Seq.Seq SpecialTransactionOutcome -> m ()
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

data PerAccountAffectIndex = PAAIConfig (Pool SqlBackend)
-- When we want to dump data to disk.
data DiskDump
type instance ATIValues DiskDump = AccountTransactionIndex
type instance ATIContext DiskDump = PerAccountAffectIndex
