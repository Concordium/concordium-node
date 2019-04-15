{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.Scheduler.Environment where

import Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict

import Data.Maybe(fromJust)

import qualified Acorn.Core as Core
import Concordium.Scheduler.Types

-- * Scheduler monad

-- |Information needed to execute transactions.
class StaticEnvironmentMonad m => SchedulerMonad m where
  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the amount of funds at the particular account address.
  -- To get the amount of funds for a contract instance use getInstance and lookup amount there.
  getAccount :: AccountAddress -> m (Maybe Account)

  -- |Commit to global state all the updates to local state that have
  -- accumulated through the execution.
  commitStateAndAccountChanges :: ChangeSet -> m ()

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed.
  commitModule :: Core.ModuleRef -> Interface -> ValueInterface -> m Bool

  -- |Create new instance in the global state.
  -- If an instance with the given address already exists do nothing and return @False@.
  putNewInstance :: Instance -> m Bool

  -- |Create new account in the global state. Return @True@ if the account was
  -- successfully created and @False@ if the account address already existed.
  putNewAccount :: Account -> m Bool

  -- |Pay for execution. Return the amount remaining on the account.
  -- Payment needs to be performed
  -- PRECONDITION: There should be enough funds on the account before this function is called.
  -- Otherwise the function should fail raising an exception.
  payForExecution :: AccountAddress -> Energy -> m Amount

  -- |Refund the remaining execution cost.
  refundEnergy :: AccountAddress -> Energy -> m ()

  -- |Return a free address that can be assigned a new contract instance
  firstFreeAddress :: m ContractAddress

-- |This is a derived notion that is used inside a transaction to keep track of
-- the state of the world during execution. Local state of contracts and amounts
-- on contracts might need to be rolled back for various reasons, so we do not
-- want to commit it to global state.
class StaticEnvironmentMonad m => TransactionMonad m where
  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need nod provide it
  -- separately.
  withInstance :: ContractAddress -> Amount -> Value -> m a -> m a

  -- |And the same for amounts on accounts and contracts. The amounts can change
  -- due to simple transfers and constract calls.
  withAmount :: Address -> Amount -> m a -> m a

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  getCurrentAccount :: AccountAddress -> m (Maybe Account)

  getChanges :: m ChangeSet


-- |The set of changes to be commited on a successful transaction.
data ChangeSet = ChangeSet
    {newAccounts :: Map.HashMap AccountAddress Account
    ,newContractStates :: Map.HashMap ContractAddress (Amount, Value)
    }

emptyCS :: ChangeSet
emptyCS = ChangeSet Map.empty Map.empty


addAmountToCS :: ChangeSet -> Account -> Amount -> ChangeSet
addAmountToCS cs acc amnt = cs { newAccounts = Map.insert (accountAddress acc) (acc { accountAmount = amnt }) (newAccounts cs) }

addContractStatesToCS :: ChangeSet -> ContractAddress -> Amount -> Value -> ChangeSet
addContractStatesToCS cs addr amnt val = cs { newContractStates = Map.insert addr (amnt, val) (newContractStates cs) }

-- |NB: INVARIANT: This function expects that the contract already exists in the
-- changeset map. This will be true during execution since the only way a contract can
-- possibly send a message is if its local state exists in this map (
addContractAmountToCS :: ChangeSet -> ContractAddress -> Amount -> ChangeSet
addContractAmountToCS cs addr amnt = cs { newContractStates = Map.adjust (\(_, val) -> (amnt, val)) addr (newContractStates cs) }


-- |A concrete implementation of TransactionMonad based on SchedulerMonad.
newtype LocalT m a = LocalT { _runLocalT :: StateT ChangeSet m a }
  deriving(Functor, Applicative, Monad, MonadState ChangeSet, MonadTrans)

runLocalT :: LocalT m a -> m (a, ChangeSet)
runLocalT (LocalT st) = runStateT st emptyCS

{-# INLINE evalLocalT #-}
evalLocalT :: Monad m => LocalT m a -> m a
evalLocalT (LocalT st) = evalStateT st emptyCS

execLocalT :: Monad m => LocalT m a -> m ChangeSet
execLocalT (LocalT st) = execStateT st emptyCS

instance StaticEnvironmentMonad m => StaticEnvironmentMonad (LocalT m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = lift getChainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces = lift . getModuleInterfaces

instance SchedulerMonad m => TransactionMonad (LocalT m) where
  {-# INLINE withInstance #-}
  withInstance cref amount val cont = do
    modify' (\s -> addContractStatesToCS s cref amount val)
    cont

  {-# INLINE withAmount #-}
  -- inlining may help with the common case where the address format is statically known
  withAmount addr amount cont =
    case addr of
      AddressAccount acc -> do
        accdata <- lift (fromJust <$> getAccount acc)
        modify' (\s -> addAmountToCS s accdata amount)
      AddressContract addrc -> modify' (\s -> addContractAmountToCS s addrc amount)
    >> cont

  getCurrentContractInstance addr = do
    ChangeSet{..} <- get
    lift $ do mistance <- getContractInstance addr
              case mistance of
                Nothing -> return Nothing
                Just i -> case Map.lookup addr newContractStates of
                            Nothing -> return $ Just i
                            Just (amnt, newmodel) -> return $ Just (i { iamount = amnt, imodel = newmodel })

  {-# INLINE getCurrentAccount #-}
  getCurrentAccount acc = do
    ChangeSet{..} <- get
    case Map.lookup acc newAccounts of
      Nothing -> lift $! getAccount acc
      Just a -> return $ Just a

  {-# INLINE getChanges #-}
  getChanges = get

instance SchedulerMonad m => InterpreterMonad (LocalT m) where
  getCurrentContractState caddr = do
    ChangeSet{..} <- get
    lift $ do mistance <- getContractInstance caddr
              case mistance of
                Nothing -> return Nothing
                Just i -> case Map.lookup caddr newContractStates of
                            Nothing -> return $ Just (instanceImplements i, imodel i)
                            Just (_, newmodel) -> return $ Just (instanceImplements i, newmodel)

