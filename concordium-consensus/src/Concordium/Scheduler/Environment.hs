{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.Scheduler.Environment where

import qualified Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict

import Data.Maybe(fromJust)
import Lens.Micro.Platform

import qualified Acorn.Core as Core
import Concordium.Scheduler.Types

import qualified Concordium.ID.Types as ID

-- * Scheduler monad

-- |Information needed to execute transactions.
class StaticEnvironmentMonad m => SchedulerMonad m where
  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the amount of funds at the particular account address.
  -- To get the amount of funds for a contract instance use getInstance and lookup amount there.
  getAccount :: AccountAddress -> m (Maybe Account)

  -- |Check whether an account with given registration id exists.
  accountRegIdExists :: ID.AccountRegistrationID -> m Bool

  -- |Commit to global state all the updates to local state that have
  -- accumulated through the execution.
  commitStateAndAccountChanges :: ChangeSet -> m ()

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed.
  commitModule :: Core.ModuleRef -> Interface -> ValueInterface -> m Bool

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: (ContractAddress -> Instance) -> m ContractAddress

  -- |Bump the next available transaction nonce of the account. The account is assumed to exist.
  increaseAccountNonce :: AccountAddress -> m ()

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

-- |This is a derived notion that is used inside a transaction to keep track of
-- the state of the world during execution. Local state of contracts and amounts
-- on contracts might need to be rolled back for various reasons, so we do not
-- want to commit it to global state.
class StaticEnvironmentMonad m => TransactionMonad m where
  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need not provide it
  -- separately.
  withInstance :: ContractAddress -> Amount -> Value -> m a -> m a

  -- |And the same for amounts on accounts and contracts. The amounts can change
  -- due to simple transfers and constract calls.
  withAmount :: Address -> Amount -> m a -> m a

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  getCurrentAccount :: AccountAddress -> m (Maybe Account)

  getChanges :: m ChangeSet

  -- |Get the amount of gas remaining for the transaction.
  getEnergy :: m Energy

  -- |Decrease the remaining energy by the given amount. If not enough is left
  -- reject the transaction.
  tickEnergy :: Energy -> m ()

  -- |Set the remaining energy to be the given value.
  putEnergy :: Energy -> m ()

  rejectTransaction :: RejectReason -> m a


-- |The set of changes to be commited on a successful transaction.
data ChangeSet = ChangeSet
    {_newAccounts :: Map.HashMap AccountAddress Account -- ^Accounts whose states changed.
    ,_newContractStates :: Map.HashMap ContractAddress (Amount, Value) -- ^Contracts whose states changed.
    }
makeLenses ''ChangeSet

emptyCS :: ChangeSet
emptyCS = ChangeSet Map.empty Map.empty

-- |Update the amount on the account in the changeset with the given amount.
-- If the account is not yet in the changeset it is created.
addAmountToCS :: ChangeSet -> Account -> Amount -> ChangeSet
addAmountToCS cs acc amnt =
  cs & newAccounts . at (acc ^. accountAddress) ?~ (acc & accountAmount .~ amnt)

-- |Add or update the contract state in the changeset with the given amount and value.
addContractStatesToCS :: ChangeSet -> ContractAddress -> Amount -> Value -> ChangeSet
addContractStatesToCS cs addr amnt val =
  cs & newContractStates . at addr ?~ (amnt, val)

-- |NB: INVARIANT: This function expects that the contract already exists in the
-- changeset map. This will be true during execution since the only way a contract can
-- possibly send a message is if its local state exists in this map
addContractAmountToCS :: ChangeSet -> ContractAddress -> Amount -> ChangeSet
addContractAmountToCS cs addr amnt =
  cs & newContractStates . at addr . mapped . _1 .~ amnt

-- |A concrete implementation of TransactionMonad based on SchedulerMonad.
newtype LocalT m a = LocalT { _runLocalT :: ExceptT RejectReason (StateT (Energy, ChangeSet) m) a }
  deriving(Functor, Applicative, Monad, MonadState (Energy, ChangeSet), MonadError RejectReason)

runLocalT :: LocalT m a -> Energy -> m (Either RejectReason a, (Energy, ChangeSet))
runLocalT (LocalT st) energy = runStateT (runExceptT st) (energy, emptyCS)

{-# INLINE evalLocalT #-}
evalLocalT :: Monad m => LocalT m a -> Energy -> m (Either RejectReason a)
evalLocalT (LocalT st) energy = evalStateT (runExceptT st) (energy, emptyCS)

evalLocalT' :: Monad m => LocalT m a -> Energy -> m (Either RejectReason a, Energy)
evalLocalT' (LocalT st) energy = do (a, (energy', _)) <- runStateT (runExceptT st) (energy, emptyCS)
                                    return (a, energy')

execLocalT :: Monad m => LocalT m a -> Energy -> m (Energy, ChangeSet)
execLocalT (LocalT st) energy = execStateT (runExceptT st) (energy, emptyCS)

liftLocal :: Monad m => m a -> LocalT m a
liftLocal m = LocalT (ExceptT (StateT (\s -> do x <- m
                                                return (Right x, s))))

instance StaticEnvironmentMonad m => StaticEnvironmentMonad (LocalT m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = liftLocal getChainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces = liftLocal . getModuleInterfaces

instance SchedulerMonad m => TransactionMonad (LocalT m) where
  {-# INLINE withInstance #-}
  withInstance cref amount val cont = do
    modify' (\(en, s) -> (en, addContractStatesToCS s cref amount val))
    cont

  {-# INLINE withAmount #-}
  -- inlining may help with the common case where the address format is statically known
  withAmount addr amount cont = do
    case addr of
      AddressAccount acc -> do
        accdata <- liftLocal (fromJust <$> getAccount acc)
        modify' (\(en, s) -> (en, addAmountToCS s accdata amount))
      AddressContract addrc -> modify' (\(en, s) -> (en, addContractAmountToCS s addrc amount))

    cont

  getCurrentContractInstance addr = do
    newStates <- use (_2 . newContractStates)
    liftLocal $ do mistance <- getContractInstance addr
                   case mistance of
                     Nothing -> return Nothing
                     Just i -> case newStates ^. at addr of
                                 Nothing -> return $ Just i
                                 Just (amnt, newmodel) -> return $ Just (updateInstance amnt newmodel i)

  {-# INLINE getCurrentAccount #-}
  getCurrentAccount acc = do
    macc <- (^. at acc) <$> use (_2 . newAccounts)
    case macc of
      Nothing -> liftLocal $! getAccount acc
      Just a -> return $ Just a

  {-# INLINE getChanges #-}
  getChanges = use _2

  {-# INLINE getEnergy #-}
  getEnergy = use _1

  {-# INLINE tickEnergy #-}
  tickEnergy tick = do
    energy <- use _1
    if tick > energy then rejectTransaction OutOfEnergy
    else _1 -= tick

  {-# INLINE putEnergy #-}
  putEnergy en = _1 .= en

  {-# INLINE rejectTransaction #-}
  rejectTransaction = throwError

instance SchedulerMonad m => InterpreterMonad (LocalT m) where
  getCurrentContractState caddr = do
    newStates <- use (_2 . newContractStates)
    liftLocal $ do mistance <- getContractInstance caddr
                   case mistance of
                     Nothing -> return Nothing
                     Just i -> case newStates ^. at caddr of
                                 Nothing -> return $ Just (instanceImplements (instanceParameters i), instanceModel i)
                                 Just (_, newmodel) -> return $ Just (instanceImplements (instanceParameters i), newmodel)
