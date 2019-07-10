{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
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

import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Cont hiding (cont)

import Data.Maybe(fromJust)
import Lens.Micro.Platform

import qualified Acorn.Core as Core
import Concordium.Scheduler.Types
import Concordium.GlobalState.BlockState(AccountUpdate(..), auAmount, emptyAccountUpdate)

import Control.Exception(assert)
import Data.Maybe(isJust)

import qualified Concordium.ID.Types as ID

-- * Scheduler monad

-- |Information needed to execute transactions.
class StaticEnvironmentMonad Core.UA m => SchedulerMonad m where
  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the amount of funds at the particular account address.
  -- To get the amount of funds for a contract instance use getInstance and lookup amount there.
  getAccount :: AccountAddress -> m (Maybe Account)

  -- |Check whether a given registration id exists in the global store.
  accountRegIdExists :: ID.CredentialRegistrationID -> m Bool

  -- |Commit to global state all the updates to local state that have
  -- accumulated through the execution. This method is also in charge of
  -- recording which accounts were affected by the transaction for reward and
  -- other purposes.
  commitStateAndAccountChanges :: ChangeSet -> m ()

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed. Also store the code of the module for archival purposes.
  commitModule :: Core.ModuleRef -> Interface -> ValueInterface -> Module -> m Bool

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: (ContractAddress -> Instance) -> m ContractAddress

  -- |Bump the next available transaction nonce of the account. The account is assumed to exist.
  increaseAccountNonce :: AccountAddress -> m ()

  -- |Add account credential to an account address. The account with this address is assumed to exist.
  addAccountCredential :: AccountAddress -> ID.CredentialDeploymentValues -> m ()

  -- |Add account encryption key to account address. The account with this address is assumed to exist.
  addAccountEncryptionKey :: AccountAddress -> ID.AccountEncryptionKey -> m ()

  -- |Create new account in the global state. Return @True@ if the account was
  -- successfully created and @False@ if the account address already existed.
  putNewAccount :: Account -> m Bool

  -- |Reduce the public balance on the account to charge for execution cost. The
  -- given amount is the amount to charge (subtract). The precondition of this
  -- method is that the account address exists and its balance is sufficient to
  -- cover the costs. These are not checked. This method should only be used
  -- directly when the given account's balance is the only one affected by the
  -- transaction, either because a transaction was rejected, or because it was a
  -- transaction such which only affects one account's balance such as
  -- DeployCredential.
  chargeExecutionCost :: AccountAddress -> Amount -> m ()
  chargeExecutionCost addr amnt = do
    macc <- getAccount addr
    assert (isJust macc) $ do
      case macc of
        Nothing -> error "chargeExecutionCost precondition violated."
        Just acc -> let balance = acc ^. accountAmount
                    in do assert (balance >= amnt) $ commitStateAndAccountChanges (csWithAccountBalance addr (balance - amnt))
                          notifyExecutionCost amnt

  -- |Notify the global state that the amount was charged for execution. This
  -- can be then reimbursed to the baker, or some other logic can be implemented
  -- on top of it.
  notifyExecutionCost :: Amount -> m ()

  -- |Notify that an identity provider had a valid credential on the sender's
  -- account and should be rewarded because of it.
  notifyIdentityProviderCredential :: ID.IdentityProviderIdentity -> m ()

  -- |Convert the given energy amount into a the amount of GTU. The exchange
  -- rate can vary depending on the current state of the blockchain.
  -- TODO: In this setup the exchange rate is determined by the blockchain, and
  -- the user (aka sender of the transaction) cannot choose to pay more to have
  -- their transaction prioritised. If the user can choose to do so then this
  -- function needs to be replaced.
  energyToGtu :: Energy -> m Amount

  -- *Operations related to bakers.

  -- |Get the baker information, or 'Nothing'.
  getBakerInfo :: BakerId -> m (Maybe BakerInfo)

  -- |Add a new baker with a fresh baker id.
  -- Moreover also update the next available baker id.
  addBaker :: BakerCreationInfo -> m BakerId

  -- |Remove a baker with the given id from the baker pool.
  removeBaker :: BakerId -> m ()

  -- |Replace the given baker's verification key with the given value.
  -- The function may assume that the baker exists.
  updateBakerSignKey :: BakerId -> BakerSignVerifyKey -> m ()

  -- |Replace the given baker's reward account with the given value.
  -- The function may assume that the baker exists.
  updateBakerAccount :: BakerId -> AccountAddress -> m ()

  -- |Delegate the stake from an account to a baker. The account is
  -- assumed to exist, although the baker is not.  Returns 'True'
  -- if the delegation was successful, and 'False' if the baker is
  -- not valid.
  delegateStake :: AccountAddress -> Maybe BakerId -> m Bool

-- |This is a derived notion that is used inside a transaction to keep track of
-- the state of the world during execution. Local state of contracts and amounts
-- on contracts might need to be rolled back for various reasons, so we do not
-- want to commit it to global state.
class StaticEnvironmentMonad Core.UA m => TransactionMonad m where
  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need not provide it
  -- separately.
  withInstance :: ContractAddress -> Amount -> Value -> m a -> m a

  -- |And the same for amounts on accounts and contracts. The amounts can change
  -- due to simple transfers and contract calls.
  withAmount :: Address -> Amount -> m a -> m a

  -- |Specialized 'withAmount' with default implementation.
  withAccountAmount :: AccountAddress -> Amount -> m a -> m a
  withAccountAmount = withAmount . AddressAccount

  -- |Specialized 'withAmount' with default implementation.
  withContractAmount :: ContractAddress -> Amount -> m a -> m a
  withContractAmount = withAmount . AddressContract

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the current amount on the given account address. This value changes
  -- throughout the execution of the transaction.
  getCurrentAmount :: AccountAddress -> m (Maybe Amount)

  -- |Get the amount of gas remaining for the transaction.
  getEnergy :: m Energy

  -- |Decrease the remaining energy by the given amount. If not enough is left
  -- reject the transaction and set remaining amount to 0
  tickEnergy :: Energy -> m ()

  -- |Set the remaining energy to be the given value.
  putEnergy :: Energy -> m ()

  -- |Reject a transaction with a given reason, terminating processing of this transaction.
  rejectTransaction :: RejectReason -> m a

  -- |If the computation yields a @Just a@ result return it, otherwise fail the
  -- transaction with the given reason.
  {-# INLINE rejectingWith #-}
  rejectingWith :: m (Maybe a) -> RejectReason -> m a
  rejectingWith c reason = c >>= \case Just a -> return a
                                       Nothing -> rejectTransaction reason


  -- |If the computation yields a @Right b@ result return it, otherwise fail the
  -- transaction after transforming the reject message.
  {-# INLINE rejectingWith' #-}
  rejectingWith' :: m (Either a b) -> (a -> RejectReason) -> m b
  rejectingWith' c reason = c >>= \case Right b -> return b
                                        Left a -> rejectTransaction (reason a)


-- |The set of changes to be commited on a successful transaction.
data ChangeSet = ChangeSet
    {_accountUpdates :: !(Map.HashMap AccountAddress AccountUpdate) -- ^Accounts whose states changed.
    ,_instanceUpdates :: !(Map.HashMap ContractAddress (Amount, Value)) -- ^Contracts whose states changed.
    }

emptyCS :: ChangeSet
emptyCS = ChangeSet Map.empty Map.empty

csWithAccountBalance :: AccountAddress -> Amount -> ChangeSet
csWithAccountBalance addr !amnt = ChangeSet (Map.singleton addr (emptyAccountUpdate addr & auAmount ?~ amnt)) Map.empty

makeLenses ''ChangeSet

-- |Update the amount on the account (given by address) in the changeset with
-- the given amount. If the account is not yet in the changeset it is created.
addAmountToCS' :: ChangeSet -> AccountAddress -> Amount -> ChangeSet
addAmountToCS' !cs addr !amnt =
  cs & accountUpdates . at addr %~ (\case Just upd -> Just (upd & auAmount ?~ amnt)
                                          Nothing -> Just (emptyAccountUpdate addr & auAmount ?~ amnt))

-- |Increase the amount on the given account in the changeset.
-- It is assumed that the account is already in the changeset and that its balance
-- is already affected (the auAmount field is set).
increaseAmountCS :: ChangeSet -> AccountAddress -> Amount -> ChangeSet
increaseAmountCS !cs addr !amnt = cs & (accountUpdates . ix addr . auAmount ) %~
                                     (\case Just a -> Just $! (a + amnt)
                                            Nothing -> error "increaaseAmountCS precondition violated.")

-- |Modify the amount on the given account in the changeset by a given delta.
-- It is assumed that the account is already in the changeset and that its balance
-- is already affected (the auAmount field is set).
modifyAmountCS :: ChangeSet -> AccountAddress -> AmountDelta -> ChangeSet
modifyAmountCS !cs addr !amnt = cs & (accountUpdates . ix addr . auAmount ) %~
                                     (\case Just a -> Just (applyAmountDelta amnt a)
                                            Nothing -> error "increaaseAmountCS precondition violated.")

-- |Update the amount on the account in the changeset with the given amount.
-- If the account is not yet in the changeset it is created.
addAmountToCS :: ChangeSet -> Account -> Amount -> ChangeSet
addAmountToCS cs !acc = addAmountToCS' cs (acc ^. accountAddress)

-- |Add or update the contract state in the changeset with the given amount and value.
addContractStatesToCS :: ChangeSet -> ContractAddress -> Amount -> Value -> ChangeSet
addContractStatesToCS cs addr amnt val =
  cs & instanceUpdates . at addr ?~ (amnt, val)

-- |NB: INVARIANT: This function expects that the contract already exists in the
-- changeset map. This will be true during execution since the only way a contract can
-- possibly send a message is if its local state exists in this map
addContractAmountToCS :: ChangeSet -> ContractAddress -> Amount -> ChangeSet
addContractAmountToCS cs addr amnt =
  cs & instanceUpdates . at addr . mapped . _1 .~ amnt

-- |A concrete implementation of TransactionMonad based on SchedulerMonad. We
-- use the continuation monad transformer instead of the ExceptT transformer in
-- order to avoid expensive bind operation of the latter. The bind operation is
-- expensive because it needs to check at each step whether the result is @Left@
-- or @Right@.
newtype LocalT r m a = LocalT { _runLocalT :: ContT (Either RejectReason r) (StateT (Energy, ChangeSet) m) a }
  deriving(Functor, Applicative, Monad, MonadState (Energy, ChangeSet))

runLocalT :: Monad m => LocalT a m a -> Energy -> m (Either RejectReason a, (Energy, ChangeSet))
runLocalT (LocalT st) energy = runStateT (runContT st (return . Right)) (energy, emptyCS)

runLocalTWithAmount ::
  SchedulerMonad m =>
  AccountAddress     -- ^Address of the account initiating the transaction.
  -> Amount          -- ^The balance on the account after subtracting the deposit for gas.
  -> LocalT a m a    -- ^The computation to run in the modified environment with reduced amount on the initial account.
  -> Energy          -- ^Amount of gas allowed to be consumed.
  -> m (Either RejectReason a, (Energy, ChangeSet))
runLocalTWithAmount addr amount st = runLocalT (withAccountAmount addr amount st)

{-# INLINE evalLocalT #-}
evalLocalT :: Monad m => LocalT a m a -> Energy -> m (Either RejectReason a)
evalLocalT (LocalT st) energy = evalStateT (runContT st (return . Right)) (energy, emptyCS)

evalLocalT' :: Monad m => LocalT a m a -> Energy -> m (Either RejectReason a, Energy)
evalLocalT' (LocalT st) energy = do (a, (energy', _)) <- runStateT (runContT st (return . Right)) (energy, emptyCS)
                                    return (a, energy')

execLocalT :: Monad m => LocalT a m a -> Energy -> m (Energy, ChangeSet)
execLocalT (LocalT st) energy = execStateT (runContT st (return . Right)) (energy, emptyCS)

{-# INLINE liftLocal #-}
liftLocal :: Monad m => m a -> LocalT r m a
liftLocal m = LocalT (ContT (\k -> StateT (\s -> m >>= flip runStateT s . k)))
                                                    
instance StaticEnvironmentMonad Core.UA m => StaticEnvironmentMonad Core.UA (LocalT r m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = liftLocal getChainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces = liftLocal . getModuleInterfaces

instance SchedulerMonad m => TransactionMonad (LocalT r m) where
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
    newStates <- use (_2 . instanceUpdates)
    liftLocal $ do mistance <- getContractInstance addr
                   case mistance of
                     Nothing -> return Nothing
                     Just i -> case newStates ^. at addr of
                                 Nothing -> return $ Just i
                                 Just (amnt, newmodel) -> return $ Just (updateInstance amnt newmodel i)

  {-# INLINE getCurrentAmount #-}
  getCurrentAmount acc = do
    macc <- (^. at acc) <$> use (_2 . accountUpdates)
    case macc of
      Just upd | Just a <- upd ^. auAmount -> return (Just a)
      _ -> liftLocal $! ((^. accountAmount) <$>) <$> getAccount acc
      
  {-# INLINE getEnergy #-}
  getEnergy = use _1

  {-# INLINE tickEnergy #-}
  tickEnergy tick = do
    energy <- use _1
    if tick > energy then _1 .= 0 >> rejectTransaction OutOfEnergy  -- set remaining to 0
    else _1 -= tick

  {-# INLINE putEnergy #-}
  putEnergy en = _1 .= en

  {-# INLINE rejectTransaction #-}
  rejectTransaction reason = LocalT (ContT (\_ -> return (Left reason)))

instance SchedulerMonad m => InterpreterMonad NoAnnot (LocalT r m) where
  getCurrentContractState caddr = do
    newStates <- use (_2 . instanceUpdates)
    liftLocal $ do mistance <- getContractInstance caddr
                   case mistance of
                     Nothing -> return Nothing
                     Just i -> case newStates ^. at caddr of
                                 Nothing -> return $ Just (instanceImplements (instanceParameters i), instanceModel i)
                                 Just (_, newmodel) -> return $ Just (instanceImplements (instanceParameters i), newmodel)
