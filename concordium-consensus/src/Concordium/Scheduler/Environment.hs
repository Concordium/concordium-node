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
import Control.Monad.Cont hiding (cont)

import Data.Void
import Data.Word
import Lens.Micro.Platform

import qualified Acorn.Core as Core
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Cost as Cost
import Concordium.GlobalState.BlockState(AccountUpdate(..), auAmount, emptyAccountUpdate)
import qualified Concordium.Types.Acorn.Interfaces as Interfaces

import Control.Exception(assert)

import qualified Concordium.ID.Types as ID

-- * Scheduler monad

-- |Information needed to execute transactions in the form that is easy to use.
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
  commitChanges :: ChangeSet -> m ()

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed. Also store the code of the module for archival purposes.
  commitModule :: Core.ModuleRef -> Interface -> ValueInterface -> Module -> m Bool

  -- |Check whehter we already cache the expression in a linked format.
  -- It is valid for the implementation to always return 'Nothing', although this
  -- will affect memory use since linked expressions will not be shared.
  smTryGetLinkedExpr :: Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExprWithDeps Void))

  -- |Store the linked expression in the linked expresssion cache.
  -- It is valid for this to be a no-op.
  smPutLinkedExpr :: Core.ModuleRef -> Core.Name -> LinkedExprWithDeps Void -> m ()

  -- |Try to get a linked contract init and receive methods.
  -- It is valid for the implementation to always return 'Nothing', although this
  -- will affect memory use since each contract instance will have a different
  -- in-memory linked code.
  smTryGetLinkedContract :: Core.ModuleRef -> Core.TyName -> m (Maybe (LinkedContractValue Void))

  -- |Store a fully linked contract in the linked contracts cache.
  -- It is valid for the implementation to be a no-op.
  smPutLinkedContract :: Core.ModuleRef -> Core.TyName -> LinkedContractValue Void -> m ()

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: (ContractAddress -> Instance) -> m ContractAddress

  -- |Bump the next available transaction nonce of the account. The account is assumed to exist.
  increaseAccountNonce :: Account -> m ()

  -- |Add account credential to an account address. The account with this address is assumed to exist.
  addAccountCredential :: Account -> ID.CredentialDeploymentValues -> m ()

  -- |Add account encryption key to account address. The account with this address is assumed to exist.
  addAccountEncryptionKey :: Account -> ID.AccountEncryptionKey -> m ()

  -- |Create new account in the global state. Return @True@ if the account was
  --  successfully created and @False@ if the account address already existed.
  putNewAccount :: Account -> m Bool

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

  -- *Other metadata.

  -- |Retrieve the identity provider with given id, if possible.
  getIPInfo :: IdentityProviderIdentity -> m (Maybe IdentityProviderData)

  -- |Get cryptographic parameters for the current state.
  getCrypoParams :: m CryptographicParameters


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
  withInstanceState :: Instance -> Value -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withAccountToContractAmount :: Account -> Instance -> Amount -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withAccountToAccountAmount :: Account -> Account -> Amount -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withContractToAccountAmount :: Instance -> Account -> Amount -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withContractToContractAmount :: Instance -> Instance -> Amount -> m a -> m a

  {-# INLINE withToContractAmount #-}
  withToContractAmount :: Either Instance Account -> Instance -> Amount -> m a -> m a
  withToContractAmount (Left i) = withContractToContractAmount i
  withToContractAmount (Right a) = withAccountToContractAmount a

  {-# INLINE withToAccountAmount #-}
  withToAccountAmount :: Either Instance Account -> Account -> Amount -> m a -> m a
  withToAccountAmount (Left i) = withContractToAccountAmount i
  withToAccountAmount (Right a) = withAccountToAccountAmount a

  getCurrentAccount :: AccountAddress -> m (Maybe Account)

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Link an expression into an expression ready to run.
  -- The expression is part of the given module
  linkExpr :: Core.ModuleRef -> (UnlinkedExpr Void, Word64) -> m (LinkedExpr Void, Word64)

  -- |Link a contract's init, receive methods and implemented constraints.
  linkContract :: Core.ModuleRef -> Core.TyName -> UnlinkedContractValue Void -> m (LinkedContractValue Void)

  {-# INLINE getCurrentAmount #-}
  getCurrentAmount :: Either Instance Account -> m Amount
  getCurrentAmount (Left i) = getCurrentContractAmount i
  getCurrentAmount (Right a) = getCurrentAccountAmount a

  -- |Get the current amount on the given account. This value changes
  -- throughout the execution of the transaction.
  getCurrentAccountAmount :: Account -> m Amount

  -- |Same as above, but for contracts.
  getCurrentContractAmount :: Instance -> m Amount

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
  rejectingWith !c reason = c >>= \case Just a -> return a
                                        Nothing -> rejectTransaction reason


  -- |If the computation yields a @Right b@ result return it, otherwise fail the
  -- transaction after transforming the reject message.
  {-# INLINE rejectingWith' #-}
  rejectingWith' :: m (Either a b) -> (a -> RejectReason) -> m b
  rejectingWith' !c reason = c >>= \case Right b -> return b
                                         Left a -> rejectTransaction (reason a)


-- |The set of changes to be commited on a successful transaction.
data ChangeSet = ChangeSet
    {_accountUpdates :: !(Map.HashMap AccountAddress AccountUpdate) -- ^Accounts whose states changed.
    ,_instanceUpdates :: !(Map.HashMap ContractAddress (AmountDelta, Value)) -- ^Contracts whose states changed.
    ,_linkedExprs :: !(Map.HashMap (Core.ModuleRef, Core.Name) (LinkedExprWithDeps NoAnnot)) -- ^Newly linked expressions.
    ,_linkedContracts :: !(Map.HashMap (Core.ModuleRef, Core.TyName) (LinkedContractValue NoAnnot))
    }

makeLenses ''ChangeSet

emptyCS :: ChangeSet
emptyCS = ChangeSet Map.empty Map.empty Map.empty Map.empty

csWithAccountDelta :: AccountAddress -> AmountDelta -> ChangeSet
csWithAccountDelta addr !amnt =
  emptyCS & accountUpdates . at addr ?~ (emptyAccountUpdate addr & auAmount ?~ amnt)

-- |Record an update for the given account in the changeset. If the account is
-- not yet in the changeset it is created.
{-# INLINE addAmountToCS #-}
addAmountToCS :: Account -> AmountDelta -> ChangeSet -> ChangeSet
addAmountToCS acc !amnt !cs =
  cs & accountUpdates . at addr %~ (\case Just upd -> Just (upd & auAmount %~ \case Just x -> Just (x + amnt)
                                                                                    Nothing -> Just amnt
                                                           )
                                          Nothing -> Just (emptyAccountUpdate addr & auAmount ?~ amnt))

  where addr = acc ^. accountAddress

-- |Modify the amount on the given account in the changeset by a given delta.
-- It is assumed that the account is already in the changeset and that its balance
-- is already affected (the auAmount field is set).
{-# INLINE modifyAmountCS #-}
modifyAmountCS :: AccountAddress -> AmountDelta -> ChangeSet -> ChangeSet
modifyAmountCS addr !amnt !cs = cs & (accountUpdates . ix addr . auAmount ) %~
                                     (\case Just a -> Just (a + amnt)
                                            Nothing -> error "modifyAmountCS precondition violated.")


-- |Add or update the contract state in the changeset with the given value.
-- |NB: If the instance is not yet in the changeset we assume that its balance is
-- as listed in the given instance structure.
addContractStatesToCS :: Instance -> Value -> ChangeSet -> ChangeSet
addContractStatesToCS istance val cs =
  cs & instanceUpdates . at addr %~ \case Just (amnt, _) -> Just (amnt, val)
                                          Nothing -> Just (0, val)
  where addr = instanceAddress . instanceParameters $ istance

-- |Add the given delta to the change set for the given contract instance.
-- NB: If the contract is not yet in the changeset it is added, taking the
-- model as given in the first argument to be current model (local state)
addContractAmountToCS :: Instance -> AmountDelta -> ChangeSet -> ChangeSet
addContractAmountToCS istance amnt cs =
    (cs & instanceUpdates . at addr %~ \case Just (d, v) -> Just (d + amnt, v)
                                             Nothing -> Just (amnt, model))
  where addr = instanceAddress . instanceParameters $ istance
        model = instanceModel istance

data LocalState = LocalState{
  -- |Energy left for the computation.
  _energyLeft :: !Energy,
  -- |Changes accumulated thus far.
  _changeSet :: !ChangeSet
  }

makeLenses ''LocalState

data TransactionContext = TransactionContext{
  -- |Header of the transaction initiating the transaction.
  _tcTxSender :: !AccountAddress,
  _tcDepositedAmount :: !Amount
  }

makeLenses ''TransactionContext

-- |A concrete implementation of TransactionMonad based on SchedulerMonad. We
-- use the continuation monad transformer instead of the ExceptT transformer in
-- order to avoid expensive bind operation of the latter. The bind operation is
-- expensive because it needs to check at each step whether the result is @Left@
-- or @Right@.
newtype LocalT r m a = LocalT { _runLocalT :: ContT (Either RejectReason r) (RWST TransactionContext () LocalState m) a }
  deriving(Functor, Applicative, Monad, MonadState LocalState, MonadReader TransactionContext)

runLocalT :: SchedulerMonad m => LocalT a m a -> Amount -> AccountAddress -> Energy -> m (Either RejectReason a, LocalState)
runLocalT (LocalT st) _tcDepositedAmount _tcTxSender energy = do
  let s = LocalState energy emptyCS
  (a, s', ()) <- runRWST (runContT st (return . Right)) ctx s
  return (a, s')

  where ctx = TransactionContext{..}

{-# INLINE energyUsed #-}
-- |Compute how much energy was used from the upper bound in the header of a
-- transaction and the amount left.
energyUsed :: TransactionHeader -> Energy -> Energy
energyUsed meta energy = thGasAmount meta - energy

-- |Given the deposited amount and the remaining amount of gas compute how much
-- the sender of the transaction should be charged, as well as how much energy was used
-- for execution.
-- This function assumes that the deposited energy is not less than the used energy.
computeExecutionCharge :: SchedulerMonad m => TransactionHeader -> Energy -> m (Energy, Amount)
computeExecutionCharge meta energy =
  let used = energyUsed meta energy
  in (used, ) <$> energyToGtu used

-- |Reduce the public balance on the account to charge for execution cost. The
-- given amount is the amount to charge (subtract). The precondition of this
-- method is that the account exists and its balance is sufficient to
-- cover the costs. These are not checked.
--
-- NB: This method should only be used directly when the given account's balance
-- is the only one affected by the transaction, either because a transaction was
-- rejected, or because it was a transaction which only affects one account's
-- balance such as DeployCredential, or DeployModule.
chargeExecutionCost :: SchedulerMonad m => Account -> Amount -> m ()
chargeExecutionCost acc amnt =
    let balance = acc ^. accountAmount
    in do assert (balance >= amnt) $
              commitChanges (csWithAccountDelta (acc ^. accountAddress) (amountDiff 0 amnt))
          notifyExecutionCost amnt

-- |Given an account which is initiating the top-level transaction and the
-- deposited amount, run the given computation in the modified environment where
-- the balance on the account is decreased by the deposited amount. Return the
-- amount of energy __used__ by the computation and any result returned. The
-- function __ensures__ that the amount of energy is not more than the
-- deposited amount. The function __assumes__ the following
--
--   * The account exists in the account database.
--   * The deposited amount exists in the public account value.
--   * The deposited amount is __at least__ Cost.checkHeader (i.e., minimum transaction cost).
withDeposit ::
  SchedulerMonad m =>
  Account
  -- ^Address of the account initiating the transaction.
  -> TransactionHeader
  -- ^Header of the transaction we are running.
  -> LocalT a m a
  -- ^The computation to run in the modified environment with reduced amount on the initial account.
  -> (LocalState -> a -> m ValidResult)
  -- ^Continuation for the successful branch of the computation.
  -- It gets the result of the previous computation as input, in particular the
  -- remaining energy and the ChangeSet.
  -> m TxResult
withDeposit acc txHeader comp k = do
  let totalEnergyToUse = thGasAmount txHeader
  -- we assume we have already checked the header, so we have a bit less left over
  let energy = totalEnergyToUse - Cost.checkHeader
  -- record how much we have deposited. This cannot be touched during execution.
  depositedAmount <- energyToGtu totalEnergyToUse
  (res, ls) <- runLocalT comp depositedAmount (thSender txHeader) energy
  case res of
    Left reason -> do
      -- the only effect of this transaction is reduced balance
      -- compute how much we must charge and reject the transaction
      (usedEnergy, payment) <- computeExecutionCharge txHeader (ls ^. energyLeft)
      chargeExecutionCost acc payment
      return $! TxValid (TxReject reason payment usedEnergy)
    Right a ->
      -- in this case we invoke the continuation
      TxValid <$!> k ls a

{-# INLINE defaultSuccess #-}
-- |Default continuation to use with 'withDeposit'. It records events and charges for the energy
-- used, and nothing else.
defaultSuccess ::
  SchedulerMonad m =>
  TransactionHeader
  -> Account -> LocalState -> [Event] -> m ValidResult
defaultSuccess meta senderAccount = \ls events -> do
  (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
  chargeExecutionCost senderAccount energyCost
  commitChanges (ls ^. changeSet)
  return $ TxSuccess events energyCost usedEnergy

-- {-# INLINE evalLocalT #-}
-- evalLocalT :: Monad m => LocalT a m a -> Energy -> m (Either RejectReason a)
-- evalLocalT (LocalT st) energy = evalStateT (runContT st (return . Right)) (energy, emptyCS)

-- evalLocalT' :: Monad m => LocalT a m a -> Energy -> m (Either RejectReason a, Energy)
-- evalLocalT' (LocalT st) energy = do (a, (energy', _)) <- runStateT (runContT st (return . Right)) (energy, emptyCS)
--                                     return (a, energy')

-- execLocalT :: Monad m => LocalT a m a -> Energy -> m (Energy, ChangeSet)
-- execLocalT (LocalT st) energy = execStateT (runContT st (return . Right)) (energy, emptyCS)

{-# INLINE liftLocal #-}
liftLocal :: Monad m => m a -> LocalT r m a
liftLocal m = LocalT (ContT (\k -> RWST (\r s -> m >>= \f -> runRWST (k f) r s)))

instance StaticEnvironmentMonad Core.UA m => StaticEnvironmentMonad Core.UA (LocalT r m) where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = liftLocal getChainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces = liftLocal . getModuleInterfaces

instance SchedulerMonad m => LinkerMonad Void (LocalT r m) where
  {-# INLINE getExprInModule #-}
  getExprInModule mref n = liftLocal $
    getModuleInterfaces mref >>= \case
      Nothing -> return Nothing
      Just (_, viface) -> return $ Map.lookup n (viDefs viface)

  tryGetLinkedExpr mref n = liftLocal (smTryGetLinkedExpr mref n)
    
  putLinkedExpr mref n linked = liftLocal (smPutLinkedExpr mref n linked)


instance SchedulerMonad m => TransactionMonad (LocalT r m) where
  {-# INLINE withInstanceState #-}
  withInstanceState istance val cont = do
    changeSet %= addContractStatesToCS istance val
    cont

  {-# INLINE withAccountToAccountAmount #-}
  withAccountToAccountAmount fromAcc toAcc amount cont = do
    changeSet %= addAmountToCS toAcc (amountToDelta amount)
    changeSet %= addAmountToCS fromAcc (amountDiff 0 amount)
    cont

  {-# INLINE withAccountToContractAmount #-}
  withAccountToContractAmount fromAcc toAcc amount cont = do
    changeSet %= addContractAmountToCS toAcc (amountToDelta amount)
    changeSet %= addAmountToCS fromAcc (amountDiff 0 amount)
    cont

  {-# INLINE withContractToAccountAmount #-}
  withContractToAccountAmount fromAcc toAcc amount cont = do
    changeSet %= addAmountToCS toAcc (amountToDelta amount)
    changeSet %= addContractAmountToCS fromAcc (amountDiff 0 amount)
    cont

  {-# INLINE withContractToContractAmount #-}
  withContractToContractAmount fromAcc toAcc amount cont = do
    changeSet %= addContractAmountToCS toAcc (amountToDelta amount)
    changeSet %= addContractAmountToCS fromAcc (amountDiff 0 amount)
    cont

  getCurrentAccount addr =
    liftLocal (getAccount addr) >>= \case
      Just acc -> do
        amnt <- getCurrentAccountAmount acc
        return . Just $ (acc & accountAmount .~ amnt)
      Nothing -> return Nothing

  getCurrentContractInstance addr = do
    newStates <- use (changeSet . instanceUpdates)
    liftLocal $! do
      mistance <- getContractInstance addr
      case mistance of
        Nothing -> return Nothing
        Just i ->
          case newStates ^. at addr of
            Nothing -> return $ Just i
            Just (delta, newmodel) ->
              let !updated = updateInstance delta newmodel i
              in return (Just updated)

  {-# INLINE getCurrentAccountAmount #-}
  getCurrentAccountAmount acc = do
    let addr = acc ^. accountAddress
    let amnt = acc ^. accountAmount
    txCtx <- ask
    -- additional delta that arises due to the deposit
    let additionalDelta =
          if txCtx ^. tcTxSender == addr
          then amountDiff 0 (txCtx ^. tcDepositedAmount)
          else 0
    macc <- (^. at addr) <$> use (changeSet . accountUpdates)
    case macc of
      Just upd ->
        -- if we are looking up the account that initiated the transaction we also take into account
        -- the deposited amount
        return $ applyAmountDelta additionalDelta (applyAmountDelta (upd ^. auAmount . non 0) amnt)
      Nothing -> return (applyAmountDelta additionalDelta amnt)

  {-# INLINE getCurrentContractAmount #-}
  getCurrentContractAmount inst = do
    let amnt = instanceAmount inst
    let addr = instanceAddress . instanceParameters $ inst
    use (changeSet . instanceUpdates . at addr) >>= \case
      Just (delta, _) -> return $! applyAmountDelta delta amnt
      Nothing -> return amnt

  -- FIXME: Determine what is best ratio for energy/term size.
  linkExpr mref unlinked = do
    energy <- use energyLeft
    linkWithMaxSize mref unlinked (fromIntegral energy `div` 100) >>= \case
      Just (le, termSize) -> do
        tickEnergy (fromIntegral termSize * 100)
        return (leExpr le, termSize)
      Nothing -> rejectTransaction OutOfEnergy


  linkContract mref cname unlinked = do
    lCache <- use (changeSet . linkedContracts)
    case Map.lookup (mref, cname) lCache of
      Nothing ->
        liftLocal (smTryGetLinkedContract mref cname) >>= \case
          Nothing -> do
            cvInitMethod <- linkExpr mref (Interfaces.cvInitMethod unlinked)
            cvReceiveMethod <- linkExpr mref (Interfaces.cvReceiveMethod unlinked)
            cvImplements <- mapM (\iv -> do
                                     ivSenders <- mapM (linkExpr mref) (Interfaces.ivSenders iv)
                                     ivGetters <- mapM (linkExpr mref) (Interfaces.ivGetters iv)
                                     return Interfaces.ImplementsValue{..}
                                 ) (Interfaces.cvImplements unlinked)
            let linked = Interfaces.ContractValue{..}
            changeSet . linkedContracts %= (Map.insert (mref, cname) linked)
            return linked
          Just cv -> return cv
      Just cv -> return cv
      
  {-# INLINE getEnergy #-}
  getEnergy = use energyLeft

  {-# INLINE tickEnergy #-}
  tickEnergy !tick = do
    energy <- getEnergy
    if tick > energy then energyLeft .= 0 >> rejectTransaction OutOfEnergy  -- set remaining to 0
    else modify' (\ls -> ls { _energyLeft = energy - tick})

  {-# INLINE putEnergy #-}
  putEnergy en = energyLeft .= en

  {-# INLINE rejectTransaction #-}
  rejectTransaction reason = LocalT (ContT (\_ -> return (Left reason)))


instance SchedulerMonad m => InterpreterMonad NoAnnot (LocalT r m) where
  getCurrentContractState caddr = do
    newStates <- use (changeSet . instanceUpdates)
    liftLocal $! do
      mistance <- getContractInstance caddr
      case mistance of
        Nothing -> return Nothing
        Just i ->
          case newStates ^. at caddr of
            Nothing -> return $ Just (instanceImplements (instanceParameters i), instanceModel i)
            Just (_, newmodel) -> return $ Just (instanceImplements (instanceParameters i), newmodel)
