{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.Scheduler.Environment where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

import Control.Monad.RWS.Strict
import Control.Monad.Cont hiding (cont)

import Lens.Micro.Platform

import qualified Acorn.Core as Core
import Acorn.Types(InterpreterEnergy)
import Concordium.Scheduler.Types
import qualified Concordium.Scheduler.Cost as Cost
import Concordium.GlobalState.BlockState(AccountUpdate(..), auAmount, emptyAccountUpdate)
import Concordium.GlobalState.BakerInfo(BakerError)
import qualified Concordium.Types.Acorn.Interfaces as Interfaces
import Concordium.GlobalState.AccountTransactionIndex

import Control.Exception(assert)

import qualified Concordium.ID.Types as ID

type SpecialBetaAccounts = Set.HashSet AccountAddress

-- |Whether the current energy limit is block energy or current transaction energy.
data EnergyLimitReason = BlockEnergy | TransactionEnergy
    deriving(Eq, Show)

emptySpecialBetaAccounts :: SpecialBetaAccounts
emptySpecialBetaAccounts = Set.empty

-- |A class to convert to and from 'Energy' used by the scheduler.
-- The function should satisfy
--
--   * @toEnergy (fromEnergy x) <= x@
class ResourceMeasure a where
  toEnergy :: a -> Energy
  fromEnergy :: Energy -> a

instance ResourceMeasure Energy where
  {-# INLINE toEnergy #-}
  toEnergy = id
  {-# INLINE fromEnergy #-}
  fromEnergy = id

-- |Measures the cost of linking.
instance ResourceMeasure LinkedTermSize where
  {-# INLINE toEnergy #-}
  toEnergy = Cost.link
  {-# INLINE fromEnergy #-}
  fromEnergy = Cost.maxLink

-- |Meaures the cost of running the interpreter.
instance ResourceMeasure InterpreterEnergy where
  {-# INLINE toEnergy #-}
  toEnergy = Cost.fromInterpreterEnergy
  {-# INLINE fromEnergy #-}
  fromEnergy = Cost.toInterpreterEnergy

-- |Measures the cost of __storing__ the given amount of bytes.
instance ResourceMeasure ByteSize where
  {-# INLINE toEnergy #-}
  toEnergy = Cost.storeBytes
  {-# INLINE fromEnergy #-}
  fromEnergy = Cost.maxStorage

-- |Measures the cost of __looking up__ the given amount of bytes.
instance ResourceMeasure Cost.LookupByteSize where
  {-# INLINE toEnergy #-}
  toEnergy = Cost.lookupBytes
  {-# INLINE fromEnergy #-}
  fromEnergy = Cost.maxLookup

-- * Scheduler monad

-- |Information needed to execute transactions in the form that is easy to use.
class (CanRecordFootprint (Footprint (ATIStorage m)), StaticEnvironmentMonad Core.UA m) => SchedulerMonad m where

  tlNotifyAccountEffect :: Footprint (ATIStorage m) -> TransactionSummary -> m ()

  -- |Get maximum allowed block energy.
  getMaxBlockEnergy :: m Energy

  -- |Get adddresses of special beta accounts which during the beta phase will
  -- have special privileges.
  getSpecialBetaAccounts :: m SpecialBetaAccounts

  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the amount of funds at the particular account address.
  -- To get the amount of funds for a contract instance use getInstance and lookup amount there.
  getAccount :: AccountAddress -> m (Maybe Account)

  -- |Check whether a given registration id exists in the global state.
  accountRegIdExists :: ID.CredentialRegistrationID -> m Bool

  -- |Commit to global state all the updates to local state that have
  -- accumulated through the execution. This method is also in charge of
  -- recording which accounts were affected by the transaction for reward and
  -- other purposes.
  -- Precondition: Each account affected in the change set must exist in the
  -- block state.
  commitChanges :: ChangeSet -> m ()

  -- |Observe a single transaction footprint.
  observeTransactionFootprint :: m a -> m (a, Footprint (ATIStorage m))

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed. Also store the code of the module for archival purposes.
  commitModule :: Core.ModuleRef -> Interface -> ValueInterface -> Module -> m Bool

  -- |Check whether we already cache the expression in a linked format.
  -- It is valid for the implementation to always return 'Nothing', although this
  -- will affect memory use since linked expressions will not be shared.
  smTryGetLinkedExpr :: Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExprWithDeps NoAnnot))

  -- |Store the linked expression in the linked expresssion cache.
  -- It is valid for this to be a no-op.
  smPutLinkedExpr :: Core.ModuleRef -> Core.Name -> LinkedExprWithDeps NoAnnot -> m ()

  -- |Try to get a linked contract init and receive methods.
  -- It is valid for the implementation to always return 'Nothing', although this
  -- will affect memory use since each contract instance will have a different
  -- in-memory linked code.
  smTryGetLinkedContract :: Core.ModuleRef -> Core.TyName -> m (Maybe (LinkedContractValue NoAnnot))

  -- |Store a fully linked contract in the linked contracts cache.
  -- It is valid for the implementation to be a no-op.
  smPutLinkedContract :: Core.ModuleRef -> Core.TyName -> LinkedContractValue NoAnnot -> m ()

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: (ContractAddress -> Instance) -> m ContractAddress

  -- |Bump the next available transaction nonce of the account.
  -- Precondition: the account exists in the block state.
  increaseAccountNonce :: Account -> m ()

  -- FIXME: This method should not be here, but rather in the transaction monad.
  -- |Add account credential to an account address.
  -- Precondition: The account with this address exists in the block state.
  addAccountCredential :: Account -> ID.CredentialDeploymentValues -> m ()

  -- |Create new account in the global state. Return @True@ if the account was
  --  successfully created and @False@ if the account address already existed.
  putNewAccount :: Account -> m Bool

  -- |Notify energy used by the current execution.
  -- Add to the current running total of energy used.
  markEnergyUsed :: Energy -> m ()

  -- |Get the currently used amount of block energy.
  getUsedEnergy :: m Energy

  getRemainingEnergy :: m Energy
  getRemainingEnergy = do
    maxEnergy <- getMaxBlockEnergy
    usedEnergy <- getUsedEnergy
    return $! if usedEnergy <= maxEnergy then maxEnergy - usedEnergy else 0

  -- |Get the next transaction index in the block, and increase the internal counter
  bumpTransactionIndex :: m TransactionIndex

  -- |Notify the global state that the amount was charged for execution. This
  -- can be then reimbursed to the baker, or some other logic can be implemented
  -- on top of it.
  notifyExecutionCost :: Amount -> m ()

  -- |Notify that an identity provider had a valid credential on the sender's
  -- account and should be rewarded because of it.
  notifyIdentityProviderCredential :: ID.IdentityProviderIdentity -> m ()

  -- |Convert the given energy amount into an amount of GTU. The exchange
  -- rate can vary depending on the current state of the blockchain.
  -- TODO: In this setup the exchange rate is determined by the blockchain, and
  -- the user (aka sender of the transaction) cannot choose to pay more to have
  -- their transaction prioritised. If the user can choose to do so then this
  -- function needs to be replaced.
  energyToGtu :: Energy -> m Amount

  -- *Operations related to bakers.

  -- |Get the baker information, or 'Nothing' if a baker with the given baker id
  -- doesn't exist.
  getBakerAccountAddress :: BakerId -> m (Maybe AccountAddress)

  -- |Add a new baker with a fresh baker id.
  -- Moreover also update the next available baker id.
  -- If succesful, return the baker's ID
  -- If the baker's signature key or aggregation key is already in used it returns
  -- a baker error (either DuplicateSignKey or DuplicateAggregationKey)
  addBaker :: BakerInfo -> m (Either BakerError BakerId)

  -- |Remove a baker with the given id from the baker pool.
  removeBaker :: BakerId -> m ()

  -- |Replace the given baker's verification key with the given value.
  -- Return 'True' if the signing key was updated, and 'False' in case
  -- it lead to a duplicate signing key.
  -- Precondition: the baker exists.
  updateBakerSignKey :: BakerId -> BakerSignVerifyKey -> m Bool

  -- |Replace the given baker's reward account with the given value.
  -- Precondition: the baker exists and the reward account
  -- also exists in the global state.
  updateBakerAccount :: BakerId -> AccountAddress -> m ()

  -- |Replace the given baker's aggregation verification key with the given
  -- value. Return true if the key was succesfully updated and false in case
  -- it leads to a duplicate aggregation key.
  -- Precondition: The baker exists.
  updateBakerAggregationKey :: BakerId -> BakerAggregationVerifyKey -> m Bool

  -- |Replace the given baker's election verification key with the given key
  -- Return true if the key was succesfully updated and false otherwise.
  -- Precondition: the baker exists.
  updateBakerElectionKey :: BakerId -> BakerElectionVerifyKey -> m ()

  -- |Delegate the stake from an account to a baker. The baker is not assumed to exist.
  -- Returns 'True' if the delegation was successful, and 'False' if the baker is
  -- not valid.
  -- Delegating to `Nothing` undelegates the stake from any baker that it was delegated
  -- to in the past.
  -- Precondition: the account exists.
  delegateStake :: AccountAddress -> Maybe BakerId -> m Bool

  -- |Update the election difficulty (birk parameter) in the global state.
  updateElectionDifficulty :: ElectionDifficulty -> m ()

  -- *Other metadata.

  -- |Retrieve the identity provider with given id, if possible.
  getIPInfo :: IdentityProviderIdentity -> m (Maybe IpInfo)

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

  -- |Transfer an amount from the first account to the second and run the
  -- computation in the modified environment.
  withAccountToAccountAmount :: Account -> Account -> Amount -> m a -> m a

  -- |Transfer an amount from the given instance to the given account and run the
  -- computation in the modified environment.
  withContractToAccountAmount :: Instance -> Account -> Amount -> m a -> m a

  -- |Transfer an amount from the first instance to the second and run the
  -- computation in the modified environment.
  withContractToContractAmount :: Instance -> Instance -> Amount -> m a -> m a

  -- |Transfer an amount from the first given instance or account to the instance in the second
  -- parameter and run the computation in the modified environment.
  {-# INLINE withToContractAmount #-}
  withToContractAmount :: Either Instance Account -> Instance -> Amount -> m a -> m a
  withToContractAmount (Left i) = withContractToContractAmount i
  withToContractAmount (Right a) = withAccountToContractAmount a

  -- |Transfer an amount from the first given instance or account to the account in the second
  -- parameter and run the computation in the modified environment.
  {-# INLINE withToAccountAmount #-}
  withToAccountAmount :: Either Instance Account -> Account -> Amount -> m a -> m a
  withToAccountAmount (Left i) = withContractToAccountAmount i
  withToAccountAmount (Right a) = withAccountToAccountAmount a

  getCurrentAccount :: AccountAddress -> m (Maybe Account)

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Link an expression into an expression ready to run.
  -- This charges for the size of the linked expression. The linker is run with the current remaining
  -- energy, and if that is not sufficient to pay for the size of the resulting expression, it will
  -- abort when this limit is reached, and this function will reject the transaction with 'OutOfEnergy'
  -- or outOfBlockEnergy.
  -- The expression is part of the given module.
  linkExpr :: Core.ModuleRef -> (UnlinkedExpr NoAnnot, UnlinkedTermSize) -> m (LinkedExpr NoAnnot)

  -- |Link a contract's init, receive methods and implemented constraints.
  linkContract :: Core.ModuleRef -> Core.TyName -> UnlinkedContractValue NoAnnot -> m (LinkedContractValue NoAnnot)

  {-# INLINE getCurrentAmount #-}
  getCurrentAmount :: Either Instance Account -> m Amount
  getCurrentAmount (Left i) = getCurrentContractAmount i
  getCurrentAmount (Right a) = getCurrentAccountAmount a

  -- |Get the current amount on the given account. This value changes
  -- throughout the execution of the transaction.
  getCurrentAccountAmount :: Account -> m Amount

  -- |Same as above, but for contracts.
  getCurrentContractAmount :: Instance -> m Amount

  -- |Get the amount of energy remaining for the transaction.
  getEnergy :: m (Energy, EnergyLimitReason)

  -- |Decrease the remaining energy by the given amount. If not enough is left
  -- reject the transaction and set remaining amount to 0.
  -- If block energy limit would be reached instead, then reject the transaction
  -- with 'outOfBlockEnergy' instead.
  tickEnergy :: Energy -> m ()

  -- |Reject a transaction with a given reason, terminating processing of this transaction.
  -- If the reason is OutOfEnergy this function __must__ ensure that the remaining energy
  -- is set to 0.
  rejectTransaction :: RejectReason -> m a

  -- |Fail transaction processing because we would have exceeded maximum block energy limit.
  outOfBlockEnergy :: m a

  -- |If the computation yields a @Just a@ result return it, otherwise fail the
  -- transaction with the given reason.
  -- If the reject message is 'OutOfEnergy' this function __shall__ ensure
  -- that no energy is left.
  {-# INLINE rejectingWith #-}
  rejectingWith :: m (Maybe a) -> RejectReason -> m a
  rejectingWith !c reason = c >>= \case Just a -> return a
                                        Nothing -> rejectTransaction reason


  -- |If the computation yields a @Right b@ result return it, otherwise fail the
  -- transaction after transforming the reject message.
  -- If the resulting reject message is 'OutOfEnergy' this function __shall__ ensure
  -- that no energy is left.
  {-# INLINE rejectingWith' #-}
  rejectingWith' :: m (Either a b) -> (a -> RejectReason) -> m b
  rejectingWith' !c reason = c >>= \case Right b -> return b
                                         Left a -> rejectTransaction (reason a)


-- |The set of changes to be commited on a successful transaction.
data ChangeSet = ChangeSet
    {_affectedTx :: !TransactionHash, -- transaction affected by this changeset
     _accountUpdates :: !(Map.HashMap AccountAddress AccountUpdate) -- ^Accounts whose states changed.
    ,_instanceUpdates :: !(Map.HashMap ContractAddress (AmountDelta, Value)) -- ^Contracts whose states changed.
    ,_linkedExprs :: !(Map.HashMap (Core.ModuleRef, Core.Name) (LinkedExprWithDeps NoAnnot)) -- ^Newly linked expressions.
    ,_linkedContracts :: !(Map.HashMap (Core.ModuleRef, Core.TyName) (LinkedContractValue NoAnnot))
    }

makeLenses ''ChangeSet

emptyCS :: TransactionHash -> ChangeSet
emptyCS txHash = ChangeSet txHash Map.empty Map.empty Map.empty Map.empty

csWithAccountDelta :: TransactionHash -> AccountAddress -> AmountDelta -> ChangeSet
csWithAccountDelta txHash addr !amnt =
  (emptyCS txHash) & accountUpdates . at addr ?~ (emptyAccountUpdate addr & auAmount ?~ amnt)

-- |Record an addition to the amount of the given account in the changeset.
{-# INLINE addAmountToCS #-}
addAmountToCS :: Account -> AmountDelta -> ChangeSet -> ChangeSet
addAmountToCS acc !amnt !cs =
  -- Check whether there already is an 'AccountUpdate' for the given account in the changeset.
  -- If so, modify it accordingly, otherwise add a new entry.
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

-- |Whether the transaction energy limit is reached because of transaction max energy limit,
-- or because of block energy limit
data LimitReason = TransactionHeader | BlockEnergyLimit

data LocalState = LocalState{
  -- |Energy left for the computation.
  _energyLeft :: !Energy,
  -- |Changes accumulated thus far.
  _changeSet :: !ChangeSet,
  _blockEnergyLeft :: !Energy
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
newtype LocalT r m a = LocalT { _runLocalT :: ContT (Either (Maybe RejectReason) r) (RWST TransactionContext () LocalState m) a }
  deriving(Functor, Applicative, Monad, MonadState LocalState, MonadReader TransactionContext)

runLocalT :: SchedulerMonad m
          => LocalT a m a
          -> TransactionHash
          -> Amount
          -> AccountAddress
          -> Energy -- Energy limit by the transaction header.
          -> Energy -- remaining block energy
          -> m (Either (Maybe RejectReason) a, LocalState)
runLocalT (LocalT st) txHash _tcDepositedAmount _tcTxSender _energyLeft _blockEnergyLeft = do
  let s = LocalState{_changeSet = emptyCS txHash,..}
  (a, s', ()) <- runRWST (runContT st (return . Right)) ctx s
  return (a, s')

  where !ctx = TransactionContext{..}

{-# INLINE energyUsed #-}
-- |Compute how much energy was used from the upper bound in the header of a
-- transaction and the amount left.
energyUsed :: TransactionHeader -> Energy -> Energy
energyUsed meta energy = thEnergyAmount meta - energy

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
chargeExecutionCost :: SchedulerMonad m => TransactionHash -> Account -> Amount -> m ()
chargeExecutionCost txHash acc amnt =
    let balance = acc ^. accountAmount
    in do assert (balance >= amnt) $
              commitChanges (csWithAccountDelta txHash (acc ^. accountAddress) (amountDiff 0 amnt))
          notifyExecutionCost amnt

data WithDepositContext = WithDepositContext{
  _wtcSenderAccount :: !Account,
  -- ^Address of the account initiating the transaction.
  _wtcTransactionType :: !TransactionType,
  -- ^Type of the top-level transaction.
  _wtcTransactionHash :: !TransactionHash,
  -- ^Hash of the top-level transaction.
  _wtcTransactionHeader :: !TransactionHeader,
  -- ^Header of the transaction we are running.
  _wtcTransactionCheckHeaderCost :: !Energy,
  -- ^Cost to be charged for checking the transaction header.
  _wtcCurrentlyUsedBlockEnergy :: !Energy,
  -- ^Energy currently used by the block.
  _wtcTransactionIndex :: !TransactionIndex
  -- ^Index of the transaction in a block.
  }

makeLenses ''WithDepositContext

-- |Given an account which is initiating the top-level transaction and the
-- deposited amount, run the given computation in the modified environment where
-- the balance on the account is decreased by the deposited amount. Return the
-- amount of energy __used__ by the computation and any result returned. The
-- function __ensures__ that the amount of energy is not more than the
-- deposited amount. The function __assumes__ the following
--
--   * The account exists in the account database.
--   * The deposited amount exists in the public account value.
--   * The deposited amount is __at least__ Cost.checkHeader applied to the respective parameters (i.e., minimum transaction cost).
withDeposit ::
  SchedulerMonad m
  => WithDepositContext
  -> LocalT a m a
  -- ^The computation to run in the modified environment with reduced amount on the initial account.
  -> (LocalState -> a -> m (ValidResult, Amount, Energy))
  -- ^Continuation for the successful branch of the computation.
  -- It gets the result of the previous computation as input, in particular the
  -- remaining energy and the ChangeSet. It should return the result, and the amount that was charged
  -- for the execution.
  -> m (Maybe TransactionSummary)
withDeposit wtc comp k = do
  let txHeader = wtc ^. wtcTransactionHeader
  let tsHash = wtc ^. wtcTransactionHash
  let totalEnergyToUse = thEnergyAmount txHeader
  maxEnergy <- getMaxBlockEnergy
  -- - here is safe due to precondition that currently used energy is less than the maximum block energy
  let beLeft = maxEnergy - wtc ^. wtcCurrentlyUsedBlockEnergy
  -- we assume we have already checked the header, so we have a bit less left over
  let energy = totalEnergyToUse - wtc ^. wtcTransactionCheckHeaderCost
  -- record how much we have deposited. This cannot be touched during execution.
  depositedAmount <- energyToGtu totalEnergyToUse
  (res, ls) <- runLocalT comp tsHash depositedAmount (thSender txHeader) energy beLeft
  case res of
    -- Failure: maximum block energy exceeded
    Left Nothing -> return Nothing
    -- Failure: transaction fails (out of energy or actual failure by transaction logic)
    Left (Just reason) -> do
      -- The only effect of this transaction is that the sender is charged for the execution cost
      -- (energy ticked so far).
      (usedEnergy, payment) <- computeExecutionCharge txHeader (ls ^. energyLeft)
      chargeExecutionCost tsHash (wtc ^. wtcSenderAccount) payment
      return $! Just $! TransactionSummary{
        tsSender = Just (thSender txHeader),
        tsCost = payment,
        tsEnergyCost = usedEnergy,
        tsResult = TxReject reason,
        tsType = Just (wtc ^. wtcTransactionType),
        tsIndex = wtc ^. wtcTransactionIndex,
        ..
        }
    -- Computation successful
    Right a -> do
      -- In this case we invoke the continuation, which should charge for the used energy.
      (tsResult, tsCost, tsEnergyCost) <- k ls a
      return $! Just $! TransactionSummary{
        tsSender = Just (thSender txHeader),
        tsType = Just (wtc ^. wtcTransactionType),
        tsIndex = wtc ^. wtcTransactionIndex,
        ..
        }

{-# INLINE defaultSuccess #-}
-- |Default continuation to use with 'withDeposit'. It charges for the energy used, commits the changes
-- from the current changeset and returns the recorded events, the amount corresponding to the
-- used energy and the used energy.
defaultSuccess ::
  SchedulerMonad m => WithDepositContext -> LocalState -> [Event] -> m (ValidResult, Amount, Energy)
defaultSuccess wtc = \ls events -> do
  let txHash = wtc ^. wtcTransactionHash
      meta = wtc ^. wtcTransactionHeader
      senderAccount = wtc ^. wtcSenderAccount
  (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
  chargeExecutionCost txHash senderAccount energyCost
  commitChanges (ls ^. changeSet)
  return $ (TxSuccess events, energyCost, usedEnergy)

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

instance SchedulerMonad m => LinkerMonad NoAnnot (LocalT r m) where
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
    !txCtx <- ask
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

  linkExpr mref unlinked = withExternal (linkExprWithMaxSize mref unlinked)

  linkContract mref cname unlinked = do
    lCache <- use (changeSet . linkedContracts)
    case Map.lookup (mref, cname) lCache of
      Nothing ->
        liftLocal (smTryGetLinkedContract mref cname) >>= \case
          Nothing -> do
            cvInitMethod <- linkExprWithSize mref (Interfaces.cvInitMethod unlinked)
            cvReceiveMethod <- linkExprWithSize mref (Interfaces.cvReceiveMethod unlinked)
            cvImplements <- mapM (\iv -> do
                                     ivSenders <- mapM (linkExprWithSize mref) (Interfaces.ivSenders iv)
                                     ivGetters <- mapM (linkExprWithSize mref) (Interfaces.ivGetters iv)
                                     return Interfaces.ImplementsValue{..}
                                 ) (Interfaces.cvImplements unlinked)
            let linked = Interfaces.ContractValue{..}
            changeSet . linkedContracts %= (Map.insert (mref, cname) linked)
            return linked
          Just cv -> return cv
      Just cv -> return cv
      where
        -- like linkExpr above, but retains the size
        linkExprWithSize mref' unlinked' = withExternal (fmap (fmap (\(a,b) -> ((a,b), b))) . linkExprWithMaxSize mref' unlinked')

  {-# INLINE getEnergy #-}
  getEnergy = do
    beLeft <- use blockEnergyLeft
    txLeft <- use energyLeft
    if beLeft < txLeft
    then return (beLeft, BlockEnergy)
    else return (txLeft, TransactionEnergy)

  {-# INLINE tickEnergy #-}
  tickEnergy !tick = do
    (energy, reason) <- getEnergy
    if tick > energy then
      case reason of
        BlockEnergy -> outOfBlockEnergy
        TransactionEnergy -> rejectTransaction OutOfEnergy  -- NB: sets the remaining energy to 0
    else do
      energyLeft -= tick
      blockEnergyLeft -= tick

  {-# INLINE rejectTransaction #-}
  rejectTransaction OutOfEnergy = energyLeft .= 0 >> LocalT (ContT (\_ -> return (Left (Just OutOfEnergy))))
  rejectTransaction reason = LocalT (ContT (\_ -> return (Left (Just reason))))

  {-# INLINE outOfBlockEnergy #-}
  outOfBlockEnergy = LocalT (ContT (\_ -> return (Left Nothing)))

-- |Call an external method that can fail with running out of energy.
-- Depending on what is the current limit, either remaining transaction energy,
-- or remaining block energy, this function will report the appropriate failure.
--
-- This function takes an action that:
--
-- * Takes a 'ResourceMeasure' to which the remaining energy should be converted.
-- * Should return __how much of the 'ResourceMeasure' was used__ (which is then converted
--   back to 'Energy'), or 'Nothing' to signal failure because of running out of the 'ResourceMeasure'.
-- * Should satisfy that the returned energy is <= given energy, even though
--   this is not necessary for the correctness of this function. In the case
--   where the returned energy exceeds remaining energy this function will
--   return either with 'OutOfEnergy' or 'outOfBlockEnergy'.
withExternal :: (ResourceMeasure r, TransactionMonad m) => (r ->  m (Maybe (a, r))) -> m a
withExternal f = do
  (availableEnergy, reason) <- getEnergy
  f (fromEnergy availableEnergy) >>= \case
    Nothing | BlockEnergy <- reason -> outOfBlockEnergy
    Nothing | TransactionEnergy <- reason -> rejectTransaction OutOfEnergy -- this sets remaining to 0
    Just (result, usedEnergy) -> do
      -- tickEnergy is safe even if usedEnergy > available energy, even though this case
      -- should not happen for well-behaved actions.
      tickEnergy (toEnergy usedEnergy)
      return result

-- |Like 'withExternal' but takes a pure action that only transforms energy and does
-- not return a value. This is a convenience wrapper only.
withExternalPure_ :: (ResourceMeasure r, TransactionMonad m) => (r -> Maybe r) -> m ()
withExternalPure_ f = withExternal (return . fmap ((),) . f)

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
