{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.Scheduler.Environment where

import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Map as Map
import Data.Foldable

import Control.Monad.Cont hiding (cont)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, runStateT)

import Lens.Micro.Platform

import Concordium.Logger
import Concordium.Crypto.EncryptedTransfers
import Concordium.Utils
import qualified Concordium.Wasm as Wasm
import Concordium.Scheduler.Types
import qualified Concordium.Cost as Cost
import Concordium.GlobalState.Types
import Concordium.GlobalState.Classes (MGSTrans(..))
import Concordium.GlobalState.Account (EncryptedAmountUpdate(..), AccountUpdate(..), auAmount, auEncrypted, auReleaseSchedule, emptyAccountUpdate, stakedAmount)
import Concordium.GlobalState.BlockState (AccountOperations(..))
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.AccountTransactionIndex
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as ARS

import Control.Exception(assert)

import qualified Concordium.ID.Types as ID

-- |Whether the current energy limit is block energy or current transaction energy.
data EnergyLimitReason = BlockEnergy | TransactionEnergy
    deriving(Eq, Show)

-- * Scheduler monad

class (Monad m) => StaticInformation m where
  -- |Get the chain information for the current block.
  getChainMetadata :: m ChainMetadata

  -- |Get a module interface, if available.
  getModuleInterfaces :: ModuleRef -> m (Maybe Wasm.ModuleInterface)

  -- |Get maximum allowed block energy.
  getMaxBlockEnergy :: m Energy

  -- |Get maximum number of account creation transactions per block.
  getAccountCreationLimit :: m CredentialsPerBlockLimit

-- |Information needed to execute transactions in the form that is easy to use.
class (Monad m, StaticInformation m, CanRecordFootprint (Footprint (ATIStorage m)), AccountOperations m, MonadLogger m, IsProtocolVersion pv)
    => SchedulerMonad pv m | m -> pv where

  -- |Notify the transaction log that a transaction had the given footprint. The
  -- nature of the footprint will depend on the configuration, e.g., it could be
  -- nothing, or the set of accounts affected by the transaction.
  tlNotifyAccountEffect :: Footprint (ATIStorage m) -> TransactionSummary -> m ()

  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe Instance)

  -- |Get the amount of funds at the particular account address.
  -- To get the amount of funds for a contract instance use getInstance and lookup amount there.
  getAccount :: AccountAddress -> m (Maybe (Account m))

  -- |Get the 'AccountIndex' for an account, if it exists.
  getAccountIndex :: AccountAddress -> m (Maybe AccountIndex)

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
  commitModule :: (Wasm.ModuleInterface, Wasm.WasmModule) -> m Bool

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: (ContractAddress -> Instance) -> m ContractAddress

  -- |Bump the next available transaction nonce of the account.
  -- Precondition: the account exists in the block state.
  increaseAccountNonce :: Account m -> m ()

  -- FIXME: This method should not be here, but rather in the transaction monad.
  -- |Update account credentials.
  -- Preconditions:
  -- - The account exists in the block state.
  -- - The account threshold is reasonable.
  updateAccountCredentials :: Account m
                        -> [ID.CredentialIndex]
                        -- ^ The indices of credentials to remove from the account.
                        -> Map.Map ID.CredentialIndex ID.AccountCredential
                        -- ^ The new credentials to add.
                        -> ID.AccountThreshold
                        -- ^ The new account threshold
                        -> m ()

  -- |Create and add an empty account with the given public key, address and credential.
  -- If an account with the given address already exists, @Nothing@ is returned.
  -- Otherwise, the new account is returned, and the credential is added to the known credentials.
  --
  -- It is not checked if the account's credential is a duplicate.
  createAccount :: CryptographicParameters -> AccountAddress -> ID.AccountCredential -> m (Maybe (Account m))

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

  -- |Record that the amount was charged for execution. Amount is distributed
  -- at the end of block execution in accordance with the tokenomics principles.
  notifyExecutionCost :: Amount -> m ()

  -- |Notify the state that an amount has been transferred from public to
  -- encrypted or vice-versa.
  notifyEncryptedBalanceChange :: AmountDelta -> m ()

  -- |Convert the given energy amount into an amount of GTU. The exchange
  -- rate can vary depending on the current state of the blockchain.
  -- TODO: In this setup the exchange rate is determined by the blockchain, and
  -- the user (aka sender of the transaction) cannot choose to pay more to have
  -- their transaction prioritised. If the user can choose to do so then this
  -- function needs to be replaced.
  energyToGtu :: Energy -> m Amount

  -- *Operations related to bakers.

  -- |Register this account as a baker.
  -- The following results are possible:
  --
  -- * @BASuccess id@: the baker was created with the specified 'BakerId'.
  --   @id@ is always chosen to be the account index.
  --
  -- * @BAInvalidAccount@: the address does not resolve to a valid account.
  --
  -- * @BAAlreadyBaker@: the account is already registered as a baker.
  --
  -- * @BAInsufficientBalance@: the balance on the account is insufficient to
  --   stake the specified amount.
  --
  -- * @BADuplicateAggregationKey@: the aggregation key is already in use.
  --
  -- Note that if two results could apply, the first in this list takes precedence.  
  addBaker :: AccountAddress -> BakerAdd -> m BakerAddResult

  -- |Remove the baker associated with an account.
  -- The removal takes effect after a cooling-off period.
  -- Removal may fail if the baker is already cooling-off from another change (e.g. stake reduction).
  --
  -- The following results are possible:
  --
  -- * @BRRemoved e@: the baker was removed, and will be in cooling-off until epoch @e@.
  --   The change will take effect in epoch @e+1@.
  --
  -- * @BRInvalidBaker@: the account address is not valid, or the account is not a baker.
  --
  -- * @BRChangePending@: the baker is currently in a cooling-off period and so cannot be removed.
  removeBaker :: AccountAddress -> m BakerRemoveResult

  -- |Update the keys associated with an account.
  -- It is assumed that the keys have already been checked for validity/ownership as
  -- far as is necessary.
  -- The only check on the keys is that the aggregation key is not a duplicate.
  --
  -- The following results are possible:
  --
  -- * @BKUSuccess@: the keys were updated
  --
  -- * @BKUInvalidBaker@: the account does not exist or is not currently a baker.
  --
  -- * @BKUDuplicateAggregationKey@: the aggregation key is a duplicate.
  updateBakerKeys :: AccountAddress -> BakerKeyUpdate -> m BakerKeyUpdateResult

  -- |Update the stake associated with an account.
  -- A reduction in stake will be delayed by the current cool-off period.
  -- A change will not be made if there is already a cooling-off change
  -- pending for the baker.
  --
  -- The following results are possible:
  --
  -- * @BSUStakeIncreased@: the baker's stake was increased.
  --   This will take effect in the epoch after next.
  --
  -- * @BSUStakeReduced e@: the baker's stake was reduced.
  --   This will cool-off until epoch @e@ and take effect in epoch @e+1@.
  --
  -- * @BSUStakeUnchanged@: there is no change to the baker's stake, but this update was successful.
  --
  -- * @BSUInvalidBaker@: the account does not exist, or is not currently a baker.
  --
  -- * @BSUChangePending@: the change could not be made since the account is already in a cooling-off period.
  --
  -- * @BSUInsufficientBalance@: the account does not have sufficient balance to cover the staked amount.
  updateBakerStake :: AccountAddress -> Amount -> m BakerStakeUpdateResult

  -- |Update whether the baker automatically restakes the rewards it earns.
  --
  -- The following results are possible:
  --
  -- * @BREUUpdated id@: the flag was updated.
  --
  -- * @BREUInvalidBaker@: the account does not exists, or is not currently a baker.
  updateBakerRestakeEarnings :: AccountAddress -> Bool -> m BakerRestakeEarningsUpdateResult

  -- *Operations on account keys

  -- | Updates the credential verification keys 
  -- Preconditions:
  -- * The account exists
  -- * The account has keys defined at the specified indices
  updateCredentialKeys :: AccountAddress -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m ()

  -- *Other metadata.

  -- |Retrieve the identity provider with given id, if possible.
  getIPInfo :: IdentityProviderIdentity -> m (Maybe IpInfo)

  -- |Retrieve the identity provider with given id, if possible.
  getArInfos :: [ID.ArIdentity] -> m (Maybe [ArInfo])

  -- |Get cryptographic parameters for the current state.
  getCryptoParams :: m CryptographicParameters

  -- * Chain updates

  -- |Get the current authorized keys for updates.
  getUpdateKeyCollection :: m UpdateKeysCollection

  -- |Get the next sequence number of updates of a given type.
  getNextUpdateSequenceNumber :: UpdateType -> m UpdateSequenceNumber

  -- |Add an update to the relevant update queue. The update is
  -- assumed to have the next sequence number for its update type.
  -- The next sequence number will be correspondingly incremented,
  -- and any queued updates of the given type with a later effective
  -- time are cancelled.
  enqueueUpdate :: TransactionTime -> UpdateValue -> m ()



-- |This is a derived notion that is used inside a transaction to keep track of
-- the state of the world during execution. Local state of contracts and amounts
-- on contracts might need to be rolled back for various reasons, so we do not
-- want to commit it to global state.
class (StaticInformation m, IsProtocolVersion pv) => TransactionMonad pv m | m -> pv where
  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need not provide it
  -- separately.
  withInstanceState :: Instance -> Wasm.ContractState -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withAccountToContractAmount :: Account m -> Instance -> Amount -> m a -> m a

  -- |Transfer an amount from the first account to the second and run the
  -- computation in the modified environment.
  withAccountToAccountAmount :: Account m -> Account m -> Amount -> m a -> m a

  -- |Transfer an amount from the given instance to the given account and run the
  -- computation in the modified environment.
  withContractToAccountAmount :: Instance -> Account m -> Amount -> m a -> m a

  -- |Transfer an amount from the first instance to the second and run the
  -- computation in the modified environment.
  withContractToContractAmount :: Instance -> Instance -> Amount -> m a -> m a

  -- |Transfer a scheduled amount from the first address to the second and run
  -- the computation in the modified environment.
  withScheduledAmount :: Account m -> Account m -> Amount -> [(Timestamp, Amount)] -> TransactionHash -> m a -> m a

  -- |Replace encrypted amounts on an account up to (but not including) the
  -- given limit with a new amount.
  replaceEncryptedAmount :: Account m -> EncryptedAmountAggIndex -> EncryptedAmount -> m ()

  -- |Replace encrypted amounts on an account up to (but not including) the
  -- given limit with a new amount, as well as adding the given amount to the
  -- public balance of the account
  addAmountFromEncrypted :: Account m -> Amount -> EncryptedAmountAggIndex -> EncryptedAmount -> m ()

  -- |Add a new encrypted amount to an account, and return its index.
  -- This may assume this is the only update to encrypted amounts on the given account
  -- in this transaction.
  --
  -- This should be used on the receiver's account when an encrypted amount is
  -- sent to it.
  addEncryptedAmount :: Account m -> EncryptedAmount -> m EncryptedAmountIndex

  -- |Add an encrypted amount to the self-balance of an account.
  -- This may assume this is the only update to encrypted amounts on the given account
  -- in this transaction.
  --
  -- This should be used when transferring from public to encrypted balance.
  addSelfEncryptedAmount :: Account m -> Amount -> EncryptedAmount -> m ()

  -- |Transfer an amount from the first given instance or account to the instance in the second
  -- parameter and run the computation in the modified environment.
  {-# INLINE withToContractAmount #-}
  withToContractAmount :: Either (Account m, Instance) (Account m) -> Instance -> Amount -> m a -> m a
  withToContractAmount (Left (_, i)) = withContractToContractAmount i
  withToContractAmount (Right a) = withAccountToContractAmount a

  -- |Transfer an amount from the first given instance or account to the account in the second
  -- parameter and run the computation in the modified environment.
  {-# INLINE withToAccountAmount #-}
  withToAccountAmount :: Either (Account m, Instance) (Account m) -> Account m -> Amount -> m a -> m a
  withToAccountAmount (Left (_, i)) = withContractToAccountAmount i
  withToAccountAmount (Right a) = withAccountToAccountAmount a

  getCurrentContractInstance :: ContractAddress -> m (Maybe Instance)

  {-# INLINE getCurrentAvailableAmount #-}
  getCurrentAvailableAmount :: Either (Account m, Instance) (Account m) -> m Amount
  getCurrentAvailableAmount (Left (_, i)) = getCurrentContractAmount i
  getCurrentAvailableAmount (Right a) = getCurrentAccountAvailableAmount a

  -- |Get an account with its state at the start of the transaction.
  getStateAccount :: AccountAddress -> m (Maybe (Account m))

  -- |Get the current total public balance of an account.
  -- This accounts for any pending changes in the course of execution of the transaction.
  -- This includes any funds that cannot be spent due to lock-up or baking.
  getCurrentAccountTotalAmount :: Account m -> m Amount

  -- |Get the current available public balance of an account.
  -- This accounts for any pending changes in the course of execution of the transaction.
  -- The available balance excludes funds that are locked due to a lock-up release schedule
  -- or due to being staked for baking, or both.  That is @available = total - max locked staked@.
  getCurrentAccountAvailableAmount :: Account m -> m Amount

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

  -- |Try to run the first computation. If it leads to `reject` for a logic reason then
  -- try the second computation. If the left computation fails with out of energy then the
  -- entire computation is aborted.
  orElse :: m a -> m a -> m a

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
    {_affectedTx :: !TransactionHash, -- ^Transaction affected by this changeset.
     _accountUpdates :: !(HMap.HashMap AccountAddress AccountUpdate) -- ^Accounts whose states changed.
    ,_instanceUpdates :: !(HMap.HashMap ContractAddress (AmountDelta, Wasm.ContractState)) -- ^Contracts whose states changed.
    ,_instanceInits :: !(HSet.HashSet ContractAddress) -- ^Contracts that were initialized.
    ,_encryptedChange :: !AmountDelta -- ^Change in the encrypted balance of the system as a result of this contract's execution.
    ,_addedReleaseSchedules :: !(Map.Map AccountAddress Timestamp) -- ^The release schedules added to accounts on this block, to be added on the per block map.
    }

makeLenses ''ChangeSet

emptyCS :: TransactionHash -> ChangeSet
emptyCS txHash = ChangeSet txHash HMap.empty HMap.empty HSet.empty 0 Map.empty

csWithAccountDelta :: TransactionHash -> AccountAddress -> AmountDelta -> ChangeSet
csWithAccountDelta txHash addr !amnt =
  (emptyCS txHash) & accountUpdates . at addr ?~ (emptyAccountUpdate addr & auAmount ?~ amnt)

-- |Record an addition to the amount of the given account in the changeset.
{-# INLINE addAmountToCS #-}
addAmountToCS :: (AccountOperations m) => Account m -> AmountDelta -> ChangeSet -> m ChangeSet
addAmountToCS acc !amnt !cs = do
  addr <- getAccountAddress acc
  -- Check whether there already is an 'AccountUpdate' for the given account in the changeset.
  -- If so, modify it accordingly, otherwise add a new entry.
  return $ cs & accountUpdates . at addr %~ (\case Just upd -> Just (upd & auAmount %~ \case
                                                                       Just x -> Just (x + amnt)
                                                                       Nothing -> Just amnt
                                                                    )
                                                   Nothing -> Just (emptyAccountUpdate addr & auAmount ?~ amnt))

-- |Record a list of scheduled releases that has to be pushed into the global map and into the map of the account.
{-# INLINE addScheduledAmountToCS #-}
addScheduledAmountToCS :: AccountOperations m => Account m -> ([(Timestamp, Amount)], TransactionHash) -> ChangeSet -> m ChangeSet
addScheduledAmountToCS _ ([], _) cs = return cs
addScheduledAmountToCS acc rel@(((fstRel, _):_), _) !cs = do
  addr <- getAccountAddress acc
  return $ cs & accountUpdates . at addr %~ (\case Just upd -> Just (upd & auReleaseSchedule %~ Just . maybe [rel] (rel :))
                                                   Nothing -> Just (emptyAccountUpdate addr & auReleaseSchedule ?~ [rel]))
              & addedReleaseSchedules %~ (Map.alter (\case
                                                        Nothing -> Just fstRel
                                                        Just rel' -> Just $ min fstRel rel') addr)

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
addContractStatesToCS :: Instance -> Wasm.ContractState -> ChangeSet -> ChangeSet
addContractStatesToCS istance newState =
  instanceUpdates . at addr %~ \case Just (amnt, _) -> Just (amnt, newState)
                                     Nothing -> Just (0, newState)
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

-- |Add the given contract address to the set of initialized contract instances.
-- As the changes on the blockstate are already performed in the handler for this operation,
-- we just log the contract address as we don't need to modify the blockstate with
-- the information we add to the change set.
{-# INLINE addContractInitToCS #-}
addContractInitToCS :: Instance -> ChangeSet -> ChangeSet
addContractInitToCS istance cs =
    cs { _instanceInits = HSet.insert addr (cs ^. instanceInits) }
  where addr = instanceAddress . instanceParameters $ istance

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

-- |A monad transformer adding reading an environment of type @r@
-- and updating a state of type @s@ to an inner monad @m@.
type RST r s m = ReaderT r (StateT s m)

-- |Unwrap an RST computation as a function.
runRST :: RST r s m a -> r -> s -> m (a, s)
runRST rst r s = flip runStateT s . flip runReaderT r $ rst

-- |A concrete implementation of TransactionMonad based on SchedulerMonad. We
-- use the continuation monad transformer instead of the ExceptT transformer in
-- order to avoid expensive bind operation of the latter. The bind operation is
-- expensive because it needs to check at each step whether the result is @Left@
-- or @Right@.
newtype LocalT r m a = LocalT { _runLocalT :: ContT (Either (Maybe RejectReason) r) (RST TransactionContext LocalState m) a }
  deriving(Functor, Applicative, Monad, MonadState LocalState, MonadReader TransactionContext)

runLocalT :: SchedulerMonad pv m
          => LocalT a m a
          -> TransactionHash
          -> Amount
          -> AccountAddress
          -> Energy -- Energy limit by the transaction header.
          -> Energy -- remaining block energy
          -> m (Either (Maybe RejectReason) a, LocalState)
runLocalT (LocalT st) txHash _tcDepositedAmount _tcTxSender _energyLeft _blockEnergyLeft = do
  let s = LocalState{_changeSet = emptyCS txHash,..}
  (a, s') <- runRST (runContT st (return . Right)) ctx s
  return (a, s')

  where !ctx = TransactionContext{..}

instance BlockStateTypes (LocalT r m) where
    type BlockState (LocalT r m) = BlockState m
    type UpdatableBlockState (LocalT r m) = UpdatableBlockState m
    type Account (LocalT r m) = Account m

{-# INLINE energyUsed #-}
-- |Compute how much energy was used from the upper bound in the header of a
-- transaction and the amount left.
energyUsed :: TransactionHeader -> Energy -> Energy
energyUsed meta energy = thEnergyAmount meta - energy

-- |Given the deposited amount and the remaining amount of gas compute how much
-- the sender of the transaction should be charged, as well as how much energy was used
-- for execution.
-- This function assumes that the deposited energy is not less than the used energy.
computeExecutionCharge :: SchedulerMonad pv m => TransactionHeader -> Energy -> m (Energy, Amount)
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
chargeExecutionCost :: (AccountOperations m) => SchedulerMonad pv m => TransactionHash -> Account m -> Amount -> m ()
chargeExecutionCost txHash acc amnt = do
    balance <- getAccountAmount acc
    addr <- getAccountAddress acc
    assert (balance >= amnt) $
          commitChanges (csWithAccountDelta txHash addr (amountDiff 0 amnt))
    notifyExecutionCost amnt

data WithDepositContext m = WithDepositContext{
  _wtcSenderAccount :: !(Account m),
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
  SchedulerMonad pv m
  => WithDepositContext m
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
        tsType = TSTAccountTransaction $ Just $ wtc ^. wtcTransactionType,
        tsIndex = wtc ^. wtcTransactionIndex,
        ..
        }
    -- Computation successful
    Right a -> do
      -- In this case we invoke the continuation, which should charge for the used energy.
      (tsResult, tsCost, tsEnergyCost) <- k ls a
      return $! Just $! TransactionSummary{
        tsSender = Just (thSender txHeader),
        tsType = TSTAccountTransaction $ Just $ wtc ^. wtcTransactionType,
        tsIndex = wtc ^. wtcTransactionIndex,
        ..
        }

{-# INLINE defaultSuccess #-}
-- |Default continuation to use with 'withDeposit'. It charges for the energy used, commits the changes
-- from the current changeset and returns the recorded events, the amount corresponding to the
-- used energy and the used energy.
defaultSuccess ::
  SchedulerMonad pv m => WithDepositContext m -> LocalState -> [Event] -> m (ValidResult, Amount, Energy)
defaultSuccess wtc = \ls events -> do
  let txHash = wtc ^. wtcTransactionHash
      meta = wtc ^. wtcTransactionHeader
      senderAccount = wtc ^. wtcSenderAccount
  (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
  chargeExecutionCost txHash senderAccount energyCost
  commitChanges (ls ^. changeSet)
  return (TxSuccess events, energyCost, usedEnergy)

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
liftLocal m = LocalT (ContT (\k -> (lift . lift) m >>= k))


instance MonadTrans (LocalT r) where
  {-# INLINE lift #-}
  lift = liftLocal

instance StaticInformation m => StaticInformation (LocalT r m) where
  {-# INLINE getMaxBlockEnergy #-}
  getMaxBlockEnergy = liftLocal getMaxBlockEnergy

  {-# INLINE getChainMetadata #-}
  getChainMetadata = liftLocal getChainMetadata

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces = liftLocal . getModuleInterfaces

  {-# INLINE getAccountCreationLimit #-}
  getAccountCreationLimit = liftLocal getAccountCreationLimit

deriving via (MGSTrans (LocalT r) m) instance AccountOperations m => AccountOperations (LocalT r m)

instance SchedulerMonad pv m => TransactionMonad pv (LocalT r m) where
  {-# INLINE withInstanceState #-}
  withInstanceState istance val cont = do
    changeSet %= addContractStatesToCS istance val
    cont

  {-# INLINE withAccountToAccountAmount #-}
  withAccountToAccountAmount fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ (addAmountToCS toAcc (amountToDelta amount) cs >>=
                  addAmountToCS fromAcc (amountDiff 0 amount))
    cont

  {-# INLINE withAccountToContractAmount #-}
  withAccountToContractAmount fromAcc toAcc amount cont = do
    cs <- changeSet <%= addContractAmountToCS toAcc (amountToDelta amount)
    changeSet <~ addAmountToCS fromAcc (amountDiff 0 amount) cs
    cont

  {-# INLINE withContractToAccountAmount #-}
  withContractToAccountAmount fromAcc toAcc amount cont = do
    cs <- use changeSet
    cs' <- addAmountToCS toAcc (amountToDelta amount) cs
    changeSet .= addContractAmountToCS fromAcc (amountDiff 0 amount) cs'
    cont

  {-# INLINE withContractToContractAmount #-}
  withContractToContractAmount fromAcc toAcc amount cont = do
    changeSet %= addContractAmountToCS toAcc (amountToDelta amount)
    changeSet %= addContractAmountToCS fromAcc (amountDiff 0 amount)
    cont

  {-# INLINE withScheduledAmount #-}
  withScheduledAmount fromAcc toAcc sentAmount releases txh cont = do
    cs <- use changeSet
    cs' <- addAmountToCS fromAcc (amountDiff 0 sentAmount) cs
    cs'' <- addScheduledAmountToCS toAcc (releases, txh) cs'
    changeSet .= cs''
    cont

  replaceEncryptedAmount acc aggIndex newAmount = do
    addr <- getAccountAddress acc
    changeSet . accountUpdates . at' addr . non (emptyAccountUpdate addr) . auEncrypted ?= ReplaceUpTo{..}

  addAmountFromEncrypted acc amount aggIndex newAmount = do
    replaceEncryptedAmount acc aggIndex newAmount
    cs <- use changeSet
    cs' <- addAmountToCS acc (amountToDelta amount) cs
    changeSet .= cs'
    changeSet . encryptedChange += amountDiff 0 amount

  addEncryptedAmount acc newAmount = do
    addr <- getAccountAddress acc
    changeSet . accountUpdates . at' addr . non (emptyAccountUpdate addr) . auEncrypted ?= Add{..}
    getAccountEncryptedAmountNextIndex acc

  addSelfEncryptedAmount acc transferredAmount newAmount = do
    addr <- getAccountAddress acc
    cs <- use changeSet
    changeSet <~ addAmountToCS acc (amountDiff 0 transferredAmount) cs
    changeSet . accountUpdates . at' addr . non (emptyAccountUpdate addr) . auEncrypted ?= AddSelf{..}
    changeSet . encryptedChange += amountToDelta transferredAmount

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

  {-# INLINE getStateAccount #-}
  getStateAccount = liftLocal . getAccount

  getCurrentAccountTotalAmount acc = do
    addr <- getAccountAddress acc
    oldTotal <- getAccountAmount acc
    !txCtx <- ask
    -- If the account is the sender, subtract the deposit
    let netDeposit = if txCtx ^. tcTxSender == addr
          then oldTotal - (txCtx ^. tcDepositedAmount)
          else oldTotal
    macc <- use (changeSet . accountUpdates . at addr)
    case macc of
      Just upd -> do
        -- Apply any pending delta and add any new scheduled releases
        let newReleases = case upd ^. auReleaseSchedule of
                Nothing -> 0
                Just l -> foldl' (\t l' -> t + foldl' (+) 0 (snd <$> fst l')) 0 l
        return $ applyAmountDelta (upd ^. auAmount . non 0) netDeposit + newReleases
      Nothing -> return netDeposit

  getCurrentAccountAvailableAmount acc = do
    addr <- getAccountAddress acc
    oldTotal <- getAccountAmount acc
    oldLockedUp <- ARS._totalLockedUpBalance <$> getAccountReleaseSchedule acc
    bkr <- getAccountBaker acc
    let staked = maybe 0 (^. stakedAmount) bkr
    !txCtx <- ask
    -- If the account is the sender, subtract the deposit
    let netDeposit = if txCtx ^. tcTxSender == addr
          then oldTotal - (txCtx ^. tcDepositedAmount)
          else oldTotal
    macc <- use (changeSet . accountUpdates . at addr)
    case macc of
      Just upd -> do
        -- Apply any pending delta and add any new scheduled releases
        let newReleases = case upd ^. auReleaseSchedule of
                Nothing -> 0
                Just l -> foldl' (\t l' -> t + foldl' (+) 0 (snd <$> fst l')) 0 l
        return $ applyAmountDelta (upd ^. auAmount . non 0) netDeposit + newReleases
                  - max (oldLockedUp + newReleases) staked
      Nothing -> return $ netDeposit - max oldLockedUp staked

  {-# INLINE getCurrentContractAmount #-}
  getCurrentContractAmount inst = do
    let amnt = instanceAmount inst
    let addr = instanceAddress . instanceParameters $ inst
    use (changeSet . instanceUpdates . at addr) >>= \case
      Just (delta, _) -> return $! applyAmountDelta delta amnt
      Nothing -> return amnt

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

  {-# INLINE orElse #-}
  orElse (LocalT l) (LocalT r) = LocalT $ ContT $ \k -> do
     initChangeSet <- use changeSet
     runContT l k >>= \case
       Left (Just reason) | reason /= OutOfEnergy -> do
         -- reset changeSet, the left computation will have no effect at all other than
         -- energy use.
         changeSet .= initChangeSet
         runContT r k
       x -> return x

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
withExternal :: (Cost.ResourceMeasure r, TransactionMonad pv m) => (r ->  m (Maybe (a, r))) -> m a
withExternal f = do
  (availableEnergy, reason) <- getEnergy
  f (Cost.fromEnergy availableEnergy) >>= \case
    Nothing | BlockEnergy <- reason -> outOfBlockEnergy
    Nothing | TransactionEnergy <- reason -> rejectTransaction OutOfEnergy -- this sets remaining to 0
    Just (result, usedEnergy) -> do
      -- tickEnergy is safe even if usedEnergy > available energy, even though this case
      -- should not happen for well-behaved actions.
      tickEnergy (Cost.toEnergy usedEnergy)
      return result

-- |Like 'withExternal' but takes a pure action that only transforms energy and does
-- not return a value. This is a convenience wrapper only.
withExternalPure_ :: (Cost.ResourceMeasure r, TransactionMonad pv m) => (r -> Maybe r) -> m ()
withExternalPure_ f = withExternal (return . fmap ((),) . f)


-- |Helper function to log when a transaction was invalid.
{-# INLINE logInvalidBlockItem #-}
logInvalidBlockItem :: SchedulerMonad pv m => BlockItem -> FailureKind -> m ()
logInvalidBlockItem WithMetadata{wmdData=NormalTransaction{},..} fk =
  logEvent Scheduler LLWarning $ "Transaction with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk
logInvalidBlockItem WithMetadata{wmdData=CredentialDeployment cred} fk =
  logEvent Scheduler LLWarning $ "Credential with registration id " ++ (show . ID.credId . credential $ cred) ++ " was invalid with reason " ++ show fk
logInvalidBlockItem WithMetadata{wmdData=ChainUpdate{},..} fk =
  logEvent Scheduler LLWarning $ "Chain update with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk

{-# INLINE logInvalidTransaction #-}
logInvalidTransaction :: SchedulerMonad pv m => Transaction -> FailureKind -> m ()
logInvalidTransaction WithMetadata{..} fk =
  logEvent Scheduler LLWarning $ "Transaction with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk

logInvalidCredential :: SchedulerMonad pv m => CredentialDeploymentWithMeta -> FailureKind -> m ()
logInvalidCredential WithMetadata{..} fk =
  logEvent Scheduler LLWarning $ "Credential with registration id " ++ (show . ID.credId . credential $ wmdData) ++ " was invalid with reason " ++ show fk

logInvalidChainUpdate :: SchedulerMonad pv m => WithMetadata UpdateInstruction -> FailureKind -> m ()
logInvalidChainUpdate WithMetadata{..} fk =
  logEvent Scheduler LLWarning $ "Chain update with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk
