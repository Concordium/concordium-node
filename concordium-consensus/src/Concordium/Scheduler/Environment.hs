{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Concordium.Scheduler.Environment where

import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable

import Control.Monad.Cont hiding (cont)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Reader (ReaderT(..), runReaderT)
import Control.Monad.Trans.State.Strict (StateT(..), runStateT)
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.Crypto.EncryptedTransfers
import Concordium.Utils
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.Scheduler.Types
import qualified Concordium.Cost as Cost
import Concordium.GlobalState.Types
import Concordium.GlobalState.Classes (MGSTrans(..))
import Concordium.GlobalState.Account (EncryptedAmountUpdate(..), AccountUpdate(..), auAmount, auEncrypted, auReleaseSchedule, emptyAccountUpdate)
import Concordium.GlobalState.BlockState (AccountOperations(..), NewInstanceData, ContractStateOperations (..), ModuleQuery(..), InstanceInfo, InstanceInfoType (..), InstanceInfoTypeV (iiState, iiParameters), iiBalance, UpdatableContractState)
import Concordium.GlobalState.BakerInfo

import qualified Concordium.TransactionVerification as TVer

import Control.Exception(assert)

import qualified Concordium.ID.Types as ID
import Concordium.Wasm (IsWasmVersion)
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import qualified Concordium.Wasm as GSWasm
import Data.Proxy

-- |An account index together with the canonical address. Sometimes it is
-- difficult to pass an IndexedAccount and we only need the addresses. That is
-- when this type is useful.
type IndexedAccountAddress = (AccountIndex, AccountAddress)

-- |Whether the current energy limit is block energy or current transaction energy.
data EnergyLimitReason = BlockEnergy | TransactionEnergy
    deriving(Eq, Show)

-- * Scheduler monad

class (Monad m) => StaticInformation m where
  -- |Get the chain information for the current block.
  getChainMetadata :: m ChainMetadata

  -- |Get a module interface, if available.
  getModuleInterfaces :: ModuleRef -> m (Maybe (GSWasm.ModuleInterface (InstrumentedModuleRef m)))

  -- |Get maximum allowed block energy.
  getMaxBlockEnergy :: m Energy

  -- |Get maximum number of account creation transactions per block.
  getAccountCreationLimit :: m CredentialsPerBlockLimit

  -- |Return a contract instance if it exists at the given address.
  getContractInstance :: ContractAddress -> m (Maybe (InstanceInfo m))

  -- |Get the amount of funds at the particular account address at the start of a transaction.
  getStateAccount :: AccountAddress -> m (Maybe (IndexedAccount m))

  -- |Get the current exchange rates, that is the Euro per NRG, micro CCD per Euro and the energy rate.
  getExchangeRates :: m ExchangeRates

-- |Information needed to execute transactions in the form that is easy to use.
class (Monad m, StaticInformation m, AccountOperations m, ContractStateOperations m, ModuleQuery m, MonadLogger m, MonadProtocolVersion m, TVer.TransactionVerifier m)
    => SchedulerMonad m where

  -- |Get the 'AccountIndex' for an account, if it exists.
  getAccountIndex :: AccountAddress -> m (Maybe AccountIndex)

  -- |Check whether the given account address would clash with any existing
  -- account's address. The behaviour of this will generally depend on the
  -- protocol version.
  addressWouldClash :: AccountAddress -> m Bool

  -- |Commit to global state all the updates to local state that have
  -- accumulated through the execution. This method is also in charge of
  -- recording which accounts were affected by the transaction for reward and
  -- other purposes.
  -- Precondition: Each account affected in the change set must exist in the
  -- block state.
  commitChanges :: ChangeSet m -> m ()

  -- |Commit a module interface and module value to global state. Returns @True@
  -- if this was successful, and @False@ if a module with the given Hash already
  -- existed. Also store the code of the module for archival purposes.
  commitModule :: IsWasmVersion v => (GSWasm.ModuleInterfaceV v, Wasm.WasmModuleV v) -> m Bool

  -- |Create new instance in the global state.
  -- The instance is parametrised by the address, and the return value is the
  -- address assigned to the new instance.
  putNewInstance :: IsWasmVersion v => NewInstanceData (InstrumentedModuleRef m v) v -> m ContractAddress

  -- |Bump the next available transaction nonce of the account.
  -- Precondition: the account exists in the block state.
  increaseAccountNonce :: IndexedAccount m -> m ()

  -- FIXME: This method should not be here, but rather in the transaction monad.
  -- |Update account credentials.
  -- Preconditions:
  -- - The account exists in the block state.
  -- - The account threshold is reasonable.
  updateAccountCredentials :: AccountIndex
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
  addBaker
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => AccountIndex
    -> BakerAdd
    -> m BakerAddResult

  -- |From chain parameters version >= 1, this operation is used to add/remove/update a baker.
  -- For details of the behaviour and return values, see
  -- 'Concordium.GlobalState.BlockState.bsoConfigureBaker'.
  configureBaker
    :: (SupportsDelegation (MPV m))
    => AccountIndex
    -> BakerConfigure
    -> m BakerConfigureResult

  -- |From chain parameters version >= 1, this operation is used to add/remove/update a delegator.
  -- For details of the behaviour and return values, see
  -- 'Concordium.GlobalState.BlockState.bsoConfigureDelegation'.
  configureDelegation
    :: (SupportsDelegation (MPV m))
    => AccountIndex
    -> DelegationConfigure
    -> m DelegationConfigureResult

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
  removeBaker
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => AccountIndex
    -> m BakerRemoveResult

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
  updateBakerKeys
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => AccountIndex
    -> BakerKeyUpdate
    -> m BakerKeyUpdateResult

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
  updateBakerStake
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => AccountIndex
    -> Amount
    -> m BakerStakeUpdateResult

  -- |Update whether the baker automatically restakes the rewards it earns.
  --
  -- The following results are possible:
  --
  -- * @BREUUpdated id@: the flag was updated.
  --
  -- * @BREUInvalidBaker@: the account does not exists, or is not currently a baker.
  updateBakerRestakeEarnings :: (AccountVersionFor (MPV m) ~ 'AccountV0) => AccountIndex -> Bool -> m BakerRestakeEarningsUpdateResult

  -- *Operations on account keys

  -- | Updates the credential verification keys 
  -- Preconditions:
  -- * The account exists
  -- * The account has keys defined at the specified indices
  updateCredentialKeys :: AccountIndex -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m ()

  -- * Chain updates

  -- |Get the current authorized keys for updates.
  getUpdateKeyCollection :: m (UpdateKeysCollection (ChainParametersVersionFor (MPV m)))

  -- |Get the next sequence number of updates of a given type.
  getNextUpdateSequenceNumber :: UpdateType -> m UpdateSequenceNumber

  -- |Add an update to the relevant update queue. The update is
  -- assumed to have the next sequence number for its update type.
  -- The next sequence number will be correspondingly incremented,
  -- and any queued updates of the given type with a later effective
  -- time are cancelled.
  enqueueUpdate :: TransactionTime -> UpdateValue (ChainParametersVersionFor (MPV m)) -> m ()

-- |Contract state that is lazily thawed. This is used in the scheduler when
-- looking up contracts. When looking them up first time we don't convert the
-- state since this might not be needed.
data TemporaryContractState contractState (v :: Wasm.WasmVersion) =
  Frozen (contractState v)
  | Thawed (UpdatableContractState v)

{-# INLINE getRuntimeReprV0 #-}
getRuntimeReprV0 :: ContractStateOperations m => TemporaryContractState (ContractState m) GSWasm.V0 -> m Wasm.ContractState
getRuntimeReprV0 (Frozen cs) = thawContractState cs
getRuntimeReprV0 (Thawed cs) = return cs

{-# INLINE getRuntimeReprV1 #-}
getRuntimeReprV1 :: ContractStateOperations m => TemporaryContractState (ContractState m) GSWasm.V1 -> m StateV1.MutableState
getRuntimeReprV1 (Frozen cs) = thawContractState cs
getRuntimeReprV1 (Thawed cs) = return cs

getStateSizeV0 :: ContractStateOperations m => TemporaryContractState (ContractState m) GSWasm.V0 -> m Wasm.ByteSize
getStateSizeV0 (Frozen cs) = stateSizeV0 cs
getStateSizeV0 (Thawed cs) = return $ Wasm.contractStateSize cs

-- |Updatable instance information. This is used in the scheduler to efficiently
-- update contract states.
type UInstanceInfo m = InstanceInfoType (InstrumentedModuleRef m) (TemporaryContractState (ContractState m))

-- |Updatable instance information, versioned variant. This is used in the scheduler to efficiently
-- update contract states.
type UInstanceInfoV m = InstanceInfoTypeV (InstrumentedModuleRef m) (TemporaryContractState (ContractState m))

-- |This is a derived notion that is used inside a transaction to keep track of
-- the state of the world during execution. Local state of contracts and amounts
-- on contracts might need to be rolled back for various reasons, so we do not
-- want to commit it to global state.
class (StaticInformation m, ContractStateOperations m, MonadProtocolVersion m) => TransactionMonad m where
  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need not provide it
  -- separately.
  withInstanceStateV0 :: UInstanceInfoV m GSWasm.V0 -> UpdatableContractState GSWasm.V0 -> m a -> m a

  -- |Execute the code in a temporarily modified environment. This is needed in
  -- nested calls to transactions which might end up failing at the end. Thus we
  -- keep track of changes locally first, and only commit them at the end.
  -- Instance keeps track of its own address hence we need not provide it
  -- separately.
  withInstanceStateV1 :: UInstanceInfoV m GSWasm.V1 -> UpdatableContractState GSWasm.V1 -> (ModificationIndex -> m a) -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withAccountToContractAmountV0 :: AccountIndex -> UInstanceInfoV m GSWasm.V0 -> Amount -> m a -> m a

  -- |Transfer amount from the first address to the second and run the
  -- computation in the modified environment.
  withAccountToContractAmountV1 :: AccountIndex -> UInstanceInfoV m GSWasm.V1 -> Amount -> m a -> m a

  -- |Transfer an amount from the first account to the second and run the
  -- computation in the modified environment.
  withAccountToAccountAmount :: IndexedAccount m -> IndexedAccount m -> Amount -> m a -> m a

  -- |Transfer an amount from the given instance to the given account and run the
  -- computation in the modified environment.
  withContractToAccountAmountV0 :: ContractAddress -> IndexedAccount m -> Amount -> m c -> m c

  -- |Transfer an amount from the given instance to the given account and run the
  -- computation in the modified environment.
  withContractToAccountAmountV1 :: ContractAddress -> IndexedAccount m -> Amount -> m c -> m c

  -- |Transfer an amount from the first instance to the second and run the
  -- computation in the modified environment.
  withContractToContractAmountV0 :: (GSWasm.WasmVersion, ContractAddress) -> UInstanceInfoV m GSWasm.V0 -> Amount -> m a -> m a

  -- |Transfer an amount from the first instance to the second and run the
  -- computation in the modified environment.
  withContractToContractAmountV1 :: (GSWasm.WasmVersion, ContractAddress) -> UInstanceInfoV m GSWasm.V1 -> Amount -> m a -> m a

  -- |Transfer a scheduled amount from the first address to the second and run
  -- the computation in the modified environment.
  --
  -- Precondition: The list of releases MUST be non-empty and the timestamps MUST be in increasing
  -- order.
  withScheduledAmount :: IndexedAccount m -> IndexedAccount m -> Amount -> [(Timestamp, Amount)] -> TransactionHash -> m a -> m a

  -- |Replace encrypted amounts on an account up to (but not including) the
  -- given limit with a new amount.
  replaceEncryptedAmount :: IndexedAccount m -> EncryptedAmountAggIndex -> EncryptedAmount -> m ()

  -- |Replace encrypted amounts on an account up to (but not including) the
  -- given limit with a new amount, as well as adding the given amount to the
  -- public balance of the account
  addAmountFromEncrypted :: IndexedAccount m -> Amount -> EncryptedAmountAggIndex -> EncryptedAmount -> m ()

  -- |Add a new encrypted amount to an account, and return its index.
  -- This may assume this is the only update to encrypted amounts on the given account
  -- in this transaction.
  --
  -- This should be used on the receiver's account when an encrypted amount is
  -- sent to it.
  addEncryptedAmount :: IndexedAccount m -> EncryptedAmount -> m EncryptedAmountIndex

  -- |Add an encrypted amount to the self-balance of an account.
  -- This may assume this is the only update to encrypted amounts on the given account
  -- in this transaction.
  --
  -- This should be used when transferring from public to encrypted balance.
  addSelfEncryptedAmount :: IndexedAccount m -> Amount -> EncryptedAmount -> m ()

  -- |Transfer an amount from the first given instance or account to the instance in the second
  -- parameter and run the computation in the modified environment.
  {-# INLINE withToContractAmountV0 #-}
  withToContractAmountV0 :: Either (Wasm.WasmVersion, ContractAddress) AccountIndex -> UInstanceInfoV m GSWasm.V0 -> Amount -> m a -> m a
  withToContractAmountV0 (Left i) = withContractToContractAmountV0 i
  withToContractAmountV0 (Right a) = withAccountToContractAmountV0 a

  {-# INLINE withToContractAmountV1 #-}
  withToContractAmountV1 :: Either (Wasm.WasmVersion, ContractAddress) AccountIndex -> UInstanceInfoV m GSWasm.V1 -> Amount -> m a -> m a
  withToContractAmountV1 (Left i) = withContractToContractAmountV1 i
  withToContractAmountV1 (Right a) = withAccountToContractAmountV1 a

  getCurrentContractInstance :: ContractAddress -> m (Maybe (UInstanceInfo m))

  -- |Charge for additional state that will be needed to store the additional
  -- state for V1 contracts affected by the transaction. **This should only be
  -- called at the end of the transaction.**
  chargeV1Storage :: m ()

  -- |Get the current total public balance of an account.
  -- This accounts for any pending changes in the course of execution of the transaction.
  -- This includes any funds that cannot be spent due to lock-up or baking.
  getCurrentAccountTotalAmount :: IndexedAccount m -> m Amount

  -- |Get the current available public balance of an account.
  -- This accounts for any pending changes in the course of execution of the transaction.
  -- The available balance excludes funds that are locked due to a lock-up release schedule
  -- or due to being staked for baking, or both.  That is @available = total - max locked staked@.
  getCurrentAccountAvailableAmount :: IndexedAccount m -> m Amount

  -- |Get the current available public balance of a contract. This accounts for
  -- the exact current state of the contract during transaction, e.g., in a
  -- nested contract call.
  getCurrentContractAmount :: HasInstanceAddress addr => Wasm.SWasmVersion v -> addr -> m Amount

  -- |Get the current contract instance state, together with the modification
  -- index of the last modification.
  getCurrentContractInstanceState :: UInstanceInfoV m GSWasm.V1 -> m (ModificationIndex, TemporaryContractState (ContractState m) GSWasm.V1)

  -- |Get the current modification index for the instance. If the instance has
  -- not yet been modified during execution of the transaction 0 is returned.
  -- Otherwise the index is at least 1.
  getCurrentModificationIndex :: UInstanceInfoV m GSWasm.V1 -> m ModificationIndex

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

  -- |Try to run the first computation. If it leads to `Left err` then abort and revert all the changes
  -- apart from consumed energy.
  withRollback :: m (Either a b) -> m (Either a b)

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

  -- |Add a contract upgrade to the 'ChangeSet'
  addContractUpgrade :: ContractAddress
                     -- ^The instance that should be upgraded.
                     -> GSWasm.ModuleInterfaceA (InstrumentedModuleRef m GSWasm.V1)
                     -- ^The set of receive names exposed by this new module for the instance.
                     -> Set.Set GSWasm.ReceiveName
                     -- ^The new module to use for execution.
                     -> m ()


-- |Index that keeps track of modifications of smart contracts inside a single
-- transaction. This is used to cheaply detect whether a contract state has
-- changed or not when a contract calls another.
type ModificationIndex = Word

-- |A modified state of a V1 instance. This is the state that is maintained
-- during the execution of a transaction.
--
-- The type parameter `mr` is a technical necessity since we have to maintain a
-- new module interface. Since modules are parametrized by the monad (i.e.,
-- either persistent or basic) we need to parametrize this state update as well,
-- seeing that the scheduler works with any state. On top of this, we often have
-- "newtype wrappers" @t m@ around a monad @m@ but with the property that
-- @InstrumentedModuleRef (t m) ~ InstrumentedModuleRef m@. In order for this to
-- work we actually need to parametrize the @InstanceV1Update'@ by a type
-- function @mr@ so that the typechecker can see the property that if
--
-- @InstrumentedModuleRef (t m) ~ InstrumentedModuleRef m@
--
-- then also
--
-- @InstanceV1Update (t m) ~ InstanceV1Update m@.
--
-- That is why we have the auxiliary type definition @InstanceV1Update'@
-- parametrized by the type function @mr@ and then a simplified type alias
-- @InstanceV1Update@ on top.
data InstanceV1Update' mr = InstanceV1Update {
    -- |The modification index.
    index :: !ModificationIndex,
    -- |Amount changed
    amountChange :: !AmountDelta,
    -- |Present if a state change has ocurred.
    newState :: !(Maybe (UpdatableContractState GSWasm.V1)),
    -- |Present if the contract has been upgraded.
    -- Contract upgrades are only supported from PV 5 and onwards.
    newInterface  :: !(Maybe (GSWasm.ModuleInterfaceA (mr GSWasm.V1), Set.Set GSWasm.ReceiveName))
}

type InstanceV1Update m = InstanceV1Update' (InstrumentedModuleRef m)

type ChangeSet m = ChangeSet' (InstrumentedModuleRef m)

-- |The set of changes to be committed on a successful transaction.
--
-- The reason for parametrizing by a type function @mr@ is the same as for
-- @InstanceV1Update@.
data ChangeSet' mr = ChangeSet
    {_accountUpdates :: !(HMap.HashMap AccountIndex AccountUpdate) -- ^Accounts whose states changed.
    -- |V0 contracts whose states changed. Any time we are updating a contract we know which version it is.
    -- We thus know where to look.
    ,_instanceV0Updates :: !(HMap.HashMap ContractAddress (ModificationIndex, AmountDelta, Maybe (UpdatableContractState GSWasm.V0)))
    -- |V1 contracts whose state changed (and/or) has been upgraded. Any time we are updating a contract we know which version it is.
    -- We thus know where to look.
    ,_instanceV1Updates :: !(HMap.HashMap ContractAddress (InstanceV1Update' mr))
    ,_instanceInits :: !(HSet.HashSet ContractAddress) -- ^Contracts that were initialized.
    ,_encryptedChange :: !AmountDelta -- ^Change in the encrypted balance of the system as a result of this contract's execution.
    ,_addedReleaseSchedules :: !(Map.Map AccountAddress Timestamp) -- ^The release schedules added to accounts on this block, to be added on the per block map.
    }

makeLenses ''ChangeSet'

emptyCS :: Proxy m -> ChangeSet m
emptyCS Proxy = ChangeSet HMap.empty HMap.empty HMap.empty HSet.empty 0 Map.empty

-- |Record an addition to the amount of the given account in the changeset.
{-# INLINE addAmountToCS #-}
addAmountToCS :: AccountOperations m => IndexedAccount m -> AmountDelta -> ChangeSet m -> m (ChangeSet m)
addAmountToCS = addAmountToCS' . fst

-- |Record an addition to the amount of the given account in the changeset.
{-# INLINE addAmountToCS' #-}
addAmountToCS' :: Monad m => AccountIndex -> AmountDelta -> ChangeSet m -> m (ChangeSet m)
addAmountToCS' ai !amnt !cs =
  -- Check whether there already is an 'AccountUpdate' for the given account in the changeset.
  -- If so, modify it accordingly, otherwise add a new entry.
  return $ cs & accountUpdates . at ai %~ (\case Just upd -> Just (upd & auAmount %~ \case
                                                                     Just x -> Just (x + amnt)
                                                                     Nothing -> Just amnt
                                                                 )
                                                 Nothing -> Just (emptyAccountUpdate ai & auAmount ?~ amnt))


-- |Record a list of scheduled releases that has to be pushed into the global map and into the map of the account.
--
-- Precondition: The list of releases MUST be non-empty and the timestamps MUST be in increasing
-- order.
{-# INLINE addScheduledAmountToCS #-}
addScheduledAmountToCS :: AccountOperations m => IndexedAccount m -> ([(Timestamp, Amount)], TransactionHash) -> ChangeSet m -> m (ChangeSet m)
addScheduledAmountToCS _ ([], _) cs = return cs
addScheduledAmountToCS (ai, acc) rel@(((fstRel, _):_), _) !cs = do
  addr <- getAccountCanonicalAddress acc
  return $ cs & accountUpdates . at ai %~ (\case Just upd -> Just (upd & auReleaseSchedule %~ Just . maybe [rel] (rel :))
                                                 Nothing -> Just (emptyAccountUpdate ai & auReleaseSchedule ?~ [rel]))
              & addedReleaseSchedules %~ (Map.alter (\case
                                                        Nothing -> Just fstRel
                                                        Just rel' -> Just $ min fstRel rel') addr)

-- |Modify the amount on the given account in the changeset by a given delta.
-- It is assumed that the account is already in the changeset and that its balance
-- is already affected (the auAmount field is set).
{-# INLINE modifyAmountCS #-}
modifyAmountCS :: Proxy m -> AccountIndex -> AmountDelta -> ChangeSet m -> ChangeSet m
modifyAmountCS Proxy ai !amnt !cs = cs & (accountUpdates . ix ai . auAmount ) %~ upd
  where upd (Just a) = Just (a + amnt)
        upd Nothing = error "modifyAmountCS precondition violated."


-- |Add or update the contract state in the changeset with the new state.
addContractStatesToCSV0 :: HasInstanceAddress a => Proxy m -> a -> ModificationIndex -> UpdatableContractState GSWasm.V0 -> ChangeSet m -> ChangeSet m
addContractStatesToCSV0 Proxy istance curIdx newState =
  instanceV0Updates . at addr %~ \case Just (_, amnt, _) -> Just (curIdx, amnt, Just newState)
                                       Nothing -> Just (curIdx, 0, Just newState)
  where addr = instanceAddress istance

-- |Add or update the contract state in the changeset with the new state.
addContractStatesToCSV1 :: HasInstanceAddress a => Proxy m -> a -> ModificationIndex -> UpdatableContractState GSWasm.V1 -> ChangeSet m -> ChangeSet m
addContractStatesToCSV1 Proxy istance curIdx stateUpdate =
  instanceV1Updates . at addr %~ 
      \case Just InstanceV1Update{..} -> Just $! InstanceV1Update curIdx amountChange (Just stateUpdate) newInterface
            Nothing -> Just $! InstanceV1Update curIdx 0 (Just stateUpdate) Nothing
  where addr = instanceAddress istance

-- |Add the given delta to the change set for the given contract instance.
-- NB: If the contract is not yet in the changeset it is added.
addContractAmountToCSV0 :: (Monad m) => ContractAddress -> AmountDelta -> ChangeSet m -> m (ChangeSet m)
addContractAmountToCSV0 addr amnt cs =
    -- updating amounts does not update the modification index. Only state updates do.
    pure $ cs & instanceV0Updates . at addr %~ \case 
                                              Just (idx, d, v) -> Just (idx, d + amnt, v)
                                              Nothing -> Just (0, amnt, Nothing)

-- |Add the given delta to the change set for the given contract instance.
addContractAmountToCSV1 :: (Monad m) => ContractAddress -> AmountDelta -> ChangeSet m -> m (ChangeSet m)
addContractAmountToCSV1 addr amnt cs =
    -- updating amounts does not update the modification index. Only state updates do.
    pure $ cs & instanceV1Updates . at addr %~ \case 
                                            Just InstanceV1Update{..} -> Just $! InstanceV1Update index (amountChange + amnt) newState newInterface
                                            Nothing -> Just $! InstanceV1Update 0 amnt Nothing Nothing

-- |Add the given contract address to the set of initialized contract instances.
-- As the changes on the blockstate are already performed in the handler for this operation,
-- we just log the contract address as we don't need to modify the blockstate with
-- the information we add to the change set.
{-# INLINE addContractInitToCS #-}
addContractInitToCS :: Proxy m -> ContractAddress -> ChangeSet m -> ChangeSet m
addContractInitToCS Proxy addr cs =
    cs { _instanceInits = HSet.insert addr (cs ^. instanceInits) }


-- |Add the contract upgrade to the 'ChangeSet'.
-- We only care about the most recent contract upgrade.
{-# INLINE addContractUpgradeToCS #-}
addContractUpgradeToCS :: Proxy m -> ContractAddress -> GSWasm.ModuleInterfaceA (InstrumentedModuleRef m GSWasm.V1) -> Set.Set GSWasm.ReceiveName -> ChangeSet m -> ChangeSet m
addContractUpgradeToCS Proxy addr updatedMod updatedReceiveNames cs = do
    cs & instanceV1Updates . at addr %~ \case
                                          Just InstanceV1Update{..} -> Just $! InstanceV1Update index amountChange newState (Just (updatedMod, updatedReceiveNames))
                                          Nothing -> Just $! InstanceV1Update 0 0 Nothing (Just (updatedMod, updatedReceiveNames))

-- |Whether the transaction energy limit is reached because of transaction max energy limit,
-- or because of block energy limit
data LimitReason = TransactionHeader | BlockEnergyLimit

data LocalState m = LocalState{
  -- |Energy left for the computation.
  _energyLeft :: !Energy,
  -- |Changes accumulated thus far.
  _changeSet :: !(ChangeSet m),
  -- |Maximum number of modified contract instances. This is an implementation
  -- detail and is not directly exposed to contracts, or anywhere else. What
  -- this supports is keeping track of whether a V1 contract has been modified
  -- in a given period. A V1 contract execution reports back whether each
  -- segment of execution (that is, between interrupts) has called any state
  -- modification functions. If that is so a new checkpoint is made and the
  -- contract state is recorded with the current modification index. During
  -- handling of the interrupt the state of the contract that initiated it might
  -- be changed. If that is so then the modification index of a specific
  -- contract is incremented. When we get to resuming execution at the point the
  -- interrupt we check the current modification index of the contract. If it
  -- has changed since the start we signal that the contract's state has
  -- changed.
  --
  -- This index is only ever incremented, and it is incremented on each
  -- modification of smart contract instance state by the scheduler. It is
  -- unaffected by updates to the balance of the contract.
  _nextContractModificationIndex :: !ModificationIndex,
  _blockEnergyLeft :: !Energy
  }

makeLenses ''LocalState

data TransactionContext = TransactionContext{
  -- |Header of the transaction initiating the transaction.
  _tcTxSender :: !AccountIndex,
  _tcDepositedAmount :: !Amount
  }

makeLenses ''TransactionContext

-- |A monad transformer adding reading an environment of type @r@
-- and updating a state of type @s@ to an inner monad @m@.
type RST r s m = ReaderT r (StateT s m)

-- |Unwrap a RST computation as a function.
runRST :: RST r s m a -> r -> s -> m (a, s)
runRST rst r s = flip runStateT s . flip runReaderT r $ rst

-- |A concrete implementation of TransactionMonad based on SchedulerMonad. We
-- use the continuation monad transformer instead of the ExceptT transformer in
-- order to avoid expensive bind operation of the latter. The bind operation is
-- expensive because it needs to check at each step whether the result is @Left@
-- or @Right@.
newtype LocalT r m a = LocalT { _runLocalT :: ContT (Either (Maybe RejectReason) r) (RST TransactionContext (LocalState m) m) a }
  deriving(Functor, Applicative, Monad, MonadState (LocalState m), MonadReader TransactionContext)

runLocalT :: forall m a . Monad m
          => LocalT a m a
          -> Amount
          -> AccountIndex
          -> Energy -- Energy limit by the transaction header.
          -> Energy -- remaining block energy
          -> m (Either (Maybe RejectReason) a, LocalState m)
runLocalT (LocalT st) _tcDepositedAmount _tcTxSender _energyLeft _blockEnergyLeft = do
  -- The initial contract modification index must start at 1 since 0 is the
  -- "initial state" of all contracts (as recorded in the changeset).
  let s = LocalState{_changeSet = emptyCS (Proxy @m),_nextContractModificationIndex = 1,..}
  (a, s') <- runRST (runContT st (return . Right)) ctx s
  return (a, s')

  where !ctx = TransactionContext{..}

instance (MonadProtocolVersion m) => MonadProtocolVersion (LocalT r m) where
  type MPV (LocalT r m) = MPV m

instance BlockStateTypes (LocalT r m) where
    type BlockState (LocalT r m) = BlockState m
    type UpdatableBlockState (LocalT r m) = UpdatableBlockState m
    type Account (LocalT r m) = Account m
    type ContractState (LocalT r m) = ContractState m
    type BakerInfoRef (LocalT r m) = BakerInfoRef m
    type InstrumentedModuleRef (LocalT r m) = InstrumentedModuleRef m

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
chargeExecutionCost :: forall m . (AccountOperations m) => SchedulerMonad m => IndexedAccount m -> Amount -> m ()
chargeExecutionCost (ai, acc) amnt = do
    balance <- getAccountAmount acc
    let csWithAccountDelta = emptyCS (Proxy @m) & accountUpdates . at ai ?~ (emptyAccountUpdate ai & auAmount ?~ amountDiff 0 amnt)
    assert (balance >= amnt) $
          commitChanges csWithAccountDelta
    notifyExecutionCost amnt

data WithDepositContext m = WithDepositContext{
  _wtcSenderAccount :: !(IndexedAccount m),
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
  => WithDepositContext m
  -> LocalT a m a
  -- ^The computation to run in the modified environment with reduced amount on the initial account.
  -> (LocalState m -> a -> m (ValidResult, Amount, Energy))
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
  (res, ls) <- runLocalT comp depositedAmount (wtc ^. wtcSenderAccount . _1) energy beLeft
  case res of
    -- Failure: maximum block energy exceeded
    Left Nothing -> return Nothing
    -- Failure: transaction fails (out of energy or actual failure by transaction logic)
    Left (Just reason) -> do
      -- The only effect of this transaction is that the sender is charged for the execution cost
      -- (energy ticked so far).
      (usedEnergy, payment) <- computeExecutionCharge txHeader (ls ^. energyLeft)
      chargeExecutionCost (wtc ^. wtcSenderAccount) payment
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
  SchedulerMonad m => WithDepositContext m -> LocalState m -> [Event] -> m (ValidResult, Amount, Energy)
defaultSuccess wtc = \ls events -> do
  let meta = wtc ^. wtcTransactionHeader
      senderAccount = wtc ^. wtcSenderAccount
  (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
  chargeExecutionCost senderAccount energyCost
  commitChanges (ls ^. changeSet)
  return (TxSuccess events, energyCost, usedEnergy)


{-# INLINE liftLocal #-}
liftLocal :: Monad m => m a -> LocalT r m a
liftLocal m = LocalT (ContT (\k -> ReaderT (\r -> StateT (\s -> m >>= \f -> runRST (k f) r s))))


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

  {-# INLINE getContractInstance #-}
  getContractInstance = liftLocal . getContractInstance

  {-# INLINE getStateAccount #-}
  getStateAccount = liftLocal . getStateAccount

  {-# INLINE getExchangeRates #-}
  getExchangeRates = liftLocal getExchangeRates


deriving via (MGSTrans (LocalT r) m) instance AccountOperations m => AccountOperations (LocalT r m)
deriving via (MGSTrans (LocalT r) m) instance ContractStateOperations m => ContractStateOperations (LocalT r m)

-- |Execute an inner transaction, reifying it into the return value. This
-- behaves as the given computation in case it does not exit early, and resets
-- the state of execution to the beginning in case of an error. In this case the
-- error is also returned in the return value.
runInnerTransaction :: Monad m => LocalT a m a -> LocalT r m (Either RejectReason a)
runInnerTransaction (LocalT kOrig) = LocalT $ ContT $ \k -> do
  initChangeSet <- use changeSet
  initModificationIndex <- use nextContractModificationIndex
  -- Run the given computation to the end by giving it a fresh continuation that
  -- just returns, as if this was a top-level transaction.
  comp <- runContT kOrig (return . Right)
  case comp of
    Left Nothing -> return (Left Nothing)
    Left (Just err) | err == OutOfEnergy -> energyLeft .= 0 >> return (Left (Just OutOfEnergy))
    Left (Just err) -> do
         changeSet .= initChangeSet
         nextContractModificationIndex .= initModificationIndex
         k (Left err)
    Right x -> k (Right x)

instance (MonadProtocolVersion m, StaticInformation m, AccountOperations m, ContractStateOperations m) => TransactionMonad (LocalT r m) where
  {-# INLINE withInstanceStateV0 #-}
  withInstanceStateV0 istance val cont = do
    nextModificationIndex <- use nextContractModificationIndex
    nextContractModificationIndex += 1
    changeSet %= addContractStatesToCSV0 (Proxy @m) istance nextModificationIndex val
    cont 

  {-# INLINE withInstanceStateV1 #-}
  withInstanceStateV1 istance val cont = do
    nextModificationIndex <- use nextContractModificationIndex
    nextContractModificationIndex += 1
    changeSet %= addContractStatesToCSV1 (Proxy @m) istance nextModificationIndex val
    cont nextModificationIndex

  {-# INLINE withAccountToAccountAmount #-}
  withAccountToAccountAmount fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS toAcc (amountToDelta amount) cs >>=
                              addAmountToCS fromAcc (amountDiff 0 amount))
    cont

  {-# INLINE withAccountToContractAmountV0 #-}
  withAccountToContractAmountV0 fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addContractAmountToCSV0 (instanceAddress toAcc) (amountToDelta amount) cs >>= 
                                addAmountToCS' fromAcc (amountDiff 0 amount))    
    cont

  {-# INLINE withAccountToContractAmountV1 #-}
  withAccountToContractAmountV1 fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addContractAmountToCSV1 (instanceAddress toAcc) (amountToDelta amount) cs >>=
                                addAmountToCS' fromAcc (amountDiff 0 amount))    
    cont

  {-# INLINE withContractToAccountAmountV0 #-}
  withContractToAccountAmountV0 fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS toAcc (amountToDelta amount) cs >>= 
                                addContractAmountToCSV0 fromAcc (amountDiff 0 amount))
    cont

  {-# INLINE withContractToAccountAmountV1 #-}
  withContractToAccountAmountV1 fromAcc toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS toAcc (amountToDelta amount) cs >>= 
                                addContractAmountToCSV1 fromAcc (amountDiff 0 amount))
    cont

  {-# INLINE withContractToContractAmountV0 #-}
  withContractToContractAmountV0 (wv, fromAcc) toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addContractAmountToCSV0 (instanceAddress toAcc) (amountToDelta amount) cs)
    case wv of
      GSWasm.V0 -> do 
            cs' <-  use changeSet 
            changeSet <~ liftLocal (addContractAmountToCSV0 fromAcc (amountDiff 0 amount) cs')
      GSWasm.V1 -> do 
            cs' <-  use changeSet 
            changeSet <~ liftLocal (addContractAmountToCSV1 fromAcc (amountDiff 0 amount) cs')        
    cont

  {-# INLINE withContractToContractAmountV1 #-}
  withContractToContractAmountV1 (wv, fromAcc) toAcc amount cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addContractAmountToCSV1 (instanceAddress toAcc) (amountToDelta amount) cs)
    case wv of
      GSWasm.V0 -> do 
          cs' <- use changeSet
          changeSet <~ liftLocal (addContractAmountToCSV0 fromAcc (amountDiff 0 amount) cs')
      GSWasm.V1 -> do 
          cs' <- use changeSet
          changeSet <~ liftLocal (addContractAmountToCSV1 fromAcc (amountDiff 0 amount) cs')
    cont

  {-# INLINE withScheduledAmount #-}
  withScheduledAmount fromAcc toAcc sentAmount releases txh cont = do
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS fromAcc (amountDiff 0 sentAmount) cs >>= 
                                addScheduledAmountToCS toAcc (releases, txh))    
    cont

  replaceEncryptedAmount (ai, _) aggIndex newAmount = do
    changeSet . accountUpdates . at' ai . non (emptyAccountUpdate ai) . auEncrypted ?= ReplaceUpTo{..}

  addAmountFromEncrypted acc amount aggIndex newAmount = do
    replaceEncryptedAmount acc aggIndex newAmount
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS acc (amountToDelta amount) cs)
    changeSet . encryptedChange += amountDiff 0 amount

  addEncryptedAmount (ai, acc) newAmount = do
    changeSet . accountUpdates . at' ai . non (emptyAccountUpdate ai) . auEncrypted ?= Add{..}
    getAccountEncryptedAmountNextIndex acc

  addSelfEncryptedAmount iacc@(ai, _) transferredAmount newAmount = do
    cs <- use changeSet
    changeSet <~ liftLocal (addAmountToCS iacc (amountDiff 0 transferredAmount) cs)
    changeSet . accountUpdates . at' ai . non (emptyAccountUpdate ai) . auEncrypted ?= AddSelf{..}
    changeSet . encryptedChange += amountToDelta transferredAmount

  getCurrentContractInstance addr = do
    mistance <- getContractInstance addr
    case mistance of
        Nothing -> return Nothing
        Just (InstanceInfoV0 inst) -> do
            newStates <- use (changeSet . instanceV0Updates)
            case newStates ^. at addr of
                Nothing -> do
                    return $ Just (InstanceInfoV0 inst {iiState = Frozen (iiState inst)})
                Just (_, delta, newmodel) ->
                    let !amnt = applyAmountDelta delta (iiBalance inst)
                    in return (Just . InstanceInfoV0 $ inst {iiBalance = amnt, iiState = maybe (Frozen (iiState inst)) Thawed newmodel})
        Just (InstanceInfoV1 inst) -> do
            updates <- use (changeSet . instanceV1Updates)
            case  updates ^. at' addr of
              Nothing -> return $ Just (InstanceInfoV1 inst {iiState = Frozen (iiState inst)})
              Just InstanceV1Update{..} ->
                    let !amnt = applyAmountDelta amountChange (iiBalance inst)
                    in return (Just . InstanceInfoV1 $ inst {
                            iiBalance = amnt,
                            iiState = maybe (Frozen (iiState inst)) Thawed newState,
                            iiParameters = maybe (iiParameters inst) (updateParams (iiParameters inst)) newInterface})
    where
      updateParams params (newMod, newReceiveNames) =
          InstanceParameters {
              _instanceAddress = _instanceAddress params,
              instanceOwner = instanceOwner params,
              instanceInitName = instanceInitName params,
              instanceReceiveFuns = newReceiveNames,
              instanceModuleInterface = newMod}

  chargeV1Storage = do
    xs <- use (changeSet . instanceV1Updates)
    forM_ xs $ \InstanceV1Update{..} ->
      case newState of
        Nothing -> return ()
        Just ms -> tickEnergy (Cost.toEnergy (Wasm.ByteSize (StateV1.getNewStateSize ms)))

  getCurrentContractInstanceState istance = do
    updates <- use (changeSet . instanceV1Updates)
    case updates ^. at (instanceAddress (iiParameters istance)) of
      Just InstanceV1Update{..} -> case newState of 
                                      Just s -> return (index, Thawed s)
                                      Nothing -> return (0, iiState istance)
      Nothing -> return (0, iiState istance)
        

  getCurrentModificationIndex istance = do
    newStates <- use (changeSet . instanceV1Updates)
    case newStates ^. at (instanceAddress (iiParameters istance)) of
      Just InstanceV1Update{..} -> return index
      Nothing -> return 0

  getCurrentAccountTotalAmount (ai, acc) = do
    oldTotal <- getAccountAmount acc
    !txCtx <- ask
    -- If the account is the sender, subtract the deposit
    let netDeposit = if txCtx ^. tcTxSender == ai
          then oldTotal - (txCtx ^. tcDepositedAmount)
          else oldTotal
    macc <- use (changeSet . accountUpdates . at ai)
    case macc of
      Just upd -> do
        -- Apply any pending delta and add any new scheduled releases
        let newReleases = case upd ^. auReleaseSchedule of
                Nothing -> 0
                Just l -> foldl' (\t l' -> t + foldl' (+) 0 (snd <$> fst l')) 0 l
        return $! applyAmountDelta (upd ^. auAmount . non 0) netDeposit + newReleases
      Nothing -> return netDeposit

  getCurrentAccountAvailableAmount (ai, acc) = do
    oldTotal <- getAccountAmount acc
    oldLockedUp <- getAccountLockedAmount acc
    staked <- getAccountStakedAmount acc
    !txCtx <- ask
    -- If the account is the sender, subtract the deposit
    let netDeposit = if txCtx ^. tcTxSender == ai
          then oldTotal - (txCtx ^. tcDepositedAmount)
          else oldTotal
    macc <- use (changeSet . accountUpdates . at ai)
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
  getCurrentContractAmount _ inst = do
    let addr = instanceAddress inst
    getCurrentContractInstance addr >>= \case
      Nothing -> error "Precondition violation."
      Just ist -> case ist of
          InstanceInfoV0 ii -> return (iiBalance ii)
          InstanceInfoV1 ii -> return (iiBalance ii)

  {-# INLINE getEnergy #-}
  getEnergy = do
    beLeft <- use' blockEnergyLeft
    txLeft <- use' energyLeft
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
    -- The lazy 'modify' reduces memory consumption significantly in this case.
    else modify ((energyLeft -~ tick) . (blockEnergyLeft -~ tick))

  {-# INLINE rejectTransaction #-}
  rejectTransaction OutOfEnergy = energyLeft .= 0 >> LocalT (ContT (\_ -> return (Left (Just OutOfEnergy))))
  rejectTransaction reason = LocalT (ContT (\_ -> return (Left (Just reason))))

  {-# INLINE orElse #-}
  orElse (LocalT l) (LocalT r) = LocalT $ ContT $ \k -> do
     initChangeSet <- use changeSet
     initModificationIndex <- use nextContractModificationIndex
     runContT l k >>= \case
       Left (Just reason) | reason /= OutOfEnergy -> do
         -- reset changeSet, the left computation will have no effect at all other than
         -- energy use.
         changeSet .= initChangeSet
         nextContractModificationIndex .= initModificationIndex
         runContT r k
       x -> return x

  {-# INLINE withRollback #-}
  withRollback (LocalT l) = LocalT $ ContT $ \k -> do
     initChangeSet <- use changeSet
     initModificationIndex <- use nextContractModificationIndex
     let kNew x@(Left _) = do
           changeSet .= initChangeSet
           nextContractModificationIndex .= initModificationIndex
           k x
         kNew x = k x
     runContT l kNew


  {-# INLINE outOfBlockEnergy #-}
  outOfBlockEnergy = LocalT (ContT (\_ -> return (Left Nothing)))

  {-# INLINE addContractUpgrade #-}
  addContractUpgrade cAddr newMod newReceiveNames = do
      cs <- use changeSet
      let cs' = addContractUpgradeToCS (Proxy @m) cAddr newMod newReceiveNames cs
      changeSet .=! cs'

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
withExternal :: (Cost.ResourceMeasure r, TransactionMonad m) => (r ->  m (Maybe (a, r))) -> m a
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
withExternalPure_ :: (Cost.ResourceMeasure r, TransactionMonad m) => (r -> Maybe r) -> m ()
withExternalPure_ f = withExternal (return . fmap ((),) . f)


-- |Helper function to log when a transaction was invalid.
{-# INLINE logInvalidBlockItem #-}
logInvalidBlockItem :: SchedulerMonad m => BlockItem -> FailureKind -> m ()
logInvalidBlockItem WithMetadata{wmdData=NormalTransaction{},..} fk =
  logEvent Scheduler LLWarning $ "Transaction with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk
logInvalidBlockItem WithMetadata{wmdData=CredentialDeployment cred} fk =
  logEvent Scheduler LLWarning $ "Credential with registration id " ++ (show . ID.credId . credential $ cred) ++ " was invalid with reason " ++ show fk
logInvalidBlockItem WithMetadata{wmdData=ChainUpdate{},..} fk =
  logEvent Scheduler LLWarning $ "Chain update with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk

{-# INLINE logInvalidTransaction #-}
logInvalidTransaction :: SchedulerMonad m => TVer.TransactionWithStatus -> FailureKind -> m ()
logInvalidTransaction (WithMetadata{..},_) fk =
  logEvent Scheduler LLWarning $ "Transaction with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk

logInvalidCredential :: SchedulerMonad m => TVer.CredentialDeploymentWithStatus -> FailureKind -> m ()
logInvalidCredential (WithMetadata{..},_) fk =
  logEvent Scheduler LLWarning $ "Credential with registration id " ++ (show . ID.credId . credential $ wmdData) ++ " was invalid with reason " ++ show fk

logInvalidChainUpdate :: SchedulerMonad m => TVer.ChainUpdateWithStatus -> FailureKind -> m ()
logInvalidChainUpdate (WithMetadata{..},_) fk =
  logEvent Scheduler LLWarning $ "Chain update with hash " ++ show wmdHash ++ " was invalid with reason: " ++ show fk
