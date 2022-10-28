{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
-- FIXME: This is to suppress compiler warnings for derived instances of BlockStateOperations.
-- This may be fixed in GHC 9.0.1.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-|
 Definition of the API of every BlockState implementation.

The block state holds amongs other things the status of the accounts, bakers and
bank rewards after the execution of a specific block.

We will consider the genesis state containing at least:

* accounts: a collection of accounts
* credentials: a collection of the deployed credentials
* executionCost
* mintedGTUPerSlot
* totalGTU
* centralBankGTU
* identityIssuers: a collection for the amount of notifications received by the issuer
* birkParameters
* bakers: collection of the current bakers (could be inside birkParameters)
* electionDifficulty
* transactionOutcomesValues: normal transaction outcomes in a block
* transactionOutcomesSpecial: special transction outcomes in a block

Each implementation might group these values under different structures but they
are all required.

Some invariants that must be maintained in the BlockState are:
B1. Once an account has been created, it cannot be replaced by another account with the same address.
B2. The total GTU should equal the sum of all amounts on accounts plus the central bank amount plus the reward amount.
B3. The number of notifications to identity issuers must be the same as the number of transactions that don't deploy credentials.
B4. Two bakers cannot share the same aggregate signature verify key.
B5. The amount delegated to any given baker must be the sum of the amounts o all the accounts that delegate to that baker. The total delegated amount must always equal the sum of the amounts delegated to all bakers.

These invariants are actually inviolable considering the structure of the API.
-}
module Concordium.GlobalState.BlockState where

import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.Vector as Vec
import Data.Serialize(Serialize)
import qualified Data.Serialize as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (foldl')
import qualified Data.ByteString as BS
import Data.Word
import Data.Kind (Type)

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Execution
import Concordium.Utils.Serialization
import Concordium.Types.Updates hiding (getUpdateKeysCollection)
import qualified Concordium.Wasm as Wasm
import qualified Concordium.GlobalState.Wasm as GSWasm
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Account
import Concordium.GlobalState.Persistent.BlobStore

import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Basic.BlockState.PoolRewards
import Concordium.GlobalState.BakerInfo
import qualified Concordium.Types.UpdateQueues as UQ
import Concordium.Types.Accounts
import Concordium.GlobalState.CapitalDistribution
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Types
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Types.Queries (PoolStatus, RewardStatus')
import Concordium.Types.SeedState
import Concordium.Types.Transactions hiding (BareBlockItem(..))

import qualified Concordium.ID.Types as ID
import Concordium.ID.Parameters(GlobalContext)
import Concordium.ID.Types (AccountCredential)
import Concordium.Crypto.EncryptedTransfers
import Concordium.GlobalState.ContractStateFFIHelpers (LoadCallback)
import qualified Concordium.GlobalState.ContractStateV1 as StateV1
import Concordium.GlobalState.Persistent.LMDB (FixedSizeSerialization)

-- |Hash associated with birk parameters.
newtype BirkParametersHash (pv :: ProtocolVersion) = BirkParametersHash {birkParamHash :: H.Hash}
  deriving newtype (Eq, Ord, Show, Serialize)

-- |The hashes of the block state components, which are combined
-- to produce a 'StateHash'.
data BlockStateHashInputs (pv :: ProtocolVersion) = BlockStateHashInputs {
    bshBirkParameters :: H.Hash,
    bshCryptographicParameters :: H.Hash,
    bshIdentityProviders :: H.Hash,
    bshAnonymityRevokers :: H.Hash,
    bshModules :: H.Hash,
    bshBankStatus :: H.Hash,
    bshAccounts :: H.Hash,
    bshInstances :: H.Hash,
    bshUpdates :: H.Hash,
    bshBlockRewardDetails :: BlockRewardDetailsHash (AccountVersionFor pv)
} deriving (Show)

-- |Construct a 'StateHash' from the component hashes.
makeBlockStateHash :: BlockStateHashInputs pv -> StateHash
makeBlockStateHash BlockStateHashInputs{..} = StateHashV0 $
  H.hashOfHashes
    (H.hashOfHashes
      (H.hashOfHashes
        (H.hashOfHashes bshBirkParameters bshCryptographicParameters)
        (H.hashOfHashes bshIdentityProviders bshAnonymityRevokers)
      )
      (H.hashOfHashes
        (H.hashOfHashes bshModules bshBankStatus)
        (H.hashOfHashes bshAccounts bshInstances)
      )
    )
    (H.hashOfHashes
      bshUpdates
      (brdHash bshBlockRewardDetails))

-- |Constraint that a protocol version supports transaction outcomes.
type SupportsTransactionOutcomes (pv :: ProtocolVersion) = (IsTransactionOutcomesVersion (TransactionOutcomesVersionFor pv))

-- |An auxiliary data type to express restrictions on an account.
-- Currently an account that has more than one credential is not allowed to handle encrypted transfers,
-- and an account that has a non-zero encrypted balance cannot add new credentials.
data AccountAllowance = AllowedEncryptedTransfers | AllowedMultipleCredentials
  deriving (Eq, Show)

class (BlockStateTypes m, Monad m) => AccountOperations m where

  -- | Get the address of the account
  getAccountCanonicalAddress :: Account m -> m AccountAddress

  -- | Get the current public account balance
  getAccountAmount :: Account m -> m Amount

  -- |Check whether an account is allowed to perform the given action.
  checkAccountIsAllowed :: Account m -> AccountAllowance -> m Bool

  -- |Get the amount that is staked on the account.
  -- This is 0 if the account is not staking or delegating.
  getAccountStakedAmount :: Account m -> m Amount

  -- |Get the amount that is locked in scheduled releases on the account.
  -- This is 0 if there are no pending releases on the account.
  getAccountLockedAmount :: Account m -> m Amount

  -- | Get the current public account available balance.
  -- This accounts for lock-up and staked amounts.
  -- @available = total - max locked staked@
  getAccountAvailableAmount :: Account m -> m Amount
  getAccountAvailableAmount acc = do
    total <- getAccountAmount acc
    lockedUp <- getAccountLockedAmount acc
    staked <- getAccountStakedAmount acc
    return $ total - max lockedUp staked

  -- |Get the next available nonce for this account
  getAccountNonce :: Account m -> m Nonce

  -- |Get the list of credentials deployed on the account, ordered from most
  -- recently deployed.  The list should be non-empty.
  getAccountCredentials :: Account m -> m (Map.Map ID.CredentialIndex ID.RawAccountCredential)

  -- |Get the key used to verify transaction signatures, it records the signature scheme used as well
  getAccountVerificationKeys :: Account m -> m ID.AccountInformation

  -- |Get the current encrypted amount on the account.
  getAccountEncryptedAmount :: Account m -> m AccountEncryptedAmount

  -- |Get the public key used to receive encrypted amounts.
  getAccountEncryptionKey :: Account m -> m ID.AccountEncryptionKey

  -- |Get the next index of the encrypted amount for this account. Next here refers
  -- to the index a newly added encrypted amount will receive.
  -- This has a default implementation in terms of 'getAccountEncryptedAmount',
  -- but it could be replaced by more efficient implementations for, e.g.,
  -- the persistent instance
  getAccountEncryptedAmountNextIndex :: Account m -> m EncryptedAmountIndex
  getAccountEncryptedAmountNextIndex acc = do
    AccountEncryptedAmount{..} <- getAccountEncryptedAmount acc
    return $! addToAggIndex _startIndex (maybe id (const (+1)) _aggregatedAmount $ fromIntegral (Seq.length _incomingEncryptedAmounts))

  -- |Get an encrypted amount at index, if possible.
  -- This has a default implementation in terms of `getAccountEncryptedAmount`.
  -- The implementation's complexity is linear in the difference between the start index of the current
  -- encrypted amount on the account, and the given index.
  --
  -- At each index, the 'selfAmounts' is always included, hence if the index is
  -- out of bounds we simply return the 'selfAmounts'
  getAccountEncryptedAmountAtIndex :: Account m -> EncryptedAmountAggIndex -> m (Maybe EncryptedAmount)
  getAccountEncryptedAmountAtIndex acc index = do
    AccountEncryptedAmount{..} <- getAccountEncryptedAmount acc
    let numOfAmounts = maybe id (const (+1)) _aggregatedAmount $ fromIntegral (Seq.length _incomingEncryptedAmounts)
    if index >= _startIndex && numOfAmounts >= index - _startIndex then
      let toTake = Seq.take (fromIntegral (index - _startIndex)) $ maybe id ((Seq.:<|) . fst) _aggregatedAmount _incomingEncryptedAmounts
      in return $ Just $! foldl' aggregateAmounts _selfAmount toTake
    else return Nothing

  -- |Get the release schedule for an account.
  getAccountReleaseSchedule :: Account m -> m AccountReleaseSchedule

  -- |Get the baker info (if any) attached to an account.
  getAccountBaker :: Account m -> m (Maybe (AccountBaker (AccountVersionFor (MPV m))))

  -- |Get a reference to the baker info (if any) attached to an account.
  getAccountBakerInfoRef :: Account m -> m (Maybe (BakerInfoRef m))

  -- |Get the delegator info (if any) attached to an account.
  getAccountDelegator :: Account m -> m (Maybe (AccountDelegation (AccountVersionFor (MPV m))))

  -- |Get the baker or stake delegation information attached to an account.
  getAccountStake :: Account m -> m (AccountStake (AccountVersionFor (MPV m)))

  -- |Dereference a 'BakerInfoRef' to a 'BakerInfo'.
  derefBakerInfo :: BakerInfoRef m -> m BakerInfo

  -- |Get the hash of an account.
  -- Note: this may not be implemented efficiently, and is principally intended for testing purposes.
  getAccountHash :: Account m -> m (AccountHash (AccountVersionFor (MPV m)))

-- * Active, current and next bakers/delegators
--
-- $ActiveCurrentNext
-- When referring to bakers and delegators, their stakes and capital distribution, from the
-- perspective of a block's state, we have three different views: active, current and next.
--
-- __Active__ bakers and delegators are accounts that have a staking record (baker or delegator) on
-- the account itself. The active bakers and delegators are also indexed by the active bakers
-- structure.  An active baker/delegator may or may not be current and/or next. However, generally
-- a current or next baker/delegator will be active. (Note: this concept of "active" is not
-- connected to the concept of "passive" delegation. A passive delegator does not delegate to a
-- specific baker, but, in a sense, to all bakers. As such, a passive delegator would still be
-- considered active in the present sense.)
--
-- __Current__ (epoch) bakers and delegators represent a snapshot of the stake that is used for calculating
-- the lottery power and reward distribution for the current epoch ('P1'-'P3') or payday ('P4'-).
-- These are captured by two structures: the /current epoch bakers/, and the /current capital
-- distribution/ ('P4'-). The former captures the effective stake of each baker, and from 'P4' the
-- pool commission rates (see 'bsoGetCurrentEpochFullBakersEx'). The current capital distribution
-- captures the equity and delegated capital of each baker and delegator, which is used for
-- reward distribution.
--
-- __Next__ (epoch) bakers and delegators, like the current bakers and delegators, represent a snapshot
-- of the stake and capital distribution, but for the next epoch or reward period. As with the
-- current bakers, they are captured by two structures: the /next epoch bakers/ and the /next
-- capital distribution/.
--
-- The active bakers and delegators are updated directly by operations that modify the
-- bakers/delegators on an account.  Additionally, 'bsoTransitionEpochBakers' ('P1'-'P3') and
-- 'bsoProcessPendingChanges' ('P3'-) handle cooldowns to baker and delegator stakes.
--
-- The next bakers and delegators are updated based on the active bakers at a particular point
-- in time.  Prior to 'P4', this is done each epoch as part of 'bsoTransitionEpochBakers'.
-- From 'P4' onwards, this is done with 'bsoSetNextEpochBakers' and 'bsoSetNextCapitalDistribution'
-- the epoch before a payday. (The burden of computing the stakes and capital is shifted to
-- 'Concordium.Kontrol.Bakers.generateNextBakers'.)
--
-- The current bakers and delegators are updated from the next bakers and delegators at epoch
-- ('P1'-'P3') or payday ('P4'-) boundaries.

-- |A type family grouping mutable contract states, parametrized by the contract
-- version. This is deliberately a __closed__ and __injective__ type family. The
-- latter makes it convenient to use in arguments to functions since the
-- version is determined from the state type. The former makes sense since there
-- are only a fixed amount of contract versions supported at any given time.
--
-- As to the purpose of updatable contract state. Contract state (at least in V1
-- contracts) exists in two quite different formats. The "persistent" one
-- (persistent in the sense of persistent data structures) that is designed for
-- efficient sharing and storage, and an "updatable" one which exists only
-- during transaction execution. A persistent state is "thawed" to convert it to
-- mutable state. This state is lazily constructed from the persistent one
-- during contract execution, and supports efficient in-place updates to the
-- state. At the end of contract execution the mutable state is "frozen", which
-- converts it to the persistent version, retaining as much sharing as possible
-- with the previous version.
type family UpdatableContractState (v :: Wasm.WasmVersion) = ty | ty -> v where
  UpdatableContractState GSWasm.V0 = Wasm.ContractState
  UpdatableContractState GSWasm.V1 = StateV1.MutableState

-- |An external representation of the persistent (i.e., frozen) contract state.
-- This is used to pass this state through FFI for queries and should not be
-- used during contract execution in the scheduler since it's considered an
-- implementation detail and needs to be used together with the correct loader
-- callback. Higher-level abstractions should be used in the scheduler.
type family ExternalContractState (v :: Wasm.WasmVersion) = ty | ty -> v where
  ExternalContractState GSWasm.V0 = Wasm.ContractState
  ExternalContractState GSWasm.V1 = StateV1.PersistentState

class (BlockStateTypes m, Monad m) => ContractStateOperations m where
    -- |Convert a persistent state to a mutable one that can be updated by the
    -- scheduler. This function must generate independent mutable states for
    -- each invocation, where independent means that updates to different
    -- versions are __not__ reflected in others.
    thawContractState :: ContractState m v -> m (UpdatableContractState v)

    -- |Convert a persistent state to its external representation that can be
    -- passed through FFI. The state should be used together with the
    -- callbacks returned by 'getV1StateContext'.
    externalContractState :: ContractState m v -> m (ExternalContractState v)

    -- |Get the callback to allow loading the contract state. Contracts are
    -- executed on the other end of FFI, and state is managed by Haskell, this
    -- gives access to state across the FFI boundary.
    --
    -- V0 state is a simple byte-array which is copied over the FFI boundary, so
    -- it does not require an analogous construct.
    getV1StateContext :: m LoadCallback

    -- |Size of the persistent V0 state. The way charging is done for V0
    -- contracts requires us to get this information when loading the state __at
    -- a specific point in execution__. The specific point matters since
    -- different failures of a transaction lead to different outcomes hashes.
    -- Thus to retain semantics of V0 contracts we need to look up contract
    -- state size for the "persistent" contract state.
    stateSizeV0 :: ContractState m GSWasm.V0 -> m Wasm.ByteSize

    -- |Convert the entire contract state to a byte array. This should generally
    -- only be used for testing.
    contractStateToByteString :: ContractState m v -> m BS.ByteString

-- |Information about an instance returned by block state queries. The type
-- parameter @contractState@ determines the concrete representation of the
-- contract state, the @instrumentedModule@ determines the concrete representation
-- of the instrumented module i.e. the artifact for exectution
-- and @v@ determines the instance version. The fields of this
-- record are not strict because this is just an intermediate type that is
-- quickly deconstructed. The @contractState@ parameter is given in this way, as
-- opposed to passing m directly, so that type unification sees that if
-- @ContractState m ~ ContractState n@ then @InstanceInfo m v ~ InstanceInfo n
-- v@
data InstanceInfoTypeV
    (instrumentedModule :: Wasm.WasmVersion -> Type)
    (contractState :: Wasm.WasmVersion -> Type)
    (v :: Wasm.WasmVersion)
    = InstanceInfoV {
    -- |Immutable parameters that do not change after the instance is created.
    iiParameters :: InstanceParameters (instrumentedModule v),
    -- |The state that will be modified during execution.
    iiState :: contractState v,
    -- |The current balance of the contract.
    iiBalance :: Amount
    }

deriving instance (Eq (instrumentedModule v), Eq (contractState v)) => Eq (InstanceInfoTypeV instrumentedModule contractState v)
deriving instance Show (contractState v) => Show (InstanceInfoTypeV instrumentedModule contractState v)

instance HasInstanceAddress (InstanceInfoTypeV instrumentedModule contractState v) where
  {-# INLINE instanceAddress #-}
  instanceAddress = instanceAddress . iiParameters

-- |Information about either V0 or V1 instance. The type parameter defines the
-- concrete representation of the contract state. The @contractState@ parameter
-- is given in this way, as opposed to passing m directly and using
-- @ContractState m@, so that type unification sees that if @ContractState m ~
-- ContractState n@ then @InstanceInfo m v ~ InstanceInfo n v@
data InstanceInfoType
  (instrumentedModule :: Wasm.WasmVersion -> Type)
  (contractState :: Wasm.WasmVersion -> Type)
  = InstanceInfoV0 (InstanceInfoTypeV instrumentedModule contractState GSWasm.V0)
  | InstanceInfoV1 (InstanceInfoTypeV instrumentedModule contractState GSWasm.V1)

deriving instance (Eq (instrumentedModule GSWasm.V0), Eq (instrumentedModule GSWasm.V1), Eq (contractState GSWasm.V0), Eq (contractState GSWasm.V1)) => Eq (InstanceInfoType instrumentedModule contractState)
deriving instance (Show (contractState GSWasm.V0), Show (contractState GSWasm.V1)) => Show (InstanceInfoType instrumentedModule contractState)

-- |An alias for the most common part of the instance information, parametrized
-- by the context monad @m@.
type InstanceInfoV m = InstanceInfoTypeV (InstrumentedModuleRef m) (ContractState m)
type InstanceInfo m = InstanceInfoType (InstrumentedModuleRef m) (ContractState m)

class (Monad m, BlockStateTypes m) => ModuleQuery m where
    -- |Get a module artifact from an 'InstrumentedModuleRef'.
    getModuleArtifact :: Wasm.IsWasmVersion v => InstrumentedModuleRef m v -> m (GSWasm.InstrumentedModuleV v)

-- |We create a wrapper here so we can
-- derive another 'HashableTo' instance which omits
-- the exact 'RejectReason' in the resulting hash.
newtype TransactionSummaryV1 = TransactionSummaryV1 {_transactionSummaryV1 :: TransactionSummary' ValidResult}
    deriving(Eq, Show)

-- |A 'HashableTo' instance for a 'TransactionSummary'' which
-- omits the exact reject reason.
-- Failures are simply tagged with a '0x1' byte.
-- Note. The hash is computed lazily on purpose as the summary can be large
-- i.e., the resulting events.
instance HashableTo H.Hash TransactionSummaryV1 where
  getHash (TransactionSummaryV1 summary) = H.hashLazy $! S.runPutLazy $!
      putMaybe S.put (tsSender summary) <>
      S.put (tsHash summary) <>
      S.put (tsCost summary) <>
      S.put (tsEnergyCost summary) <>
      S.put (tsType summary) <>
      encodeValidResult (tsResult summary) <>
      S.put (tsIndex summary)
    where
      -- |Encode the 'ValidResult' omitting the exact 'RejectReason' if the
      -- transaction failed. Otherwise we encode the resulting events in the
      -- resulting outcome hash.
      encodeValidResult :: S.Putter ValidResult
      encodeValidResult (TxSuccess events) = S.putWord8 0 <> putListOf putEvent events
      -- We omit the exact 'RejectReason'.
      encodeValidResult (TxReject _) = S.putWord8 1



instance (MonadBlobStore m, MonadProtocolVersion m) => BlobStorable m TransactionSummaryV1 where
  storeUpdate s@(TransactionSummaryV1 ts) = return (putTransactionSummary ts, s)
  load = do
    s <- getTransactionSummary (protocolVersion @(MPV m))
    return . return $! TransactionSummaryV1 s

-- Generic instance based on the HashableTo instance
instance Monad m => MHashableTo m H.Hash TransactionSummaryV1 where

-- |The block query methods can query block state. They are needed by
-- consensus itself to compute stake, get a list of and information about
-- bakers, finalization committee, etc.
class (ContractStateOperations m, AccountOperations m, ModuleQuery m) => BlockStateQuery m where
    -- |Get the module source from the module table as deployed to the chain.
    getModule :: BlockState m -> ModuleRef -> m (Maybe Wasm.WasmModule)

    -- |Get the module source from the module table as deployed to the chain.
    getModuleInterface :: BlockState m -> ModuleRef -> m (Maybe (GSWasm.ModuleInterface (InstrumentedModuleRef m)))

    -- |Get the account state from the account table of the state instance.
    getAccount :: BlockState m -> AccountAddress -> m (Maybe (AccountIndex, Account m))
    -- |Check whether an account exists for the given account address.
    accountExists :: BlockState m -> AccountAddress -> m Bool

    -- |Get all the active bakers in ascending order.
    getActiveBakers :: BlockState m -> m [BakerId]

    -- |Get the currently-registered (i.e. active) bakers with their delegators, as well as the
    -- set of passive delegators. In each case, the lists are ordered in ascending Id order,
    -- with no duplicates.
    getActiveBakersAndDelegators :: (SupportsDelegation (MPV m)) => BlockState m -> m ([ActiveBakerInfo m], [ActiveDelegatorInfo])

    -- |Get the registered delegators of a pool. Changes are reflected immediately here and will be effective in the next reward period.
    -- The baker id is used to identify the pool and Nothing is used for the passive delegators.
    -- Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
    getActiveDelegators
      :: (SupportsDelegation (MPV m))
      => BlockState m
      -- |Nothing for the passive pool, otherwise the baker id of the pool.
      -> Maybe BakerId
      -> m (Maybe [(AccountAddress, ActiveDelegatorInfo)])

    -- |Get the delegators of a pool for the reward period. Changes are not reflected here until the next reward period.
    -- The baker id is used to identify the pool and Nothing is used for the passive delegators.
    -- Returns Nothing if it fails to identify the baker pool. Should always return a value for the passive delegators.
    getCurrentDelegators
      :: (SupportsDelegation (MPV m))
      => BlockState m
      -- |Nothing for the passive pool, otherwise the baker id of the pool.
      -> Maybe BakerId
      -> m (Maybe [(AccountAddress, DelegatorCapital)])

    -- |Query an account by the id of the credential that belonged to it.
    getAccountByCredId :: BlockState m -> ID.RawCredentialRegistrationID -> m (Maybe (AccountIndex, Account m))

    -- |Query an account by the account index that belonged to it.
    getAccountByIndex :: BlockState m -> AccountIndex -> m (Maybe (AccountIndex, Account m))

    -- |Get the contract state from the contract table of the state instance.
    getContractInstance :: BlockState m -> ContractAddress -> m (Maybe (InstanceInfo m))

    -- |Get the list of addresses of modules existing in the given block state.
    getModuleList :: BlockState m -> m [ModuleRef]
    -- |Get the list of account addresses existing in the given block state.
    -- This returns the canonical addresses.
    getAccountList :: BlockState m -> m [AccountAddress]
    -- |Get the list of contract instances existing in the given block state.
    -- The list should be returned in ascending order of addresses.
    getContractInstanceList :: BlockState m -> m [ContractAddress]

    -- |Get the seed state, from which the leadership election nonce
    -- is derived.
    getSeedState :: BlockState m -> m SeedState

    -- |Get the bakers for the epoch in which the block was baked.
    getCurrentEpochBakers :: BlockState m -> m FullBakers

    -- |Get the bakers for the next epoch. (See $ActiveCurrentNext.)
    getNextEpochBakers :: BlockState m -> m FullBakers

    -- |Get the bakers for a particular (future) slot, provided genesis timestamp and slot duration.
    -- This is used for protocol version 'P1' to 'P3'.
    -- This should not be used for a slot less than the slot of the block.
    getSlotBakersP1 :: (AccountVersionFor (MPV m) ~ 'AccountV0) => BlockState m -> Slot -> m FullBakers

    -- |Get the account of a baker. This may return an account even
    -- if the account is not (currently) a baker, since a 'BakerId'
    -- uniquely determines an account over time.
    getBakerAccount :: BlockState m -> BakerId -> m (Maybe (Account m))

    -- |Get reward summary for this block.
    getRewardStatus :: BlockState m -> m (RewardStatus' Epoch)

    -- |Get the outcome of a transaction in the given block.
    getTransactionOutcome :: BlockState m -> TransactionIndex -> m (Maybe TransactionSummary)

    -- |Get the transactionOutcomesHash of a given block.
    getTransactionOutcomesHash :: BlockState m -> m TransactionOutcomesHash

    -- |Get the stateHash of a given block.
    getStateHash :: BlockState m -> m StateHash

    -- |Get all transaction outcomes for this block.
    getOutcomes :: BlockState m -> m (Vec.Vector TransactionSummary)

    -- |Get special transactions outcomes (for administrative transactions, e.g., baker reward)
    -- They should be returned in the order that they were emitted.
    getSpecialOutcomes :: BlockState m -> m (Seq.Seq SpecialTransactionOutcome)

    -- |Get the identity provider info for a given block given by its id.
    getIdentityProvider :: BlockState m -> ID.IdentityProviderIdentity -> m (Maybe IpInfo)

    -- |Get all identity providers for a given block.
    getAllIdentityProviders :: BlockState m -> m [IpInfo]

    -- |Get the anonymity revokers with given ids. Returns 'Nothing' if any of the
    -- anonymity revokers are not found.
    getAnonymityRevokers :: BlockState m -> [ID.ArIdentity] -> m (Maybe [ArInfo])

    getAllAnonymityRevokers :: BlockState m -> m [ArInfo]

    -- |Get the value of the election difficulty parameter for a future timestamp.
    -- This function applies queued election difficultly updates as appropriate.
    getElectionDifficulty :: BlockState m -> Timestamp -> m ElectionDifficulty
    -- |Get the next sequence number for a particular update type.
    getNextUpdateSequenceNumber :: BlockState m -> UpdateType -> m UpdateSequenceNumber
    -- |Get the value of the election difficulty that was used to bake this block.
    getCurrentElectionDifficulty :: BlockState m -> m ElectionDifficulty
    -- |Get the current chain parameters and pending updates.
    getUpdates :: BlockState m -> m (UQ.Updates (MPV m))
    -- |Get pending changes to the time parameters.
    getPendingTimeParameters :: BlockState m -> m [(TransactionTime, TimeParameters (ChainParametersVersionFor (MPV m)))]
    -- |Get pending changes to the pool parameters.
    getPendingPoolParameters :: BlockState m -> m [(TransactionTime, PoolParameters (ChainParametersVersionFor (MPV m)))]
    -- |Get the protocol update status. If a protocol update has taken effect,
    -- returns @Left protocolUpdate@. Otherwise, returns @Right pendingProtocolUpdates@.
    -- The @pendingProtocolUpdates@ is a (possibly-empty) list of timestamps and protocol
    -- updates that have not yet taken effect.
    getProtocolUpdateStatus :: BlockState m -> m UQ.ProtocolUpdateStatus

    -- |Get the current cryptographic parameters of the chain.
    getCryptographicParameters :: BlockState m -> m CryptographicParameters

    -- |Get the block's UpdateKeysCollection
    getUpdateKeysCollection :: BlockState m -> m (UpdateKeysCollection (ChainParametersVersionFor (MPV m)))
    
    -- |Get the current energy to microCCD exchange rate
    getEnergyRate :: BlockState m -> m EnergyRate

    -- |Get the epoch time of the next scheduled payday.
    getPaydayEpoch :: (SupportsDelegation (MPV m)) => BlockState m -> m Epoch

    -- |Get a 'PoolStatus' record describing the status of a baker pool (when the 'BakerId' is
    -- provided) or the passive delegators (when 'Nothing' is provided). The result is 'Nothing'
    -- if the 'BakerId' is not currently a baker.
    getPoolStatus :: (SupportsDelegation (MPV m))
        => BlockState m -> Maybe BakerId -> m (Maybe PoolStatus)

-- |Distribution of newly-minted GTU.
data MintAmounts = MintAmounts {
    -- |Minted amount allocated to the BakingRewardAccount
    mintBakingReward :: !Amount,
    -- |Minted amount allocated to the FinalizationRewardAccount
    mintFinalizationReward :: !Amount,
    -- |Minted amount allocated ot the foundation account
    mintDevelopmentCharge :: !Amount
  } deriving (Eq,Show)

instance Semigroup MintAmounts where
  a1 <> a2 = MintAmounts {
      mintBakingReward = mintBakingReward a1 + mintBakingReward a2,
      mintFinalizationReward = mintFinalizationReward a1 + mintFinalizationReward a2,
      mintDevelopmentCharge = mintDevelopmentCharge a1 + mintDevelopmentCharge a2
    }

instance Monoid MintAmounts where
  mempty = MintAmounts 0 0 0
  mconcat = foldl' (<>) mempty

mintTotal :: MintAmounts -> Amount
mintTotal MintAmounts{..} = mintBakingReward + mintFinalizationReward + mintDevelopmentCharge


-- |Data needed by blockstate to create a new instance. This contains all data
-- except the instance address and the derived instance hashes. The address is
-- determined when the instance is inserted in the instance table. The hashes
-- are computed on insertion.
--
-- The fields of this type are deliberately not strict since this is just an intermediate type
-- to simplify function API. Thus values are immediately deconstructed.
-- It is parameterized by the concrete instrumented module @im@ and the
-- WasmVersion @v@.
data NewInstanceData im v = NewInstanceData {
  -- |Name of the init method used to initialize the contract.
  nidInitName :: Wasm.InitName,
  -- |Receive functions suitable for this instance.
  nidEntrypoints :: Set.Set Wasm.ReceiveName,
  -- |Module interface that contains the code of the contract.
  nidInterface :: GSWasm.ModuleInterfaceA im,
  -- |Initial state of the instance.
  nidInitialState :: UpdatableContractState v,
  -- |Initial balance.
  nidInitialAmount :: Amount,
  -- |Owner/creator of the instance.
  nidOwner :: AccountAddress
  }

-- |Information about a delegator.
data ActiveDelegatorInfo = ActiveDelegatorInfo {
    -- |ID of the delegator.
    activeDelegatorId :: !DelegatorId,
    -- |Amount delegated to the target pool.
    activeDelegatorStake :: !Amount,
    -- |Any pending change to delegator.
    activeDelegatorPendingChange :: !(StakePendingChange' Timestamp)
  }
  deriving (Eq, Show)

-- |Information about a baker, including its delegators.
data ActiveBakerInfo' bakerInfoRef = ActiveBakerInfo {
    -- |A reference to the baker info for the baker.
    activeBakerInfoRef :: !bakerInfoRef,
    -- |The equity capital of the active baker.
    activeBakerEquityCapital :: !Amount,
    -- |Any pending change to the baker's stake.
    activeBakerPendingChange :: !(StakePendingChange' Timestamp),
    -- |Information about the delegators to the baker in ascending order of 'DelegatorId'.
    -- (There must be no duplicate 'DelegatorId's.)
    activeBakerDelegators :: ![ActiveDelegatorInfo]
  }
  deriving (Eq, Show)

-- |Information about a baker, including its delegators.
type ActiveBakerInfo m = ActiveBakerInfo' (BakerInfoRef m)

-- |Block state update operations parametrized by a monad. The operations which
-- mutate the state all also return an 'UpdatableBlockState' handle. This is to
-- support different implementations, from pure ones to stateful ones.
class (BlockStateQuery m) => BlockStateOperations m where
  -- |Get the module from the module table of the state instance.
  bsoGetModule :: UpdatableBlockState m -> ModuleRef -> m (Maybe (GSWasm.ModuleInterface (InstrumentedModuleRef m)))
  -- |Get an account by its address.
  bsoGetAccount :: UpdatableBlockState m -> AccountAddress -> m (Maybe (IndexedAccount m))
  -- |Get the index of an account.
  bsoGetAccountIndex :: UpdatableBlockState m -> AccountAddress -> m (Maybe AccountIndex)
  -- | Get account by the index.
  bsoGetAccountByIndex :: UpdatableBlockState m -> AccountIndex -> m (Maybe (Account m))
  -- |Get the contract state from the contract table of the state instance.
  bsoGetInstance :: UpdatableBlockState m -> ContractAddress -> m (Maybe (InstanceInfo m))

  -- |Check whether the given account address would clash with any existing address.
  bsoAddressWouldClash :: UpdatableBlockState m -> ID.AccountAddress -> m Bool

  -- |Check whether the given credential registration ID exists.
  bsoRegIdExists :: UpdatableBlockState m -> ID.CredentialRegistrationID -> m Bool

  -- |Create and add an empty account with the given public key, address and credential.
  -- If an account with the given address already exists, @Nothing@ is returned.
  -- Otherwise, the new account is returned, and the credential is added to the known credentials.
  --
  -- It is not checked if the account's credential is a duplicate.
  bsoCreateAccount :: UpdatableBlockState m -> GlobalContext -> AccountAddress -> AccountCredential -> m (Maybe (Account m), UpdatableBlockState m)

  -- |Add a new smart contract instance to the state.
  bsoPutNewInstance :: forall v . Wasm.IsWasmVersion v => UpdatableBlockState m -> NewInstanceData (InstrumentedModuleRef m v) v -> m (ContractAddress, UpdatableBlockState m)
  -- |Add the module to the global state. If a module with the given address
  -- already exists return @False@.
  bsoPutNewModule :: Wasm.IsWasmVersion v => UpdatableBlockState m -> (GSWasm.ModuleInterfaceV v, Wasm.WasmModuleV v) -> m (Bool, UpdatableBlockState m)

  -- |Modify an existing account with given data (which includes the address of the account).
  -- This method is only called when an account exists and can thus assume this.
  -- NB: In case we are adding a credential to an account this method __must__ also
  -- update the global set of known credentials.
  --
  -- It is the responsibility of the caller to ensure that the change does not lead to a
  -- negative account balance or a situation where the staked or locked balance
  -- exceeds the total balance on the account.
  bsoModifyAccount :: UpdatableBlockState m -> AccountUpdate -> m (UpdatableBlockState m)

  -- |Update the public keys for a specific credential on an account.
  --
  -- The caller must ensure that the account exists and has a credential with the given
  -- index.
  bsoSetAccountCredentialKeys :: UpdatableBlockState m -> AccountIndex -> ID.CredentialIndex -> ID.CredentialPublicKeys -> m (UpdatableBlockState m)

  -- |Update the set of credentials on a given account by: removing credentials, adding
  -- credentials, and updating the account threshold (i.e. number of credentials that are
  -- required for a valid signature from the account).  Added credentials will be be added
  -- to the global set of known credentials.
  --
  -- The caller is responsible for establishing the following preconditions:
  --  * The account exists and is valid.
  --  * The credential indexes to remove already exist on the account.
  --  * The credentials to add have not been used before (i.e. 'bsoRegIdExists' returns
  --    @False@ for each of them).
  --  * The credential indexes that are added do not already have credentials, after the
  --    removals have occurred.
  --  * The account threshold is at least 1 and at most the total number of credentials
  --    on the account after the specified credentials have been removed and added.
  --
  -- The removed credentials will be considered removed in the order of the provided list.
  -- This ordering is significant because it affects the account hash.
  bsoUpdateAccountCredentials ::
    UpdatableBlockState m
    -> AccountIndex
    -> [ID.CredentialIndex]
    -- ^Credentials to remove
    -> Map.Map ID.CredentialIndex AccountCredential
    -- ^Credentials to add
    -> ID.AccountThreshold
    -- ^New account threshold
    -> m (UpdatableBlockState m)

  -- |Replace the instance with given change in owned amount, potentially
  -- a new state and maybe new instance parameters depending on whether the contract has been upgraded.
  -- This method is only called when it is
  -- known the instance exists, and is of the version specified by the type
  -- parameter v. These preconditions can thus be assumed by any implementor.
  bsoModifyInstance :: forall v. Wasm.IsWasmVersion v =>
                      UpdatableBlockState m
                    -> ContractAddress
                    -> AmountDelta
                    -> Maybe (UpdatableContractState v)
                    -> Maybe (GSWasm.ModuleInterfaceA (InstrumentedModuleRef m v), Set.Set Wasm.ReceiveName)
                    -> m (UpdatableBlockState m)

  -- |Notify that some amount was transferred from/to encrypted balance of some account.
  bsoNotifyEncryptedBalanceChange :: UpdatableBlockState m -> AmountDelta -> m (UpdatableBlockState m)

  -- |Get the seed state associated with the block state.
  bsoGetSeedState :: UpdatableBlockState m -> m SeedState

  -- |Set the seed state associated with the block state.
  --
  -- Note: on no account should the epoch length be changed using this
  -- function (or otherwise).  The epoch length is assumed to be constant,
  -- so that epochs can always be calculated by dividing slot number by the
  -- epoch length.  Any change would throw off this calculation.
  bsoSetSeedState :: UpdatableBlockState m -> SeedState -> m (UpdatableBlockState m)

  -- |Replace the current epoch bakers with the next epoch bakers.
  -- This does not change the next epoch bakers.
  bsoRotateCurrentEpochBakers :: UpdatableBlockState m -> m (UpdatableBlockState m)

  -- |Update the set containing the next epoch bakers, to use for next epoch.
  bsoSetNextEpochBakers :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> 
      [(BakerInfoRef m, Amount)] -> m (UpdatableBlockState m)

  -- |Update the bakers for the next epoch.
  --
  -- 1. The current epoch bakers are replaced with the next epoch bakers.
  --
  -- 2. The active bakers are processed to apply any removals or stake reductions.
  --
  -- 3. The next epoch bakers are derived from the active bakers.
  --
  -- Note that instead of iteratively calling this for a succession of epochs,
  -- it should always be sufficient to just call it for the last two of them.
  bsoTransitionEpochBakers
    :: (AccountVersionFor (MPV m) ~ 'AccountV0)
    => UpdatableBlockState m
    -> Epoch
    -- ^The new epoch
    -> m (UpdatableBlockState m)


  -- |Process a pending changes on all bakers and delegators.
  -- Pending changes are only applied if they are effective according to the supplied guard
  -- function.
  -- For bakers pending removal, this removes the baker record and removes the baker from the active
  -- bakers (transferring any delegators to passive delegation).
  -- For bakers pending stake reduction, this reduces the stake.
  -- For delegators pending removal, this removes the delegation record and removes the record of
  -- the delegation from the active bakers index.
  -- For delegators pending stake reduction, this reduces the stake.
  bsoProcessPendingChanges
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> (Timestamp -> Bool)
    -- ^Guard determining if a change is effective
    -> m (UpdatableBlockState m)

  -- |Get the list of all active bakers in ascending order.
  bsoGetActiveBakers :: UpdatableBlockState m -> m [BakerId]

  -- |Get the currently-registered (i.e. active) bakers with their delegators, as well as the
  -- set of passive delegators. In each case, the lists are ordered in ascending Id order,
  -- with no duplicates.
  bsoGetActiveBakersAndDelegators
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> m ([ActiveBakerInfo m], [ActiveDelegatorInfo])

  -- |Get the bakers for the epoch in which the block was baked.
  bsoGetCurrentEpochBakers :: UpdatableBlockState m -> m FullBakers

  -- |Get the bakers for the epoch in which the block was baked, together with their commission rates.
  bsoGetCurrentEpochFullBakersEx :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> m FullBakersEx

  -- |Get the bakers for the epoch in which the block was baked.
  bsoGetCurrentCapitalDistribution
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> m CapitalDistribution

  -- |Register this account as a baker.
  -- The following results are possible:
  --
  -- * @BASuccess id@: the baker was created with the specified 'BakerId'.
  --   @id@ is always chosen to be the account index.
  --
  -- * @BAInvalidAccount@: the address does not resolve to a valid account.
  --
  -- * @BAAlreadyBaker id@: the account is already registered as a baker.
  --
  -- * @BADuplicateAggregationKey@: the aggregation key is already in use.
  --
  -- Note that if two results could apply, the first in this list takes precedence.
  --
  -- The caller MUST ensure that the staked amount does not exceed the total
  -- balance on the account.
  bsoAddBaker
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => UpdatableBlockState m
    -> AccountIndex
    -> BakerAdd
    -> m (BakerAddResult, UpdatableBlockState m)

  -- |From chain parameters version >= 1, this operation is used to add/remove/update a baker.
  -- When adding baker, it is assumed that 'AccountIndex' account is NOT a baker and NOT a delegator.
  --
  -- When the argument is 'BakerConfigureAdd', the caller __must ensure__ that:
  -- * the account is valid;
  -- * the account is not a baker;
  -- * the account is not a delegator;
  -- * the account has sufficient balance to cover the stake.
  --
  -- The function behaves as follows:
  --
  -- 1. If the account index is not valid, return 'BCInvalidAccount'.
  -- 2. If the baker's capital is less than the minimum threshold, return 'BCStakeUnderThreshold'.
  -- 3. If the transaction fee commission is not in the acceptable range, return
  --    'BCTransactionFeeCommissionNotInRange'.
  -- 4. If the baking reward commission is not in the acceptable range, return
  --    'BCBakingRewardCommissionNotInRange'.
  -- 5. If the finalization reward commission is not in the acceptable range, return
  --    'BCFinalizationRewardCommissionNotInRange'.
  -- 6. If the aggregation key is a duplicate, return 'BCDuplicateAggregationKey'.
  -- 7. Add the baker to the account, updating the indexes as follows:
  --    * add an empty pool for the baker in the active bakers;
  --    * add the baker's equity capital to the total active capital;
  --    * add the baker's aggregation key to the aggregation key set.
  -- 8. Return @BCSuccess []@.
  --
  -- When the argument is 'BakerConfigureUpdate', the caller __must ensure__ that:
  -- * the account is valid;
  -- * the account is a baker;
  -- * if the stake is being updated, then the account balance exceeds the new stake.
  --
  -- The function behaves as follows, building a list @events@:
  --
  -- 1. If keys are supplied: if the aggregation key duplicates an existing aggregation key @key@
  --    (except this baker's current aggregation key), return @BCDuplicateAggregationKey key@;
  --    otherwise, update the keys with the supplied @keys@, update the aggregation key index
  --    (removing the old key and adding the new one), and append @BakerConfigureUpdateKeys keys@
  --    to @events@.
  -- 2. If the restake earnings flag is supplied: update the account's flag to the supplied value
  --    @restakeEarnings@ and append @BakerConfigureRestakeEarnings restakeEarnings@ to @events@.
  -- 3. If the open-for-delegation configuration is supplied:
  --    (1) update the account's configuration to the supplied value @openForDelegation@;
  --    (2) if @openForDelegation == ClosedForAll@, transfer all delegators in the baker's pool to
  --        passive delegation; and
  --    (3) append @BakerConfigureOpenForDelegation openForDelegation@ to @events@.
  -- 4. If the metadata URL is supplied: update the account's metadata URL to the supplied value
  --    @metadataURL@ and append @BakerConfigureMetadataURL metadataURL@ to @events@.
  -- 5. If the transaction fee commission is supplied:
  --    (1) if the commission does not fall within the current range according to the chain
  --        parameters, return @BCTransactionFeeCommissionNotInRange@; otherwise,
  --    (2) update the account's transaction fee commission rate to the the supplied value @tfc@;
  --    (3) append @BakerConfigureTransactionFeeCommission tfc@ to @events@.
  -- 6. If the baking reward commission is supplied:
  --    (1) if the commission does not fall within the current range according to the chain
  --        parameters, return @BCBakingRewardCommissionNotInRange@; otherwise,
  --    (2) update the account's baking reward commission rate to the the supplied value @brc@;
  --    (3) append @BakerConfigureBakingRewardCommission brc@ to @events@.
  -- 6. If the finalization reward commission is supplied:
  --    (1) if the commission does not fall within the current range according to the chain
  --        parameters, return @BCFinalizationRewardCommissionNotInRange@; otherwise,
  --    (2) update the account's finalization reward commission rate to the the supplied value @frc@;
  --    (3) append @BakerConfigureFinalizationRewardCommission frc@ to @events@.
  -- 7. If the capital is supplied: if there is a pending change to the baker's capital, return
  --    @BCChangePending@; otherwise:
  --    * if the capital is 0, mark the baker as pending removal at @bcuSlotTimestamp@ plus the
  --      the current baker cooldown period according to the chain parameters, and append
  --      @BakerConfigureStakeReduced 0@ to @events@;
  --    * if the capital is less than the current minimum equity capital, return @BCStakeUnderThreshold@;
  --    * if the capital is (otherwise) less than the current equity capital of the baker, mark the
  --      baker as pending stake reduction to the new capital at @bcuSlotTimestamp@ plus the
  --      current baker cooldown period according to the chain parameters and append
  --      @BakerConfigureStakeReduced capital@ to @events@;
  --    * if the capital is equal to the baker's current equity capital, do nothing, append
  --      @BakerConfigureStakeIncreased capital@ to @events@;
  --    * if the capital is greater than the baker's current equity capital, increase the baker's
  --      equity capital to the new capital (updating the total active capital in the active baker
  --      index by adding the difference between the new and old capital) and append
  --      @BakerConfigureStakeIncreased capital@ to @events@.
  -- 8. return @BCSuccess events bid@, where @bid@ is the baker's ID.
  --
  -- Note: in the case of an early return (i.e. not @BCSuccess@), the state is not updated.
  bsoConfigureBaker
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> AccountIndex
    -> BakerConfigure
    -> m (BakerConfigureResult, UpdatableBlockState m)
  
  -- |Constrain the baker's commission rates to fall in the given ranges.
  -- If the account is invalid or not a baker, this does nothing.
  bsoConstrainBakerCommission
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> AccountIndex
    -> CommissionRanges
    -> m (UpdatableBlockState m)

  -- |From chain parameters version >= 1, this operation is used to add/remove/update a delegator.
  -- When adding delegator, it is assumed that 'AccountIndex' account is NOT a baker and NOT a delegator.
  --
  -- When the argument is 'DelegationConfigureAdd', the caller __must ensure__ that:
  -- * the account is valid;
  -- * the account is not a baker;
  -- * the account is not a delegator;
  -- * the delegated amount does not exceed the account's balance.
  --
  -- The function behaves as follows:
  --
  -- 1. If the delegation target is a valid baker that is not 'OpenForAll', return 'DCPoolClosed'.
  -- 2. If the delegation target is baker id @bid@, but the baker does not exist, return
  --    @DCInvalidDelegationTarget bid@.
  -- 3. Update the active bakers index to record:
  --    * the delegator delegates to the target pool;
  --    * the target pool's delegated capital is increased by the delegated amount;
  --    * the total active capital is increased by the delegated amount.
  -- 4. Update the account to record the specified delegation.
  -- 5. If the amount delegated to the delegation target exceeds the leverage bound, return
  --    'DCPoolStakeOverThreshold' and revert any changes.
  -- 6. If the amount delegated to the delegation target exceed the capital bound, return
  --    'DCPoolOverDelegated' and revert any changes.
  -- 7. Return @DCSuccess []@ with the updated state.
  --
  -- When the argument is 'DelegationConfigureUpdate', the caller __must ensure__ that:
  -- * the account is valid;
  -- * the account is a delegator;
  -- * if the delegated amount is updated, it does not exceed the account's balance.
  --
  -- The function behaves as follows, building a list @events@:
  --
  -- 1. If the delegation target is specified as @target@:
  --    (1) If the delegation target is a valid baker that is not 'OpenForAll', return 'DCPoolClosed'.
  --    (2) If the delegation target is baker id @bid@, but the baker does not exist, return
  --        @DCInvalidDelegationTarget bid@.
  --    (3) Update the active bakers index to: remove the delegator and delegated amount from the
  --        old baker pool, and add the delegator and delegated amount to the new baker pool.
  --        (Note, the total delegated amount is unchanged at this point.)
  --    (4) Update the account to record the new delegation target.
  --    (5) Append @DelegationConfigureDelegationTarget target@ to @events@. [N.B. if the target is
  --        pool is the same as the previous value, steps (1)-(4) will do nothing and may be skipped
  --        by the implementation. This relies on the invariant that delegators delegate only to
  --        valid pools.]
  -- 2. If the "restake earnings" flag is specified as @restakeEarnings@:
  --    (1) Update the restake earnings flag on the account to match @restakeEarnings@.
  --    (2) Append @DelegationConfigureRestakeEarnings restakeEarnings@ to @events@.
  -- 3. If the delegated capital is specified as @capital@: if there is a pending change to the
  --    delegator's stake, return 'DCChangePending'; otherwise:
  --    * If the new capital is 0, mark the delegator as pending removal at the slot timestamp
  --      plus the delegator cooldown chain parameter, and append
  --      @DelegationConfigureStakeReduced capital@ to @events@; otherwise
  --    * If the the new capital is less than the current staked capital (but not 0), mark the
  --      delegator as pending stake reduction to @capital@ at the slot timestamp plus the
  --      delegator cooldown chain parameter, and append @DelegationConfigureStakeReduced capital@
  --      to @events@;
  --    * If the new capital is equal to the current staked capital, append
  --      @DelegationConfigureStakeIncreased capital@ to @events@.
  --    * If the new capital is greater than the current staked capital by @delta > 0@:
  --      * increase the total active capital by @delta@,
  --      * increase the delegator's target pool delegated capital by @delta@,
  --      * set the baker's delegated capital to @capital@, and
  --      * append @DelegationConfigureStakeIncreased capital@ to @events@.
  -- 4. If the amount delegated to the delegation target exceeds the leverage bound, return
  --    'DCPoolStakeOverThreshold' and revert any changes.
  -- 5. If the amount delegated to the delegation target exceed the capital bound, return
  --    'DCPoolOverDelegated' and revert any changes.
  -- 6. Return @DCSuccess events@ with the updated state.
  --
  -- Note, if the return code is anything other than 'DCSuccess', the original state is returned.
  -- If the preconditions are violated, the function may return 'DCInvalidAccount' (if the account
  -- is not valid) or 'DCInvalidDelegator' (when updating, if the account is not a delegator).
  -- However, this behaviour is not guaranteed, and could result in violations of the state
  -- invariants.
  bsoConfigureDelegation
    :: (SupportsDelegation (MPV m))
    => UpdatableBlockState m
    -> AccountIndex
    -> DelegationConfigure
    -> m (DelegationConfigureResult, UpdatableBlockState m)

  -- |Update the keys associated with an account.
  -- It is assumed that the keys have already been checked for validity/ownership as
  -- far as is necessary.
  -- The only check on the keys is that the aggregation key is not a duplicate.
  --
  -- The following results are possible:
  --
  -- * @BKUSuccess id@: the keys were updated
  --
  -- * @BKUInvalidBaker@: the account does not exist or is not currently a baker.
  --
  -- * @BKUDuplicateAggregationKey@: the aggregation key is a duplicate.
  bsoUpdateBakerKeys :: (AccountVersionFor (MPV m) ~ 'AccountV0) => UpdatableBlockState m -> AccountIndex -> BakerKeyUpdate -> m (BakerKeyUpdateResult, UpdatableBlockState m)

  -- |Update the stake associated with an account.
  -- A reduction in stake will be delayed by the current cool-off period.
  -- A change will not be made if there is already a cooling-off change
  -- pending for the baker.
  --
  -- A change can specify the new amount to stake and whether or not to restake reward earnings,
  -- although both of these are optional.  Either all changes will be applied, or none of them.
  --
  -- The following results are possible:
  --
  -- * @BSUStakeIncreased id@: the baker's stake was increased.
  --   This will take effect in the epoch after next.
  --
  -- * @BSUStakeReduced id e@: the baker's stake was reduced, effective from epoch @e@.
  --
  -- * @BSUStakeUnchanged od@: there is no change to the baker's stake, but this update was successful.
  --
  -- * @BSUInvalidBaker@: the account does not exist, or is not currently a baker.
  --
  -- * @BSUChangePending id@: the change could not be made since the account is already in a cooling-off period.
  --
  -- The caller MUST ensure that the staked amount does not exceed the total balance on the account.
  bsoUpdateBakerStake
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => UpdatableBlockState m
    -> AccountIndex
    -> Amount
    -> m (BakerStakeUpdateResult, UpdatableBlockState m)

  -- |Update whether a baker's earnings are automatically restaked.
  --
  -- The following results are possible:
  --
  -- * @BREUUpdated id@: the flag was updated.
  --
  -- * @BREUInvalidBaker@: the account does not exists, or is not currently a baker.
  bsoUpdateBakerRestakeEarnings :: (AccountVersionFor (MPV m) ~ 'AccountV0) => UpdatableBlockState m -> AccountIndex -> Bool -> m (BakerRestakeEarningsUpdateResult, UpdatableBlockState m)

  -- |Remove the baker associated with an account.
  -- The removal takes effect after a cooling-off period.
  -- Removal may fail if the baker is already cooling-off from another change (e.g. stake reduction).
  --
  -- The following results are possible:
  --
  -- * @BRRemoved id e@: the baker was removed, effective from epoch @e@.
  --
  -- * @BRInvalidBaker@: the account address is not valid, or the account is not a baker.
  --
  -- * @BRChangePending id@: the baker is currently in a cooling-off period and so cannot be removed.
  bsoRemoveBaker
    :: (AccountVersionFor (MPV m) ~ 'AccountV0, ChainParametersVersionFor (MPV m) ~ 'ChainParametersV0)
    => UpdatableBlockState m
    -> AccountIndex
    -> m (BakerRemoveResult, UpdatableBlockState m)

  -- |Add an amount to a baker or delegator's account as a reward. The accounts's stake is increased
  -- correspondingly if it is set to restake rewards.
  -- If the id refers to an account, the reward is paid to the account, and the
  -- address of the account is returned.  If the id does not refer to an account
  -- then no change is made and @Nothing@ is returned.
  bsoRewardAccount :: UpdatableBlockState m -> AccountIndex -> Amount -> m (Maybe AccountAddress, UpdatableBlockState m)

  -- |Function 'bsoGetBakerPoolRewardDetails' returns a map with the 'BakerPoolRewardDetails' for each
  -- current-epoch baker (in the 'CapitalDistribution' returned by 'bsoGetCurrentCapitalDistribution').
  bsoGetBakerPoolRewardDetails :: SupportsDelegation (MPV m) => UpdatableBlockState m -> m (Map.Map BakerId BakerPoolRewardDetails)

  -- |Update the transaction fee rewards accruing to a baker pool by the specified delta. It is a
  -- precondition that the given baker is a current-epoch baker.
  -- Note, in practice, this is only used to increase the amount accrued to a baker
  -- as 'bsoRotateCurrentCapitalDistribution' resets the rewards to bakers.
  bsoUpdateAccruedTransactionFeesBaker :: SupportsDelegation (MPV m) => UpdatableBlockState m -> BakerId -> AmountDelta -> m (UpdatableBlockState m)

  -- |Mark that the given bakers have signed a finalization proof included in a block during the
  -- reward period. Any baker ids that are not current-epoch bakers will be ignored.
  -- (This is significant, as the finalization record in a block may be signed by a finalizer that
  -- has since been removed as a baker.)
  -- Note, the finalization-awake status is reset by 'bsoRotateCurrentCapitalDistribution'.
  bsoMarkFinalizationAwakeBakers :: SupportsDelegation (MPV m) => UpdatableBlockState m -> [BakerId] -> m (UpdatableBlockState m)

  -- |Update the transaction fee rewards accrued to be distributed to the passive delegators.
  -- Note, unlike 'bsoUpdateAccruedTransactionFeesBaker', this is __not__ reset by
  -- 'bsoRotateCurrentCapitalDistribution'. When the passive rewards are paid out, this function is
  -- called to reduce the accrued rewards by the corresponding amount.
  bsoUpdateAccruedTransactionFeesPassive :: SupportsDelegation (MPV m) => UpdatableBlockState m -> AmountDelta -> m (UpdatableBlockState m)

  -- |Get the accrued transaction fee rewards to the passive delegators.
  bsoGetAccruedTransactionFeesPassive :: SupportsDelegation (MPV m) => UpdatableBlockState m -> m Amount

  -- |Update the transaction fee rewards accruing to the foundation account.
  -- As with 'bsoUpdateAccruedTransactionFeesPassive', this is used to accrue the rewards and to
  -- reset the accrued amount when the rewards are paid out.
  bsoUpdateAccruedTransactionFeesFoundationAccount :: SupportsDelegation (MPV m) => UpdatableBlockState m -> AmountDelta -> m (UpdatableBlockState m)

  -- |Get the transaction fee rewards accruing to the foundation account.
  bsoGetAccruedTransactionFeesFoundationAccount :: SupportsDelegation (MPV m) => UpdatableBlockState m -> m Amount

  -- |Add an amount to the foundation account.
  bsoRewardFoundationAccount :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Get the foundation account.
  bsoGetFoundationAccount :: UpdatableBlockState m -> m (Account m)

  -- |Mint currency and distribute it to the BakerRewardAccount,
  -- FinalizationRewardAccount and foundation account.
  -- This increases the total GTU in circulation.
  bsoMint :: UpdatableBlockState m -> MintAmounts -> m (UpdatableBlockState m)

  -- |Get the identity provider data for the given identity provider, or Nothing if
  -- the identity provider with given ID does not exist.
  bsoGetIdentityProvider :: UpdatableBlockState m -> ID.IdentityProviderIdentity -> m (Maybe IpInfo)

  -- |Get the anonymity revokers with given ids. Returns 'Nothing' if any of the
  -- anonymity revokers are not found.
  bsoGetAnonymityRevokers :: UpdatableBlockState m -> [ID.ArIdentity] -> m (Maybe [ArInfo])

  -- |Get the current cryptographic parameters. The idea is that these will be
  -- periodically updated and so they must be part of the block state.
  bsoGetCryptoParams :: UpdatableBlockState m -> m CryptographicParameters

  -- |Get the epoch time of the next scheduled payday.
  bsoGetPaydayEpoch :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> m Epoch

  -- |Get the mint rate of the next scheduled payday.
  bsoGetPaydayMintRate :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> m MintRate

  -- |Set the epoch of the next scheduled payday.
  bsoSetPaydayEpoch :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> Epoch -> m (UpdatableBlockState m)

  -- |Set the mint rate of the next scheduled payday.
  bsoSetPaydayMintRate :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> MintRate -> m (UpdatableBlockState m)

  -- |Set the transaction outcomes for the block.
  bsoSetTransactionOutcomes :: UpdatableBlockState m -> [TransactionSummary] -> m (UpdatableBlockState m)

  -- |Add a special transaction outcome.
  bsoAddSpecialTransactionOutcome :: UpdatableBlockState m -> SpecialTransactionOutcome -> m (UpdatableBlockState m)

  -- |Process queued updates.
  bsoProcessUpdateQueues
    :: UpdatableBlockState m
    -> Timestamp
    -> m (Map.Map TransactionTime (UpdateValue (ChainParametersVersionFor (MPV m))), UpdatableBlockState m)

  -- |Unlock the amounts up to the given timestamp
  bsoProcessReleaseSchedule :: UpdatableBlockState m -> Timestamp -> m (UpdatableBlockState m)

  -- |Get the current 'Authorizations' for validating updates.
  bsoGetUpdateKeyCollection
    :: UpdatableBlockState m
    -> m (UpdateKeysCollection (ChainParametersVersionFor (MPV m)))
  -- |Get the next 'UpdateSequenceNumber' for a given update type.
  bsoGetNextUpdateSequenceNumber :: UpdatableBlockState m -> UpdateType -> m UpdateSequenceNumber

  -- |Enqueue an update to take effect at the specified time.
  bsoEnqueueUpdate
    :: UpdatableBlockState m
    -> TransactionTime
    -> (UpdateValue (ChainParametersVersionFor (MPV m)))
    -> m (UpdatableBlockState m)

  -- |Overwrite the election difficulty, removing any queued election difficulty updates.
  -- This is intended to be used for protocol updates that affect the election difficulty in
  -- tandem with the slot duration.
  -- Note that this does not affect the next sequence number for election difficulty updates.
  bsoOverwriteElectionDifficulty :: UpdatableBlockState m -> ElectionDifficulty -> m (UpdatableBlockState m)

  -- |Clear the protocol update and any queued protocol updates.
  -- This is intended to be used to reset things after a protocol update has taken effect.
  -- This does not affect the next sequence number for protocol updates.
  bsoClearProtocolUpdate :: UpdatableBlockState m -> m (UpdatableBlockState m)

  -- |Add the given accounts and timestamps to the per-block account release schedule.
  -- PRECONDITION: The given timestamp must be the first timestamp for a release for the given account.
  bsoAddReleaseSchedule :: UpdatableBlockState m -> [(AccountAddress, Timestamp)] -> m (UpdatableBlockState m)

  -- |Get the current energy rate.
  bsoGetEnergyRate :: UpdatableBlockState m -> m EnergyRate

  -- |Get the current chain parameters.
  bsoGetChainParameters :: UpdatableBlockState m -> m (ChainParameters (MPV m))

  -- * Reward details

  -- |Get the number of blocks baked in this epoch, both in total and
  -- per baker.
  bsoGetEpochBlocksBaked :: UpdatableBlockState m -> m (Word64, [(BakerId, Word64)])

  -- |Record that the given baker has baked a block in the current epoch. It is a precondition that
  -- the given baker is a current-epoch baker.
  bsoNotifyBlockBaked :: UpdatableBlockState m -> BakerId -> m (UpdatableBlockState m)

  -- |Clear the tracking of baked blocks in the current epoch.
  -- Should be called whenever a new epoch is entered.
  -- (Only used prior to protocol P4.)
  bsoClearEpochBlocksBaked :: (AccountVersionFor (MPV m) ~ 'AccountV0) => UpdatableBlockState m -> m (UpdatableBlockState m)

  -- |Set the next capital distribution.
  bsoSetNextCapitalDistribution ::
    (SupportsDelegation (MPV m)) =>
    UpdatableBlockState m ->
    CapitalDistribution ->
    m (UpdatableBlockState m)

  -- |Set the current capital distribution to the current value of the next capital distribution.
  -- The next capital distribution is unchanged.
  -- This also clears transaction rewards and block counts accruing to baker pools.
  -- The passive delegator and foundation transaction rewards are not affected.
  bsoRotateCurrentCapitalDistribution :: (SupportsDelegation (MPV m)) => UpdatableBlockState m -> m (UpdatableBlockState m)

  -- |Get the current status of the various accounts.
  bsoGetBankStatus :: UpdatableBlockState m -> m BankStatus

  -- |Set the status of the special reward accounts.
  bsoSetRewardAccounts :: UpdatableBlockState m -> RewardAccounts -> m (UpdatableBlockState m)

-- | Block state storage operations
class (BlockStateOperations m, FixedSizeSerialization (BlockStateRef m)) => BlockStateStorage m where
    -- |Derive a mutable state instance from a block state instance. The mutable
    -- state instance supports all the operations needed by the scheduler for
    -- block execution. Semantically the 'UpdatableBlockState' must be a copy,
    -- changes to it must not affect 'BlockState', but an efficient
    -- implementation should expect that only a small subset of the state will
    -- change, and thus a variant of copy-on-write should be used.
    thawBlockState :: BlockState m -> m (UpdatableBlockState m)

    -- |Freeze a mutable block state instance. The mutable state instance will
    -- not be used afterwards and the implementation can thus avoid copying
    -- data.
    freezeBlockState :: UpdatableBlockState m -> m (BlockState m)

    -- |Discard a mutable block state instance.  The mutable state instance will
    -- not be used afterwards.
    dropUpdatableBlockState :: UpdatableBlockState m -> m ()

    -- |Mark the given state instance as no longer needed and eventually
    -- discharge it. This can happen, for instance, when a block becomes dead
    -- due to finalization. The block state instance will not be accessed after
    -- this method is called.
    purgeBlockState :: BlockState m -> m ()

    -- |Mark a block state for archive: i.e. it will no longer be needed by
    -- consensus (but could be required for historical queries).
    archiveBlockState :: BlockState m -> m ()

    -- |Ensure that a block state is stored and return a reference to it.
    saveBlockState :: BlockState m -> m (BlockStateRef m)

    -- |Load a block state from a reference, given its state hash.
    loadBlockState :: StateHash -> BlockStateRef m -> m (BlockState m)

    -- |Serialize the block state to a byte string.
    -- This serialization does not include transaction outcomes.
    serializeBlockState :: BlockState m -> m BS.ByteString

    -- |Retrieve the callback that is needed to read state that is not in
    -- memory. This is needed for using V1 contract state.
    blockStateLoadCallback :: m LoadCallback

    -- |Shut down any caches used by the block state. This is used to free
    -- up the memory in the case where the block state is no longer being
    -- actively used, in particular, after a protocol update.
    collapseCaches :: m ()

instance (Monad (t m), MonadTrans t, ModuleQuery m) => ModuleQuery (MGSTrans t m) where
  getModuleArtifact = lift . getModuleArtifact
  {-# INLINE getModuleArtifact #-}

instance (Monad (t m), MonadTrans t, BlockStateQuery m) => BlockStateQuery (MGSTrans t m) where
  getModule s = lift . getModule s
  getModuleInterface s = lift . getModuleInterface s
  getAccount s = lift . getAccount s
  accountExists s = lift . accountExists s
  getActiveBakers = lift . getActiveBakers
  getActiveBakersAndDelegators = lift . getActiveBakersAndDelegators
  getActiveDelegators bs = lift . getActiveDelegators bs
  getCurrentDelegators bs = lift . getCurrentDelegators bs
  getAccountByCredId s = lift . getAccountByCredId s
  getAccountByIndex s = lift . getAccountByIndex s
  getBakerAccount s = lift . getBakerAccount s
  getContractInstance s = lift . getContractInstance s
  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList
  getSeedState = lift . getSeedState
  getCurrentEpochBakers = lift . getCurrentEpochBakers
  getNextEpochBakers = lift . getNextEpochBakers
  getSlotBakersP1 d = lift . getSlotBakersP1 d
  getRewardStatus = lift . getRewardStatus
  getTransactionOutcome s = lift . getTransactionOutcome s
  getTransactionOutcomesHash = lift . getTransactionOutcomesHash
  getStateHash = lift . getStateHash
  getOutcomes = lift . getOutcomes
  getSpecialOutcomes = lift . getSpecialOutcomes
  getAllIdentityProviders s = lift $ getAllIdentityProviders s
  getAllAnonymityRevokers s = lift $ getAllAnonymityRevokers s
  getElectionDifficulty s = lift . getElectionDifficulty s
  getNextUpdateSequenceNumber s = lift . getNextUpdateSequenceNumber s
  getCurrentElectionDifficulty = lift . getCurrentElectionDifficulty
  getUpdates = lift . getUpdates
  getPendingTimeParameters = lift . getPendingTimeParameters
  getPendingPoolParameters = lift . getPendingPoolParameters
  getProtocolUpdateStatus = lift . getProtocolUpdateStatus
  getCryptographicParameters = lift . getCryptographicParameters
  getIdentityProvider s = lift . getIdentityProvider s
  getAnonymityRevokers s = lift . getAnonymityRevokers s
  getUpdateKeysCollection s = lift $ getUpdateKeysCollection s
  getEnergyRate s = lift $ getEnergyRate s
  getPaydayEpoch = lift . getPaydayEpoch
  getPoolStatus s = lift . getPoolStatus s
  {-# INLINE getModule #-}
  {-# INLINE getAccount #-}
  {-# INLINE accountExists #-}
  {-# INLINE getAccountByCredId #-}
  {-# INLINE getAccountByIndex #-}
  {-# INLINE getBakerAccount #-}
  {-# INLINE getContractInstance #-}
  {-# INLINE getModuleList #-}
  {-# INLINE getAccountList #-}
  {-# INLINE getContractInstanceList #-}
  {-# INLINE getSeedState #-}
  {-# INLINE getCurrentEpochBakers #-}
  {-# INLINE getSlotBakersP1 #-}
  {-# INLINE getRewardStatus #-}
  {-# INLINE getOutcomes #-}
  {-# INLINE getTransactionOutcome #-}
  {-# INLINE getTransactionOutcomesHash #-}
  {-# INLINE getSpecialOutcomes #-}
  {-# INLINE getAllIdentityProviders #-}
  {-# INLINE getAllAnonymityRevokers #-}
  {-# INLINE getElectionDifficulty #-}
  {-# INLINE getNextUpdateSequenceNumber #-}
  {-# INLINE getCurrentElectionDifficulty #-}
  {-# INLINE getUpdates #-}
  {-# INLINE getProtocolUpdateStatus #-}
  {-# INLINE getCryptographicParameters #-}
  {-# INLINE getIdentityProvider #-}
  {-# INLINE getAnonymityRevokers #-}
  {-# INLINE getUpdateKeysCollection #-}
  {-# INLINE getEnergyRate #-}

instance (Monad (t m), MonadTrans t, AccountOperations m) => AccountOperations (MGSTrans t m) where
  getAccountCanonicalAddress = lift . getAccountCanonicalAddress
  getAccountAmount = lift. getAccountAmount
  checkAccountIsAllowed acc = lift . checkAccountIsAllowed acc
  getAccountStakedAmount = lift . getAccountStakedAmount
  getAccountLockedAmount = lift . getAccountLockedAmount
  getAccountAvailableAmount = lift . getAccountAvailableAmount
  getAccountNonce = lift . getAccountNonce
  getAccountCredentials = lift . getAccountCredentials
  getAccountVerificationKeys = lift . getAccountVerificationKeys
  getAccountEncryptedAmount = lift . getAccountEncryptedAmount
  getAccountEncryptionKey = lift . getAccountEncryptionKey
  getAccountReleaseSchedule = lift . getAccountReleaseSchedule
  getAccountBaker = lift . getAccountBaker
  getAccountDelegator = lift . getAccountDelegator
  getAccountStake = lift . getAccountStake
  getAccountBakerInfoRef = lift . getAccountBakerInfoRef
  derefBakerInfo = lift . derefBakerInfo
  getAccountHash = lift . getAccountHash
  {-# INLINE getAccountCanonicalAddress #-}
  {-# INLINE getAccountAmount #-}
  {-# INLINE getAccountAvailableAmount #-}
  {-# INLINE checkAccountIsAllowed #-}
  {-# INLINE getAccountCredentials #-}
  {-# INLINE getAccountNonce #-}
  {-# INLINE getAccountVerificationKeys #-}
  {-# INLINE getAccountEncryptedAmount #-}
  {-# INLINE getAccountReleaseSchedule #-}
  {-# INLINE getAccountBaker #-}
  {-# INLINE getAccountStake #-}
  {-# INLINE getAccountBakerInfoRef #-}
  {-# INLINE derefBakerInfo #-}
  {-# INLINE getAccountHash #-}

instance (Monad (t m), MonadTrans t, ContractStateOperations m) => ContractStateOperations (MGSTrans t m) where
  thawContractState = lift . thawContractState
  {-# INLINE thawContractState #-}
  externalContractState = lift . externalContractState
  {-# INLINE externalContractState #-}
  stateSizeV0 = lift . stateSizeV0
  {-# INLINE stateSizeV0 #-}
  getV1StateContext = lift getV1StateContext
  {-# INLINE contractStateToByteString #-}
  contractStateToByteString = lift . contractStateToByteString
  {-# INLINE getV1StateContext #-}

instance (Monad (t m), MonadTrans t, BlockStateOperations m) => BlockStateOperations (MGSTrans t m) where
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetAccountIndex s = lift . bsoGetAccountIndex s
  bsoGetAccountByIndex s = lift . bsoGetAccountByIndex s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoAddressWouldClash s = lift . bsoAddressWouldClash s
  bsoRegIdExists s = lift . bsoRegIdExists s
  bsoCreateAccount s gc accAddr cdv = lift $ bsoCreateAccount s gc accAddr cdv
  bsoPutNewInstance s = lift . bsoPutNewInstance s
  bsoPutNewModule s miface = lift (bsoPutNewModule s miface)
  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoSetAccountCredentialKeys s aa ci pk = lift $ bsoSetAccountCredentialKeys s aa ci pk
  bsoUpdateAccountCredentials s aa remove add thrsh = lift $ bsoUpdateAccountCredentials s aa remove add thrsh
  bsoModifyInstance s caddr amount model newModule = lift $ bsoModifyInstance s caddr amount model newModule
  bsoNotifyEncryptedBalanceChange s = lift . bsoNotifyEncryptedBalanceChange s
  bsoGetSeedState = lift . bsoGetSeedState
  bsoSetSeedState s ss = lift $ bsoSetSeedState s ss
  bsoRotateCurrentEpochBakers = lift . bsoRotateCurrentEpochBakers
  bsoProcessPendingChanges s g = lift $ bsoProcessPendingChanges s g
  bsoTransitionEpochBakers s e = lift $ bsoTransitionEpochBakers s e
  bsoGetActiveBakers = lift . bsoGetActiveBakers
  bsoGetActiveBakersAndDelegators = lift . bsoGetActiveBakersAndDelegators
  bsoGetCurrentEpochBakers = lift . bsoGetCurrentEpochBakers
  bsoGetCurrentEpochFullBakersEx = lift . bsoGetCurrentEpochFullBakersEx
  bsoGetCurrentCapitalDistribution = lift . bsoGetCurrentCapitalDistribution
  bsoAddBaker s addr a = lift $ bsoAddBaker s addr a
  bsoConfigureBaker s aconfig a = lift $ bsoConfigureBaker s aconfig a
  bsoConstrainBakerCommission s acct ranges = lift $ bsoConstrainBakerCommission s acct ranges
  bsoConfigureDelegation s aconfig a = lift $ bsoConfigureDelegation s aconfig a
  bsoUpdateBakerKeys s addr a = lift $ bsoUpdateBakerKeys s addr a
  bsoUpdateBakerStake s addr a = lift $ bsoUpdateBakerStake s addr a
  bsoUpdateBakerRestakeEarnings s addr a = lift $ bsoUpdateBakerRestakeEarnings s addr a
  bsoRemoveBaker s = lift . bsoRemoveBaker s
  bsoRewardAccount s aid amt = lift $ bsoRewardAccount s aid amt
  bsoGetBakerPoolRewardDetails s = lift $ bsoGetBakerPoolRewardDetails s
  bsoRewardFoundationAccount s = lift . bsoRewardFoundationAccount s
  bsoGetFoundationAccount = lift . bsoGetFoundationAccount
  bsoMint s = lift . bsoMint s
  bsoGetIdentityProvider s ipId = lift $ bsoGetIdentityProvider s ipId
  bsoGetAnonymityRevokers s arId = lift $ bsoGetAnonymityRevokers s arId
  bsoGetCryptoParams s = lift $ bsoGetCryptoParams s
  bsoGetPaydayEpoch s = lift $ bsoGetPaydayEpoch s
  bsoGetPaydayMintRate s = lift $ bsoGetPaydayMintRate s
  bsoSetPaydayEpoch s e = lift $ bsoSetPaydayEpoch s e
  bsoSetPaydayMintRate s r = lift $ bsoSetPaydayMintRate s r
  bsoUpdateAccruedTransactionFeesBaker s bid f = lift $ bsoUpdateAccruedTransactionFeesBaker s bid f
  bsoMarkFinalizationAwakeBakers s bids = lift $ bsoMarkFinalizationAwakeBakers s bids
  bsoUpdateAccruedTransactionFeesPassive s f = lift $ bsoUpdateAccruedTransactionFeesPassive s f
  bsoGetAccruedTransactionFeesPassive = lift . bsoGetAccruedTransactionFeesPassive
  bsoUpdateAccruedTransactionFeesFoundationAccount s f = lift $ bsoUpdateAccruedTransactionFeesFoundationAccount s f
  bsoGetAccruedTransactionFeesFoundationAccount = lift . bsoGetAccruedTransactionFeesFoundationAccount
  bsoSetTransactionOutcomes s = lift . bsoSetTransactionOutcomes s
  bsoAddSpecialTransactionOutcome s = lift . bsoAddSpecialTransactionOutcome s
  bsoProcessUpdateQueues s = lift . bsoProcessUpdateQueues s
  bsoProcessReleaseSchedule s = lift . bsoProcessReleaseSchedule s
  bsoGetUpdateKeyCollection = lift . bsoGetUpdateKeyCollection
  bsoGetNextUpdateSequenceNumber s = lift . bsoGetNextUpdateSequenceNumber s
  bsoEnqueueUpdate s tt payload = lift $ bsoEnqueueUpdate s tt payload
  bsoOverwriteElectionDifficulty s = lift . bsoOverwriteElectionDifficulty s
  bsoClearProtocolUpdate = lift . bsoClearProtocolUpdate
  bsoAddReleaseSchedule s l = lift $ bsoAddReleaseSchedule s l
  bsoGetEnergyRate = lift . bsoGetEnergyRate
  bsoGetChainParameters = lift . bsoGetChainParameters
  bsoGetEpochBlocksBaked = lift . bsoGetEpochBlocksBaked
  bsoNotifyBlockBaked s = lift . bsoNotifyBlockBaked s
  bsoClearEpochBlocksBaked = lift . bsoClearEpochBlocksBaked
  bsoSetNextCapitalDistribution s cd = lift $ bsoSetNextCapitalDistribution s cd
  bsoRotateCurrentCapitalDistribution = lift . bsoRotateCurrentCapitalDistribution
  bsoSetNextEpochBakers s = lift . bsoSetNextEpochBakers s
  bsoGetBankStatus = lift . bsoGetBankStatus
  bsoSetRewardAccounts s = lift . bsoSetRewardAccounts s
  {-# INLINE bsoGetModule #-}
  {-# INLINE bsoGetAccount #-}
  {-# INLINE bsoGetAccountIndex #-}
  {-# INLINE bsoGetInstance #-}
  {-# INLINE bsoAddressWouldClash #-}
  {-# INLINE bsoRegIdExists #-}
  {-# INLINE bsoCreateAccount #-}
  {-# INLINE bsoPutNewInstance #-}
  {-# INLINE bsoPutNewModule #-}
  {-# INLINE bsoModifyAccount #-}
  {-# INLINE bsoSetAccountCredentialKeys #-}
  {-# INLINE bsoUpdateAccountCredentials #-}
  {-# INLINE bsoModifyInstance #-}
  {-# INLINE bsoNotifyEncryptedBalanceChange #-}
  {-# INLINE bsoGetSeedState #-}
  {-# INLINE bsoSetSeedState #-}
  {-# INLINE bsoTransitionEpochBakers #-}
  {-# INLINE bsoAddBaker #-}
  {-# INLINE bsoConfigureBaker #-}
  {-# INLINE bsoUpdateBakerKeys #-}
  {-# INLINE bsoUpdateBakerStake #-}
  {-# INLINE bsoUpdateBakerRestakeEarnings #-}
  {-# INLINE bsoRemoveBaker #-}
  {-# INLINE bsoRewardAccount #-}
  {-# INLINE bsoGetFoundationAccount #-}
  {-# INLINE bsoRewardFoundationAccount #-}
  {-# INLINE bsoMint #-}
  {-# INLINE bsoGetIdentityProvider #-}
  {-# INLINE bsoGetAnonymityRevokers #-}
  {-# INLINE bsoGetCryptoParams #-}
  {-# INLINE bsoSetTransactionOutcomes #-}
  {-# INLINE bsoAddSpecialTransactionOutcome #-}
  {-# INLINE bsoProcessUpdateQueues #-}
  {-# INLINE bsoProcessReleaseSchedule #-}
  {-# INLINE bsoGetUpdateKeyCollection #-}
  {-# INLINE bsoGetNextUpdateSequenceNumber #-}
  {-# INLINE bsoEnqueueUpdate #-}
  {-# INLINE bsoOverwriteElectionDifficulty #-}
  {-# INLINE bsoClearProtocolUpdate #-}
  {-# INLINE bsoAddReleaseSchedule #-}
  {-# INLINE bsoGetEnergyRate #-}
  {-# INLINE bsoGetChainParameters #-}
  {-# INLINE bsoGetEpochBlocksBaked #-}
  {-# INLINE bsoNotifyBlockBaked #-}
  {-# INLINE bsoClearEpochBlocksBaked #-}
  {-# INLINE bsoSetNextEpochBakers #-}
  {-# INLINE bsoGetBankStatus #-}
  {-# INLINE bsoSetRewardAccounts #-}
  {-# INLINE bsoGetCurrentEpochBakers #-}

instance (Monad (t m), MonadTrans t, BlockStateStorage m) => BlockStateStorage (MGSTrans t m) where
    thawBlockState = lift . thawBlockState
    freezeBlockState = lift . freezeBlockState
    dropUpdatableBlockState = lift . dropUpdatableBlockState
    purgeBlockState = lift . purgeBlockState
    archiveBlockState = lift . archiveBlockState
    saveBlockState = lift . saveBlockState
    loadBlockState hsh = lift . loadBlockState hsh
    serializeBlockState = lift . serializeBlockState
    blockStateLoadCallback = lift blockStateLoadCallback
    collapseCaches = lift collapseCaches
    {-# INLINE thawBlockState #-}
    {-# INLINE freezeBlockState #-}
    {-# INLINE dropUpdatableBlockState #-}
    {-# INLINE purgeBlockState #-}
    {-# INLINE archiveBlockState #-}
    {-# INLINE saveBlockState #-}
    {-# INLINE loadBlockState #-}
    {-# INLINE serializeBlockState #-}
    {-# INLINE blockStateLoadCallback #-}
    {-# INLINE collapseCaches #-}

deriving via (MGSTrans MaybeT m) instance BlockStateQuery m => BlockStateQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance AccountOperations m => AccountOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance ContractStateOperations m => ContractStateOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance ModuleQuery m => ModuleQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateOperations m => BlockStateOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateStorage m => BlockStateStorage (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance BlockStateQuery m => BlockStateQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance AccountOperations m => AccountOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance ContractStateOperations m => ContractStateOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance ModuleQuery m => ModuleQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateOperations m => BlockStateOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateStorage m => BlockStateStorage (ExceptT e m)

deriving via (MGSTrans (ReaderT r) m) instance AccountOperations m => AccountOperations (ReaderT r m)
