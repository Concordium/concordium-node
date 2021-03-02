{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
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
import Data.Functor
import qualified Data.Vector as Vec
import Data.Serialize(Serialize)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Foldable (foldl')
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import System.IO (Handle)

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Updates
import qualified Concordium.Wasm as Wasm
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Account

import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Updates as Basic
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Types
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import Concordium.Types.SeedState
import Concordium.Types.Transactions hiding (BareBlockItem(..))

import qualified Concordium.ID.Types as ID
import Concordium.ID.Parameters(GlobalContext)
import Concordium.ID.Types (AccountCredential, CredentialValidTo, AccountKeys)
import Concordium.Crypto.EncryptedTransfers

-- |The hashes of the block state components, which are combined
-- to produce a 'StateHash'.
data BlockStateHashInputs = BlockStateHashInputs {
    bshBirkParameters :: H.Hash,
    bshCryptographicParameters :: H.Hash,
    bshIdentityProviders :: H.Hash,
    bshAnonymityRevokers :: H.Hash,
    bshModules :: H.Hash,
    bshBankStatus :: H.Hash,
    bshAccounts :: H.Hash,
    bshInstances :: H.Hash,
    bshUpdates :: H.Hash,
    bshEpochBlocks :: EpochBlocksHash
} deriving (Show)

-- |Construct a 'StateHash' from the component hashes.
makeBlockStateHash :: BlockStateHashInputs -> StateHash
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
      (ebHash bshEpochBlocks))

class (BlockStateTypes m, Monad m) => AccountOperations m where

  -- | Get the address of the account
  getAccountAddress :: Account m -> m AccountAddress

  -- | Get the current public account balance
  getAccountAmount :: Account m -> m Amount

  -- | Get the current public account available balance.
  -- This accounts for lock-up and staked amounts.
  -- @available = total - max locked staked@
  getAccountAvailableAmount :: Account m -> m Amount
  getAccountAvailableAmount acc = do
    total <- getAccountAmount acc
    lockedUp <- _totalLockedUpBalance <$> getAccountReleaseSchedule acc
    staked <- getAccountBaker acc <&> \case
      Nothing -> 0
      Just bkr -> _stakedAmount bkr
    return $ total - max lockedUp staked

  -- |Get the next available nonce for this account
  getAccountNonce :: Account m -> m Nonce

  -- |Get the list of credentials deployed on the account, ordered from most
  -- recently deployed.  The list should be non-empty.
  -- TODO: Use 'Data.List.Nonempty'
  getAccountCredentials :: Account m -> m [AccountCredential]

  -- |Get the last expiry time of a credential on the account.
  getAccountMaxCredentialValidTo :: Account m -> m CredentialValidTo

  -- |Get the key used to verify transaction signatures, it records the signature scheme used as well
  getAccountVerificationKeys :: Account m -> m ID.AccountKeys

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
  getAccountBaker :: Account m -> m (Maybe AccountBaker)

-- |The block query methods can query block state. They are needed by
-- consensus itself to compute stake, get a list of and information about
-- bakers, finalization committee, etc.
class AccountOperations m => BlockStateQuery m where
    -- |Get the module source from the module table as deployed to the chain.
    getModule :: BlockState m -> ModuleRef -> m (Maybe Wasm.WasmModule)
    -- |Get the account state from the account table of the state instance.
    getAccount :: BlockState m -> AccountAddress -> m (Maybe (Account m))
    -- |Get the contract state from the contract table of the state instance.
    getContractInstance :: BlockState m -> ContractAddress -> m (Maybe Instance)

    -- |Get the list of addresses of modules existing in the given block state.
    getModuleList :: BlockState m -> m [ModuleRef]
    -- |Get the list of account addresses existing in the given block state.
    getAccountList :: BlockState m -> m [AccountAddress]
    -- |Get the list of contract instances existing in the given block state.
    getContractInstanceList :: BlockState m -> m [Instance]

    -- |Get the seed state, from which the leadership election nonce
    -- is derived.
    getSeedState :: BlockState m -> m SeedState

    -- |Get the bakers for the epoch in which the block was baked.
    getCurrentEpochBakers :: BlockState m -> m FullBakers

    -- |Get the bakers for a particular (future) slot.
    getSlotBakers :: BlockState m -> Slot -> m FullBakers

    -- |Get the account of a baker. This may return an account even
    -- if the account is not (currently) a baker, since a 'BakerId'
    -- uniquely determines an account over time.
    getBakerAccount :: BlockState m -> BakerId -> m (Maybe (Account m))

    -- |Get reward summary for this block.
    getRewardStatus :: BlockState m -> m BankStatus

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

    getAllIdentityProviders :: BlockState m -> m [IpInfo]

    getAllAnonymityRevokers :: BlockState m -> m [ArInfo]

    -- |Get the value of the election difficulty parameter for a future timestamp.
    -- This function applies queued election difficultly updates as appropriate.
    getElectionDifficulty :: BlockState m -> Timestamp -> m ElectionDifficulty
    -- |Get the next sequence number for a particular update type.
    getNextUpdateSequenceNumber :: BlockState m -> UpdateType -> m UpdateSequenceNumber
    -- |Get the value of the election difficulty that was used to bake this block.
    getCurrentElectionDifficulty :: BlockState m -> m ElectionDifficulty
    -- |Get the current chain parameters and pending updates.
    getUpdates :: BlockState m -> m Basic.Updates
    -- |Get the protocol update status. If a protocol update has taken effect,
    -- returns @Left protocolUpdate@. Otherwise, returns @Right pendingProtocolUpdates@.
    -- The @pendingProtocolUpdates@ is a (possibly-empty) list of timestamps and protocol
    -- updates that have not yet taken effect.
    getProtocolUpdateStatus :: BlockState m -> m (Either ProtocolUpdate [(TransactionTime, ProtocolUpdate)])

    -- |Get the current cryptographic parameters of the chain.
    getCryptographicParameters :: BlockState m -> m CryptographicParameters

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

-- |Block state update operations parametrized by a monad. The operations which
-- mutate the state all also return an 'UpdatableBlockState' handle. This is to
-- support different implementations, from pure ones to stateful ones.
class (BlockStateQuery m) => BlockStateOperations m where
  -- |Get the module from the module table of the state instance.
  bsoGetModule :: UpdatableBlockState m -> ModuleRef -> m (Maybe Wasm.ModuleInterface)
  -- |Get an account by its address.
  bsoGetAccount :: UpdatableBlockState m -> AccountAddress -> m (Maybe (Account m))
  -- |Get the index of an account.
  bsoGetAccountIndex :: UpdatableBlockState m -> AccountAddress -> m (Maybe AccountIndex)
  -- |Get the contract state from the contract table of the state instance.
  bsoGetInstance :: UpdatableBlockState m -> ContractAddress -> m (Maybe Instance)

  -- |Check whether an the given credential registration ID exists.
  -- Return @True@ iff so.
  bsoRegIdExists :: UpdatableBlockState m -> ID.CredentialRegistrationID -> m Bool

  -- |Create and add an empty account with the given public key, address and credential.
  -- If an account with the given address already exists, @Nothing@ is returned.
  -- Otherwise, the new account is returned, and the credential is added to the known credentials.
  --
  -- It is not checked if the account's credential is a duplicate.
  bsoCreateAccount :: UpdatableBlockState m -> GlobalContext -> AccountKeys -> AccountAddress -> AccountCredential -> m (Maybe (Account m), UpdatableBlockState m)

  -- |Add a new smart contract instance to the state.
  bsoPutNewInstance :: UpdatableBlockState m -> (ContractAddress -> Instance) -> m (ContractAddress, UpdatableBlockState m)
  -- |Add the module to the global state. If a module with the given address
  -- already exists return @False@.
  bsoPutNewModule :: UpdatableBlockState m -> (Wasm.ModuleInterface, Wasm.WasmModule) -> m (Bool, UpdatableBlockState m)

  -- |Modify an existing account with given data (which includes the address of the account).
  -- This method is only called when an account exists and can thus assume this.
  -- NB: In case we are adding a credential to an account this method __must__ also
  -- update the global set of known credentials.
  --
  -- It is the responsibility of the caller to ensure that the change does not lead to a
  -- negative account balance or a situation where the staked or locked balance
  -- exceeds the total balance on the account.
  bsoModifyAccount :: UpdatableBlockState m -> AccountUpdate -> m (UpdatableBlockState m)
  -- |Replace the instance with given data. The rest of the instance data (instance parameters) stays the same.
  -- This method is only called when it is known the instance exists, and can thus assume it.
  bsoModifyInstance :: UpdatableBlockState m
                    -> ContractAddress
                    -> AmountDelta
                    -> Wasm.ContractState
                    -> m (UpdatableBlockState m)

  -- FIXME: remove
  -- |Notify the block state that the given amount was spent on execution.
  --bsoNotifyExecutionCost :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Notify that some amount was transferred from/to encrypted balance of some account.
  bsoNotifyEncryptedBalanceChange :: UpdatableBlockState m -> AmountDelta -> m (UpdatableBlockState m)


  -- FIXME: remove
  -- |Notify the block state that the given identity issuer's credential was
  -- used by a sender of the transaction.
  --bsoNotifyIdentityIssuerCredential :: UpdatableBlockState m -> ID.IdentityProviderIdentity -> m (UpdatableBlockState m)

  -- FIXME: remove
  -- |Get the execution reward for the current block.
  -- bsoGetExecutionCost :: UpdatableBlockState m -> m Amount

  -- |Get the seed state associated with the block state.
  bsoGetSeedState :: UpdatableBlockState m -> m SeedState

  -- |Set the seed state associated with the block state.
  --
  -- Note: on no account should the epoch length be changed using this
  -- function (or otherwise).  The epoch length is assumed to be constant,
  -- so that epochs can always be calculated by dividing slot number by the
  -- epoch length.  Any change would throw off this calculation.
  bsoSetSeedState :: UpdatableBlockState m -> SeedState -> m (UpdatableBlockState m)

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
    :: UpdatableBlockState m
    -> Epoch
    -- ^The new epoch
    -> m (UpdatableBlockState m)

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
  bsoAddBaker :: UpdatableBlockState m -> AccountAddress -> BakerAdd -> m (BakerAddResult, UpdatableBlockState m)

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
  bsoUpdateBakerKeys :: UpdatableBlockState m -> AccountAddress -> BakerKeyUpdate -> m (BakerKeyUpdateResult, UpdatableBlockState m)

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
  bsoUpdateBakerStake :: UpdatableBlockState m -> AccountAddress -> Amount -> m (BakerStakeUpdateResult, UpdatableBlockState m)

  -- |Update whether a baker's earnings are automatically restaked.
  --
  -- The following results are possible:
  --
  -- * @BREUUpdated id@: the flag was updated.
  --
  -- * @BREUInvalidBaker@: the account does not exists, or is not currently a baker.
  bsoUpdateBakerRestakeEarnings :: UpdatableBlockState m -> AccountAddress -> Bool -> m (BakerRestakeEarningsUpdateResult, UpdatableBlockState m)

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
  bsoRemoveBaker :: UpdatableBlockState m -> AccountAddress -> m (BakerRemoveResult, UpdatableBlockState m)

  -- |Add an amount to a baker's account as a reward. The baker's stake is increased
  -- correspondingly if the baker is set to restake rewards.
  -- If the baker id refers to an account, the reward is paid to the account, and the
  -- address of the account is returned.  If the id does not refer to an account
  -- then no change is made and @Nothing@ is returned.
  --
  -- TODO: Change the interface to support new tokenomics.
  bsoRewardBaker :: UpdatableBlockState m -> BakerId -> Amount -> m (Maybe AccountAddress, UpdatableBlockState m)

  -- |Add an amount to the foundation account.
  bsoRewardFoundationAccount :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Get the foundation account.
  bsoGetFoundationAccount :: UpdatableBlockState m -> m (Account m)

  -- FIXME: Remove
  -- |Set the amount of minted GTU per slot.
  -- bsoSetInflation :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Mint currency and distribute it to the BakerRewardAccount,
  -- FinalizationRewardAccount and foundation account.
  -- This increases the total GTU in circulation.
  bsoMint :: UpdatableBlockState m -> MintAmounts -> m (UpdatableBlockState m)

  -- FIXME: Remove
  -- |Subtract the amount from the central bank. Return the new amount. The
  -- precondition of this method is that the amount on the account is
  -- sufficient.
  -- bsoDecrementCentralBankGTU :: UpdatableBlockState m -> Amount -> m (Amount, UpdatableBlockState m)

  -- |Get the identity provider data for the given identity provider, or Nothing if
  -- the identity provider with given ID does not exist.
  bsoGetIdentityProvider :: UpdatableBlockState m -> ID.IdentityProviderIdentity -> m (Maybe IpInfo)

  -- |Get the anonymity revokers with given ids. Returns 'Nothing' if any of the
  -- anonymity revokers are not found.
  bsoGetAnonymityRevokers :: UpdatableBlockState m -> [ID.ArIdentity] -> m (Maybe [ArInfo])

  -- |Get the current cryptographic parameters. The idea is that these will be
  -- periodically updated and so they must be part of the block state.
  bsoGetCryptoParams :: UpdatableBlockState m -> m CryptographicParameters

  -- |Set the list of transaction outcomes for the block.
  bsoSetTransactionOutcomes :: UpdatableBlockState m -> [TransactionSummary] -> m (UpdatableBlockState m)

  -- |Add a special transaction outcome.
  bsoAddSpecialTransactionOutcome :: UpdatableBlockState m -> SpecialTransactionOutcome -> m (UpdatableBlockState m)

  -- |Process queued updates.
  bsoProcessUpdateQueues :: UpdatableBlockState m -> Timestamp -> m (Map.Map TransactionTime UpdateValue, UpdatableBlockState m)

  -- |Unlock the amounts up to the given timestamp
  bsoProcessReleaseSchedule :: UpdatableBlockState m -> Timestamp -> m (UpdatableBlockState m)

  -- |Get the current 'Authorizations' for validating updates.
  bsoGetCurrentAuthorizations :: UpdatableBlockState m -> m Authorizations

  -- |Get the next 'UpdateSequenceNumber' for a given update type.
  bsoGetNextUpdateSequenceNumber :: UpdatableBlockState m -> UpdateType -> m UpdateSequenceNumber

  -- |Enqueue an update to take effect at the specified time.
  bsoEnqueueUpdate :: UpdatableBlockState m -> TransactionTime -> UpdateValue -> m (UpdatableBlockState m)

  -- |Add the given accounts and timestamps to the per-block account release schedule.
  -- PRECONDITION: The given timestamp must be the first timestamp for a release for the given account.
  bsoAddReleaseSchedule :: UpdatableBlockState m -> [(AccountAddress, Timestamp)] -> m (UpdatableBlockState m)

  -- |Get the current energy rate.
  bsoGetEnergyRate :: UpdatableBlockState m -> m EnergyRate

  -- |Get the current chain parameters.
  bsoGetChainParameters :: UpdatableBlockState m -> m ChainParameters

  -- |Get the number of blocks baked in this epoch, both in total and
  -- per baker.
  bsoGetEpochBlocksBaked :: UpdatableBlockState m -> m (Word64, [(BakerId, Word64)])

  -- |Record that the given baker has baked a block in the current epoch.
  bsoNotifyBlockBaked :: UpdatableBlockState m -> BakerId -> m (UpdatableBlockState m)

  -- |Clear the tracking of baked blocks in the current epoch.
  -- Should be called whenever a new epoch is entered.
  bsoClearEpochBlocksBaked :: UpdatableBlockState m -> m (UpdatableBlockState m)

  -- |Get the current status of the various accounts.
  bsoGetBankStatus :: UpdatableBlockState m -> m BankStatus

  -- |Set the status of the special reward accounts.
  bsoSetRewardAccounts :: UpdatableBlockState m -> RewardAccounts -> m (UpdatableBlockState m)

-- | Block state storage operations
class (BlockStateOperations m, Serialize (BlockStateRef m)) => BlockStateStorage m where
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

    -- |Ensure that the given block state is full loaded into memory
    -- (where applicable).
    cacheBlockState :: BlockState m -> m (BlockState m)

    -- |Serialize the block state to a byte string.
    -- This serialization does not include transaction outcomes.
    serializeBlockState :: BlockState m -> m LBS.ByteString

    -- |Serialize the block state to a file handle.
    -- This serialization does not include transaction outcomes.
    writeBlockState :: Handle -> BlockState m -> m ()


instance (Monad (t m), MonadTrans t, BlockStateQuery m) => BlockStateQuery (MGSTrans t m) where
  getModule s = lift . getModule s
  getAccount s = lift . getAccount s
  getBakerAccount s = lift . getBakerAccount s
  getContractInstance s = lift . getContractInstance s
  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList
  getSeedState = lift . getSeedState
  getCurrentEpochBakers = lift . getCurrentEpochBakers
  getSlotBakers s = lift . getSlotBakers s
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
  getProtocolUpdateStatus = lift . getProtocolUpdateStatus
  getCryptographicParameters = lift . getCryptographicParameters
  {-# INLINE getModule #-}
  {-# INLINE getAccount #-}
  {-# INLINE getBakerAccount #-}
  {-# INLINE getContractInstance #-}
  {-# INLINE getModuleList #-}
  {-# INLINE getAccountList #-}
  {-# INLINE getContractInstanceList #-}
  {-# INLINE getSeedState #-}
  {-# INLINE getCurrentEpochBakers #-}
  {-# INLINE getSlotBakers #-}
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

instance (Monad (t m), MonadTrans t, AccountOperations m) => AccountOperations (MGSTrans t m) where
  getAccountAddress = lift . getAccountAddress
  getAccountAmount = lift. getAccountAmount
  getAccountAvailableAmount = lift . getAccountAvailableAmount
  getAccountNonce = lift . getAccountNonce
  getAccountCredentials = lift . getAccountCredentials
  getAccountMaxCredentialValidTo = lift . getAccountMaxCredentialValidTo
  getAccountVerificationKeys = lift . getAccountVerificationKeys
  getAccountEncryptedAmount = lift . getAccountEncryptedAmount
  getAccountEncryptionKey = lift . getAccountEncryptionKey
  getAccountReleaseSchedule = lift . getAccountReleaseSchedule
  getAccountBaker = lift . getAccountBaker
  {-# INLINE getAccountAddress #-}
  {-# INLINE getAccountAmount #-}
  {-# INLINE getAccountAvailableAmount #-}
  {-# INLINE getAccountCredentials #-}
  {-# INLINE getAccountMaxCredentialValidTo #-}
  {-# INLINE getAccountNonce #-}
  {-# INLINE getAccountVerificationKeys #-}
  {-# INLINE getAccountEncryptedAmount #-}
  {-# INLINE getAccountReleaseSchedule #-}
  {-# INLINE getAccountBaker #-}

instance (Monad (t m), MonadTrans t, BlockStateOperations m) => BlockStateOperations (MGSTrans t m) where
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetAccountIndex s = lift . bsoGetAccountIndex s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s
  bsoCreateAccount s gc accKeys accAddr cdv = lift $ bsoCreateAccount s gc accKeys accAddr cdv
  bsoPutNewInstance s = lift . bsoPutNewInstance s
  bsoPutNewModule s miface = lift (bsoPutNewModule s miface)
  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoModifyInstance s caddr amount model = lift $ bsoModifyInstance s caddr amount model
  bsoNotifyEncryptedBalanceChange s = lift . bsoNotifyEncryptedBalanceChange s
  bsoGetSeedState = lift . bsoGetSeedState
  bsoSetSeedState s ss = lift $ bsoSetSeedState s ss
  bsoTransitionEpochBakers s e = lift $ bsoTransitionEpochBakers s e
  bsoAddBaker s addr a = lift $ bsoAddBaker s addr a
  bsoUpdateBakerKeys s addr a = lift $ bsoUpdateBakerKeys s addr a
  bsoUpdateBakerStake s addr a = lift $ bsoUpdateBakerStake s addr a
  bsoUpdateBakerRestakeEarnings s addr a = lift $ bsoUpdateBakerRestakeEarnings s addr a
  bsoRemoveBaker s = lift . bsoRemoveBaker s
  bsoRewardBaker s bid amt = lift $ bsoRewardBaker s bid amt
  bsoRewardFoundationAccount s = lift . bsoRewardFoundationAccount s
  bsoGetFoundationAccount = lift . bsoGetFoundationAccount
  bsoMint s = lift . bsoMint s
  bsoGetIdentityProvider s ipId = lift $ bsoGetIdentityProvider s ipId
  bsoGetAnonymityRevokers s arId = lift $ bsoGetAnonymityRevokers s arId
  bsoGetCryptoParams s = lift $ bsoGetCryptoParams s
  bsoSetTransactionOutcomes s = lift . bsoSetTransactionOutcomes s
  bsoAddSpecialTransactionOutcome s = lift . bsoAddSpecialTransactionOutcome s
  bsoProcessUpdateQueues s = lift . bsoProcessUpdateQueues s
  bsoProcessReleaseSchedule s = lift . bsoProcessReleaseSchedule s
  bsoGetCurrentAuthorizations = lift . bsoGetCurrentAuthorizations
  bsoGetNextUpdateSequenceNumber s = lift . bsoGetNextUpdateSequenceNumber s
  bsoEnqueueUpdate s tt payload = lift $ bsoEnqueueUpdate s tt payload
  bsoAddReleaseSchedule s l = lift $ bsoAddReleaseSchedule s l
  bsoGetEnergyRate = lift . bsoGetEnergyRate
  bsoGetChainParameters = lift . bsoGetChainParameters
  bsoGetEpochBlocksBaked = lift . bsoGetEpochBlocksBaked
  bsoNotifyBlockBaked s = lift . bsoNotifyBlockBaked s
  bsoClearEpochBlocksBaked = lift . bsoClearEpochBlocksBaked
  bsoGetBankStatus = lift . bsoGetBankStatus
  bsoSetRewardAccounts s = lift . bsoSetRewardAccounts s
  {-# INLINE bsoGetModule #-}
  {-# INLINE bsoGetAccount #-}
  {-# INLINE bsoGetAccountIndex #-}
  {-# INLINE bsoGetInstance #-}
  {-# INLINE bsoRegIdExists #-}
  {-# INLINE bsoCreateAccount #-}
  {-# INLINE bsoPutNewInstance #-}
  {-# INLINE bsoPutNewModule #-}
  {-# INLINE bsoModifyAccount #-}
  {-# INLINE bsoModifyInstance #-}
  {-# INLINE bsoNotifyEncryptedBalanceChange #-}
  {-# INLINE bsoGetSeedState #-}
  {-# INLINE bsoSetSeedState #-}
  {-# INLINE bsoTransitionEpochBakers #-}
  {-# INLINE bsoAddBaker #-}
  {-# INLINE bsoUpdateBakerKeys #-}
  {-# INLINE bsoUpdateBakerStake #-}
  {-# INLINE bsoUpdateBakerRestakeEarnings #-}
  {-# INLINE bsoRemoveBaker #-}
  {-# INLINE bsoRewardBaker #-}
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
  {-# INLINE bsoGetCurrentAuthorizations #-}
  {-# INLINE bsoGetNextUpdateSequenceNumber #-}
  {-# INLINE bsoEnqueueUpdate #-}
  {-# INLINE bsoAddReleaseSchedule #-}
  {-# INLINE bsoGetEnergyRate #-}
  {-# INLINE bsoGetChainParameters #-}
  {-# INLINE bsoGetEpochBlocksBaked #-}
  {-# INLINE bsoNotifyBlockBaked #-}
  {-# INLINE bsoClearEpochBlocksBaked #-}
  {-# INLINE bsoGetBankStatus #-}
  {-# INLINE bsoSetRewardAccounts #-}
instance (Monad (t m), MonadTrans t, BlockStateStorage m) => BlockStateStorage (MGSTrans t m) where
    thawBlockState = lift . thawBlockState
    freezeBlockState = lift . freezeBlockState
    dropUpdatableBlockState = lift . dropUpdatableBlockState
    purgeBlockState = lift . purgeBlockState
    archiveBlockState = lift . archiveBlockState
    saveBlockState = lift . saveBlockState
    loadBlockState hsh = lift . loadBlockState hsh
    cacheBlockState = lift . cacheBlockState
    serializeBlockState = lift . serializeBlockState
    writeBlockState fh bs = lift $ writeBlockState fh bs
    {-# INLINE thawBlockState #-}
    {-# INLINE freezeBlockState #-}
    {-# INLINE dropUpdatableBlockState #-}
    {-# INLINE purgeBlockState #-}
    {-# INLINE archiveBlockState #-}
    {-# INLINE saveBlockState #-}
    {-# INLINE loadBlockState #-}
    {-# INLINE cacheBlockState #-}
    {-# INLINE serializeBlockState #-}
    {-# INLINE writeBlockState #-}

deriving via (MGSTrans MaybeT m) instance BlockStateQuery m => BlockStateQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance AccountOperations m => AccountOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateOperations m => BlockStateOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateStorage m => BlockStateStorage (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance BlockStateQuery m => BlockStateQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance AccountOperations m => AccountOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateOperations m => BlockStateOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateStorage m => BlockStateStorage (ExceptT e m)
