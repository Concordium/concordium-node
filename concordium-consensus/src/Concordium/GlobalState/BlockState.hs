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
B4. Two bakers cannot share the same signature verify key.
B5. The amount delegated to any given baker must be the sum of the amounts o all the accounts that delegate to that baker. The total delegated amount must always equal the sum of the amounts delegated to all bakers.

These invariants are actually inviolable considering the structure of the API.
-}
module Concordium.GlobalState.BlockState where

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data.Ratio
import Data.Word
import qualified Data.Vector as Vec
import Data.Serialize(Serialize)
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Foldable (foldl')

import Concordium.Types
import Concordium.Types.Execution
import qualified Concordium.Wasm as Wasm
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Account

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Types
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.SeedState
import Concordium.Types.Transactions hiding (BareBlockItem(..))

import qualified Concordium.ID.Types as ID
import Concordium.ID.Parameters(GlobalContext)
import Concordium.ID.Types (CredentialDeploymentValues, CredentialValidTo, AccountKeys)
import Concordium.Crypto.EncryptedTransfers

-- |Index of the module in the module table. Reflects when the module was added
-- to the table.
type ModuleIndex = Word64

-- |Module stored in block state.
data Module = Module {
    moduleInterface :: !Wasm.ModuleInterface,
    moduleIndex :: !ModuleIndex
}

class (BlockStateTypes m,  Monad m) => BakerQuery m where

  -- |If baker with given ID exists, get the stake delegated to that baker
  getBakerStake :: Bakers m -> BakerId -> m (Maybe Amount)

  -- |If baker with given signature verification key exists, get the baker's baker ID
  getBakerFromKey :: Bakers m -> BakerSignVerifyKey -> m (Maybe BakerId)

  -- |Get the sum total stake of all bakers
  getTotalBakerStake :: Bakers m -> m Amount

  -- |If baker with given ID exists, get the baker's account address and verification keys
  getBakerInfo :: Bakers m -> BakerId -> m (Maybe BakerInfo)

  -- |Get baker IDs and full baker information (verification keys, account addresses, and stake) for all bakers
  getFullBakerInfos :: Bakers m -> m (Map.Map BakerId FullBakerInfo)

bakerData :: BakerQuery m => BakerId -> Bakers m -> m (Maybe (BakerInfo, LotteryPower))
bakerData bid bkrs = do
  totalStake <- getTotalBakerStake bkrs
  runMaybeT $ do
    bInfo <- MaybeT (getBakerInfo bkrs bid)
    stake <- MaybeT (getBakerStake bkrs bid)
    return (bInfo, stake % totalStake)


class (BlockStateTypes m,  Monad m) => AccountOperations m where

  -- | Get the address of the account
  getAccountAddress :: Account m -> m AccountAddress

  -- | Get the current public account balance
  getAccountAmount :: Account m -> m Amount

  -- |Get the next available nonce for this account
  getAccountNonce :: Account m -> m Nonce

  -- |Get the list of credentials deployed on the account, ordered from most
  -- recently deployed.  The list should be non-empty.
  getAccountCredentials :: Account m -> m [CredentialDeploymentValues]

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
    return $! addToAggIndex _startIndex (fromIntegral (Seq.length _incomingEncryptedAmounts))

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
    if index >= _startIndex && fromIntegral (Seq.length _incomingEncryptedAmounts) >= index - _startIndex then
      let toTake = Seq.take (fromIntegral (index - _startIndex)) _incomingEncryptedAmounts
      in return $ Just $! foldl' aggregateAmounts _selfAmount toTake
    else return Nothing

  -- |Get the baker to which this account's stake is delegated (if any)
  getAccountStakeDelegate :: Account m -> m (Maybe BakerId)

  -- |The set of instances belonging to this account
  -- TODO: Revisit choice of datastructure. Additions and removals
  -- are expected to be rare. The set is traversed when stake delegation
  -- changes.
  getAccountInstances :: Account m -> m (Set ContractAddress)

  -- |Create an empty account with the given public key, address and credential.
  createNewAccount :: GlobalContext -> AccountKeys -> AccountAddress -> CredentialDeploymentValues -> m (Account m)

  -- |Update the public account balance
  updateAccountAmount :: Account m -> Amount -> m (Account m)

class (BlockStateTypes m, BakerQuery m) => BirkParametersOperations m where

    getSeedState :: BirkParameters m -> m SeedState

    updateBirkParametersForNewEpoch :: SeedState -> BirkParameters m -> m (BirkParameters m)

    getElectionDifficulty :: BirkParameters m -> m ElectionDifficulty

    getCurrentBakers :: BirkParameters m -> m (Bakers m)

    getLotteryBakers :: BirkParameters m -> m (Bakers m)

    updateSeedState :: (SeedState -> SeedState) -> BirkParameters m -> m (BirkParameters m)

birkBaker :: (BakerQuery m, BirkParametersOperations m) => BakerId -> BirkParameters m -> m (Maybe (BakerInfo, LotteryPower))
birkBaker bid bps = bakerData bid =<< getCurrentBakers bps

birkEpochBaker :: (BakerQuery m, BirkParametersOperations m) => BakerId -> BirkParameters m -> m (Maybe (BakerInfo, LotteryPower))
birkEpochBaker bid bps = bakerData bid =<< getLotteryBakers bps

birkLeadershipElectionNonce :: BirkParametersOperations m => BirkParameters m -> m LeadershipElectionNonce
birkLeadershipElectionNonce bps = currentSeed <$> getSeedState bps

birkEpochBakerByKeys :: (BakerQuery m, BirkParametersOperations m) => BakerSignVerifyKey -> BirkParameters m -> m (Maybe (BakerId, BakerInfo, LotteryPower))
birkEpochBakerByKeys sigKey bps = do
    lotteryBakers <- getLotteryBakers bps
    mbid <- getBakerFromKey lotteryBakers sigKey
    case mbid of
        Just bid -> do
            baker <- birkEpochBaker bid bps
            return $ baker <&> \(binfo, lotPow) -> (bid, binfo, lotPow)
        _ -> return Nothing

-- |The block query methods can query block state. They are needed by
-- consensus itself to compute stake, get a list of and information about
-- bakers, finalization committee, etc.
class (BirkParametersOperations m, AccountOperations m) => BlockStateQuery m where
    -- |Get the module from the module table of the state instance.
    getModule :: BlockState m -> ModuleRef -> m (Maybe Module)
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

    -- |Get Birk parameters from the point of view of this block state. Although
    -- these will not change as often as the rest of the block state, they are
    -- still block dependent.
    getBlockBirkParameters :: BlockState m -> m (BirkParameters m)

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
    getSpecialOutcomes :: BlockState m -> m [SpecialTransactionOutcome]

    getAllIdentityProviders :: BlockState m -> m [IpInfo]

    getAllAnonymityRevokers :: BlockState m -> m [ArInfo]

-- |Block state update operations parametrized by a monad. The operations which
-- mutate the state all also return an 'UpdatableBlockState' handle. This is to
-- support different implementations, from pure ones to stateful ones.
class (BlockStateQuery m) => BlockStateOperations m where
  -- |Get the module from the module table of the state instance.
  bsoGetModule :: UpdatableBlockState m -> ModuleRef -> m (Maybe Wasm.ModuleInterface)
  -- |Get an account by its address.
  bsoGetAccount :: UpdatableBlockState m -> AccountAddress -> m (Maybe (Account m))
  -- |Get the contract state from the contract table of the state instance.
  bsoGetInstance :: UpdatableBlockState m -> ContractAddress -> m (Maybe Instance)

  -- |Check whether an the given credential registration ID exists.
  -- Return @True@ iff so.
  bsoRegIdExists :: UpdatableBlockState m -> ID.CredentialRegistrationID -> m Bool

  -- |Try to add a new account to the state. If an account with the address already exists
  -- return @False@, and if the account was successfully added return @True@.
  -- Any credentials on the account are added to the known credentials. (It is not checked
  -- if the credentials are duplicates.)  If the account delegates, the bakers are updated.
  -- (It is not checked that the delegation is valid.)
  -- A new account must not have any associated instances.
  bsoPutNewAccount :: UpdatableBlockState m -> Account m -> m (Bool, UpdatableBlockState m)
  -- |Add a new smart contract instance to the state.
  bsoPutNewInstance :: UpdatableBlockState m -> (ContractAddress -> Instance) -> m (ContractAddress, UpdatableBlockState m)
  -- |Add the module to the global state. If a module with the given address
  -- already exists return @False@.
  bsoPutNewModule :: UpdatableBlockState m -> Wasm.ModuleInterface -> m (Bool, UpdatableBlockState m)

  -- |Modify an existing account with given data (which includes the address of the account).
  -- This method is only called when an account exists and can thus assume this.
  -- NB: In case we are adding a credential to an account this method __must__ also
  -- update the global set of known credentials.
  bsoModifyAccount :: UpdatableBlockState m -> AccountUpdate -> m (UpdatableBlockState m)
  -- |Replace the instance with given data. The rest of the instance data (instance parameters) stays the same.
  -- This method is only called when it is known the instance exists, and can thus assume it.
  bsoModifyInstance :: UpdatableBlockState m
                    -> ContractAddress
                    -> AmountDelta
                    -> Wasm.ContractState
                    -> m (UpdatableBlockState m)

  -- |Notify the block state that the given amount was spent on execution.
  bsoNotifyExecutionCost :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Notify that some amount was transferred from/to encrypted balance of some account.
  bsoNotifyEncryptedBalanceChange :: UpdatableBlockState m -> AmountDelta -> m (UpdatableBlockState m)


  -- |Notify the block state that the given identity issuer's credential was
  -- used by a sender of the transaction.
  bsoNotifyIdentityIssuerCredential :: UpdatableBlockState m -> ID.IdentityProviderIdentity -> m (UpdatableBlockState m)

  -- |Get the execution reward for the current block.
  bsoGetExecutionCost :: UpdatableBlockState m -> m Amount

  -- |Get Birk parameters from the point of view of this block state. Although
  -- these will not change as often as the rest of the block state, they are
  -- still block dependent. They are needed in 'UpdatableBlockState' because in
  -- particular the reward accounts for the bakers and others will need to be
  -- determined at the end of the block, and they might have changed as a result
  -- of block execution.
  bsoGetBlockBirkParameters :: UpdatableBlockState m -> m (BirkParameters m)

  -- |Get the account address for a given baker.
  bsoGetBakerAccountAddress :: UpdatableBlockState m -> BakerId -> m (Maybe AccountAddress)
  bsoGetBakerAccountAddress s bid = do
    bps <- bsoGetBlockBirkParameters s
    fmap (_bakerAccount . fst) <$> birkBaker bid bps

  -- |Get the reward account of the given baker.
  bsoGetEpochBakerAccount :: UpdatableBlockState m -> BakerId -> m (Maybe (Account m))
  bsoGetEpochBakerAccount s bid = do
    bps <- bsoGetBlockBirkParameters s
    account <- fmap (_bakerAccount . fst) <$> birkEpochBaker bid bps
    join <$> mapM (bsoGetAccount s) account


  -- |Add a new baker to the baker pool. Assign a fresh baker identity to the
  -- new baker and return the assigned identity.
  -- This method should also update the next available baker id in the system.
  -- If a baker with the given signing key already exists do nothing and
  -- return 'Nothing'
  bsoAddBaker :: UpdatableBlockState m -> BakerInfo -> m (Either BakerError BakerId, UpdatableBlockState m)

  -- |Update an existing baker's information. The method may assume that the baker with
  -- the given Id exists.
  -- If a baker with a given signing key already exists return 'False', and if the baker
  -- was successfully updated return 'True'.
  -- If updating the account the precondition of this method is that the reward account exists.
  bsoUpdateBaker :: UpdatableBlockState m -> Basic.BakerUpdate -> m (Bool, UpdatableBlockState m)

  -- |Remove a baker from the list of allowed bakers. Return 'True' if a baker
  -- with given 'BakerId' existed, and 'False' otherwise.
  bsoRemoveBaker :: UpdatableBlockState m -> BakerId -> m (Bool, UpdatableBlockState m)

  -- |Set the amount of minted GTU per slot.
  bsoSetInflation :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

  -- |Mint currency in the central bank. Return the new amount
  bsoMint :: UpdatableBlockState m -> Amount -> m (Amount, UpdatableBlockState m)

  -- |Subtract the amount from the central bank. Return the new amount. The
  -- precondition of this method is that the amount on the account is
  -- sufficient.
  bsoDecrementCentralBankGTU :: UpdatableBlockState m -> Amount -> m (Amount, UpdatableBlockState m)

  -- |Change the given account's stake delegation. Return 'False' if the target
  -- is an invalid baker (and delegation is unchanged), and 'True' otherwise.
  -- The method requires that the account already exists.
  bsoDelegateStake :: UpdatableBlockState m -> AccountAddress -> Maybe BakerId -> m (Bool, UpdatableBlockState m)

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

  -- |Update the birk parameters of a block state
  bsoUpdateBirkParameters :: UpdatableBlockState m -> BirkParameters m -> m (UpdatableBlockState m)

  -- |Directly set the election difficulty birk parameter of a block state.
  bsoSetElectionDifficulty :: UpdatableBlockState m -> ElectionDifficulty -> m (UpdatableBlockState m)

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

    -- |Load a block state from a reference.
    loadBlockState :: BlockStateRef m -> m (BlockState m)

instance (Monad (t m), MonadTrans t, BirkParametersOperations m) => BirkParametersOperations (MGSTrans t m) where
    getSeedState = lift . getSeedState
    updateBirkParametersForNewEpoch s = lift . updateBirkParametersForNewEpoch s
    getElectionDifficulty = lift . getElectionDifficulty
    getCurrentBakers = lift . getCurrentBakers
    getLotteryBakers = lift . getLotteryBakers
    updateSeedState f = lift . updateSeedState f
    {-# INLINE getSeedState #-}
    {-# INLINE updateBirkParametersForNewEpoch #-}
    {-# INLINE getElectionDifficulty #-}
    {-# INLINE getCurrentBakers #-}
    {-# INLINE getLotteryBakers #-}
    {-# INLINE updateSeedState #-}

instance (Monad (t m), MonadTrans t, BlockStateQuery m) => BlockStateQuery (MGSTrans t m) where
  getModule s = lift . getModule s
  getAccount s = lift . getAccount s
  getContractInstance s = lift . getContractInstance s
  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList
  getBlockBirkParameters = lift . getBlockBirkParameters
  getRewardStatus = lift . getRewardStatus
  getTransactionOutcome s = lift . getTransactionOutcome s
  getTransactionOutcomesHash = lift . getTransactionOutcomesHash
  getStateHash = lift . getStateHash
  getOutcomes = lift . getOutcomes
  getSpecialOutcomes = lift . getSpecialOutcomes
  getAllIdentityProviders s = lift $ getAllIdentityProviders s
  getAllAnonymityRevokers s = lift $ getAllAnonymityRevokers s
  {-# INLINE getModule #-}
  {-# INLINE getAccount #-}
  {-# INLINE getContractInstance #-}
  {-# INLINE getModuleList #-}
  {-# INLINE getAccountList #-}
  {-# INLINE getContractInstanceList #-}
  {-# INLINE getBlockBirkParameters #-}
  {-# INLINE getRewardStatus #-}
  {-# INLINE getOutcomes #-}
  {-# INLINE getTransactionOutcome #-}
  {-# INLINE getTransactionOutcomesHash #-}
  {-# INLINE getSpecialOutcomes #-}
  {-# INLINE getAllIdentityProviders #-}
  {-# INLINE getAllAnonymityRevokers #-}

instance (Monad (t m), MonadTrans t, BakerQuery m) => BakerQuery (MGSTrans t m) where
  getBakerStake bs = lift . getBakerStake bs
  getBakerFromKey bs = lift . getBakerFromKey bs
  getTotalBakerStake = lift . getTotalBakerStake
  getBakerInfo bs = lift . getBakerInfo bs
  getFullBakerInfos = lift . getFullBakerInfos
  {-# INLINE getBakerStake #-}
  {-# INLINE getBakerFromKey #-}
  {-# INLINE getTotalBakerStake #-}
  {-# INLINE getBakerInfo #-}
  {-# INLINE getFullBakerInfos #-}

instance (Monad (t m), MonadTrans t, AccountOperations m) => AccountOperations (MGSTrans t m) where
  getAccountAddress = lift . getAccountAddress
  getAccountAmount = lift. getAccountAmount
  getAccountNonce = lift . getAccountNonce
  getAccountCredentials = lift . getAccountCredentials
  getAccountMaxCredentialValidTo = lift . getAccountMaxCredentialValidTo
  getAccountVerificationKeys = lift . getAccountVerificationKeys
  getAccountEncryptedAmount = lift . getAccountEncryptedAmount
  getAccountEncryptionKey = lift . getAccountEncryptionKey
  getAccountStakeDelegate = lift . getAccountStakeDelegate
  getAccountInstances = lift . getAccountInstances
  createNewAccount gc ks addr = lift . createNewAccount gc ks addr
  updateAccountAmount acc = lift . updateAccountAmount acc
  {-# INLINE getAccountAddress #-}
  {-# INLINE getAccountAmount #-}
  {-# INLINE getAccountCredentials #-}
  {-# INLINE getAccountMaxCredentialValidTo #-}
  {-# INLINE getAccountNonce #-}
  {-# INLINE getAccountVerificationKeys #-}
  {-# INLINE getAccountEncryptedAmount #-}
  {-# INLINE getAccountStakeDelegate #-}
  {-# INLINE getAccountInstances #-}
  {-# INLINE createNewAccount #-}
  {-# INLINE updateAccountAmount #-}

instance (Monad (t m), MonadTrans t, BlockStateOperations m) => BlockStateOperations (MGSTrans t m) where
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s
  bsoPutNewAccount s = lift . bsoPutNewAccount s
  bsoPutNewInstance s = lift . bsoPutNewInstance s
  bsoPutNewModule s miface = lift (bsoPutNewModule s miface)
  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoModifyInstance s caddr amount model = lift $ bsoModifyInstance s caddr amount model
  bsoNotifyExecutionCost s = lift . bsoNotifyExecutionCost s
  bsoNotifyEncryptedBalanceChange s = lift . bsoNotifyEncryptedBalanceChange s
  bsoNotifyIdentityIssuerCredential s = lift . bsoNotifyIdentityIssuerCredential s
  bsoGetExecutionCost = lift . bsoGetExecutionCost
  bsoGetBlockBirkParameters = lift . bsoGetBlockBirkParameters
  bsoAddBaker s = lift . bsoAddBaker s
  bsoUpdateBaker s = lift . bsoUpdateBaker s
  bsoRemoveBaker s = lift . bsoRemoveBaker s
  bsoSetInflation s = lift . bsoSetInflation s
  bsoMint s = lift . bsoMint s
  bsoDecrementCentralBankGTU s = lift . bsoDecrementCentralBankGTU s
  bsoDelegateStake s acct bid = lift $ bsoDelegateStake s acct bid
  bsoGetIdentityProvider s ipId = lift $ bsoGetIdentityProvider s ipId
  bsoGetAnonymityRevokers s arId = lift $ bsoGetAnonymityRevokers s arId
  bsoGetCryptoParams s = lift $ bsoGetCryptoParams s
  bsoSetTransactionOutcomes s = lift . bsoSetTransactionOutcomes s
  bsoAddSpecialTransactionOutcome s = lift . bsoAddSpecialTransactionOutcome s
  bsoUpdateBirkParameters bps = lift . bsoUpdateBirkParameters bps
  bsoSetElectionDifficulty s d = lift $ bsoSetElectionDifficulty s d
  {-# INLINE bsoGetModule #-}
  {-# INLINE bsoGetAccount #-}
  {-# INLINE bsoGetInstance #-}
  {-# INLINE bsoRegIdExists #-}
  {-# INLINE bsoPutNewAccount #-}
  {-# INLINE bsoPutNewInstance #-}
  {-# INLINE bsoPutNewModule #-}
  {-# INLINE bsoModifyAccount #-}
  {-# INLINE bsoModifyInstance #-}
  {-# INLINE bsoNotifyExecutionCost #-}
  {-# INLINE bsoNotifyIdentityIssuerCredential #-}
  {-# INLINE bsoGetExecutionCost #-}
  {-# INLINE bsoNotifyEncryptedBalanceChange #-}
  {-# INLINE bsoGetBlockBirkParameters #-}
  {-# INLINE bsoAddBaker #-}
  {-# INLINE bsoUpdateBaker #-}
  {-# INLINE bsoRemoveBaker #-}
  {-# INLINE bsoSetInflation #-}
  {-# INLINE bsoMint #-}
  {-# INLINE bsoDecrementCentralBankGTU #-}
  {-# INLINE bsoDelegateStake #-}
  {-# INLINE bsoGetIdentityProvider #-}
  {-# INLINE bsoGetAnonymityRevokers #-}
  {-# INLINE bsoGetCryptoParams #-}
  {-# INLINE bsoSetTransactionOutcomes #-}
  {-# INLINE bsoAddSpecialTransactionOutcome #-}
  {-# INLINE bsoUpdateBirkParameters #-}

instance (Monad (t m), MonadTrans t, BlockStateStorage m) => BlockStateStorage (MGSTrans t m) where
    thawBlockState = lift . thawBlockState
    freezeBlockState = lift . freezeBlockState
    dropUpdatableBlockState = lift . dropUpdatableBlockState
    purgeBlockState = lift . purgeBlockState
    archiveBlockState = lift . archiveBlockState
    saveBlockState = lift . saveBlockState
    loadBlockState = lift . loadBlockState
    {-# INLINE thawBlockState #-}
    {-# INLINE freezeBlockState #-}
    {-# INLINE dropUpdatableBlockState #-}
    {-# INLINE purgeBlockState #-}
    {-# INLINE archiveBlockState #-}
    {-# INLINE saveBlockState #-}
    {-# INLINE loadBlockState #-}

deriving via (MGSTrans MaybeT m) instance BirkParametersOperations m => BirkParametersOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateQuery m => BlockStateQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance AccountOperations m => AccountOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BakerQuery m => BakerQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateOperations m => BlockStateOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateStorage m => BlockStateStorage (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance BirkParametersOperations m => BirkParametersOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateQuery m => BlockStateQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance AccountOperations m => AccountOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BakerQuery m => BakerQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateOperations m => BlockStateOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateStorage m => BlockStateStorage (ExceptT e m)
