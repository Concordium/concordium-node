{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DerivingVia #-}
module Concordium.GlobalState.BlockState where

import Lens.Micro.Platform
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.Strict hiding (ask)
import Data.Word
import qualified Data.Serialize as S

import Concordium.Types
import Concordium.Types.Execution
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Block
import Concordium.Types.Acorn.Core(ModuleRef)
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders
import Concordium.Types.Transactions
import qualified Data.PQueue.Prio.Max as Queue

import Data.Maybe

import qualified Concordium.ID.Types as ID

type ModuleIndex = Word64

data Module = Module {
    moduleInterface :: Interface Core.UA,
    moduleValueInterface :: UnlinkedValueInterface Core.NoAnnot,
    moduleIndex :: !ModuleIndex,
    moduleSource :: Core.Module Core.UA
}


-- |The block query methods can query block state. They are needed by
-- consensus itself to compute stake, get a list of and information about
-- bakers, finalization committee, etc.
class (Monad m, BlockStateTypes m) => BlockStateQuery m where
    -- |Get the module from the module table of the state instance.
    getModule :: BlockState m -> ModuleRef -> m (Maybe Module)
    -- |Get the account state from the account table of the state instance.
    getAccount :: BlockState m -> AccountAddress -> m (Maybe Account)
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
    getBlockBirkParameters :: BlockState m -> m BirkParameters

    -- |Get reward summary for this block.
    getRewardStatus :: BlockState m -> m BankStatus

    -- |Get the outcome of a transaction in the given block.
    getTransactionOutcome :: BlockState m -> TransactionHash -> m (Maybe ValidResult)

    -- |Get special transactions outcomes (for administrative transactions, e.g., baker reward)
    getSpecialOutcomes :: BlockState m -> m [SpecialTransactionOutcome]

data EncryptedAmountUpdate = Replace !EncryptedAmount -- ^Replace the encrypted amount, such as when compressing.
                           | Add !EncryptedAmount     -- ^Add an encrypted amount to the list of encrypted amounts.
                           | Empty                    -- ^Do nothing to the encrypted amount.

-- |An update to an account state.
data AccountUpdate = AccountUpdate {
  -- |Address of the affected account.
  _auAddress :: !AccountAddress
  -- |Optionally a new account nonce.
  ,_auNonce :: !(Maybe Nonce)
  -- |Optionally an update to the account amount.
  ,_auAmount :: !(Maybe AmountDelta)
  -- |Optionally an encryption key.
  ,_auEncryptionKey :: !(Maybe ID.AccountEncryptionKey)
  -- |Optionally an update to the encrypted amounts.
  ,_auEncrypted :: !EncryptedAmountUpdate
  -- |Optionally a new credential.
  ,_auCredential :: !(Maybe ID.CredentialDeploymentValues)
  }
makeLenses ''AccountUpdate

emptyAccountUpdate :: AccountAddress -> AccountUpdate
emptyAccountUpdate addr = AccountUpdate addr Nothing Nothing Nothing Empty Nothing

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: AccountUpdate -> Account -> Account
updateAccount !upd !acc =
  acc {_accountNonce = (acc ^. accountNonce) & setMaybe (upd ^. auNonce),
       _accountAmount = fst (acc & accountAmount <%~ applyAmountDelta (upd ^. auAmount . non 0)),
       _accountCredentials =
          case upd ^. auCredential of
            Nothing -> acc ^. accountCredentials
            Just c -> Queue.insert (ID.pExpiry (ID.cdvPolicy c)) c (acc ^. accountCredentials),
       _accountEncryptionKey =
          case upd ^. auEncryptionKey of
            Nothing -> acc ^. accountEncryptionKey
            Just ek -> Just ek, -- relies on the invariant that the scheduler should have checked, encryption key cannot be redefined.
       _accountEncryptedAmount =
          case upd ^. auEncrypted of
            Empty -> acc ^. accountEncryptedAmount
            Add ea -> ea:(acc ^. accountEncryptedAmount)
            Replace ea -> [ea]
    }

  where setMaybe (Just x) _ = x
        setMaybe Nothing y = y


-- |Block state update operations parametrized by a monad. The operations which
-- mutate the state all also return an 'UpdatableBlockState' handle. This is to
-- support different implementations, from pure ones to stateful ones.
class BlockStateQuery m => BlockStateOperations m where
  -- |Get the module from the module table of the state instance.
  bsoGetModule :: UpdatableBlockState m -> ModuleRef -> m (Maybe Module)
  -- |Get an account by its address.
  bsoGetAccount :: UpdatableBlockState m -> AccountAddress -> m (Maybe Account)
  -- |Get the contract state from the contract table of the state instance.
  bsoGetInstance :: UpdatableBlockState m -> ContractAddress -> m (Maybe Instance)

  -- |Check whether an the given credential registration ID exists.
  -- Return @True@ iff so.
  bsoRegIdExists :: UpdatableBlockState m -> ID.CredentialRegistrationID -> m Bool

  -- |Try to add a new account to the state. If an account with the address already exists
  -- return @False@, and if the account was successfully added return @True@.
  bsoPutNewAccount :: UpdatableBlockState m -> Account -> m (Bool, UpdatableBlockState m)
  -- |Add a new smart contract instance to the state.
  bsoPutNewInstance :: UpdatableBlockState m -> (ContractAddress -> Instance) -> m (ContractAddress, UpdatableBlockState m)
  -- |Add the module to the global state. If a module with the given address
  -- already exists return @False@.
  bsoPutNewModule :: UpdatableBlockState m
                  -> ModuleRef
                  -> Interface Core.UA
                  -> UnlinkedValueInterface Core.NoAnnot
                  -> Core.Module Core.UA
                  -> m (Bool, UpdatableBlockState m)

  -- |Consult the linked expression cache for whether this definitionn is already linked.
  bsoTryGetLinkedExpr :: UpdatableBlockState m -> Core.ModuleRef -> Core.Name -> m (Maybe (LinkedExprWithDeps Core.NoAnnot))

  -- |Put a new linked expression to the cache.
  -- This method may assume that the module with given reference is already in the state (i.e., putNewModule was called before).
  bsoPutLinkedExpr :: UpdatableBlockState m -> Core.ModuleRef -> Core.Name -> LinkedExprWithDeps Core.NoAnnot -> m (UpdatableBlockState m)

  -- |Try to get linked contract code from the cache.
  bsoTryGetLinkedContract :: UpdatableBlockState m
                          -> Core.ModuleRef
                          -> Core.TyName
                          -> m (Maybe (LinkedContractValue Core.NoAnnot))

  -- |Store the linked contract code in the linked code cache.
  -- This method may assume that the module with given reference is already in the state (i.e., putNewModule was called before).
  bsoPutLinkedContract :: UpdatableBlockState m
                       -> Core.ModuleRef
                       -> Core.TyName
                       -> LinkedContractValue Core.NoAnnot
                       -> m (UpdatableBlockState m)

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
                    -> Value Core.NoAnnot
                    -> m (UpdatableBlockState m)

  -- |Notify the block state that the given amount was spent on execution.
  bsoNotifyExecutionCost :: UpdatableBlockState m -> Amount -> m (UpdatableBlockState m)

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
  bsoGetBlockBirkParameters :: UpdatableBlockState m -> m BirkParameters

  -- |Get the 'BakerInfo' for a given baker.
  bsoGetBakerInfo :: UpdatableBlockState m -> BakerId -> m (Maybe BakerInfo)
  bsoGetBakerInfo s bid = do
    bps <- bsoGetBlockBirkParameters s
    return $! fst <$> birkBaker bid bps

  -- |Get the reward account of the given baker.
  bsoGetEpochBakerAccount :: UpdatableBlockState m -> BakerId -> m (Maybe Account)
  bsoGetEpochBakerAccount s bid = do
    bps <- bsoGetBlockBirkParameters s
    let binfo = fst <$> (birkEpochBaker bid bps)
    join <$> mapM (bsoGetAccount s . _bakerAccount) binfo


  -- |Add a new baker to the baker pool. Assign a fresh baker identity to the

  -- new baker and return the assigned identity.
  -- This method should also update the next available baker id in the system.
  -- If a baker with the given signing key already exists do nothing and
  -- return 'Nothing'
  bsoAddBaker :: UpdatableBlockState m -> BakerCreationInfo -> m (Maybe BakerId, UpdatableBlockState m)
  
  -- |Update an existing baker's information. The method may assume that the baker with 
  -- the given Id exists.
  -- If a baker with a given signing key already exists return 'False', and if the baker
  -- was successfully updated return 'True'.
  -- If updating the account the precondition of this method is that the reward account exists.
  bsoUpdateBaker :: UpdatableBlockState m -> BakerUpdate -> m (Bool, UpdatableBlockState m)

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

  -- |Get the current cryptographic parameters. The idea is that these will be
  -- periodically updated and so they must be part of the block state.
  bsoGetCryptoParams :: UpdatableBlockState m -> m CryptographicParameters

  -- |Set the list of transaction outcomes for the block.
  bsoSetTransactionOutcomes :: UpdatableBlockState m -> [(TransactionHash, ValidResult)] -> m (UpdatableBlockState m)

  -- |Add a special transaction outcome.
  bsoAddSpecialTransactionOutcome :: UpdatableBlockState m -> SpecialTransactionOutcome -> m (UpdatableBlockState m)

  -- |Update the birk parameters of a block state
  bsoUpdateBirkParameters :: UpdatableBlockState m -> BirkParameters -> m (UpdatableBlockState m)

-- | Block state storage operations
class BlockStateOperations m => BlockStateStorage m where
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

    -- |Serialize a block state.
    putBlockState :: BlockState m -> m S.Put

    -- |Deserialize a block state.
    getBlockState :: S.Get (m (BlockState m))


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
  getSpecialOutcomes = lift . getSpecialOutcomes
  {-# INLINE getModule #-}
  {-# INLINE getAccount #-}
  {-# INLINE getContractInstance #-}
  {-# INLINE getModuleList #-}
  {-# INLINE getAccountList #-}
  {-# INLINE getContractInstanceList #-}
  {-# INLINE getBlockBirkParameters #-}
  {-# INLINE getRewardStatus #-}
  {-# INLINE getTransactionOutcome #-}
  {-# INLINE getSpecialOutcomes #-}

instance (Monad (t m), MonadTrans t, BlockStateOperations m) => BlockStateOperations (MGSTrans t m) where
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s
  bsoPutNewAccount s = lift . bsoPutNewAccount s
  bsoPutNewInstance s = lift . bsoPutNewInstance s
  bsoPutNewModule s mref iface viface source = lift (bsoPutNewModule s mref iface viface source)
  bsoTryGetLinkedExpr s mref n = lift (bsoTryGetLinkedExpr s mref n)
  bsoPutLinkedExpr s mref n linked = lift (bsoPutLinkedExpr s mref n linked)
  bsoTryGetLinkedContract s mref n = lift (bsoTryGetLinkedContract s mref n)
  bsoPutLinkedContract s mref n linked = lift (bsoPutLinkedContract s mref n linked)
  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoModifyInstance s caddr amount model = lift $ bsoModifyInstance s caddr amount model
  bsoNotifyExecutionCost s = lift . bsoNotifyExecutionCost s
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
  bsoGetCryptoParams s = lift $ bsoGetCryptoParams s
  bsoSetTransactionOutcomes s = lift . bsoSetTransactionOutcomes s
  bsoAddSpecialTransactionOutcome s = lift . bsoAddSpecialTransactionOutcome s
  bsoUpdateBirkParameters bps = lift . bsoUpdateBirkParameters bps
  {-# INLINE bsoGetModule #-}
  {-# INLINE bsoGetAccount #-}
  {-# INLINE bsoGetInstance #-}
  {-# INLINE bsoRegIdExists #-}
  {-# INLINE bsoPutNewAccount #-}
  {-# INLINE bsoPutNewInstance #-}
  {-# INLINE bsoPutNewModule #-}
  {-# INLINE bsoTryGetLinkedExpr #-}
  {-# INLINE bsoPutLinkedExpr #-}
  {-# INLINE bsoTryGetLinkedContract #-}
  {-# INLINE bsoPutLinkedContract #-}
  {-# INLINE bsoModifyAccount #-}
  {-# INLINE bsoModifyInstance #-}
  {-# INLINE bsoNotifyExecutionCost #-}
  {-# INLINE bsoNotifyIdentityIssuerCredential #-}
  {-# INLINE bsoGetExecutionCost #-}
  {-# INLINE bsoGetBlockBirkParameters #-}
  {-# INLINE bsoAddBaker #-}
  {-# INLINE bsoUpdateBaker #-}
  {-# INLINE bsoRemoveBaker #-}
  {-# INLINE bsoSetInflation #-}
  {-# INLINE bsoMint #-}
  {-# INLINE bsoDecrementCentralBankGTU #-}
  {-# INLINE bsoDelegateStake #-}
  {-# INLINE bsoGetIdentityProvider #-}
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
    putBlockState = lift . putBlockState
    getBlockState = fmap lift getBlockState
    {-# INLINE thawBlockState #-}
    {-# INLINE freezeBlockState #-}
    {-# INLINE dropUpdatableBlockState #-}
    {-# INLINE purgeBlockState #-}
    {-# INLINE archiveBlockState #-}
    {-# INLINE putBlockState #-}
    {-# INLINE getBlockState #-}


deriving via (MGSTrans MaybeT m) instance BlockStateQuery m => BlockStateQuery (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateOperations m => BlockStateOperations (MaybeT m)
deriving via (MGSTrans MaybeT m) instance BlockStateStorage m => BlockStateStorage (MaybeT m)

deriving via (MGSTrans (ExceptT e) m) instance BlockStateQuery m => BlockStateQuery (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateOperations m => BlockStateOperations (ExceptT e m)
deriving via (MGSTrans (ExceptT e) m) instance BlockStateStorage m => BlockStateStorage (ExceptT e m)

-- deriving via (BSMTrans g (RWST r w s) m) instance (BlockStateQuery g m, Monoid w) => BlockStateQuery g (RWST r w s m)
-- deriving via (BSMTrans g (RWST r w s) m) instance (BlockStateOperations g m, Monoid w) => BlockStateOperations g (RWST r w s m)

deriving instance BlockStateQuery m => BlockStateQuery (TreeStateM s m)
deriving instance BlockStateOperations m => BlockStateOperations (TreeStateM s m)
deriving instance BlockStateStorage m => BlockStateStorage (TreeStateM s m)

data TransferReason =
  -- |Transfer because of a top-level transaction recorded on a block.
  DirectTransfer {
    -- |Id of the top-level transaction.
    trdtId :: !TransactionHash,
    -- |Source account.
    trdtSource :: !AccountAddress,
    -- |Amount transferred
    trdtAmount :: !Amount,
    -- |Recepient.
    trdtTarget :: !AccountAddress
    } |
  -- |Transfer from accout to contract
  AccountToContractTransfer {
    -- |Id of the top-level transaction.
    tractId :: !TransactionHash,
    -- |From which account was the transfer made.
    tractSource :: !AccountAddress,
    -- |How much was transferred.
    tractAmount :: !Amount,
    -- |To which contract.
    tractTarget :: !ContractAddress
  } |
  -- |Generated transaction from a contract to account.
  -- Transaction hash is of the original top-level transaction.
  ContractToAccountTransfer {
    -- |Id of the top-level transaction.
    trcatId :: !TransactionHash,
    -- |From which contract
    trcatSource :: !ContractAddress,
    -- |Amount transferred.
    trcatAmount :: !Amount,
    -- |Recepient account.
    trcatTarget :: !AccountAddress
    } |
  ContractToContractTransfer {
    -- |Id of the top-level transaction.
    trcctId :: !TransactionHash,
    -- |From which contract
    trcctSource :: !ContractAddress,
    -- |Amount transferred.
    trcctAmount :: !Amount,
    -- |Recepient account.
    trcctTarget :: !ContractAddress
    } |
  CredentialDeployment {
    -- |Id of the top-level transaction.
    trcdId :: !TransactionHash,
    -- |Which account sent the transaction.
    trcdSource :: !AccountAddress,
    -- |To which account was the credential deployed.
    trcdAccount :: !AccountAddress,
    -- |Credential registration ID.values which were deployed deployed.
    trcdCredentialRegId :: !ID.CredentialRegistrationID
    } |
  -- |Baking reward (here meaning the actual block reward + execution reward for block transactions).
  BakingRewardTransfer {
    -- |Id of the baker.
    trbrBaker :: !BakerId,
    -- |Account address of the baker.
    trbrAccount :: !AccountAddress,
    -- |Reward amount.
    trbrAmount :: !Amount
    } |
  -- |Cost of a transaction.
  ExecutionCost {
    trecId :: !TransactionHash,
    -- |Sender of the transaction.
    trecSource :: !AccountAddress,
    -- |Execution cost.
    trecAmount :: !Amount,
    -- |Baker id of block baker.
    trecBaker :: !BakerId
    }
  deriving(Show)

resultToReasons :: (BlockMetadata bp, TransactionData tx) => bp -> tx -> ValidResult -> [TransferReason]
resultToReasons bp tx res =
  case res of
       TxReject _ a _ -> [ExecutionCost trId sender a baker]
       TxSuccess events a _ -> mapMaybe extractReason events ++ [ExecutionCost trId sender a baker]
  where extractReason (Transferred (AddressAccount source) amount (AddressAccount target)) =
          Just (DirectTransfer trId source amount target)
        extractReason (Transferred (AddressContract source) amount (AddressAccount target)) =
          Just (ContractToAccountTransfer trId source amount target)
        extractReason (Transferred (AddressAccount source) amount (AddressContract target)) =
          Just (AccountToContractTransfer trId source amount target)
        extractReason (Transferred (AddressContract source) amount (AddressContract target)) =
          Just (ContractToContractTransfer trId source amount target)
        extractReason (Updated target (AddressAccount source) amount _) =
          Just (AccountToContractTransfer trId source amount target)
        extractReason (Updated target (AddressContract source) amount _) =
          Just (ContractToContractTransfer trId source amount target)
        extractReason (CredentialDeployed regid address) =
          Just (CredentialDeployment trId sender address regid)
        extractReason _ = Nothing
        
        trId = transactionHash tx
        sender = thSender (transactionHeader tx)
        baker = blockBaker bp

specialToReason :: BlockMetadata bp => bp -> SpecialTransactionOutcome -> TransferReason
specialToReason bp (BakingReward acc amount) = BakingRewardTransfer (blockBaker bp) acc amount

type LogTransferMethod m = BlockHash -> Slot -> TransferReason -> m ()

-- |Account transfer logger monad.
class Monad m => ATLMonad m where
  atlLogTransfer :: LogTransferMethod m

newtype ATLoggerT m a = ATLoggerT {_runATLoggerT :: ReaderT (LogTransferMethod m) m a}
    deriving(Functor, Applicative, Monad, MonadIO)

newtype ATSilentLoggerT m a = ATSilentLoggerT { runATSilentLoggerT :: m a }
    deriving(Functor, Applicative, Monad, MonadIO)

instance Monad m => ATLMonad (ATSilentLoggerT m) where
  {-# INLINE atlLogTransfer #-}
  atlLogTransfer = \_ _ _ -> return ()

instance Monad m => ATLMonad (ATLoggerT m) where
  {-# INLINE atlLogTransfer #-}
  atlLogTransfer bh slot reason = ATLoggerT $ do
    lm <- ask
    lift (lm bh slot reason)

-- |Run an action handling transfer events with the given log method.
{-# INLINE runATLoggerT #-}
runATLoggerT :: ATLoggerT m a -> LogTransferMethod m -> m a
runATLoggerT = runReaderT . _runATLoggerT

-- |Run an action discarding all events.
{-# INLINE runSilentLogger #-}
runSilentLogger :: (Monad m) => ATLoggerT m a -> m a
runSilentLogger a = runATLoggerT a (\_ _ _ -> pure ())

instance MonadTrans ATLoggerT where
    lift = ATLoggerT . lift

instance (ATLMonad m, Monoid w) => ATLMonad (RWST r w s m) where
    atlLogTransfer src lvl msg = lift (atlLogTransfer src lvl msg)

instance ATLMonad m => ATLMonad (StateT s m) where
    atlLogTransfer src lvl msg = lift (atlLogTransfer src lvl msg)

instance ATLMonad m => ATLMonad (MaybeT m) where
    atlLogTransfer src lvl msg = lift (atlLogTransfer src lvl msg)

instance ATLMonad m => ATLMonad (ExceptT e m) where
    atlLogTransfer src lvl msg = lift (atlLogTransfer src lvl msg)

instance (Monad (t m), MonadTrans t, ATLMonad m) => ATLMonad (MGSTrans t m) where
    atlLogTransfer src lvl msg = lift (atlLogTransfer src lvl msg)
