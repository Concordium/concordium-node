{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DerivingVia #-}
module Concordium.GlobalState.BlockState where

import Data.Time
import Lens.Micro.Platform
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import Control.Monad

import Concordium.Types
import Concordium.Types.Execution (ValidResult)
import Concordium.GlobalState.Block
import Concordium.Types.Acorn.Core(ModuleRef)
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Modules hiding (getModule)
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Transactions (TransactionHash)

import Data.Void

import qualified Concordium.ID.Types as ID


class (Eq bp, Show bp, BlockData bp) => BlockPointerData bp where
    type BlockState' bp :: *
    -- |Hash of the block
    bpHash :: bp -> BlockHash
    -- |The block itself
    bpBlock :: bp -> Block
    -- |Pointer to the parent (circular reference for genesis block)
    bpParent :: bp -> bp
    -- |Pointer to the last finalized block (circular for genesis)
    bpLastFinalized :: bp -> bp
    -- |Height of the block in the tree
    bpHeight :: bp -> BlockHeight
    -- |The handle for accessing the state (of accounts, contracts, etc.) at the end of the block.
    bpState :: bp -> BlockState' bp
    -- |Time at which the block was first received
    bpReceiveTime :: bp -> UTCTime
    -- |Time at which the block was first considered part of the tree (validated)
    bpArriveTime :: bp -> UTCTime
    -- |Number of transactions in a block
    bpTransactionCount :: bp -> Int

type family BlockPointer (m :: * -> *) :: *

type BlockState (m :: * -> *) = BlockState' (BlockPointer m)


-- |The block query methods can query block state. They are needed by
-- consensus itself to compute stake, get a list of and information about
-- bakers, finalization committee, etc.
class Monad m => BlockStateQuery m where
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

    -- |Get the outcome of a transaction in the latest block.
    getTransactionOutcome :: BlockState m -> TransactionHash -> m (Maybe ValidResult)

type family UpdatableBlockState (m :: * -> *) :: *

data EncryptedAmountUpdate = Replace !EncryptedAmount -- ^Replace the encrypted amount, such as when compressing.
                           | Add !EncryptedAmount     -- ^Add an encrypted amount to the list of encrypted amounts.
                           | Empty                    -- ^Do nothing to the encrypted amount.

-- |An update to an account state.
data AccountUpdate = AccountUpdate {
  -- |Address of the affected account.
  _auAddress :: !AccountAddress
  -- |Optionally a new account nonce.
  ,_auNonce :: !(Maybe Nonce)
  -- |Optionally a new account amount.
  ,_auAmount :: !(Maybe Amount)
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
       _accountAmount = (acc ^. accountAmount) & setMaybe (upd ^. auAmount),
       _accountCredentials =
          case upd ^. auCredential of
            Nothing -> acc ^. accountCredentials
            Just c -> c : (acc ^. accountCredentials),
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
                  -> UnlinkedValueInterface Void
                  -> Core.Module Core.UA
                  -> m (Bool, UpdatableBlockState m)

  -- |Modify an existing account with given data (which includes the address of the account).
  -- This method is only called when an account exists and can thus assume this.
  -- NB: In case we are adding a credential to an account this method __must__ also
  -- update the global set of known credentials.
  bsoModifyAccount :: UpdatableBlockState m -> AccountUpdate -> m (UpdatableBlockState m)
  -- |Replace the instance with given data. The rest of the instance data (instance parameters) stays the same.
  -- This method is only called when it is known the instance exists, and can thus assume it.
  bsoModifyInstance :: UpdatableBlockState m
                    -> ContractAddress
                    -> Amount
                    -> Value Void
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

  -- |Get the account of the given baker.
  bsoGetBakerAccount :: UpdatableBlockState m -> BakerId -> m (Maybe Account)
  bsoGetBakerAccount s bid = do
    binfo <- bsoGetBakerInfo s bid
    join <$> mapM (bsoGetAccount s . _bakerAccount) binfo


  -- |Add a new baker to the baker pool. Assign a fresh baker identity to the 
  -- new baker and return the assigned identity.
  -- This method should also update the next available baker id in the system.
  bsoAddBaker :: UpdatableBlockState m -> BakerCreationInfo -> m (BakerId, UpdatableBlockState m)
  
  -- |Update an existing baker's information. The method may assume that the baker with 
  -- the given Id exists.
  bsoUpdateBaker :: UpdatableBlockState m -> BakerUpdate -> m (UpdatableBlockState m)

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
  --  is an invalid baker (and delegation is unchanged), and 'True' otherwise.
  bsoDelegateStake :: UpdatableBlockState m -> AccountAddress -> Maybe BakerId -> m (Bool, UpdatableBlockState m)

  -- |Get the identity provider data for the given identity provider, or Nothing if
  -- the identity provider with given ID does not exist.
  bsoGetIdentityProvider :: UpdatableBlockState m -> ID.IdentityProviderIdentity -> m (Maybe IdentityProviderData)

  -- |Get the current cryptographic parameters. The idea is that these will be
  -- periodically updated and so they must be part of the block state.
  bsoGetCryptoParams :: UpdatableBlockState m -> m CryptographicParameters

  -- |Set the list of transaction outcomes for the block.
  bsoSetTransactionOutcomes :: UpdatableBlockState m -> [(TransactionHash, ValidResult)] -> m (UpdatableBlockState m)


newtype BSMTrans t (m :: * -> *) a = BSMTrans (t m a)
    deriving (Functor, Applicative, Monad, MonadTrans)
type instance UpdatableBlockState (BSMTrans t m) = UpdatableBlockState m
type instance BlockPointer (BSMTrans t m) = BlockPointer m

instance (Monad (t m), MonadTrans t, BlockStateQuery m) => BlockStateQuery (BSMTrans t m) where
  getModule s = lift . getModule s
  getAccount s = lift . getAccount s
  getContractInstance s = lift . getContractInstance s
  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList
  getBlockBirkParameters = lift . getBlockBirkParameters
  getRewardStatus = lift . getRewardStatus
  getTransactionOutcome s = lift . getTransactionOutcome s
  {-# INLINE getModule #-}
  {-# INLINE getAccount #-}
  {-# INLINE getContractInstance #-}
  {-# INLINE getModuleList #-}
  {-# INLINE getAccountList #-}
  {-# INLINE getContractInstanceList #-}
  {-# INLINE getBlockBirkParameters #-}
  {-# INLINE getRewardStatus #-}
  {-# INLINE getTransactionOutcome #-}

instance (Monad (t m), MonadTrans t, BlockStateOperations m) => BlockStateOperations (BSMTrans t m) where
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s
  bsoPutNewAccount s = lift . bsoPutNewAccount s
  bsoPutNewInstance s = lift . bsoPutNewInstance s
  bsoPutNewModule s mref iface viface source = lift (bsoPutNewModule s mref iface viface source)
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


type instance BlockPointer (MaybeT m) = BlockPointer m
type instance UpdatableBlockState (MaybeT m) = UpdatableBlockState m
deriving via (BSMTrans MaybeT m) instance BlockStateQuery m => BlockStateQuery (MaybeT m)
deriving via (BSMTrans MaybeT m) instance BlockStateOperations m => BlockStateOperations (MaybeT m)

type instance BlockPointer (RWST r w s m) = BlockPointer m
type instance UpdatableBlockState (RWST r w s m) = UpdatableBlockState m
deriving via (BSMTrans (RWST r w s) m) instance (BlockStateQuery m, Monoid w) => BlockStateQuery (RWST r w s m)
deriving via (BSMTrans (RWST r w s) m) instance (BlockStateOperations m, Monoid w) => BlockStateOperations (RWST r w s m)
