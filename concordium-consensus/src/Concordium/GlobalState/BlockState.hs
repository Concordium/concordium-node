{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.BlockState where

import Data.Time
import Lens.Micro.Platform
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS

import Concordium.Types
import Concordium.GlobalState.Block
import Concordium.Types.Acorn.Core(ModuleRef)
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Interfaces
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Modules

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
    getBirkParameters :: BlockState m -> m BirkParameters

    -- |Get reward summary for this block.
    getRewardStatus :: BlockState m -> m BankStatus


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
  ,_auCredential :: !(Maybe ID.CredentialDeploymentInformation)
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

data BakerUpdate = BakerUpdate {
  -- |Identity of the baker to update.
  _buId :: !BakerId,
  -- |Optionally update the baker's reward account.
  _buAccount :: !(Maybe AccountAddress),
  -- |Optionally update the baker's public verification key.
  _buSignKey :: !(Maybe BakerSignVerifyKey),
  -- |Optionally update the baker's lottery power.
  _buLotteryPower :: !(Maybe LotteryPower)
}

emptyBakerUpdate :: BakerId -> BakerUpdate
emptyBakerUpdate bid = BakerUpdate bid Nothing Nothing Nothing

makeLenses ''BakerUpdate

updateBaker :: BakerUpdate -> BakerInfo -> BakerInfo
updateBaker !BakerUpdate{..} !binfo =
  binfo {
    bakerSignatureVerifyKey = fromMaybe (binfo & bakerSignatureVerifyKey) _buSignKey,
    bakerAccount = fromMaybe (binfo & bakerAccount) _buAccount,
    bakerLotteryPower = fromMaybe (binfo & bakerLotteryPower) _buLotteryPower
  }

-- |Block state update operations parametrized by a monad. The operations which
-- mutate the state all also return an 'UpdatableBlockState' handle. This is to
-- support different implementations, from pure ones to stateful ones.
class BlockStateQuery m => BlockStateOperations m where
  -- |Get the module from the module table of the state instance.
  bsoGetModule :: UpdatableBlockState m -> ModuleRef -> m (Maybe Module)
  bsoGetAccount :: UpdatableBlockState m -> AccountAddress -> m (Maybe Account)
  -- |Get the contract state from the contract table of the state instance.
  bsoGetInstance :: UpdatableBlockState m -> ContractAddress -> m (Maybe Instance)

  -- |Check whether an the given credential registration ID exists.
  -- Return @True@ iff so.
  bsoRegIdExists :: UpdatableBlockState m -> ID.CredentialRegistrationID -> m Bool

  -- |Try to add a new account to the state. If an account with the address already exists
  -- return @False@, and if the account was successfully added return @True@.
  bsoPutNewAccount :: UpdatableBlockState m -> Account -> m (Bool, UpdatableBlockState m)
  bsoPutNewInstance :: UpdatableBlockState m -> (ContractAddress -> Instance) -> m (ContractAddress, UpdatableBlockState m)
  -- |Add the module to the global state. If a module with the given address
  -- already exists return @False@.
  bsoPutNewModule :: UpdatableBlockState m -> ModuleRef -> Interface -> ValueInterface -> Core.Module -> m (Bool, UpdatableBlockState m)

  -- |Modify an existing account with given data (which includes the address of the account).
  -- This method is only called when an account exists and can thus assume this.
  -- NB: In case we are adding a credential to an account this method __must__ also
  -- update the global set of known credentials.
  bsoModifyAccount :: UpdatableBlockState m -> AccountUpdate -> m (UpdatableBlockState m)
  -- |Replace the instance with given data. The rest of the instance data (instance parameters) stays the same.
  -- This method is only called when it is known the instance exists, and can thus assume it.
  bsoModifyInstance :: UpdatableBlockState m -> ContractAddress -> Amount -> Value -> m (UpdatableBlockState m)

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
  bsoGetBirkParameters :: UpdatableBlockState m -> m BirkParameters

  bsoGetBakerInfo :: UpdatableBlockState m -> BakerId -> m (Maybe BakerInfo)
  bsoGetBakerInfo s bid = do
    BirkParameters{..} <- bsoGetBirkParameters s
    return $! Map.lookup bid birkBakers

  -- |Get the account of the given baker.
  bsoGetBakerAccount :: UpdatableBlockState m -> BakerId -> m (Maybe Account)
  bsoGetBakerAccount s bid = do
    BirkParameters{..} <- bsoGetBirkParameters s
    let maddr = bakerAccount <$> Map.lookup bid birkBakers
    case maddr of
      Nothing -> return Nothing
      Just addr -> bsoGetAccount s addr


  -- |Add a new baker to the baker pool. Assign a fresh baker identity to the 
  -- new baker and return the assigned identity.
  -- This method should also update the next available baker id in the system.
  bsoAddBaker :: UpdatableBlockState m -> BakerInfo -> m (BakerId, UpdatableBlockState m)
  
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

instance BlockStateQuery m => BlockStateQuery (MaybeT m) where

  getModule s = lift . getModule s
  getAccount s = lift . getAccount s
  getContractInstance s = lift . getContractInstance s

  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList

  getBirkParameters = lift . getBirkParameters

  getRewardStatus = lift . getRewardStatus


type instance UpdatableBlockState (MaybeT m) = UpdatableBlockState m

instance BlockStateOperations m => BlockStateOperations (MaybeT m) where
  -- |Get the module from the module table of the state instance.
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s

  bsoPutNewAccount s = bsoPutNewAccount s
  bsoPutNewInstance s = bsoPutNewInstance s
  bsoPutNewModule s mref iface viface source = lift (bsoPutNewModule s mref iface viface source)

  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoModifyInstance s caddr amount model = lift $ bsoModifyInstance s caddr amount model

  bsoNotifyExecutionCost s = lift . bsoNotifyExecutionCost s
  bsoNotifyIdentityIssuerCredential s = lift . bsoNotifyIdentityIssuerCredential s
  bsoGetExecutionCost = lift . bsoGetExecutionCost

  bsoGetBirkParameters = lift . bsoGetBirkParameters
  bsoAddBaker s = lift . bsoAddBaker s
  bsoUpdateBaker s = lift . bsoUpdateBaker s
  bsoRemoveBaker s = lift . bsoRemoveBaker s
  bsoSetInflation s = lift . bsoSetInflation s

  bsoMint s = lift . bsoMint s
  bsoDecrementCentralBankGTU s = lift . bsoDecrementCentralBankGTU s


type instance BlockPointer (MaybeT m) = BlockPointer m

instance (BlockStateQuery m, Monoid w) => BlockStateQuery (RWST r w s m) where
  getModule s = lift . getModule s
  getAccount s = lift . getAccount s
  getContractInstance s = lift . getContractInstance s

  getModuleList = lift . getModuleList
  getAccountList = lift . getAccountList
  getContractInstanceList = lift . getContractInstanceList

  getBirkParameters = lift . getBirkParameters

  getRewardStatus = lift . getRewardStatus


type instance UpdatableBlockState (RWST r w s m) = UpdatableBlockState m

instance (BlockStateOperations m, Monoid w) => BlockStateOperations (RWST r w s m) where
  -- |Get the module from the module table of the state instance.
  bsoGetModule s = lift . bsoGetModule s
  bsoGetAccount s = lift . bsoGetAccount s
  bsoGetInstance s = lift . bsoGetInstance s
  bsoRegIdExists s = lift . bsoRegIdExists s

  bsoPutNewAccount s = bsoPutNewAccount s
  bsoPutNewInstance s = bsoPutNewInstance s
  bsoPutNewModule s mref iface viface source = lift (bsoPutNewModule s mref iface viface source)

  bsoModifyAccount s = lift . bsoModifyAccount s
  bsoModifyInstance s caddr amount model = lift $ bsoModifyInstance s caddr amount model

  bsoNotifyExecutionCost s = lift . bsoNotifyExecutionCost s
  bsoNotifyIdentityIssuerCredential s = lift . bsoNotifyIdentityIssuerCredential s
  bsoGetExecutionCost = lift . bsoGetExecutionCost

  bsoGetBirkParameters = lift . bsoGetBirkParameters
  bsoAddBaker s = lift . bsoAddBaker s
  bsoUpdateBaker s = lift . bsoUpdateBaker s
  bsoRemoveBaker s = lift . bsoRemoveBaker s

  bsoSetInflation s = lift . bsoSetInflation s

  bsoMint s = lift . bsoMint s
  bsoDecrementCentralBankGTU s = lift . bsoDecrementCentralBankGTU s

type instance BlockPointer (RWST r w s m) = BlockPointer m
