{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Basic.BlockState.Account (
    module Concordium.GlobalState.Account,
    module Concordium.GlobalState.Basic.BlockState.Account,
) where

import Data.Coerce
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Lens.Micro.Platform

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.CooldownQueue
import Concordium.ID.Parameters
import Concordium.ID.Types
import Concordium.Types.HashableTo

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Conditionally

-- | Type for how a 'PersistingAccountData' value is stored as part of
--  an account. This is stored with its hash.
type AccountPersisting = Hashed' PersistingAccountDataHash PersistingAccountData

-- | Make an 'AccountPersisting' value from a 'PersistingAccountData' value.
makeAccountPersisting :: PersistingAccountData -> AccountPersisting
makeAccountPersisting = makeHashed
{-# INLINE makeAccountPersisting #-}

-- | An (in-memory) account.
data Account (av :: AccountVersion) = Account
    { -- | Account data that seldom changes. Stored separately for efficient
      --  memory use and hashing.
      _accountPersisting :: !AccountPersisting,
      -- | The next sequence number of a transaction on this account.
      _accountNonce :: !Nonce,
      -- | This account amount contains all the amount owned by the account,
      --  excluding encrypted amounts. In particular, the available amount is
      --  @accountAmount - totalLockedUpBalance accountReleaseSchedule@.
      _accountAmount :: !Amount,
      -- | The encrypted amount
      _accountEncryptedAmount :: !AccountEncryptedAmount,
      -- | Locked-up amounts and their release schedule.
      _accountReleaseSchedule :: !(AccountReleaseSchedule av),
      -- | The baker or delegation associated with the account (if any).
      _accountStaking :: !(AccountStake av),
      -- | The cooldown on the account.
      _accountStakeCooldown :: !(Conditionally (SupportsFlexibleCooldown av) Cooldowns),
      -- | The token state table of the account.
      _accountTokenStateTable :: !(Conditionally (SupportsPLT av) (Hashed' TokenStateTableHash InMemoryTokenStateTable))
    }
    deriving (Eq, Show)

makeLenses ''Account

-- | A traversal for accessing the 'AccountBaker' record of an account if it has one.
--  This can be used for getting the baker (e.g. with '(^?)') or updating it (if it already exists)
--  but not for setting it unless the account already has a baker.
accountBaker :: Traversal' (Account av) (AccountBaker av)
accountBaker f = g
  where
    g acct@Account{_accountStaking = AccountStakeBaker bkr} =
        (\bkr' -> acct{_accountStaking = AccountStakeBaker bkr'}) <$> f bkr
    g acct = pure acct

-- | Get the baker from an account, on the basis that it is known to be a baker
unsafeAccountBaker :: (HasCallStack) => SimpleGetter (Account av) (AccountBaker av)
unsafeAccountBaker = singular accountBaker

-- | A traversal for accessing the 'AccountDelegation' record of an account if it has one.
--  This can be used for getting the delegation (e.g. with '(^?)') or updating it (if it already
--  exists) but not for setting it unless the account already has a baker.
accountDelegator :: Traversal' (Account av) (AccountDelegation av)
accountDelegator f = g
  where
    g acct@Account{_accountStaking = AccountStakeDelegate del} =
        (\del' -> acct{_accountStaking = AccountStakeDelegate del'}) <$> f del
    g acct = pure acct

-- | Get the delegator on an account, on the basis that it is known to be a delegator.
unsafeAccountDelegator :: (HasCallStack) => SimpleGetter (Account av) (AccountDelegation av)
unsafeAccountDelegator = singular accountDelegator

instance HasPersistingAccountData (Account av) where
    persistingAccountData = accountPersisting . unhashed

-- | Generate hash inputs from an account for 'AccountV0' and 'AccountV1'.
accountHashInputsV0 :: (IsAccountVersion av, AccountStructureVersionFor av ~ 'AccountStructureV0) => Account av -> AccountHashInputsV0 av
accountHashInputsV0 Account{..} =
    AccountHashInputsV0
        { ahiNextNonce = _accountNonce,
          ahiAccountAmount = _accountAmount,
          ahiAccountEncryptedAmount = _accountEncryptedAmount,
          ahiAccountReleaseScheduleHash = getHash _accountReleaseSchedule,
          ahiPersistingAccountDataHash = getHash _accountPersisting,
          ahiAccountStakeHash = getAccountStakeHash _accountStaking
        }

-- | Generate hash inputs from an account for 'AccountV2'.
accountHashInputsV2 :: Account 'AccountV2 -> AccountHashInputsV2 'AccountV2
accountHashInputsV2 Account{..} =
    AccountHashInputsV2
        { ahi2NextNonce = _accountNonce,
          ahi2AccountBalance = _accountAmount,
          ahi2StakedBalance = stakedBalance,
          ahi2MerkleHash = getHash merkleInputs
        }
  where
    stakedBalance = case _accountStaking of
        AccountStakeNone -> 0
        AccountStakeBaker AccountBaker{..} -> _stakedAmount
        AccountStakeDelegate AccountDelegationV1{..} -> _delegationStakedAmount
    merkleInputs :: AccountMerkleHashInputs 'AccountV2
    merkleInputs =
        AccountMerkleHashInputsV2
            { amhi2PersistingAccountDataHash = getHash _accountPersisting,
              amhi2AccountStakeHash = getHash _accountStaking,
              amhi2EncryptedAmountHash = getHash _accountEncryptedAmount,
              amhi2AccountReleaseScheduleHash = getHash _accountReleaseSchedule
            }

-- | Generate hash inputs from an account for 'AccountV3'.
accountHashInputsV3 :: Account 'AccountV3 -> AccountHashInputsV2 'AccountV3
accountHashInputsV3 Account{..} =
    AccountHashInputsV2
        { ahi2NextNonce = _accountNonce,
          ahi2AccountBalance = _accountAmount,
          ahi2StakedBalance = stakedBalance,
          ahi2MerkleHash = getHash merkleInputs
        }
  where
    stakedBalance = case _accountStaking of
        AccountStakeNone -> 0
        AccountStakeBaker AccountBaker{..} -> _stakedAmount
        AccountStakeDelegate AccountDelegationV1{..} -> _delegationStakedAmount
    merkleInputs :: AccountMerkleHashInputs 'AccountV3
    merkleInputs =
        AccountMerkleHashInputsV3
            { amhi3PersistingAccountDataHash = getHash _accountPersisting,
              amhi3AccountStakeHash = getHash _accountStaking,
              amhi3EncryptedAmountHash = getHash _accountEncryptedAmount,
              amhi3AccountReleaseScheduleHash = getHash _accountReleaseSchedule,
              amhi3Cooldown = CooldownQueueHash $ getHash $ uncond _accountStakeCooldown
            }

-- | Generate hash inputs from an account for 'AccountV4'.
accountHashInputsV4 :: Account 'AccountV4 -> AccountHashInputsV2 'AccountV4
accountHashInputsV4 Account{..} =
    AccountHashInputsV2
        { ahi2NextNonce = _accountNonce,
          ahi2AccountBalance = _accountAmount,
          ahi2StakedBalance = stakedBalance,
          ahi2MerkleHash = getHash merkleInputs
        }
  where
    stakedBalance = case _accountStaking of
        AccountStakeNone -> 0
        AccountStakeBaker AccountBaker{..} -> _stakedAmount
        AccountStakeDelegate AccountDelegationV1{..} -> _delegationStakedAmount
    merkleInputs :: AccountMerkleHashInputs 'AccountV4
    merkleInputs =
        AccountMerkleHashInputsV4
            { amhi4PersistingAccountDataHash = getHash _accountPersisting,
              amhi4AccountStakeHash = getHash _accountStaking,
              amhi4EncryptedAmountHash = getHash _accountEncryptedAmount,
              amhi4AccountReleaseScheduleHash = getHash _accountReleaseSchedule,
              amhi4Cooldown = CooldownQueueHash $ getHash $ uncond _accountStakeCooldown
            }

-- | Generate hash inputs from an account for 'AccountV5'.
accountHashInputsV5 :: Account 'AccountV5 -> AccountHashInputsV3 'AccountV5
accountHashInputsV5 Account{..} =
    AccountHashInputsV3
        { ahi3NextNonce = _accountNonce,
          ahi3AccountBalance = _accountAmount,
          ahi3StakedBalance = stakedBalance,
          ahi3MerkleHash = getHash merkleInputs,
          ahi3TokenStateTableHash = getHash $ uncond $ _accountTokenStateTable
        }
  where
    stakedBalance = case _accountStaking of
        AccountStakeNone -> 0
        AccountStakeBaker AccountBaker{..} -> _stakedAmount
        AccountStakeDelegate AccountDelegationV1{..} -> _delegationStakedAmount
    merkleInputs :: AccountMerkleHashInputs 'AccountV5
    merkleInputs =
        AccountMerkleHashInputsV5
            { amhi5PersistingAccountDataHash = getHash _accountPersisting,
              amhi5AccountStakeHash = getHash _accountStaking,
              amhi5EncryptedAmountHash = getHash _accountEncryptedAmount,
              amhi5AccountReleaseScheduleHash = getHash _accountReleaseSchedule,
              amhi5Cooldown = CooldownQueueHash $ getHash $ uncond _accountStakeCooldown
            }

instance (IsAccountVersion av) => HashableTo (AccountHash av) (Account av) where
    getHash acc = makeAccountHash $ case accountVersion @av of
        SAccountV0 -> AHIV0 (accountHashInputsV0 acc)
        SAccountV1 -> AHIV1 (accountHashInputsV0 acc)
        SAccountV2 -> AHIV2 (accountHashInputsV2 acc)
        SAccountV3 -> AHIV3 (accountHashInputsV3 acc)
        SAccountV4 -> AHIV4 (accountHashInputsV4 acc)
        SAccountV5 -> AHIV5 (accountHashInputsV5 acc)

instance forall av. (IsAccountVersion av) => HashableTo Hash.Hash (Account av) where
    getHash = coerce @(AccountHash av) . getHash

-- | An empty cooldown queue for a given account version.
emptyCooldownQueue :: SAccountVersion av -> Conditionally (SupportsFlexibleCooldown av) Cooldowns
emptyCooldownQueue sav = conditionally (sSupportsFlexibleCooldown sav) emptyCooldowns

-- | Create an empty account with the given public key, address and credentials.
newAccountMultiCredential ::
    forall av.
    (IsAccountVersion av) =>
    -- | Cryptographic parameters, needed to derive the encryption key from the credentials.
    GlobalContext ->
    -- | The account threshold, how many credentials need to sign..
    AccountThreshold ->
    -- | Address of the account to be created.
    AccountAddress ->
    -- | Initial credentials on the account. NB: It is assumed that this map has a value at index 'initialCredentialIndex' (0).
    Map.Map CredentialIndex AccountCredential ->
    Account av
newAccountMultiCredential cryptoParams threshold _accountAddress cs =
    Account
        { _accountPersisting =
            makeAccountPersisting
                PersistingAccountData
                    { _accountEncryptionKey = toRawEncryptionKey (makeEncryptionKey cryptoParams (credId (cs Map.! initialCredentialIndex))),
                      _accountCredentials = toRawAccountCredential <$> cs,
                      _accountVerificationKeys = getAccountInformation threshold cs,
                      _accountRemovedCredentials = emptyHashedRemovedCredentials,
                      ..
                    },
          _accountNonce = minNonce,
          _accountAmount = 0,
          _accountEncryptedAmount = initialAccountEncryptedAmount,
          _accountReleaseSchedule = emptyAccountReleaseSchedule,
          _accountStaking = AccountStakeNone,
          _accountStakeCooldown = emptyCooldownQueue (accountVersion @av),
          _accountTokenStateTable =
            conditionally
                (sSupportsPLT (accountVersion @av))
                (makeHashed (InMemoryTokenStateTable{inMemoryTokenStateTable = Map.empty}))
        }

-- | Create an empty account with the given public key, address and credential.
newAccount :: (IsAccountVersion av) => GlobalContext -> AccountAddress -> AccountCredential -> Account av
newAccount cryptoParams _accountAddress credential =
    newAccountMultiCredential cryptoParams 1 _accountAddress (Map.singleton initialCredentialIndex credential)
