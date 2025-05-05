{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- Some interfaces in this module have deliberate redundant constraints (e.g. to constrain
-- protocol versions). As such, we suppress the redundant constraints warning.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides an interface for operating on peristent accounts.
module Concordium.GlobalState.Persistent.Account where

import Control.Arrow
import Control.Monad
import qualified Data.Map.Strict as Map

import Concordium.ID.Parameters
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Accounts.Releases
import Concordium.Types.Execution
import Concordium.Types.HashableTo
import Concordium.Types.Parameters

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Genesis.Data
import Concordium.GlobalState.Account
import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import Concordium.GlobalState.BlockState (AccountAllowance)
import Concordium.GlobalState.CooldownQueue
import Concordium.GlobalState.Persistent.Account.CooldownQueue (NextCooldownChange)
import Concordium.GlobalState.Persistent.Account.MigrationStateInterface
import qualified Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens as BlockState
import qualified Concordium.GlobalState.Persistent.Account.StructureV0 as V0
import qualified Concordium.GlobalState.Persistent.Account.StructureV1 as V1
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens as BlockState
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.CachedRef
import Concordium.Logger
import Concordium.Types.Conditionally (uncond)

-- * Account types

-- | A persistent account at a particular 'AccountVersion'.
data PersistentAccount (av :: AccountVersion) where
    PAV0 :: !(V0.PersistentAccount 'AccountV0) -> PersistentAccount 'AccountV0
    PAV1 :: !(V0.PersistentAccount 'AccountV1) -> PersistentAccount 'AccountV1
    PAV2 :: !(V1.PersistentAccount 'AccountV2) -> PersistentAccount 'AccountV2
    PAV3 :: !(V1.PersistentAccount 'AccountV3) -> PersistentAccount 'AccountV3
    PAV4 :: !(V1.PersistentAccount 'AccountV4) -> PersistentAccount 'AccountV4
    PAV5 :: !(V1.PersistentAccount 'AccountV5) -> PersistentAccount 'AccountV5

instance (MonadBlobStore m) => MHashableTo m (AccountHash av) (PersistentAccount av) where
    getHashM (PAV0 acc) = getHashM acc
    getHashM (PAV1 acc) = getHashM acc
    getHashM (PAV2 acc) = getHashM acc
    getHashM (PAV3 acc) = getHashM acc
    getHashM (PAV4 acc) = getHashM acc
    getHashM (PAV5 acc) = getHashM acc

instance (MonadBlobStore m) => MHashableTo m Hash.Hash (PersistentAccount av) where
    getHashM (PAV0 acc) = getHashM acc
    getHashM (PAV1 acc) = getHashM acc
    getHashM (PAV2 acc) = getHashM acc
    getHashM (PAV3 acc) = getHashM acc
    getHashM (PAV4 acc) = getHashM acc
    getHashM (PAV5 acc) = getHashM acc

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentAccount av) where
    storeUpdate (PAV0 acct) = second PAV0 <$!> storeUpdate acct
    storeUpdate (PAV1 acct) = second PAV1 <$!> storeUpdate acct
    storeUpdate (PAV2 acct) = second PAV2 <$!> storeUpdate acct
    storeUpdate (PAV3 acct) = second PAV3 <$!> storeUpdate acct
    storeUpdate (PAV4 acct) = second PAV4 <$!> storeUpdate acct
    storeUpdate (PAV5 acct) = second PAV5 <$!> storeUpdate acct
    load = case accountVersion @av of
        SAccountV0 -> fmap PAV0 <$> load
        SAccountV1 -> fmap PAV1 <$> load
        SAccountV2 -> fmap PAV2 <$> load
        SAccountV3 -> fmap PAV3 <$> load
        SAccountV4 -> fmap PAV4 <$> load
        SAccountV5 -> fmap PAV5 <$> load

-- | Type of references to persistent accounts.
type AccountRef (av :: AccountVersion) = HashedCachedRef (AccountCache av) (PersistentAccount av)

-- | A reference to persistent baker info.
data PersistentBakerInfoRef (av :: AccountVersion) where
    PBIRV0 :: !(V0.PersistentBakerInfoEx 'AccountV0) -> PersistentBakerInfoRef 'AccountV0
    PBIRV1 :: !(V0.PersistentBakerInfoEx 'AccountV1) -> PersistentBakerInfoRef 'AccountV1
    PBIRV2 :: !(V1.PersistentBakerInfoEx 'AccountV2) -> PersistentBakerInfoRef 'AccountV2
    PBIRV3 :: !(V1.PersistentBakerInfoEx 'AccountV3) -> PersistentBakerInfoRef 'AccountV3
    PBIRV4 :: !(V1.PersistentBakerInfoEx 'AccountV4) -> PersistentBakerInfoRef 'AccountV4
    PBIRV5 :: !(V1.PersistentBakerInfoEx 'AccountV5) -> PersistentBakerInfoRef 'AccountV5

instance Show (PersistentBakerInfoRef av) where
    show (PBIRV0 pibr) = show pibr
    show (PBIRV1 pibr) = show pibr
    show (PBIRV2 pibr) = show pibr
    show (PBIRV3 pibr) = show pibr
    show (PBIRV4 pibr) = show pibr
    show (PBIRV5 pibr) = show pibr

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentBakerInfoRef av) where
    storeUpdate (PBIRV0 bir) = second PBIRV0 <$!> storeUpdate bir
    storeUpdate (PBIRV1 bir) = second PBIRV1 <$!> storeUpdate bir
    storeUpdate (PBIRV2 bir) = second PBIRV2 <$!> storeUpdate bir
    storeUpdate (PBIRV3 bir) = second PBIRV3 <$!> storeUpdate bir
    storeUpdate (PBIRV4 bir) = second PBIRV4 <$!> storeUpdate bir
    storeUpdate (PBIRV5 bir) = second PBIRV5 <$!> storeUpdate bir
    load = case accountVersion @av of
        SAccountV0 -> fmap PBIRV0 <$!> load
        SAccountV1 -> fmap PBIRV1 <$!> load
        SAccountV2 -> fmap PBIRV2 <$!> load
        SAccountV3 -> fmap PBIRV3 <$!> load
        SAccountV4 -> fmap PBIRV4 <$!> load
        SAccountV5 -> fmap PBIRV5 <$!> load

-- * Account cache

-- | Type alias for the cache to use for accounts.
type AccountCache (av :: AccountVersion) = FIFOCache (PersistentAccount av)

-- | Construct a new 'AccountCache' with the given size.
newAccountCache :: Int -> IO (AccountCache av)
newAccountCache = newCache

-- * Queries

-- | Get the canonical address of the account.
accountCanonicalAddress :: (MonadBlobStore m) => PersistentAccount av -> m AccountAddress
accountCanonicalAddress (PAV0 acc) = V0.getCanonicalAddress acc
accountCanonicalAddress (PAV1 acc) = V0.getCanonicalAddress acc
accountCanonicalAddress (PAV2 acc) = V1.getCanonicalAddress acc
accountCanonicalAddress (PAV3 acc) = V1.getCanonicalAddress acc
accountCanonicalAddress (PAV4 acc) = V1.getCanonicalAddress acc
accountCanonicalAddress (PAV5 acc) = V1.getCanonicalAddress acc

-- | Get the current public account balance.
accountAmount :: (MonadBlobStore m) => PersistentAccount av -> m Amount
accountAmount (PAV0 acc) = V0.getAmount acc
accountAmount (PAV1 acc) = V0.getAmount acc
accountAmount (PAV2 acc) = V1.getAmount acc
accountAmount (PAV3 acc) = V1.getAmount acc
accountAmount (PAV4 acc) = V1.getAmount acc
accountAmount (PAV5 acc) = V1.getAmount acc

-- | Gets the amount of a baker's stake, or 'Nothing' if the account is not a baker.
--  This consists only of the active stake, and does not include any inactive stake.
accountBakerStakeAmount :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe Amount)
accountBakerStakeAmount (PAV0 acc) = V0.getBakerStakeAmount acc
accountBakerStakeAmount (PAV1 acc) = V0.getBakerStakeAmount acc
accountBakerStakeAmount (PAV2 acc) = V1.getBakerStakeAmount acc
accountBakerStakeAmount (PAV3 acc) = V1.getBakerStakeAmount acc
accountBakerStakeAmount (PAV4 acc) = V1.getBakerStakeAmount acc
accountBakerStakeAmount (PAV5 acc) = V1.getBakerStakeAmount acc

-- | Get the amount that is actively staked on an account as a baker or delegator.
accountActiveStakedAmount :: (MonadBlobStore m) => PersistentAccount av -> m Amount
accountActiveStakedAmount (PAV0 acc) = V0.getStakedAmount acc
accountActiveStakedAmount (PAV1 acc) = V0.getStakedAmount acc
accountActiveStakedAmount (PAV2 acc) = V1.getActiveStakedAmount acc
accountActiveStakedAmount (PAV3 acc) = V1.getActiveStakedAmount acc
accountActiveStakedAmount (PAV4 acc) = V1.getActiveStakedAmount acc
accountActiveStakedAmount (PAV5 acc) = V1.getActiveStakedAmount acc

-- | Get the amount that is staked on the account (both active and inactive).
accountTotalStakedAmount :: (MonadBlobStore m) => PersistentAccount av -> m Amount
accountTotalStakedAmount (PAV0 acc) = V0.getStakedAmount acc
accountTotalStakedAmount (PAV1 acc) = V0.getStakedAmount acc
accountTotalStakedAmount (PAV2 acc) =
    -- This is the same as the total staked amount in account version 2.
    V1.getActiveStakedAmount acc
accountTotalStakedAmount (PAV3 acc) = V1.getTotalStakedAmount acc
accountTotalStakedAmount (PAV4 acc) = V1.getTotalStakedAmount acc
accountTotalStakedAmount (PAV5 acc) = V1.getTotalStakedAmount acc

-- | Get the amount that is locked in scheduled releases on the account.
accountLockedAmount :: (MonadBlobStore m) => PersistentAccount av -> m Amount
accountLockedAmount (PAV0 acc) = V0.getLockedAmount acc
accountLockedAmount (PAV1 acc) = V0.getLockedAmount acc
accountLockedAmount (PAV2 acc) = V1.getLockedAmount acc
accountLockedAmount (PAV3 acc) = V1.getLockedAmount acc
accountLockedAmount (PAV4 acc) = V1.getLockedAmount acc
accountLockedAmount (PAV5 acc) = V1.getLockedAmount acc

-- | Get the current public account available balance.
-- This accounts for lock-up and staked amounts.
-- @available = total - max locked staked@
accountAvailableAmount :: (MonadBlobStore m) => PersistentAccount av -> m Amount
accountAvailableAmount (PAV0 acc) = V0.getAvailableAmount acc
accountAvailableAmount (PAV1 acc) = V0.getAvailableAmount acc
accountAvailableAmount (PAV2 acc) = V1.getAvailableAmount acc
accountAvailableAmount (PAV3 acc) = V1.getAvailableAmount acc
accountAvailableAmount (PAV4 acc) = V1.getAvailableAmount acc
accountAvailableAmount (PAV5 acc) = V1.getAvailableAmount acc

-- | Get the next account nonce for transactions from this account.
accountNonce :: (MonadBlobStore m) => PersistentAccount av -> m Nonce
accountNonce (PAV0 acc) = V0.getNonce acc
accountNonce (PAV1 acc) = V0.getNonce acc
accountNonce (PAV2 acc) = V1.getNonce acc
accountNonce (PAV3 acc) = V1.getNonce acc
accountNonce (PAV4 acc) = V1.getNonce acc
accountNonce (PAV5 acc) = V1.getNonce acc

-- | Determine if a given operation is permitted for the account.
--
--  * For 'AllowedEncryptedTransfers' the account may only have 1 credential.
--
--  * For 'AllowedMultipleCredentials' the account must have the empty encrypted balance.
accountIsAllowed :: (MonadBlobStore m) => PersistentAccount av -> AccountAllowance -> m Bool
accountIsAllowed (PAV0 acc) = V0.isAllowed acc
accountIsAllowed (PAV1 acc) = V0.isAllowed acc
accountIsAllowed (PAV2 acc) = V1.isAllowed acc
accountIsAllowed (PAV3 acc) = V1.isAllowed acc
accountIsAllowed (PAV4 acc) = V1.isAllowed acc
accountIsAllowed (PAV5 acc) = V1.isAllowed acc

-- | Get the credentials deployed on the account. This map is always non-empty and (presently)
--  will have a credential at index 'initialCredentialIndex' (0) that cannot be changed.
accountCredentials :: (MonadBlobStore m) => PersistentAccount av -> m (Map.Map CredentialIndex RawAccountCredential)
accountCredentials (PAV0 acc) = V0.getCredentials acc
accountCredentials (PAV1 acc) = V0.getCredentials acc
accountCredentials (PAV2 acc) = V1.getCredentials acc
accountCredentials (PAV3 acc) = V1.getCredentials acc
accountCredentials (PAV4 acc) = V1.getCredentials acc
accountCredentials (PAV5 acc) = V1.getCredentials acc

-- | Get the key used to verify transaction signatures, it records the signature scheme used as well.
accountVerificationKeys :: (MonadBlobStore m) => PersistentAccount av -> m AccountInformation
accountVerificationKeys (PAV0 acc) = V0.getVerificationKeys acc
accountVerificationKeys (PAV1 acc) = V0.getVerificationKeys acc
accountVerificationKeys (PAV2 acc) = V1.getVerificationKeys acc
accountVerificationKeys (PAV3 acc) = V1.getVerificationKeys acc
accountVerificationKeys (PAV4 acc) = V1.getVerificationKeys acc
accountVerificationKeys (PAV5 acc) = V1.getVerificationKeys acc

-- | Get the current encrypted amount on the account.
accountEncryptedAmount :: (MonadBlobStore m) => PersistentAccount av -> m AccountEncryptedAmount
accountEncryptedAmount (PAV0 acc) = V0.getEncryptedAmount acc
accountEncryptedAmount (PAV1 acc) = V0.getEncryptedAmount acc
accountEncryptedAmount (PAV2 acc) = V1.getEncryptedAmount acc
accountEncryptedAmount (PAV3 acc) = V1.getEncryptedAmount acc
accountEncryptedAmount (PAV4 acc) = V1.getEncryptedAmount acc
accountEncryptedAmount (PAV5 acc) = V1.getEncryptedAmount acc

-- | Get the public key used to receive encrypted amounts.
accountEncryptionKey :: (MonadBlobStore m) => PersistentAccount av -> m AccountEncryptionKey
accountEncryptionKey (PAV0 acc) = V0.getEncryptionKey acc
accountEncryptionKey (PAV1 acc) = V0.getEncryptionKey acc
accountEncryptionKey (PAV2 acc) = V1.getEncryptionKey acc
accountEncryptionKey (PAV3 acc) = V1.getEncryptionKey acc
accountEncryptionKey (PAV4 acc) = V1.getEncryptionKey acc
accountEncryptionKey (PAV5 acc) = V1.getEncryptionKey acc

-- | Get the 'AccountReleaseSummary' summarising scheduled releases for an account.
accountReleaseSummary :: (MonadBlobStore m) => PersistentAccount av -> m AccountReleaseSummary
accountReleaseSummary (PAV0 acc) = V0.getReleaseSummary acc
accountReleaseSummary (PAV1 acc) = V0.getReleaseSummary acc
accountReleaseSummary (PAV2 acc) = V1.getReleaseSummary acc
accountReleaseSummary (PAV3 acc) = V1.getReleaseSummary acc
accountReleaseSummary (PAV4 acc) = V1.getReleaseSummary acc
accountReleaseSummary (PAV5 acc) = V1.getReleaseSummary acc

-- | Get the timestamp at which the next scheduled release will occur (if any).
accountNextReleaseTimestamp :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe Timestamp)
accountNextReleaseTimestamp (PAV0 acc) = V0.getNextReleaseTimestamp acc
accountNextReleaseTimestamp (PAV1 acc) = V0.getNextReleaseTimestamp acc
accountNextReleaseTimestamp (PAV2 acc) = V1.getNextReleaseTimestamp acc
accountNextReleaseTimestamp (PAV3 acc) = V1.getNextReleaseTimestamp acc
accountNextReleaseTimestamp (PAV4 acc) = V1.getNextReleaseTimestamp acc
accountNextReleaseTimestamp (PAV5 acc) = V1.getNextReleaseTimestamp acc

-- | Get the baker (if any) attached to an account.
accountBaker :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe (AccountBaker av))
accountBaker (PAV0 acc) = V0.getBaker acc
accountBaker (PAV1 acc) = V0.getBaker acc
accountBaker (PAV2 acc) = V1.getBaker acc
accountBaker (PAV3 acc) = V1.getBaker acc
accountBaker (PAV4 acc) = V1.getBaker acc
accountBaker (PAV5 acc) = V1.getBaker acc

-- | Get a reference to the baker info (if any) attached to an account.
accountBakerInfoRef :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe (PersistentBakerInfoRef av))
accountBakerInfoRef (PAV0 acc) = fmap PBIRV0 <$> V0.getBakerInfoRef acc
accountBakerInfoRef (PAV1 acc) = fmap PBIRV1 <$> V0.getBakerInfoRef acc
accountBakerInfoRef (PAV2 acc) = fmap PBIRV2 <$> V1.getBakerInfoRef acc
accountBakerInfoRef (PAV3 acc) = fmap PBIRV3 <$> V1.getBakerInfoRef acc
accountBakerInfoRef (PAV4 acc) = fmap PBIRV4 <$> V1.getBakerInfoRef acc
accountBakerInfoRef (PAV5 acc) = fmap PBIRV5 <$> V1.getBakerInfoRef acc

-- | Get the baker and baker info reference (if any) attached to the account.
accountBakerAndInfoRef :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe (AccountBaker av, PersistentBakerInfoRef av))
accountBakerAndInfoRef (PAV0 acc) = fmap (second PBIRV0) <$> V0.getBakerAndInfoRef acc
accountBakerAndInfoRef (PAV1 acc) = fmap (second PBIRV1) <$> V0.getBakerAndInfoRef acc
accountBakerAndInfoRef (PAV2 acc) = fmap (second PBIRV2) <$> V1.getBakerAndInfoRef acc
accountBakerAndInfoRef (PAV3 acc) = fmap (second PBIRV3) <$> V1.getBakerAndInfoRef acc
accountBakerAndInfoRef (PAV4 acc) = fmap (second PBIRV4) <$> V1.getBakerAndInfoRef acc
accountBakerAndInfoRef (PAV5 acc) = fmap (second PBIRV5) <$> V1.getBakerAndInfoRef acc

-- | Get the delegator (if any) attached to the account.
accountDelegator :: (MonadBlobStore m) => PersistentAccount av -> m (Maybe (AccountDelegation av))
accountDelegator (PAV0 acc) = V0.getDelegator acc
accountDelegator (PAV1 acc) = V0.getDelegator acc
accountDelegator (PAV2 acc) = V1.getDelegator acc
accountDelegator (PAV3 acc) = V1.getDelegator acc
accountDelegator (PAV4 acc) = V1.getDelegator acc
accountDelegator (PAV5 acc) = V1.getDelegator acc

-- | Get the baker or stake delegation information attached to an account.
accountStake :: (MonadBlobStore m) => PersistentAccount av -> m (AccountStake av)
accountStake (PAV0 acc) = V0.getStake acc
accountStake (PAV1 acc) = V0.getStake acc
accountStake (PAV2 acc) = V1.getStake acc
accountStake (PAV3 acc) = V1.getStake acc
accountStake (PAV4 acc) = V1.getStake acc
accountStake (PAV5 acc) = V1.getStake acc

-- | Determine if an account has stake as a baker or delegator.
accountHasActiveStake :: PersistentAccount av -> Bool
accountHasActiveStake (PAV0 acc) = V0.hasActiveStake acc
accountHasActiveStake (PAV1 acc) = V0.hasActiveStake acc
accountHasActiveStake (PAV2 acc) = V1.hasActiveStake acc
accountHasActiveStake (PAV3 acc) = V1.hasActiveStake acc
accountHasActiveStake (PAV4 acc) = V1.hasActiveStake acc
accountHasActiveStake (PAV5 acc) = V1.hasActiveStake acc

-- | Get details about an account's stake.
accountStakeDetails :: (MonadBlobStore m) => PersistentAccount av -> m (StakeDetails av)
accountStakeDetails (PAV0 acc) = V0.getStakeDetails acc
accountStakeDetails (PAV1 acc) = V0.getStakeDetails acc
accountStakeDetails (PAV2 acc) = V1.getStakeDetails acc
accountStakeDetails (PAV3 acc) = V1.getStakeDetails acc
accountStakeDetails (PAV4 acc) = V1.getStakeDetails acc
accountStakeDetails (PAV5 acc) = V1.getStakeDetails acc

-- | Get the 'Cooldowns' for an account, if any. This is only available at account versions that
-- support flexible cooldowns.
accountCooldowns ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    PersistentAccount av ->
    m (Maybe Cooldowns)
accountCooldowns (PAV3 acc) = V1.getCooldowns acc
accountCooldowns (PAV4 acc) = V1.getCooldowns acc
accountCooldowns (PAV5 acc) = V1.getCooldowns acc

-- | Determine if an account has a pre-pre-cooldown.
accountHasPrePreCooldown ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    PersistentAccount av ->
    m Bool
accountHasPrePreCooldown = fmap check . accountCooldowns
  where
    check = maybe False (not . null . prePreCooldown)

-- | Get the 'AccountHash' for the account.
accountHash :: (MonadBlobStore m) => PersistentAccount av -> m (AccountHash av)
accountHash (PAV0 acc) = getHashM acc
accountHash (PAV1 acc) = getHashM acc
accountHash (PAV2 acc) = getHashM acc
accountHash (PAV3 acc) = getHashM acc
accountHash (PAV4 acc) = getHashM acc
accountHash (PAV5 acc) = getHashM acc

-- ** Protocol-level token queries

-- | Get the state of protocol level tokens owned by an account. This is only available at account
-- versions that support protocol level tokens.
accountTokens ::
    (MonadBlobStore m, AVSupportsPLT av) =>
    PersistentAccount av ->
    m (Map.Map BlockState.TokenIndex BlockState.TokenAccountState)
accountTokens (PAV5 acc) = do
    tast <- V1.getTokenStateTable acc
    case uncond tast of
        Null -> return Map.empty
        Some tst -> return tst

-- | Get the balance of a protocol-level token held by an account.
--  This is only available at account versions that support protocol-level tokens.
accountTokenBalance ::
    (MonadBlobStore m, AVSupportsPLT av) =>
    PersistentAccount av ->
    BlockState.TokenIndex ->
    m BlockState.TokenRawAmount
accountTokenBalance (PAV5 acc) = V1.getTokenBalance acc

-- | Look up the 'TokenStateValue' associated with a particular token and key on an account.
--  This is only available at account versions that support protocol-level tokens.
accountTokenState ::
    (MonadBlobStore m, AVSupportsPLT av) =>
    PersistentAccount av ->
    BlockState.TokenIndex ->
    BlockState.TokenStateKey ->
    m (Maybe BlockState.TokenStateValue)
accountTokenState (PAV5 acc) = V1.getTokenState acc

-- ** 'PersistentBakerInfoRef' queries

-- | Load 'BakerInfo' from a 'PersistentBakerInfoRef'.
loadBakerInfo :: (MonadBlobStore m) => PersistentBakerInfoRef av -> m BakerInfo
loadBakerInfo (PBIRV0 bir) = V0.loadBakerInfo bir
loadBakerInfo (PBIRV1 bir) = V0.loadBakerInfo bir
loadBakerInfo (PBIRV2 bir) = V1.loadBakerInfo bir
loadBakerInfo (PBIRV3 bir) = V1.loadBakerInfo bir
loadBakerInfo (PBIRV4 bir) = V1.loadBakerInfo bir
loadBakerInfo (PBIRV5 bir) = V1.loadBakerInfo bir

-- | Load 'BakerInfoEx' from a 'PersistentBakerInfoRef'.
loadPersistentBakerInfoRef :: (MonadBlobStore m) => PersistentBakerInfoRef av -> m (BakerInfoEx av)
loadPersistentBakerInfoRef (PBIRV0 bir) = V0.loadPersistentBakerInfoEx bir
loadPersistentBakerInfoRef (PBIRV1 bir) = V0.loadPersistentBakerInfoEx bir
loadPersistentBakerInfoRef (PBIRV2 bir) = V1.loadPersistentBakerInfoEx bir
loadPersistentBakerInfoRef (PBIRV3 bir) = V1.loadPersistentBakerInfoEx bir
loadPersistentBakerInfoRef (PBIRV4 bir) = V1.loadPersistentBakerInfoEx bir
loadPersistentBakerInfoRef (PBIRV5 bir) = V1.loadPersistentBakerInfoEx bir

-- | Load the 'BakerId' from a 'PersistentBakerInfoRef'.
loadBakerId :: (MonadBlobStore m) => PersistentBakerInfoRef av -> m BakerId
loadBakerId (PBIRV0 bir) = V0.loadBakerId bir
loadBakerId (PBIRV1 bir) = V0.loadBakerId bir
loadBakerId (PBIRV2 bir) = V1.loadBakerId bir
loadBakerId (PBIRV3 bir) = V1.loadBakerId bir
loadBakerId (PBIRV4 bir) = V1.loadBakerId bir
loadBakerId (PBIRV5 bir) = V1.loadBakerId bir

-- * Updates

-- | Apply an account update to an account.
updateAccount :: (MonadBlobStore m) => AccountUpdate -> PersistentAccount av -> m (PersistentAccount av)
updateAccount upd (PAV0 acc) = PAV0 <$> V0.updateAccount upd acc
updateAccount upd (PAV1 acc) = PAV1 <$> V0.updateAccount upd acc
updateAccount upd (PAV2 acc) = PAV2 <$> V1.updateAccount upd acc
updateAccount upd (PAV3 acc) = PAV3 <$> V1.updateAccount upd acc
updateAccount upd (PAV4 acc) = PAV4 <$> V1.updateAccount upd acc
updateAccount upd (PAV5 acc) = PAV5 <$> V1.updateAccount upd acc

-- | Add or remove credentials on an account.
--  The caller must ensure the following, which are not checked:
--
--  * Any credential index that is removed must already exist.
--  * The credential with index 0 must not be removed.
--  * Any credential index that is added must not exist after the removals take effect.
--  * At least one credential remains after all removals and additions.
--  * Any new threshold is at most the number of accounts remaining (and at least 1).
updateAccountCredentials ::
    (MonadBlobStore m) =>
    -- | Credentials to remove
    [CredentialIndex] ->
    -- | Credentials to add
    Map.Map CredentialIndex AccountCredential ->
    -- | New account threshold
    AccountThreshold ->
    -- | Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV0 acc) =
    PAV0 <$> V0.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV1 acc) =
    PAV1 <$> V0.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV2 acc) =
    PAV2 <$> V1.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV3 acc) =
    PAV3 <$> V1.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV4 acc) =
    PAV4 <$> V1.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc
updateAccountCredentials cuRemove cuAdd cuAccountThreshold (PAV5 acc) =
    PAV5 <$> V1.updateAccountCredentials cuRemove cuAdd cuAccountThreshold acc

-- | Optionally update the verification keys and signature threshold for an account.
--  Precondition: The credential with given credential index exists.
updateAccountCredentialKeys ::
    (MonadBlobStore m) =>
    -- | Credential to update
    CredentialIndex ->
    -- | New public keys
    CredentialPublicKeys ->
    -- | Account to update
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountCredentialKeys credIndex credKeys (PAV0 acc) =
    PAV0 <$> V0.updateAccountCredentialKeys credIndex credKeys acc
updateAccountCredentialKeys credIndex credKeys (PAV1 acc) =
    PAV1 <$> V0.updateAccountCredentialKeys credIndex credKeys acc
updateAccountCredentialKeys credIndex credKeys (PAV2 acc) =
    PAV2 <$> V1.updateAccountCredentialKeys credIndex credKeys acc
updateAccountCredentialKeys credIndex credKeys (PAV3 acc) =
    PAV3 <$> V1.updateAccountCredentialKeys credIndex credKeys acc
updateAccountCredentialKeys credIndex credKeys (PAV4 acc) =
    PAV4 <$> V1.updateAccountCredentialKeys credIndex credKeys acc
updateAccountCredentialKeys credIndex credKeys (PAV5 acc) =
    PAV5 <$> V1.updateAccountCredentialKeys credIndex credKeys acc

-- | Add an amount to the account's balance.
addAccountAmount :: (MonadBlobStore m) => Amount -> PersistentAccount av -> m (PersistentAccount av)
addAccountAmount amt (PAV0 acc) = PAV0 <$> V0.addAmount amt acc
addAccountAmount amt (PAV1 acc) = PAV1 <$> V0.addAmount amt acc
addAccountAmount amt (PAV2 acc) = PAV2 <$> V1.addAmount amt acc
addAccountAmount amt (PAV3 acc) = PAV3 <$> V1.addAmount amt acc
addAccountAmount amt (PAV4 acc) = PAV4 <$> V1.addAmount amt acc
addAccountAmount amt (PAV5 acc) = PAV5 <$> V1.addAmount amt acc

-- | Applies a pending stake change to an account. The account MUST have a pending stake change.
--  If the account does not have a pending stake change, or is not staking, then this will raise
--  an error.
applyPendingStakeChange :: (MonadBlobStore m) => PersistentAccount 'AccountV0 -> m (PersistentAccount 'AccountV0)
applyPendingStakeChange (PAV0 acc) = PAV0 <$> V0.applyPendingStakeChange acc

-- | Add an account baker in account version 0.
--  This will replace any existing staking information on the account.
addAccountBakerV0 :: (MonadBlobStore m) => BakerId -> BakerAdd -> PersistentAccount 'AccountV0 -> m (PersistentAccount 'AccountV0)
addAccountBakerV0 bid ab (PAV0 acc) = PAV0 <$> V0.addBakerV0 bid ab acc

-- | Add a baker to an account for account version 1.
--  This will replace any existing staking information on the account.
addAccountBakerV1 ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    -- | Extended baker info
    BakerInfoEx av ->
    -- | Baker's equity capital
    Amount ->
    -- | Whether earnings are restaked
    Bool ->
    -- | Account to add baker to
    PersistentAccount av ->
    m (PersistentAccount av)
addAccountBakerV1 binfo amt restake (PAV1 acc) = PAV1 <$> V0.addBakerV1 binfo amt restake acc
addAccountBakerV1 binfo amt restake (PAV2 acc) = PAV2 <$> V1.addBakerV1 binfo amt restake acc
addAccountBakerV1 binfo amt restake (PAV3 acc) = PAV3 <$> V1.addBakerV1 binfo amt restake acc
addAccountBakerV1 binfo amt restake (PAV4 acc) = PAV4 <$> V1.addBakerV1 binfo amt restake acc
addAccountBakerV1 binfo amt restake (PAV5 acc) = PAV5 <$> V1.addBakerV1 binfo amt restake acc

-- | Add a delegator to an account.
--  This will replace any existing staking information on the account.
addAccountDelegator ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    AccountDelegation av ->
    PersistentAccount av ->
    m (PersistentAccount av)
addAccountDelegator del (PAV1 acc) = PAV1 <$> V0.addDelegator del acc
addAccountDelegator del (PAV2 acc) = PAV2 <$> V1.addDelegator del acc
addAccountDelegator del (PAV3 acc) = PAV3 <$> V1.addDelegator del acc
addAccountDelegator del (PAV4 acc) = PAV4 <$> V1.addDelegator del acc
addAccountDelegator del (PAV5 acc) = PAV5 <$> V1.addDelegator del acc

-- | Update the pool info on a baker account.
--  This MUST only be called with an account that is a baker.
updateAccountBakerPoolInfo ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    BakerPoolInfoUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
updateAccountBakerPoolInfo upd (PAV1 acc) = PAV1 <$> V0.updateBakerPoolInfo upd acc
updateAccountBakerPoolInfo upd (PAV2 acc) = PAV2 <$> V1.updateBakerPoolInfo upd acc
updateAccountBakerPoolInfo upd (PAV3 acc) = PAV3 <$> V1.updateBakerPoolInfo upd acc
updateAccountBakerPoolInfo upd (PAV4 acc) = PAV4 <$> V1.updateBakerPoolInfo upd acc
updateAccountBakerPoolInfo upd (PAV5 acc) = PAV5 <$> V1.updateBakerPoolInfo upd acc

-- | Set the baker keys on a baker account.
--  This MUST only be called with an account that is a baker.
setAccountBakerKeys ::
    (MonadBlobStore m) =>
    BakerKeyUpdate ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountBakerKeys keys (PAV0 acc) = PAV0 <$> V0.setBakerKeys keys acc
setAccountBakerKeys keys (PAV1 acc) = PAV1 <$> V0.setBakerKeys keys acc
setAccountBakerKeys keys (PAV2 acc) = PAV2 <$> V1.setBakerKeys keys acc
setAccountBakerKeys keys (PAV3 acc) = PAV3 <$> V1.setBakerKeys keys acc
setAccountBakerKeys keys (PAV4 acc) = PAV4 <$> V1.setBakerKeys keys acc
setAccountBakerKeys keys (PAV5 acc) = PAV5 <$> V1.setBakerKeys keys acc

-- | Set the stake of a baker or delegator account.
--  This MUST only be called with an account that is either a baker or delegator.
--  This does no check that the staked amount is sensible, and has no effect on pending changes.
setAccountStake ::
    (MonadBlobStore m) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountStake newStake (PAV0 acc) = PAV0 <$> V0.setStake newStake acc
setAccountStake newStake (PAV1 acc) = PAV1 <$> V0.setStake newStake acc
setAccountStake newStake (PAV2 acc) = PAV2 <$> V1.setStake newStake acc
setAccountStake newStake (PAV3 acc) = PAV3 <$> V1.setStake newStake acc
setAccountStake newStake (PAV4 acc) = PAV4 <$> V1.setStake newStake acc
setAccountStake newStake (PAV5 acc) = PAV5 <$> V1.setStake newStake acc

-- | Suspend or resume the account of a validator.
--  This MUST only be called with an account that is a validator.
setAccountValidatorSuspended ::
    (MonadBlobStore m, AVSupportsValidatorSuspension av) =>
    Bool ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountValidatorSuspended isSuspended (PAV4 acc) = PAV4 <$> V1.setValidatorSuspended isSuspended acc
setAccountValidatorSuspended isSuspended (PAV5 acc) = PAV5 <$> V1.setValidatorSuspended isSuspended acc

-- | Add a specified amount to the pre-pre-cooldown inactive stake.
addAccountPrePreCooldown ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
addAccountPrePreCooldown amt (PAV3 acc) = PAV3 <$> V1.addPrePreCooldown amt acc
addAccountPrePreCooldown amt (PAV4 acc) = PAV4 <$> V1.addPrePreCooldown amt acc
addAccountPrePreCooldown amt (PAV5 acc) = PAV5 <$> V1.addPrePreCooldown amt acc

-- | Remove up to the given amount from the cooldowns, starting with pre-pre-cooldown, then
--  pre-cooldown, and finally from the amounts in cooldown, in decreasing order of timestamp.
reactivateCooldownAmount ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    Amount ->
    PersistentAccount av ->
    m (PersistentAccount av)
reactivateCooldownAmount amt (PAV3 acc) = PAV3 <$> V1.reactivateCooldownAmount amt acc
reactivateCooldownAmount amt (PAV4 acc) = PAV4 <$> V1.reactivateCooldownAmount amt acc
reactivateCooldownAmount amt (PAV5 acc) = PAV5 <$> V1.reactivateCooldownAmount amt acc

-- | Set whether a baker or delegator account restakes its earnings.
--  This MUST only be called with an account that is either a baker or delegator.
setAccountRestakeEarnings ::
    (MonadBlobStore m) =>
    Bool ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountRestakeEarnings restake (PAV0 acc) = PAV0 <$> V0.setRestakeEarnings restake acc
setAccountRestakeEarnings restake (PAV1 acc) = PAV1 <$> V0.setRestakeEarnings restake acc
setAccountRestakeEarnings restake (PAV2 acc) = PAV2 <$> V1.setRestakeEarnings restake acc
setAccountRestakeEarnings restake (PAV3 acc) = PAV3 <$> V1.setRestakeEarnings restake acc
setAccountRestakeEarnings restake (PAV4 acc) = PAV4 <$> V1.setRestakeEarnings restake acc
setAccountRestakeEarnings restake (PAV5 acc) = PAV5 <$> V1.setRestakeEarnings restake acc

-- | Set the pending change on baker or delegator account.
--  This MUST only be called with an account that is either a baker or delegator.
setAccountStakePendingChange ::
    (MonadBlobStore m) =>
    StakePendingChange av ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountStakePendingChange pc (PAV0 acc) = PAV0 <$> V0.setStakePendingChange pc acc
setAccountStakePendingChange pc (PAV1 acc) = PAV1 <$> V0.setStakePendingChange pc acc
setAccountStakePendingChange pc (PAV2 acc) = PAV2 <$> V1.setStakePendingChange pc acc
setAccountStakePendingChange pc (PAV3 acc) = PAV3 <$> V1.setStakePendingChange pc acc
setAccountStakePendingChange pc (PAV4 acc) = PAV4 <$> V1.setStakePendingChange pc acc
setAccountStakePendingChange pc (PAV5 acc) = PAV5 <$> V1.setStakePendingChange pc acc

-- | Set the target of a delegating account.
--  This MUST only be called with an account that is a delegator.
setAccountDelegationTarget ::
    (MonadBlobStore m) =>
    DelegationTarget ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountDelegationTarget target (PAV0 acc) = PAV0 <$> V0.setDelegationTarget target acc
setAccountDelegationTarget target (PAV1 acc) = PAV1 <$> V0.setDelegationTarget target acc
setAccountDelegationTarget target (PAV2 acc) = PAV2 <$> V1.setDelegationTarget target acc
setAccountDelegationTarget target (PAV3 acc) = PAV3 <$> V1.setDelegationTarget target acc
setAccountDelegationTarget target (PAV4 acc) = PAV4 <$> V1.setDelegationTarget target acc
setAccountDelegationTarget target (PAV5 acc) = PAV5 <$> V1.setDelegationTarget target acc

-- | Remove any staking on an account.
removeAccountStaking ::
    (MonadBlobStore m) =>
    PersistentAccount av ->
    m (PersistentAccount av)
removeAccountStaking (PAV0 acc) = PAV0 <$> V0.removeStaking acc
removeAccountStaking (PAV1 acc) = PAV1 <$> V0.removeStaking acc
removeAccountStaking (PAV2 acc) = PAV2 <$> V1.removeStaking acc
removeAccountStaking (PAV3 acc) = PAV3 <$> V1.removeStaking acc
removeAccountStaking (PAV4 acc) = PAV4 <$> V1.removeStaking acc
removeAccountStaking (PAV5 acc) = PAV5 <$> V1.removeStaking acc

-- | Set the commission rates on a baker account.
--  This MUST only be called with an account that is a baker.
setAccountCommissionRates ::
    (MonadBlobStore m, AVSupportsDelegation av) =>
    CommissionRates ->
    PersistentAccount av ->
    m (PersistentAccount av)
setAccountCommissionRates rates (PAV1 acc) = PAV1 <$> V0.setCommissionRates rates acc
setAccountCommissionRates rates (PAV2 acc) = PAV2 <$> V1.setCommissionRates rates acc
setAccountCommissionRates rates (PAV3 acc) = PAV3 <$> V1.setCommissionRates rates acc
setAccountCommissionRates rates (PAV4 acc) = PAV4 <$> V1.setCommissionRates rates acc
setAccountCommissionRates rates (PAV5 acc) = PAV5 <$> V1.setCommissionRates rates acc

-- | Unlock scheduled releases on an account up to and including the given timestamp.
--  This returns the next timestamp at which a release is scheduled for the account, if any,
--  as well as the updated account.
unlockAccountReleases ::
    (MonadBlobStore m) =>
    Timestamp ->
    PersistentAccount av ->
    m (Maybe Timestamp, PersistentAccount av)
unlockAccountReleases ts (PAV0 acc) = second PAV0 <$> V0.unlockReleases ts acc
unlockAccountReleases ts (PAV1 acc) = second PAV1 <$> V0.unlockReleases ts acc
unlockAccountReleases ts (PAV2 acc) = second PAV2 <$> V1.unlockReleases ts acc
unlockAccountReleases ts (PAV3 acc) = second PAV3 <$> V1.unlockReleases ts acc
unlockAccountReleases ts (PAV4 acc) = second PAV4 <$> V1.unlockReleases ts acc
unlockAccountReleases ts (PAV5 acc) = second PAV5 <$> V1.unlockReleases ts acc

-- | Process the cooldowns on an account up to and including the given timestamp.
--  This returns the next timestamp at which a cooldown expires, if any.
processAccountCooldownsUntil ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    Timestamp ->
    PersistentAccount av ->
    m (Maybe Timestamp, PersistentAccount av)
processAccountCooldownsUntil ts (PAV3 acc) =
    second PAV3 <$> V1.processCooldownsUntil ts acc
processAccountCooldownsUntil ts (PAV4 acc) =
    second PAV4 <$> V1.processCooldownsUntil ts acc
processAccountCooldownsUntil ts (PAV5 acc) =
    second PAV5 <$> V1.processCooldownsUntil ts acc

-- | Move the pre-cooldown amount on an account into cooldown with the specified release time.
--  This returns @Just (Just ts)@ if the previous next cooldown time was @ts@, but the new next
--  cooldown (i.e. the supplied timestamp) time is earlier. It returns @Just Nothing@ if the account
--  did not have a cooldown but now does. Otherwise, it returns @Nothing@.
processAccountPreCooldown ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    Timestamp ->
    PersistentAccount av ->
    m (NextCooldownChange, PersistentAccount av)
processAccountPreCooldown ts (PAV3 acc) = second PAV3 <$> V1.processPreCooldown ts acc
processAccountPreCooldown ts (PAV4 acc) = second PAV4 <$> V1.processPreCooldown ts acc
processAccountPreCooldown ts (PAV5 acc) = second PAV5 <$> V1.processPreCooldown ts acc

-- | Move the pre-pre-cooldown amount on an account into pre-cooldown.
--  It should be the case that the account has a pre-pre-cooldown amount and no pre-cooldown amount.
--  However, if there is no pre-pre-cooldown amount, this will do nothing, and if there is already
--  a pre-cooldown amount, the pre-pre-cooldown amount will be added to it.
processAccountPrePreCooldown ::
    (MonadBlobStore m, AVSupportsFlexibleCooldown av) =>
    PersistentAccount av ->
    m (PersistentAccount av)
processAccountPrePreCooldown (PAV3 acc) = PAV3 <$> V1.processPrePreCooldown acc
processAccountPrePreCooldown (PAV4 acc) = PAV4 <$> V1.processPrePreCooldown acc
processAccountPrePreCooldown (PAV5 acc) = PAV5 <$> V1.processPrePreCooldown acc

-- * Creation

-- | Make a persistent account from a transient account.
makePersistentAccount ::
    forall m av.
    (MonadBlobStore m, IsAccountVersion av) =>
    Transient.Account av ->
    m (PersistentAccount av)
makePersistentAccount tacc = case accountVersion @av of
    SAccountV0 -> PAV0 <$> V0.makePersistentAccount tacc
    SAccountV1 -> PAV1 <$> V0.makePersistentAccount tacc
    SAccountV2 -> PAV2 <$> V1.makePersistentAccount tacc
    SAccountV3 -> PAV3 <$> V1.makePersistentAccount tacc
    SAccountV4 -> PAV4 <$> V1.makePersistentAccount tacc
    SAccountV5 -> PAV5 <$> V1.makePersistentAccount tacc

-- | Create an empty account with the given public key, address and credential.
newAccount ::
    forall m av.
    (MonadBlobStore m, IsAccountVersion av) =>
    GlobalContext ->
    AccountAddress ->
    AccountCredential ->
    m (PersistentAccount av)
newAccount = case accountVersion @av of
    SAccountV0 -> \ctx addr cred -> PAV0 <$> V0.newAccount ctx addr cred
    SAccountV1 -> \ctx addr cred -> PAV1 <$> V0.newAccount ctx addr cred
    SAccountV2 -> \ctx addr cred -> PAV2 <$> V1.newAccount ctx addr cred
    SAccountV3 -> \ctx addr cred -> PAV3 <$> V1.newAccount ctx addr cred
    SAccountV4 -> \ctx addr cred -> PAV4 <$> V1.newAccount ctx addr cred
    SAccountV5 -> \ctx addr cred -> PAV5 <$> V1.newAccount ctx addr cred

-- | Make a persistent account from a genesis account.
--  The data is immediately flushed to disc and cached.
makeFromGenesisAccount ::
    forall pv av m.
    (IsProtocolVersion pv, MonadBlobStore m, AccountVersionFor pv ~ av) =>
    SProtocolVersion pv ->
    GlobalContext ->
    ChainParameters pv ->
    GenesisAccount ->
    m (PersistentAccount av)
makeFromGenesisAccount spv =
    case accountVersion @av of
        SAccountV0 -> \cryptoParams chainParameters genesisAccount ->
            PAV0 <$> V0.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount
        SAccountV1 -> \cryptoParams chainParameters genesisAccount ->
            PAV1 <$> V0.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount
        SAccountV2 -> \cryptoParams chainParameters genesisAccount ->
            PAV2 <$> V1.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount
        SAccountV3 -> \cryptoParams chainParameters genesisAccount ->
            PAV3 <$> V1.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount
        SAccountV4 -> \cryptoParams chainParameters genesisAccount ->
            PAV4 <$> V1.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount
        SAccountV5 -> \cryptoParams chainParameters genesisAccount ->
            PAV5 <$> V1.makeFromGenesisAccount spv cryptoParams chainParameters genesisAccount

-- ** 'PersistentBakerInfoRef' creation

-- | Create a 'PersistentBakerInfoRef' from a 'BakerInfoEx'.
makePersistentBakerInfoRef ::
    forall av m.
    (IsAccountVersion av, MonadBlobStore m) =>
    BakerInfoEx av ->
    m (PersistentBakerInfoRef av)
makePersistentBakerInfoRef = case accountVersion @av of
    SAccountV0 -> fmap PBIRV0 . V0.makePersistentBakerInfoEx
    SAccountV1 -> fmap PBIRV1 . V0.makePersistentBakerInfoEx
    SAccountV2 -> fmap PBIRV2 . V1.makePersistentBakerInfoEx
    SAccountV3 -> fmap PBIRV3 . V1.makePersistentBakerInfoEx
    SAccountV4 -> fmap PBIRV4 . V1.makePersistentBakerInfoEx
    SAccountV5 -> fmap PBIRV5 . V1.makePersistentBakerInfoEx

-- * Migration

-- | Migrate a 'PersistentAccount' between protocol versions according to a state migration.
--
--  When migrating P6->P7 (account version 2 to 3), the 'AccountMigration' interface is used as
--  follows:
--
--   * Accounts that previously had a pending change are updated to have a pre-pre-cooldown, and
--     'addAccountInPrePreCooldown' is called. If the pending change is a reduction in stake,
--     the reduction is applied immediately to the active stake. If the pending change is a removal,
--     the baker or delegator record is removed altogether.
--
--   * Accounts that are still delegating but were delegating to a baker for which 'isBakerRemoved'
--     returns @True@ are updated to delegate to passive delegation.
--
--   * For accounts that are still delegating, 'retainDelegator' is called to record the (new)
--     delegation amount and target.
migratePersistentAccount ::
    forall oldpv pv t m.
    ( IsProtocolVersion oldpv,
      IsProtocolVersion pv,
      SupportMigration m t,
      AccountMigration (AccountVersionFor pv) (t m),
      MonadLogger (t m)
    ) =>
    StateMigrationParameters oldpv pv ->
    PersistentAccount (AccountVersionFor oldpv) ->
    t m (PersistentAccount (AccountVersionFor pv))
migratePersistentAccount m@StateMigrationParametersTrivial (PAV0 acc) = PAV0 <$> V0.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersTrivial (PAV1 acc) = PAV1 <$> V0.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersTrivial (PAV2 acc) = PAV2 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersTrivial (PAV3 acc) = PAV3 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersTrivial (PAV4 acc) = PAV4 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersTrivial (PAV5 acc) = PAV5 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP1P2 (PAV0 acc) = PAV0 <$> V0.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP2P3 (PAV0 acc) = PAV0 <$> V0.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP3ToP4{} (PAV0 acc) = PAV1 <$> V0.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP4ToP5{} (PAV1 acc) = PAV2 <$> V1.migratePersistentAccountFromV0 m acc
migratePersistentAccount m@StateMigrationParametersP5ToP6{} (PAV2 acc) = PAV2 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP6ToP7{} (PAV2 acc) = PAV3 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP7ToP8{} (PAV3 acc) = PAV4 <$> V1.migratePersistentAccount m acc
migratePersistentAccount m@StateMigrationParametersP8ToP9{} (PAV4 acc) = PAV5 <$> V1.migratePersistentAccount m acc

-- | Migrate a 'PersistentBakerInfoRef' between protocol versions according to a state migration.
migratePersistentBakerInfoRef ::
    forall oldpv pv t m.
    (IsProtocolVersion pv, SupportMigration m t) =>
    StateMigrationParameters oldpv pv ->
    PersistentBakerInfoRef (AccountVersionFor oldpv) ->
    t m (PersistentBakerInfoRef (AccountVersionFor pv))
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV0 bir) = PBIRV0 <$> V0.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV1 bir) = PBIRV1 <$> V0.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV2 bir) = PBIRV2 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV3 bir) = PBIRV3 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV4 bir) = PBIRV4 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersTrivial (PBIRV5 bir) = PBIRV5 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP1P2 (PBIRV0 bir) = PBIRV0 <$> V0.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP2P3 (PBIRV0 bir) = PBIRV0 <$> V0.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP3ToP4{} (PBIRV0 bir) = PBIRV1 <$> V0.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP4ToP5{} (PBIRV1 bir) = PBIRV2 <$> V1.migratePersistentBakerInfoExFromV0 m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP5ToP6{} (PBIRV2 bir) = PBIRV2 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP6ToP7{} (PBIRV2 bir) = PBIRV3 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP7ToP8{} (PBIRV3 bir) = PBIRV4 <$> V1.migratePersistentBakerInfoEx m bir
migratePersistentBakerInfoRef m@StateMigrationParametersP8ToP9{} (PBIRV4 bir) = PBIRV5 <$> V1.migratePersistentBakerInfoEx m bir

-- * Conversion

-- | Converts an account to a transient (i.e. in memory) account. (Used for testing.)
toTransientAccount :: (MonadBlobStore m) => PersistentAccount av -> m (Transient.Account av)
toTransientAccount (PAV0 acc) = V0.toTransientAccount acc
toTransientAccount (PAV1 acc) = V0.toTransientAccount acc
toTransientAccount (PAV2 acc) = V1.toTransientAccount acc
toTransientAccount (PAV3 acc) = V1.toTransientAccount acc
toTransientAccount (PAV4 acc) = V1.toTransientAccount acc
toTransientAccount (PAV5 acc) = V1.toTransientAccount acc
