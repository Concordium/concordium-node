{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cooldown where

import Data.Bool.Singletons
import Data.Serialize

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.ReleaseSchedule
import Concordium.Types
import Concordium.Types.Conditionally

-- | An 'AccountIndex' and the (possibly empty) tail of the list.
data AccountListItem = AccountListItem
    { accountListEntry :: !AccountIndex,
      accountListTail :: !AccountList
    }

instance (MonadBlobStore m) => BlobStorable m AccountListItem where
    load = do
        mAccountListEntry <- load
        mAccountListTail <- load
        return (AccountListItem <$> mAccountListEntry <*> mAccountListTail)
    storeUpdate ali = do
        (pAccountListTail, newAccountListTail) <- storeUpdate (accountListTail ali)
        return
            ( put (accountListEntry ali) >> pAccountListTail,
              ali{accountListTail = newAccountListTail}
            )

-- | A possibly empty list of 'AccountIndex'es, stored under 'UnbufferedRef's.
type AccountList = Nullable (UnbufferedRef AccountListItem)

-- | Load an entire account list. This is intended for testing purposes.
loadAccountList :: (MonadBlobStore m) => AccountList -> m [AccountIndex]
loadAccountList Null = return []
loadAccountList (Some ref) = do
    AccountListItem{..} <- refLoad ref
    (accountListEntry :) <$> loadAccountList accountListTail

-- | Migrate an 'AccountList' from one context to another.
migrateAccountList :: (SupportMigration m t) => AccountList -> t m AccountList
migrateAccountList Null = return Null
migrateAccountList (Some ubRef) = do
    Some <$> migrateReference migrateAccountListItem ubRef
  where
    migrateAccountListItem ali = do
        newTail <- migrateAccountList (accountListTail ali)
        return $! ali{accountListTail = newTail}

-- | This is an indexing structure and therefore does not need to be hashed. FIXME: add more docs
data AccountsInCooldown = AccountsInCooldown
    { cooldown :: !NewReleaseSchedule,
      preCooldown :: !AccountList,
      prePreCooldown :: !AccountList
    }

-- | The cacheable instance only caches the 'cooldown' field, since the
--  'preCooldown' and 'prePreCooldown' are implemented using 'UnbufferedRef's (and so
--  would have no benefit from caching).
instance (MonadBlobStore m) => Cacheable m AccountsInCooldown where
    cache aic = do
        newCooldown <- cache (cooldown aic)
        return aic{cooldown = newCooldown}

instance (MonadBlobStore m) => BlobStorable m AccountsInCooldown where
    load = do
        cooldown <- load
        preCooldown <- load
        prePreCooldown <- load
        return (AccountsInCooldown <$> cooldown <*> preCooldown <*> prePreCooldown)
    storeUpdate aic = do
        (pCooldown, newCooldown) <- storeUpdate (cooldown aic)
        (pPreCooldown, newPreCooldown) <- storeUpdate (preCooldown aic)
        (pPrePreCooldown, newPrePreCooldown) <- storeUpdate (prePreCooldown aic)
        let putAIC = pCooldown >> pPreCooldown >> pPrePreCooldown
        return
            ( putAIC,
              AccountsInCooldown
                { cooldown = newCooldown,
                  preCooldown = newPreCooldown,
                  prePreCooldown = newPrePreCooldown
                }
            )

-- | An 'AccountsInCooldown' with no accounts in (pre)*cooldown.
emptyAccountsInCooldown :: AccountsInCooldown
emptyAccountsInCooldown =
    AccountsInCooldown
        { cooldown = emptyNewReleaseSchedule,
          preCooldown = Null,
          prePreCooldown = Null
        }

-- | Migrate 'AccountsInCooldown' from one 'BlobStore' to another.
migrateAccountsInCooldown ::
    (SupportMigration m t) =>
    AccountsInCooldown ->
    t m AccountsInCooldown
migrateAccountsInCooldown aic = do
    newCooldown <- migrateNewReleaseSchedule (cooldown aic)
    newPreCooldown <- migrateAccountList (preCooldown aic)
    newPrePreCooldown <- migrateAccountList (prePreCooldown aic)
    return $!
        AccountsInCooldown
            { cooldown = newCooldown,
              preCooldown = newPreCooldown,
              prePreCooldown = newPrePreCooldown
            }

newtype AccountsInCooldownForPV pv = AccountsInCooldownForPV
    { theAccountsInCooldownForPV ::
        Conditionally (SupportsFlexibleCooldown (AccountVersionFor pv)) AccountsInCooldown
    }

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (AccountsInCooldownForPV pv) where
    load = case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
        SFalse -> return (return (AccountsInCooldownForPV CFalse))
        STrue -> fmap (AccountsInCooldownForPV . CTrue) <$> load
    storeUpdate aicPV@(AccountsInCooldownForPV CFalse) = do
        return (return (), aicPV)
    storeUpdate (AccountsInCooldownForPV (CTrue aic)) = do
        (paic, aic') <- storeUpdate aic
        return (paic, AccountsInCooldownForPV (CTrue aic'))

-- | An 'AccountsInCooldownForPV' with no accounts in (pre)*cooldown.
emptyAccountsInCooldownForPV ::
    forall pv.
    (IsProtocolVersion pv) =>
    AccountsInCooldownForPV pv
emptyAccountsInCooldownForPV =
    AccountsInCooldownForPV (conditionally cond emptyAccountsInCooldown)
  where
    cond = sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv))

instance (MonadBlobStore m) => Cacheable m (AccountsInCooldownForPV pv) where
    cache = fmap AccountsInCooldownForPV . mapM cache . theAccountsInCooldownForPV

-- | Migrate an 'AccountsInCooldownForPV'.
--
--   * If the new protocol version (@pv@) does not support flexible cooldown, then this just
--     produces the 'emptyAccountsInCooldownForPV'.
--
--   * Otherwise, if the old protocol version (@oldpv@) does not support flexible cooldown, then
--     this produces an 'emptyAccountsInCooldownForPV' but with the 'prePreCooldown' accounts set
--     to the provided list.
--
--   * If both protocol versions support flexible cooldown, the 'AccountsInCooldown' structure is
--     simply migrated across unchanged.
migrateAccountsInCooldownForPV ::
    forall oldpv pv t m.
    (SupportMigration m t, IsProtocolVersion pv, IsProtocolVersion oldpv) =>
    Conditionally
        ( Not (SupportsFlexibleCooldown (AccountVersionFor oldpv))
            && SupportsFlexibleCooldown (AccountVersionFor pv)
        )
        AccountList ->
    AccountsInCooldownForPV oldpv ->
    t m (AccountsInCooldownForPV pv)
migrateAccountsInCooldownForPV =
    case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
        SFalse -> \_ _ -> return emptyAccountsInCooldownForPV
        STrue -> case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor oldpv)) of
            SFalse -> \(CTrue prePreCooldownAccts) _ ->
                return
                    ( AccountsInCooldownForPV
                        (CTrue (emptyAccountsInCooldown{prePreCooldown = prePreCooldownAccts}))
                    )
            STrue -> \_ (AccountsInCooldownForPV (CTrue oldAIC)) ->
                AccountsInCooldownForPV . CTrue <$> migrateAccountsInCooldown oldAIC
