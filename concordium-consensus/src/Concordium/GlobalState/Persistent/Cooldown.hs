{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.GlobalState.Persistent.Cooldown where

import Control.Monad
import Data.Bool.Singletons
import qualified Data.Map.Strict as Map
import Data.Serialize
import Lens.Micro.Platform

import qualified Concordium.GlobalState.CooldownQueue as CooldownQueue
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.ReleaseSchedule
import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Types.Option

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

-- | Prepend an 'AccountIndex' to an 'AccountList'.
consAccountList :: (MonadBlobStore m) => AccountIndex -> AccountList -> m AccountList
consAccountList accountIndex accountList = do
    ref <- refMake (AccountListItem accountIndex accountList)
    return (Some ref)

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
    { _cooldown :: !NewReleaseSchedule,
      _preCooldown :: !AccountList,
      _prePreCooldown :: !AccountList
    }

makeLenses ''AccountsInCooldown

-- | The cacheable instance only caches the 'cooldown' field, since the
--  'preCooldown' and 'prePreCooldown' are implemented using 'UnbufferedRef's (and so
--  would have no benefit from caching).
instance (MonadBlobStore m) => Cacheable m AccountsInCooldown where
    cache = cooldown cache

instance (MonadBlobStore m) => BlobStorable m AccountsInCooldown where
    load = do
        mCooldown <- load
        mPreCooldown <- load
        mPrePreCooldown <- load
        return (AccountsInCooldown <$> mCooldown <*> mPreCooldown <*> mPrePreCooldown)
    storeUpdate aic = do
        (pCooldown, newCooldown) <- storeUpdate (_cooldown aic)
        (pPreCooldown, newPreCooldown) <- storeUpdate (_preCooldown aic)
        (pPrePreCooldown, newPrePreCooldown) <- storeUpdate (_prePreCooldown aic)
        let putAIC = pCooldown >> pPreCooldown >> pPrePreCooldown
        return
            ( putAIC,
              AccountsInCooldown
                { _cooldown = newCooldown,
                  _preCooldown = newPreCooldown,
                  _prePreCooldown = newPrePreCooldown
                }
            )

-- | An 'AccountsInCooldown' with no accounts in (pre)*cooldown.
emptyAccountsInCooldown :: AccountsInCooldown
emptyAccountsInCooldown =
    AccountsInCooldown
        { _cooldown = emptyNewReleaseSchedule,
          _preCooldown = Null,
          _prePreCooldown = Null
        }

-- | Migrate 'AccountsInCooldown' from one 'BlobStore' to another.
migrateAccountsInCooldown ::
    (SupportMigration m t) =>
    AccountsInCooldown ->
    t m AccountsInCooldown
migrateAccountsInCooldown aic = do
    newCooldown <- migrateNewReleaseSchedule (_cooldown aic)
    newPreCooldown <- migrateAccountList (_preCooldown aic)
    newPrePreCooldown <- migrateAccountList (_prePreCooldown aic)
    return $!
        AccountsInCooldown
            { _cooldown = newCooldown,
              _preCooldown = newPreCooldown,
              _prePreCooldown = newPrePreCooldown
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

-- | A lens for accessing the 'AccountsInCooldown' in an 'AccountsInCooldownForPV' when the
--  protocol version supports flexible cooldown.
accountsInCooldown ::
    (PVSupportsFlexibleCooldown pv) =>
    Lens' (AccountsInCooldownForPV pv) AccountsInCooldown
accountsInCooldown =
    lens
        (uncond . theAccountsInCooldownForPV)
        (\_ aic -> AccountsInCooldownForPV (CTrue aic))

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

-- | Generate the initial 'AccountsInCooldownForPV' structure from the initial accounts.
initialAccountsInCooldown ::
    forall pv m.
    (MonadBlobStore m, IsProtocolVersion pv) =>
    [PersistentAccount (AccountVersionFor pv)] ->
    m (AccountsInCooldownForPV pv)
initialAccountsInCooldown accounts = case sSupportsFlexibleCooldown sAV of
    SFalse -> return emptyAccountsInCooldownForPV
    STrue -> do
        AccountsInCooldownForPV . CTrue
            <$> foldM checkAccount emptyAccountsInCooldown (zip [0 ..] accounts)
  where
    sAV = accountVersion @(AccountVersionFor pv)
    checkAccount aic (aid, acct) = do
        accountCooldowns acct >>= \case
            Nothing -> return aic
            Just accCooldowns -> do
                newCooldown <- case Map.lookupMin (CooldownQueue.inCooldown accCooldowns) of
                    Nothing -> return $ aic ^. cooldown
                    Just (ts, _) -> addAccountRelease ts aid (aic ^. cooldown)
                newPreCooldown <- case CooldownQueue.preCooldown accCooldowns of
                    Absent -> return $ aic ^. preCooldown
                    Present _ -> consAccountList aid (aic ^. preCooldown)
                newPrePreCooldown <- case CooldownQueue.prePreCooldown accCooldowns of
                    Absent -> return $ aic ^. prePreCooldown
                    Present _ -> consAccountList aid (aic ^. prePreCooldown)
                return $
                    aic
                        & cooldown .~ newCooldown
                        & preCooldown .~ newPreCooldown
                        & prePreCooldown .~ newPrePreCooldown

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
                        (CTrue (emptyAccountsInCooldown{_prePreCooldown = prePreCooldownAccts}))
                    )
            STrue -> \_ (AccountsInCooldownForPV (CTrue oldAIC)) ->
                AccountsInCooldownForPV . CTrue <$> migrateAccountsInCooldown oldAIC
