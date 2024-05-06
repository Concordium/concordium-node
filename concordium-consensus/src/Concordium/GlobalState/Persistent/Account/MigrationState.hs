{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.Account.MigrationState where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Bool.Singletons
import Data.Kind
import Data.Maybe
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Accounts
import Concordium.Types.Conditionally
import Concordium.Utils

import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.Account.MigrationStateInterface
import Concordium.GlobalState.Persistent.Accounts
import Concordium.GlobalState.Persistent.Bakers as Bakers
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.Cooldown
import qualified Concordium.GlobalState.Persistent.Trie as Trie

data AccountMigrationState (oldpv :: ProtocolVersion) (pv :: ProtocolVersion) = AccountMigrationState
    { -- | In the P6 -> P7 protocol update, this records the accounts that previously were in
      --  cooldown, and now will be in pre-pre-cooldown.
      _migrationPrePreCooldown ::
        !( Conditionally
            ( Not (SupportsFlexibleCooldown (AccountVersionFor oldpv))
                && SupportsFlexibleCooldown (AccountVersionFor pv)
            )
            AccountList
         ),
      -- | When migrating to P7 (and onwards), we will build up the 'PersistentActiveBakers' while
      --  traversing the account table. This should be initialised with the active bakers (that
      --  survive migration) but no delegators.
      _persistentActiveBakers ::
        !( Conditionally
            (SupportsFlexibleCooldown (AccountVersionFor pv))
            (PersistentActiveBakers (AccountVersionFor pv))
         ),
      -- | A counter to track the index of the current account as we traverse the account table.
      _currentAccountIndex :: !AccountIndex
    }
makeLenses ''AccountMigrationState

-- | Construct an initial 'PersistentActiveBakers' that records all of the bakers that are still
-- active after migration, but does not include any delegators. This only applies when migrating
-- to a protocol version from P7 onwards.
--
-- The idea is that with the P6->P7 migration, bakers that are in cooldown to be removed will
-- actually be removed as bakers.
initialPersistentActiveBakersForMigration ::
    forall oldpv av t m.
    ( IsAccountVersion av,
      SupportMigration m t,
      SupportsPersistentAccount oldpv m
    ) =>
    Accounts oldpv ->
    PersistentActiveBakers (AccountVersionFor oldpv) ->
    t m (Conditionally (SupportsFlexibleCooldown av) (PersistentActiveBakers av))
initialPersistentActiveBakersForMigration oldAccounts oldActiveBakers = case sSupportsFlexibleCooldown (accountVersion @av) of
    SFalse -> return CFalse
    STrue -> do
        bakers <- lift $ Trie.keysAsc (oldActiveBakers ^. activeBakers)
        CTrue <$> foldM accumBakers emptyPersistentActiveBakers bakers
      where
        accumBakers :: PersistentActiveBakers av -> BakerId -> t m (PersistentActiveBakers av)
        accumBakers pab bakerId =
            lift (indexedAccount (bakerAccountIndex bakerId) oldAccounts) >>= \case
                Nothing -> error "Baker account does not exist"
                Just account -> do
                    lift (accountBaker account) >>= \case
                        Nothing -> error "Baker account is not a baker."
                        Just bkr
                            | RemoveStake{} <- _bakerPendingChange bkr -> do
                                -- The baker is pending removal, so it will be removed from
                                -- the account in this update.
                                return pab
                            | otherwise -> do
                                -- The baker is still active, so add it to the persistent active
                                -- bakers.
                                newActiveBakers <- Trie.insert bakerId emptyPersistentActiveDelegators (pab ^. activeBakers)
                                newAggregationKeys <- Trie.insert (bkr ^. bakerAggregationVerifyKey) () (pab ^. aggregationKeys)
                                let newTotalActiveCapital = addActiveCapital (bkr ^. stakedAmount) (pab ^. totalActiveCapital)
                                return
                                    pab
                                        { _activeBakers = newActiveBakers,
                                          _aggregationKeys = newAggregationKeys,
                                          _totalActiveCapital = newTotalActiveCapital
                                        }

-- | An 'AccountMigrationState' in an initial state.
initialAccountMigrationState ::
    forall oldpv pv.
    (IsProtocolVersion oldpv, IsProtocolVersion pv) =>
    -- | The active bakers without the delegators.
    Conditionally
        (SupportsFlexibleCooldown (AccountVersionFor pv))
        (PersistentActiveBakers (AccountVersionFor pv)) ->
    AccountMigrationState oldpv pv
initialAccountMigrationState _persistentActiveBakers = AccountMigrationState{..}
  where
    _migrationPrePreCooldown = case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor oldpv)) of
        SFalse -> case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
            SFalse -> CFalse
            STrue -> CTrue Null
        STrue -> CFalse
    _currentAccountIndex = 0

-- | Construct an initial account migration state that
makeInitialAccountMigrationState ::
    ( IsProtocolVersion pv,
      SupportMigration m t,
      SupportsPersistentAccount oldpv m
    ) =>
    Accounts oldpv ->
    PersistentActiveBakers (AccountVersionFor oldpv) ->
    t m (AccountMigrationState oldpv pv)
makeInitialAccountMigrationState accounts pab =
    initialAccountMigrationState <$> initialPersistentActiveBakersForMigration accounts pab

-- | A monad transformer transformer that left-composes @StateT (AccountMigrationState old pv)@
--  with a given monad transformer @t@.
newtype
    AccountMigrationStateTT
        (oldpv :: ProtocolVersion)
        (pv :: ProtocolVersion)
        (t :: (Type -> Type) -> (Type -> Type))
        (m :: (Type -> Type))
        (a :: Type) = AccountMigrationStateTT
    { runAccountMigrationStateTT' ::
        StateT (AccountMigrationState oldpv pv) (t m) a
    }
    deriving newtype
        ( Functor,
          Applicative,
          Monad,
          MonadState (AccountMigrationState oldpv pv),
          MonadIO,
          LMDBAccountMap.MonadAccountMapStore
        )

runAccountMigrationStateTT ::
    AccountMigrationStateTT oldpv pv t m a ->
    AccountMigrationState oldpv pv ->
    t m (a, AccountMigrationState oldpv pv)
runAccountMigrationStateTT = runStateT . runAccountMigrationStateTT'

deriving via
    forall
        (oldpv :: ProtocolVersion)
        (pv :: ProtocolVersion)
        (t :: (Type -> Type) -> (Type -> Type))
        (m :: (Type -> Type)).
    ( StateT (AccountMigrationState oldpv pv) (t m)
    )
    instance
        (MonadBlobStore (t m)) =>
        (MonadBlobStore (AccountMigrationStateTT oldpv pv t m))

deriving via
    forall
        (oldpv :: ProtocolVersion)
        (pv :: ProtocolVersion)
        (t :: (Type -> Type) -> (Type -> Type))
        (m :: (Type -> Type)).
    ( StateT (AccountMigrationState oldpv pv) (t m)
    )
    instance
        (MonadCache c (t m)) =>
        (MonadCache c (AccountMigrationStateTT oldpv pv t m))

instance (MonadTrans t) => MonadTrans (AccountMigrationStateTT oldpv pv t) where
    lift = AccountMigrationStateTT . lift . lift

instance
    (MonadBlobStore (t m), IsProtocolVersion pv, av ~ AccountVersionFor pv) =>
    AccountMigration av (AccountMigrationStateTT oldpv pv t m)
    where
    addAccountInPrePreCooldown = do
        ai <- use currentAccountIndex
        mmpc <- use migrationPrePreCooldown
        case mmpc of
            CTrue mpc -> do
                newHead <-
                    makeUnbufferedRef
                        AccountListItem
                            { accountListEntry = ai,
                              accountListTail = mpc
                            }
                migrationPrePreCooldown .= CTrue (Some newHead)
            CFalse -> return ()

    nextAccount = currentAccountIndex %=! (+ 1)

    isBakerRemoved bakerId =
        use persistentActiveBakers >>= \case
            CFalse -> return False
            CTrue pab ->
                isNothing <$> Trie.lookup bakerId (pab ^. activeBakers)

    retainDelegator delId delAmt delTarget =
        use persistentActiveBakers >>= \case
            CTrue pab ->
                Bakers.addDelegator delTarget delId delAmt pab >>= \case
                    Left bid ->
                        error $
                            "Baker "
                                ++ show bid
                                ++ " (delegated to by "
                                ++ show delId
                                ++ ") is not a baker."
                    Right newPAB -> persistentActiveBakers .= CTrue newPAB
            CFalse -> return ()
