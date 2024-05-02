{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.Account.MigrationState where

import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Bool.Singletons
import Data.Kind
import Data.Maybe
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Conditionally
import Concordium.Utils

import Concordium.GlobalState.Persistent.Account.MigrationStateInterface
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

-- | An 'AccountMigrationState' in an initial state.
initialAccountMigrationState ::
    forall oldpv pv.
    (IsProtocolVersion oldpv, IsProtocolVersion pv) =>
    -- | The active bakers without the delegators.
    Conditionally
        (SupportsFlexibleCooldown (AccountVersionFor pv))
        (PersistentActiveBakers (AccountVersionFor pv)) ->
    AccountMigrationState oldpv pv
initialAccountMigrationState _removedBakers = AccountMigrationState{..}
  where
    _migrationPrePreCooldown = case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor oldpv)) of
        SFalse -> case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
            SFalse -> CFalse
            STrue -> CTrue Null
        STrue -> CFalse
    _currentAccountIndex = 0
    _persistentActiveBakers =
        conditionally
            (sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)))
            emptyPersistentActiveBakers

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
    deriving newtype (Functor, Applicative, Monad, MonadState (AccountMigrationState oldpv pv), MonadIO)

runAccountMigrationStateTT :: AccountMigrationStateTT oldpv pv t m a -> AccountMigrationState oldpv pv -> t m (a, AccountMigrationState oldpv pv)
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

    addDelegator delId delAmt delTarget = do
        undefined
