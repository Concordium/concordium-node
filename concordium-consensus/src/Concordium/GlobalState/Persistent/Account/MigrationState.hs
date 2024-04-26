{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.GlobalState.Persistent.Account.MigrationState where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Bool.Singletons
import Data.Kind
import Lens.Micro.Platform

import Concordium.Types
import Concordium.Types.Conditionally

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.GlobalState.Persistent.Cooldown
import Concordium.Utils
import Control.Monad.State.Class

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
      -- | A counter to track the index of the current account as we traverse the account table.
      _currentAccountIndex :: !AccountIndex
    }
makeLenses ''AccountMigrationState

-- | An 'AccountMigrationState' in an initial state.
initialAccountMigrationState :: forall oldpv pv. (IsProtocolVersion oldpv, IsProtocolVersion pv) => AccountMigrationState oldpv pv
initialAccountMigrationState = AccountMigrationState{..}
  where
    _migrationPrePreCooldown = case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor oldpv)) of
        SFalse -> case sSupportsFlexibleCooldown (accountVersion @(AccountVersionFor pv)) of
            SFalse -> CFalse
            STrue -> CTrue Null
        STrue -> CFalse
    _currentAccountIndex = 0

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

-- | Add an account to the set of accounts that should be considered in pre-pre-cooldown as part
--  of migration. This only has an effect when transitioning from a protocol version that does not
--  support flexible cooldown to one that does.
addAccountInPrePreCooldown ::
    (MonadBlobStore (t m)) =>
    AccountMigrationStateTT oldpv pv t m ()
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

-- | Increment the current account index.
nextAccount :: (Monad (t m)) => AccountMigrationStateTT oldpv pv t m ()
nextAccount = currentAccountIndex %=! (+ 1)
