{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.Scheduler.ProtocolLevelTokens.Kernel where

import Concordium.GlobalState.Classes
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Scheduler.Environment
import Concordium.Types
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Proxy

class (Monad m) => ProtocolLevelTokenKernel m where
    pltTransfer :: TokenIndex -> AccountAddress -> AccountAddress -> TokenRawAmount -> m ()
    pltMint :: TokenIndex -> AccountAddress -> TokenRawAmount -> m ()
    pltBurn :: TokenIndex -> AccountAddress -> TokenRawAmount -> m ()

instance (Monad (t m), MonadTrans t, ProtocolLevelTokenKernel m) => ProtocolLevelTokenKernel (MGSTrans t m) where
    pltTransfer tIx accFrom accTo = lift . pltTransfer tIx accFrom accTo
    pltMint tIx acc = lift . pltMint tIx acc
    pltBurn tIx acc = lift . pltBurn tIx acc
    {-# INLINE pltTransfer #-}
    {-# INLINE pltMint #-}
    {-# INLINE pltBurn #-}

instance {-# OVERLAPPABLE #-} (Monad m, SchedulerMonad m) => ProtocolLevelTokenKernel m where
    pltTransfer tIx accFrom accTo amount = do
        -- mFromAccIx <- getAccountIndex accFrom
        -- mToAccIx <- getAccountIndex accTo
        -- mFromToken <- getAccountToken fromIx tIx
        -- mToToken <- getAccountToken toIx tIx

        -- Construct a changeset corresponding to the transfer for the accounts
        -- Implement a change to the changeset in Scheduler/Environment
        let cs = emptyCS (Proxy @m)
        commitChanges cs

    pltMint tIx acc amount = undefined
    pltBurn tIx acc amount = undefined

deriving via (MGSTrans MaybeT m) instance (ProtocolLevelTokenKernel m) => ProtocolLevelTokenKernel (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance (ProtocolLevelTokenKernel m) => ProtocolLevelTokenKernel (ExceptT e m)
