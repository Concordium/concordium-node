{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.Scheduler.ProtocolLevelTokens.Kernel where

import Concordium.GlobalState.Account
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Scheduler.Environment
import Concordium.Types
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as HMap
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

instance {-# OVERLAPPABLE #-} (Monad m, MonadFail m, SchedulerMonad m) => ProtocolLevelTokenKernel m where
    pltTransfer tIx accFrom accTo amount = do
        fromAccIx <-
            getAccountIndex accFrom >>= \case
                Nothing -> fail $ "Unknown account address: " ++ show accFrom
                Just fromAccIx -> return fromAccIx
        toAccIx <-
            getAccountIndex accTo >>= \case
                Nothing -> fail $ "Unknonw account address: " ++ show accTo
                Just toAccIx -> return toAccIx

        let fromAccUpd = [(tIx, TokenAccountStateDelta{tasBalanceDelta = Just $ TokenAmountDelta (-1 * fromIntegral (theTokenRawAmount amount)), tasModuleStateDelta = []})]
        let toAccUpd = [(tIx, TokenAccountStateDelta{tasBalanceDelta = Just $ TokenAmountDelta (fromIntegral (theTokenRawAmount amount)), tasModuleStateDelta = []})]
        updatePLTState toAccIx toAccUpd (emptyCS (Proxy @m))
            >>= updatePLTState fromAccIx fromAccUpd
            >>= commitChanges

    pltMint tIx acc amount = error "TODO (drsk) implement pltMint"
    pltBurn tIx acc amount = error "TODO (drsk) implement pltBurn"

deriving via (MGSTrans MaybeT m) instance (ProtocolLevelTokenKernel m) => ProtocolLevelTokenKernel (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance (ProtocolLevelTokenKernel m) => ProtocolLevelTokenKernel (ExceptT e m)
