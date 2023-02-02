-- |This module provides a simple in-memory version of the low-level tree state.
module Concordium.KonsensusV1.TreeState.LowLevel.Memory where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.HashMap.Strict as HM
import Data.IORef
import Data.Map.Strict as Map

import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.Types
import Concordium.Types

data LowLevelDB pv = LowLeveDB
    { lldbBlockHashes :: !(HM.HashMap BlockHash BlockHeight),
      lldbBlocks :: !(Map.Map BlockHeight (StoredBlock pv)),
      lldbTransactions :: !(HM.HashMap TransactionHash FinalizedTransactionStatus),
      lldbLatestFinalizationEntry :: !(Maybe FinalizationEntry),
      lldbRoundStatus :: !RoundStatus
    }

class HasMemoryLLDB pv r | r -> pv where
    theMemoryLLDB :: r -> IORef (LowLevelDB pv)

readLLDB :: (MonadReader r m, HasMemoryLLDB pv r, MonadIO m) => m (LowLevelDB pv)
readLLDB = liftIO . readIORef =<< asks theMemoryLLDB

-- -instance