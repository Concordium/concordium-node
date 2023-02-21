module Concordium.KonsensusV1.Consensus where

import Control.Monad.State
import qualified Data.ByteString as BS

import Concordium.Types

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.TransactionVerification as TVer

class MonadMulticast m where
    sendMessage :: BS.ByteString -> m ()

-- uponTimeoutEvent :: ()
-- uponTimeoutEvent = ()

receiveBlockItem :: (MonadState (SkovData pv) m) => BlockItem -> m ()
receiveBlockItem = undefined
