module Concordium.KonsensusV1.Consensus where

import qualified Data.ByteString as BS

import Concordium.Types

class MonadMulticast m where
    sendMessage :: BS.ByteString -> m ()



-- uponTimeoutEvent :: ()
-- uponTimeoutEvent = ()

