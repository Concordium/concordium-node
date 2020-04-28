module Concordium.Skov.CatchUp.Types where

import Data.Serialize
import Control.Monad

import Concordium.Types

data CatchUpStatus = CatchUpStatus {
    -- |If this flag is set, the recipient is expected to send any
    -- blocks and finalization records the sender may be missing,
    -- followed by a CatchUpStatus message with the response flag
    -- set.
    cusIsRequest :: Bool,
    -- |If this flag is set, this message concludes a catch-up
    -- response. (The receiver should not expect to be sent
    -- further catch-up blocks unless it sends a further catch-up
    -- request.)
    cusIsResponse :: Bool,
    -- |Hash of the sender's last finalized block.
    cusLastFinalizedBlock :: BlockHash,
    -- |Height of the sender's last finalized block.
    cusLastFinalizedHeight :: BlockHeight,
    -- |Hashes of all live non-finalized leaf blocks.
    cusLeaves :: [BlockHash],
    -- |Hashes of all live non-finalized non-leaf blocks, if the message
    -- is a request.
    cusBranches :: [BlockHash]
} deriving (Show)
instance Serialize CatchUpStatus where
    put CatchUpStatus{..} = do
        putWord8 $ case (cusIsRequest, cusIsResponse) of
            (False, False) -> 0
            (True, False) -> 1
            (False, True) -> 2
            (True, True) -> 3
        put cusLastFinalizedBlock
        put cusLastFinalizedHeight
        put cusLeaves
        when cusIsRequest $ put cusBranches
    get = do
        (cusIsRequest, cusIsResponse) <- getWord8 >>= \case
            0 -> return (False, False)
            1 -> return (True, False)
            2 -> return (False, True)
            3 -> return (True, True)
            _ -> fail "Invalid flags"
        cusLastFinalizedBlock <- get
        cusLastFinalizedHeight <- get
        cusLeaves <- get
        cusBranches <- if cusIsRequest then get else return []
        return CatchUpStatus{..}
