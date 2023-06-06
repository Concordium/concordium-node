{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Concordium.Skov.CatchUp.Types where

import Control.Monad
import Data.Serialize

import Concordium.Common.Version
import Concordium.Types

type CatchUpStatus = CatchUpStatusV0

data CatchUpStatusV0
    = CatchUpStatus
        { -- |If this flag is set, the recipient is expected to send any
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
        }
    | -- |A special response when a peer does not have a (re)genesis block.
      NoGenesisCatchUpStatus
    deriving (Show)
instance Serialize CatchUpStatusV0 where
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
    put NoGenesisCatchUpStatus = putWord8 6
    get = do
        getWord8 >>= \case
            0 -> getNormal False False
            1 -> getNormal True False
            2 -> getNormal False True
            3 -> getNormal True True
            6 -> return NoGenesisCatchUpStatus
            _ -> fail "Invalid flags"
      where
        getNormal cusIsRequest cusIsResponse = do
            cusLastFinalizedBlock <- get
            cusLastFinalizedHeight <- get
            cusLeaves <- get
            cusBranches <- if cusIsRequest then get else return []
            return CatchUpStatus{..}

-- |Catch up status for consensus version 1.
data CatchUpStatusV1 = CatchUpStatusV1
    deriving (Show)

data VersionedCatchUpStatus where
    VersionedCatchUpStatusV0 :: !CatchUpStatusV0 -> VersionedCatchUpStatus
    VersionedCatchUpStatusV1 :: !CatchUpStatusV1 -> VersionedCatchUpStatus
    deriving (Show)

-- |Deserialize a 'CatchUpStatus' message with a version header.
getExactVersionedCatchUpStatus :: Get VersionedCatchUpStatus
getExactVersionedCatchUpStatus = do
    version <- getVersion
    case version of
        0 -> VersionedCatchUpStatusV0 <$> get
        1 -> return $ VersionedCatchUpStatusV1 CatchUpStatusV1
        _ -> fail $ "Unsupported catch-up status message version " ++ show version ++ "."

-- |Serialize a 'CatchUpStatus' message with a version header.
putVersionedCatchUpStatus :: Putter VersionedCatchUpStatus
putVersionedCatchUpStatus (VersionedCatchUpStatusV0 cus) = do
    putVersion 0
    put cus
putVersionedCatchUpStatus (VersionedCatchUpStatusV1 _) = do
    putVersion 1
    -- TODO: Implement catch-up for consensus version 1. Issue #826
    return ()

instance Serialize VersionedCatchUpStatus where
    put = putVersionedCatchUpStatus
    get = getExactVersionedCatchUpStatus
