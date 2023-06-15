{-# LANGUAGE GADTSyntax #-}

-- |This module provides multi-version catch-up message types and their serialization.
module Concordium.Types.CatchUp where

import Control.Applicative
import Data.Serialize

import Concordium.Common.Version

import qualified Concordium.KonsensusV1.Consensus.CatchUp.Types as V1
import qualified Concordium.Skov.CatchUp.Types as V0

data VersionedCatchUpStatus where
    -- |A special catch-up response for when we have no genesis at the given index.
    VersionedCatchUpStatusNoGenesis :: VersionedCatchUpStatus
    -- |A catch-up status message in consensus version 0.
    VersionedCatchUpStatusV0 :: !V0.CatchUpStatus -> VersionedCatchUpStatus
    -- |A catch-up status message in consensus version 1.
    VersionedCatchUpStatusV1 :: !V1.CatchUpMessage -> VersionedCatchUpStatus
    deriving (Show)

-- |Serialize a 'VersionedCatchUpStatus' message.
putVersionedCatchUpStatus :: Putter VersionedCatchUpStatus
putVersionedCatchUpStatus VersionedCatchUpStatusNoGenesis = do
    putVersion 0
    -- This is serialized as a version 0 catch-up status message with the tag byte 6.
    -- Regular version 0 catch-up status messages have tag bytes from 0 to 3.
    putWord8 6
putVersionedCatchUpStatus (VersionedCatchUpStatusV0 cus) = do
    putVersion 0
    put cus
putVersionedCatchUpStatus (VersionedCatchUpStatusV1 cus) = do
    putVersion 1
    put cus

-- |Deserialize a 'VersionedCatchUpStatus' message.
getVersionedCatchUpStatus :: Get VersionedCatchUpStatus
getVersionedCatchUpStatus =
    getVersion >>= \case
        0 -> getNoGenesis <|> (VersionedCatchUpStatusV0 <$> get)
        1 -> VersionedCatchUpStatusV1 <$> get
        version -> fail $ "Unsupported catch-up status message version " ++ show version ++ "."
  where
    getNoGenesis = do
        h <- getWord8
        if h == 6
            then return VersionedCatchUpStatusNoGenesis
            else fail "Expected no-genesis catch-up status"

instance Serialize VersionedCatchUpStatus where
    put = putVersionedCatchUpStatus
    get = getVersionedCatchUpStatus

-- |Determine whether the catch-up message is a request.
isCatchUpRequest :: VersionedCatchUpStatus -> Bool
isCatchUpRequest (VersionedCatchUpStatusV0 cus) = V0.cusIsRequest cus
isCatchUpRequest (VersionedCatchUpStatusV1 V1.CatchUpRequestMessage{}) = True
isCatchUpRequest _ = False
