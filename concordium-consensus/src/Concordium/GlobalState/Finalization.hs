{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.GlobalState.Finalization where

import Data.Serialize
import Data.Word

import qualified Concordium.Crypto.BlockSignature as Sig

import Concordium.Types

newtype FinalizationIndex = FinalizationIndex {theFinalizationIndex :: Word64} deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Serialize)


data FinalizationProof = FinalizationProof [(Word32, Sig.Signature)]
    deriving (Eq)

emptyFinalizationProof :: FinalizationProof
emptyFinalizationProof = FinalizationProof []


data FinalizationRecord = FinalizationRecord {
    finalizationIndex :: FinalizationIndex,
    finalizationBlockPointer :: BlockHash,
    finalizationProof :: FinalizationProof,
    finalizationDelay :: BlockHeight
} deriving (Eq)
instance Serialize FinalizationRecord where
    put FinalizationRecord{..} = do
        put finalizationIndex
        put finalizationBlockPointer
        let FinalizationProof sigs = finalizationProof
        put sigs
        put finalizationDelay
    get = do
        finalizationIndex <- get
        finalizationBlockPointer <- get
        sigs <- get
        let finalizationProof = FinalizationProof sigs
        finalizationDelay <- get
        return $ FinalizationRecord{..}

instance Show FinalizationRecord where
    show FinalizationRecord{..} = "FinalizationRecord{index=" ++ show (theFinalizationIndex finalizationIndex)  ++ ", block=" ++ show finalizationBlockPointer ++ "}"