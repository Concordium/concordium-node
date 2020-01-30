{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.GlobalState.Finalization where

import Data.Serialize
import Data.Word

import Control.Monad

import qualified Concordium.Crypto.BlsSignature as Bls

import Concordium.Types

newtype FinalizationIndex = FinalizationIndex {theFinalizationIndex :: Word64} deriving (Eq, Ord, Num, Real, Enum, Integral, Show)

instance Serialize FinalizationIndex where
  put (FinalizationIndex w) = putWord64be w
  get = FinalizationIndex <$> getWord64be


data FinalizationProof = FinalizationProof ([Word32], Bls.Signature)
    deriving (Eq)


putLength :: Putter Int
putLength = putWord32be . fromIntegral

getLength :: Get Int
getLength = fromIntegral <$> getWord32be

instance Serialize FinalizationProof where
  put (FinalizationProof (parties, sig)) =
    putLength (length parties) <>
    mapM_ putWord32be parties <>
    put sig

  get = do
    l <- getLength
    FinalizationProof <$> (getTwoOf (replicateM l getWord32be) get)

emptyFinalizationProof :: FinalizationProof
emptyFinalizationProof = FinalizationProof ([], Bls.emptySignature) -- this signature currently won't verify
                                                                    -- perhaps it should


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
        put finalizationProof
        put finalizationDelay
    get = do
        finalizationIndex <- get
        finalizationBlockPointer <- get
        finalizationProof <- get
        finalizationDelay <- get
        return $ FinalizationRecord{..}

instance Show FinalizationRecord where
    show FinalizationRecord{..} = "FinalizationRecord{index=" ++ show (theFinalizationIndex finalizationIndex)  ++ ", block=" ++ show finalizationBlockPointer ++ "}"
