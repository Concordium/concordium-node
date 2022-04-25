{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Concordium.GlobalState.Finalization where

import Data.Serialize
import Data.Word
import qualified Data.ByteString as BS
import Control.Exception (assert)
import Control.Monad

import Concordium.Common.Version
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types
import Concordium.Afgjort.Types


data FinalizationProof = FinalizationProof {
    finalizationProofParties :: ![Party],
    finalizationProofSignature :: !Bls.Signature
    }
    deriving (Eq)

putLength :: Putter Int
putLength = putWord32be . fromIntegral

getLength :: Get Int
getLength = fromIntegral <$> getWord32be

instance Serialize FinalizationProof where
  put (FinalizationProof parties sig) =
    putLength (length parties) <>
    mapM_ putParty parties <>
    put sig

  get = do
    l <- getLength
    FinalizationProof <$> replicateM l getParty <*> get

emptyFinalizationProof :: FinalizationProof
emptyFinalizationProof = FinalizationProof [] Bls.emptySignature    -- this signature currently won't verify
                                                                    -- perhaps it should


data FinalizationRecord = FinalizationRecord {
    finalizationIndex :: !FinalizationIndex,
    finalizationBlockPointer :: !BlockHash,
    finalizationProof :: !FinalizationProof,
    finalizationDelay :: !BlockHeight
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


-- |Read a finalization record according to the V0 format.
getFinalizationRecordV0 :: Get FinalizationRecord
getFinalizationRecordV0 = get

-- |Serialize a finalization record according to the V0 format.
putFinalizationRecordV0 :: FinalizationRecord -> Put
putFinalizationRecordV0 = put

-- |Deserialize a versioned finalization record.
-- Read the version and decide how to parse the remaining data based on the
-- version.
--
-- Currently only supports version 0
getExactVersionedFinalizationRecord :: Get FinalizationRecord
getExactVersionedFinalizationRecord =
  getVersion >>= \case
     0 -> getFinalizationRecordV0
     n -> fail $ "Unsupported FinalizationRecord version: " ++ show n

-- |Serialize a Finalization Record with a version according to the V0 format.
-- In contrast to 'putFinalizationRecordV0' this function also prepends the version.
putVersionedFinalizationRecordV0 :: FinalizationRecord -> Put
putVersionedFinalizationRecordV0 fpm = putVersion 0 <> putFinalizationRecordV0 fpm


instance Show FinalizationRecord where
    show FinalizationRecord{..} = "FinalizationRecord{index=" ++ show (theFinalizationIndex finalizationIndex)  ++ ", block=" ++ show finalizationBlockPointer ++ "}"

data BlockFinalizationData
    = NoFinalizationData
    | BlockFinalizationData !FinalizationRecord
    deriving (Eq)

instance Show BlockFinalizationData where
    show NoFinalizationData = "NoFinalizationData"
    show (BlockFinalizationData fr) = show fr

instance Serialize BlockFinalizationData where
    put NoFinalizationData = putWord32be 0
    put (BlockFinalizationData fr) = do
        let frenc = encode fr
            frencLen = BS.length frenc
        assert (frencLen < fromIntegral (maxBound :: Word32)) $
            putWord32be $ fromIntegral (BS.length frenc)
        putByteString frenc
    get = do
        len <- getWord32be
        if len == 0 then
            return NoFinalizationData
        else do
            startPos <- bytesRead
            fr <- get
            endPos <- bytesRead
            when (endPos - startPos /= fromIntegral len) $
                fail $ "Finalization record had incorrect length: expected=" ++ show len ++ ", actual=" ++ show (endPos - startPos)
            return (BlockFinalizationData fr)
