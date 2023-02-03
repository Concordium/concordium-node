{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImportQualifiedPost #-}

module Concordium.KonsensusV1.TreeState.LowLevel.LMDB where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Serialize as S
import Database.LMDB.Raw
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO.Unsafe

import Concordium.Common.Version
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types

import Concordium.GlobalState.LMDB.Helpers
import Concordium.KonsensusV1.TreeState.LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

-- |Size of 'CSize'.
sizeOfCSize :: Int
sizeOfCSize = sizeOf (undefined :: CSize)

-- |Block store for finalized blocks by height.
newtype BlockStore (pv :: ProtocolVersion) = BlockStore MDB_dbi'

instance IsProtocolVersion pv => MDBDatabase (BlockStore pv) where
    type DBKey (BlockStore pv) = BlockHeight
    type DBValue (BlockStore pv) = StoredBlock pv
    encodeKey _ (BlockHeight h) = unsafePerformIO $ do
        ptr <- malloc
        poke ptr (CSize h)
        BS.unsafePackMallocCStringLen (castPtr ptr, sizeOfCSize)
    withKey _ (BlockHeight h) f = alloca $ \ptr -> do
        poke ptr (CSize h)
        f $ MDB_val (fromIntegral sizeOfCSize) (castPtr ptr)
    decodeKey _ (MDB_val sz ptr)
        | sz == fromIntegral sizeOfCSize = do
            (CSize h) <- peek (castPtr ptr)
            return (Right (BlockHeight h))
        | otherwise = return (Left $ "decoded block store key with invalid size: " ++ show sz)

-- |Index mapping block hashes to block heights.
newtype BlockHashIndex = BlockHashIndex MDB_dbi'

instance MDBDatabase BlockHashIndex where
    type DBKey BlockHashIndex = BlockHash
    encodeKey _ = Hash.hashToByteString . blockHash
    type DBValue BlockHashIndex = BlockHeight

-- |A transaction status store table. A @TransactionStatusStore@ stores
-- 'FinalizedTransactionStatus'es indexed by 'TransactionHash'.
newtype TransactionStatusStore = TransactionStatusStore MDB_dbi'

instance MDBDatabase TransactionStatusStore where
    type DBKey TransactionStatusStore = TransactionHash
    type DBValue TransactionStatusStore = FinalizedTransactionStatus

data MetadataKey =
    MKVersion
    | MKRoundStatus
    | MKLatestFinalizationEntry
    deriving (Enum)

data MetadataVal =
    MVVersion !VersionMetadata
    | MVRoundStatus !RoundStatus
    | MVLatestFinalizationEntry !FinalizationEntry

instance S.Serialize MetadataVal where
    put (MVVersion ver) = do
        S.putWord64be 0
        S.put ver
    put (MVRoundStatus rs) = do
        S.putWord64be 1
        S.put rs
    put (MVLatestFinalizationEntry lfe) = do
        S.putWord64be 2
        S.put lfe
    get = S.getWord64be >>= \case
        0 -> MVVersion <$> S.get
        1 -> MVRoundStatus <$> S.get
        2 -> MVLatestFinalizationEntry <$> S.get
        _ -> fail "Unsupported metadata value type"

-- |The metadata store table.
-- This table is for storing version-related information.
newtype MetadataStore = MetadataStore MDB_dbi'

instance MDBDatabase MetadataStore where
    type DBKey MetadataStore = MetadataKey
    encodeKey _ k = unsafePerformIO $ do
        ptr <- malloc
        poke ptr (fromIntegral (fromEnum k) :: CSize)
        BS.unsafePackMallocCStringLen (castPtr ptr, sizeOfCSize)
    withKey _ k f = alloca $ \ptr -> do
        poke ptr (fromIntegral (fromEnum k) :: CSize)
        f $ MDB_val (fromIntegral sizeOfCSize) (castPtr ptr)
    decodeKey _ (MDB_val sz ptr)
        | sz == fromIntegral sizeOfCSize = do
            (CSize h) <- peek (castPtr ptr)
            return (Right (toEnum (fromIntegral h)))
        | otherwise = return (Left $ "decoded block store key with invalid size: " ++ show sz)
    type DBValue MetadataStore = MetadataVal

data VersionMetadata = VersionMetadata
    { -- |Version signifier for the database itself.
      vmDatabaseVersion :: !Version,
      -- |Protocol version, which may impact the storage of blocks/finalization records
      -- independently of the database version.
      vmProtocolVersion :: !ProtocolVersion
    }
    deriving (Eq)

instance Show VersionMetadata where
    show VersionMetadata{..} =
        "{databaseVersion: "
            ++ show vmDatabaseVersion
            ++ ", protocolVersion: "
            ++ show vmProtocolVersion
            ++ "}"

instance S.Serialize VersionMetadata where
    put VersionMetadata{..} = do
        S.put vmDatabaseVersion
        S.put vmProtocolVersion
    get = do
        vmDatabaseVersion <- S.get
        vmProtocolVersion <- S.get
        return VersionMetadata{..}

