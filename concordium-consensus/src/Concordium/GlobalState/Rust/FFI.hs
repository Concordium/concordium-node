{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, RecordWildCards, FlexibleInstances #-}

module Concordium.GlobalState.Rust.FFI (
  -- * GlobalState
  GlobalStateR
  , GlobalStatePtr
  , getGenesisBlockPointer
  , makeEmptyGlobalState
  , storeBlockPointer

  -- * BlockFields functions
  , BlockFields

  -- * BlockContents functions
  , BlockContents
  , blockContentsFields
  , blockContentsSignature

  -- * PendingBlock functions
  , PendingBlock(..)
  , makePendingBlock
  , makePendingBlockWithContents
  , pendingBlockUpdateTransactions

  -- * BlockPointer functions
  , BlockPointer(..)
  , makeGenesisBlockPointer
  , makeBlockPointer
  , blockPointerExtractBlockFields
  , blockPointerExtractSignature
  , blockPointerExtractBlockContents
  ) where

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SignatureScheme as SSCH
import Concordium.GlobalState.Basic.Block (BakedBlock(..), BlockTransactions(..))
import qualified Concordium.GlobalState.Basic.Block as GSBB (BlockFields(..))
import qualified Concordium.GlobalState.Basic.BlockState as BBS hiding (BlockPointer, makeBlockPointer, makeGenesisBlockPointer)
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState hiding (BlockPointer)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Monad
import Data.ByteString hiding (intercalate, map, head)
import Data.ByteString.Unsafe
import Data.ByteString.Short (toShort)
import Data.FixedByteString hiding (pack, unpack)
import Data.Maybe
import Data.Serialize
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO.Unsafe

utcTimeToTimestamp :: UTCTime -> Int
utcTimeToTimestamp = floor . utcTimeToPOSIXSeconds

timestampToUtc :: Int -> UTCTime
timestampToUtc = posixSecondsToUTCTime . realToFrac

unsafeUseAsCStringCSize :: ByteString -> (CString -> CSize -> IO a) -> IO a
unsafeUseAsCStringCSize b f = unsafeUseAsCStringLen b $ (\(x,y) -> f x (fromIntegral y))

---------------------------
-- * GlobalState FFI calls
---------------------------

foreign import ccall unsafe "global_state_new"
    makeEmptyGlobalStateF :: CString -> CSize -> IO (Ptr GlobalStateR)
foreign import ccall unsafe "&global_state_free"
    freeGlobalStateF :: FunPtr (Ptr GlobalStateR -> IO ())
foreign import ccall unsafe "get_genesis_block_pointer"
   getGenesisBlockPointerF :: Ptr GlobalStateR -> IO (Ptr BlockPointerR)
foreign import ccall unsafe "make_pending_block"
   makePendingBlockF :: Ptr GlobalStateR -> CString -> CSize -> IO (Ptr PendingBlockR)
foreign import ccall unsafe "&pending_block_free"
   freePendingBlockF :: FunPtr (Ptr PendingBlockR -> IO ())
foreign import ccall unsafe "make_block_pointer"
   makeBlockPointerF :: Ptr GlobalStateR -> Ptr PendingBlockR -> Word64 -> Word64 -> IO (Ptr BlockPointerR)
foreign import ccall unsafe "&block_pointer_free"
   freeBlockPointerF :: FunPtr (Ptr BlockPointerR -> IO ())
foreign import ccall unsafe "block_pointer_to_store"
   storeBlockPointerF :: Ptr GlobalStateR -> Ptr BlockPointerR -> IO ()

getGenesisBlockPointer :: Ptr GlobalStateR -> IO (Ptr BlockPointerR)
getGenesisBlockPointer = getGenesisBlockPointerF

storeBlockPointer :: GlobalStatePtr -> BlockPointer -> IO ()
storeBlockPointer gsptr bp = do
  withForeignPtr gsptr $ \gs ->
    withForeignPtr (blockPointerPointer bp) $ \bpp ->
    storeBlockPointerF gs bpp

---------------------------
-- * BlockFields FFI calls
---------------------------

-- |Datatype representing a BlockFields in the Rust side
data BlockFieldsR
-- |Pointer to a BlockFields in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockmetadata-class
--
-- BlockFields holds information for:
--
--    * Parent: hash of the parent block
--    * Baker: id of the baker
--    * Proof: proof for the block creation
--    * Nonce: nonce of the block
--    * LastFinalized: hash of the las finalized block
--
-- BlockFields is required to implement BlockMetadata.
newtype BlockFields = BlockFields (ForeignPtr BlockFieldsR)

-- Will always be 32b
foreign import ccall unsafe "get_pointer_from_block_fields"
    blockFieldsBlockPointerF :: Ptr BlockFieldsR -> IO CString
foreign import ccall unsafe "get_baker_from_block_fields"
    blockFieldsBlockBakerF :: Ptr BlockFieldsR -> IO Word64
foreign import ccall unsafe "get_proof_from_block_fields"
    blockFieldsBlockProofF :: Ptr BlockFieldsR -> Ptr CSize -> IO CString
foreign import ccall unsafe "get_nonce_from_block_fields"
    blockFieldsBlockNonceF :: Ptr BlockFieldsR -> Ptr CSize -> IO CString
foreign import ccall unsafe "get_last_finalized_from_block_fields"
    blockFieldsBlockLastFinalizedF :: Ptr BlockFieldsR -> IO CString
foreign import ccall unsafe "&block_fields_free"
    blockFieldsFreeF :: FunPtr (Ptr BlockFieldsR -> IO ())

blockFieldsBlockPointer :: BlockFields -> BlockHash
blockFieldsBlockPointer (BlockFields b) = unsafePerformIO $
  withForeignPtr b $ \bf -> do
  p <- blockFieldsBlockPointerF bf
  bh_str <- curry packCStringLen p 32
  return . SHA256.Hash . fromByteString $ bh_str

blockFieldsBlockBaker :: BlockFields -> BakerId
blockFieldsBlockBaker (BlockFields b) = BakerId . fromIntegral . unsafePerformIO $ withForeignPtr b blockFieldsBlockBakerF

blockFieldsBlockProof :: BlockFields -> BlockProof
blockFieldsBlockProof (BlockFields b) = unsafePerformIO $
  withForeignPtr b $ \bf ->
  alloca $ \len_ptr -> do
            p <- blockFieldsBlockProofF bf len_ptr
            len <- peek len_ptr
            bp_str <- curry packCStringLen p (fromIntegral len)
            return $! case decode bp_str of
                        Right val -> val
                        Left e -> error e

blockFieldsBlockNonce :: BlockFields -> BlockNonce
blockFieldsBlockNonce (BlockFields b) = unsafePerformIO $
  withForeignPtr b $ \bf ->
  alloca $ \len_ptr -> do
             p <- blockFieldsBlockNonceF bf len_ptr
             len <- peek len_ptr
             bn_str <- curry packCStringLen p (fromIntegral len)
             return $! case decode bn_str of
                         Right val -> val
                         Left e -> error e

blockFieldsBlockLastFinalized :: BlockFields -> BlockHash
blockFieldsBlockLastFinalized (BlockFields b) = unsafePerformIO $
  withForeignPtr b $ \bf -> do
  p <- blockFieldsBlockLastFinalizedF bf
  bn_str <- curry packCStringLen p 32
  return . SHA256.Hash . fromByteString $ bn_str

instance BlockMetadata BlockFields where
    blockPointer b = blockFieldsBlockPointer b
    blockBaker b = blockFieldsBlockBaker b
    blockProof b = blockFieldsBlockProof b
    blockNonce b = blockFieldsBlockNonce b
    blockLastFinalized b = blockFieldsBlockLastFinalized b

---------------------------
-- * BlockContents FFI calls
---------------------------

data BlockContentsR
-- |`BlockContents` holds the inner data of a block. It is the counterpart of `Block` in Rust
-- and can be retrieved from either a `PendingBlock` or a `BlockPointer`. It can be made from the
-- `GenesisData` or from `BakedBlock` content.
--
-- A `BlockContents` can be either a `GenesisData` mirror (with the corresponding associated fields)
-- or a `BakedBlock`. If it is a `BakedBlock` it can return a pointer to the `BlockFields` it holds.
--
-- Therefore the structure is as follows:
--
-- 1. GenesisData
--   * Time
--   * SlotDuration
--   * BirkParameters
--   * BakerAccounts
--   * FinalizationParameters
--   * CryptographicParameters
--   * IdentityProviders
-- 2. BakedBlock
--   * Slot
--   * BlockFields (pointer to Rust struct)
--   * Transactions
--   * Signature
--
-- BlockContents can not be an instance of `BlockMetadata` as it might be the `GenesisData` that doesn't
-- have such data, but it can be an instance of `BlockData`. For mimicking the behavior of `BlockMetadata`
-- we provide a function for `Maybe` retrieving the `BlockFields` if present.
newtype BlockContents = BlockContents (ForeignPtr BlockContentsR)

foreign import ccall unsafe "get_fields_from_block_contents"
    blockContentsFieldsF :: Ptr BlockContentsR -> IO (Ptr BlockFieldsR)
foreign import ccall unsafe "get_slot_from_block_contents"
    blockContentsSlotF :: Ptr BlockContentsR -> IO Slot
foreign import ccall unsafe "get_transactions_from_block_contents"
    blockContentsTransactionsF :: Ptr BlockContentsR -> Ptr CSize -> IO CString
foreign import ccall unsafe "get_signature_from_block_contents"
    blockContentsSignatureF :: Ptr BlockContentsR -> Ptr CSize -> IO CString
foreign import ccall unsafe "block_contents_compare"
    blockContentsCompareF :: Ptr BlockContentsR -> Ptr BlockContentsR -> IO Bool
foreign import ccall unsafe "&block_contents_free"
    blockContentsFreeF :: FunPtr (Ptr BlockContentsR -> IO ())

blockContentsFields :: BlockContents -> Maybe BlockFields
blockContentsFields (BlockContents bc) =
  if p == nullPtr
  then Nothing
  else Just . BlockFields . unsafePerformIO $ newForeignPtr blockFieldsFreeF p
  where
    p = unsafePerformIO $ withForeignPtr bc blockContentsFieldsF

blockContentsSlot :: BlockContents -> Slot
blockContentsSlot (BlockContents bc) = Slot . fromIntegral . unsafePerformIO . withForeignPtr bc $ blockContentsSlotF

blockContentsBlockTransactions :: BlockContents -> [Transaction]
blockContentsBlockTransactions (BlockContents b) = unsafePerformIO $ do
  withForeignPtr b $ \bc ->
    alloca $ \len_ptr -> do
    txs <- blockContentsTransactionsF bc len_ptr
    len <- peek len_ptr
    bt_str <- curry packCStringLen txs (fromIntegral len)
    case runGet getTransactions bt_str of
        Left e  -> fail $ "Couldn't deserialize txs " ++ show e ++ "input: " ++ show bt_str ++ "tried to take " ++ show len ++ " bytes from " ++ show txs
        Right v -> return v

blockContentsSignature :: BlockContents -> Maybe BlockSignature
blockContentsSignature (BlockContents bc) = unsafePerformIO $ do
  withForeignPtr bc $ \b ->
    alloca $ \len_ptr -> do
    sig <- blockContentsSignatureF b len_ptr
    if sig == nullPtr then do
      return Nothing
    else do
      len <- peek len_ptr
      bc_hs <- curry packCStringLen sig $ fromIntegral len
      return . Just . SSCH.Signature . toShort $ bc_hs

instance Eq BlockContents where
  BlockContents a == BlockContents b = unsafePerformIO $
    withForeignPtr a $ (\x ->
    withForeignPtr b $ (\y -> blockContentsCompareF x y))

type instance BlockFieldType BlockContents = BlockFields

instance BlockData BlockContents where
  blockSlot = blockContentsSlot
  blockFields = blockContentsFields
  blockTransactions b = blockContentsBlockTransactions b
  -- The genesis block should never get its signature checked (it has none) and it is not serialized as it is not sent or moved to rust
  verifyBlockSignature key b = case blockContentsSignature b of
    Just bcf -> Sig.verify key (runPut $ blockBodySerialize b) bcf
    Nothing -> True
  putBlock b = blockBodySerialize b  >> (putByteString . encode . fromJust . blockContentsSignature $ b)

blockBodySerialize :: BlockContents -> Put
blockBodySerialize b = do
  case blockContentsFields b of
    Just bcf -> do
        put . blockSlot $ b
        put . blockPointer $ bcf
        put . blockBaker $ bcf
        put . blockProof $ bcf
        put . blockNonce $ bcf
        put . blockLastFinalized $ bcf
        put . map trBareTransaction . blockTransactions $ b
    Nothing -> fail "Unacceptable serialization of genesis data"

---------------------------
-- * PendingBlock FFI calls
---------------------------

-- |Datatype representing a PendingBlock in the Rust side
data PendingBlockR
-- |Datatype holding the pointer to a PendingBlock in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#pendingblock-type
--
-- A PendingBlock in the Rust side must have:
--
--    * Contents (pointer to Rust struct)
--    * Hash
--
-- PendingBlock is required to implement Eq, BlockMetadata, BlockData, HashableTo
-- BlockHash, Show and BlockPendingData
data PendingBlock = PendingBlock {
  pendingBlockPointer     :: ForeignPtr PendingBlockR,
  pendingBlockReceiveTime :: UTCTime
  }

foreign import ccall unsafe "get_contents_from_pending_block"
    pendingBlockContentsF :: Ptr PendingBlockR -> IO (Ptr BlockContentsR)
foreign import ccall unsafe "get_hash_from_pending_block"
    pendingBlockHashF :: Ptr PendingBlockR -> IO CString

pendingBlockHash :: PendingBlock -> BlockHash
pendingBlockHash b = unsafePerformIO $ do
      p <- withForeignPtr (pendingBlockPointer b) pendingBlockHashF
      bh_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bh_str

pendingBlockExtractBlockFields :: PendingBlock -> BlockFields
pendingBlockExtractBlockFields pb = fromJust . blockContentsFields . pendingBlockExtractBlockContents $ pb

pendingBlockExtractBlockContents :: PendingBlock -> BlockContents
pendingBlockExtractBlockContents b = BlockContents . unsafePerformIO $ do
      p <- withForeignPtr (pendingBlockPointer b) pendingBlockContentsF
      newForeignPtr blockContentsFreeF p

-- |Update the list of transactions held in the Rust side
pendingBlockUpdateTransactions :: [Transaction] -> PendingBlock -> IO ()
pendingBlockUpdateTransactions _ _ = return ()

instance Eq PendingBlock where
  a == b = (getHash a :: BlockHash) == (getHash b :: BlockHash)

instance BlockMetadata PendingBlock where
    blockPointer = blockPointer . pendingBlockExtractBlockFields
    blockBaker = blockBaker . pendingBlockExtractBlockFields
    blockProof = blockProof . pendingBlockExtractBlockFields
    blockNonce = blockNonce . pendingBlockExtractBlockFields
    blockLastFinalized = blockLastFinalized . pendingBlockExtractBlockFields

type instance BlockFieldType PendingBlock = BlockFields

instance BlockData PendingBlock where
  blockSlot = blockSlot . pendingBlockExtractBlockContents
  blockFields = blockFields . pendingBlockExtractBlockContents
  blockTransactions = blockTransactions . pendingBlockExtractBlockContents
  verifyBlockSignature v = verifyBlockSignature v . pendingBlockExtractBlockContents
  putBlock = putBlock . pendingBlockExtractBlockContents

instance HashableTo BlockHash PendingBlock where
  getHash = pendingBlockHash

instance Show PendingBlock where
  show = show . (getHash :: PendingBlock -> BlockHash)

instance BlockPendingData PendingBlock where
  blockReceiveTime = pendingBlockReceiveTime

putFullTransaction :: Transaction -> Put
putFullTransaction (Transaction bt _ _ arrival) = do
  put bt -- bt
  put (utcTimeToTimestamp arrival) --8B time

getTransactions :: Get [Transaction]
getTransactions = do
  len <- getWord64be
  replicateM (fromIntegral len) getTransaction

getTransaction :: Get Transaction
getTransaction = do
  bt <- get
  arrival <- get
  let encoded = encode bt
  return $! Transaction bt (Data.ByteString.length encoded) (SHA256.hash encoded) (timestampToUtc arrival)

putFullBlock :: BakedBlock -> Put
putFullBlock b = do
        put (blockSlot b)
        put (blockPointer b)
        put (blockBaker b)
        put (blockProof b)
        put (blockNonce b)
        put (blockLastFinalized b)
        putWord64be (fromIntegral $ Prelude.length (blockTransactions b))
        mapM_ putFullTransaction (blockTransactions b)

-- |Create a PendingBlock
-- This function must initialize the PendingBlockR of the Rust side
makePendingBlock :: GlobalStatePtr -> BakedBlock -> UTCTime -> IO PendingBlock
makePendingBlock gsptr bb pendingBlockReceiveTime = do
  p <- withForeignPtr gsptr $ \g -> unsafeUseAsCStringCSize (runPut $ (putFullBlock $ bb) >> put (bbSignature bb)) $ makePendingBlockF g
  pendingBlockPointer <- newForeignPtr freePendingBlockF p
  return PendingBlock{..}

-- |Create a PendingBlock using a BlockContents ptr
-- This function must initialize the PendingBlockR of the Rust side
makePendingBlockWithContents :: GlobalStatePtr -> BlockContents -> UTCTime -> IO PendingBlock
makePendingBlockWithContents gsptr bb pendingBlockReceiveTime = do
  let bf = fromJust . blockFields $ bb
      b = BakedBlock (blockSlot bb)
                     (GSBB.BlockFields (blockPointer bf) (blockBaker bf) (blockProof bf) (blockNonce bf) (blockLastFinalized bf))
                     (BlockTransactions (blockTransactions bb))
                     (fromJust (blockContentsSignature bb))
  p <- withForeignPtr gsptr $ \g -> unsafeUseAsCStringCSize (runPut $ (putFullBlock $ b) >> put (bbSignature b)) $ makePendingBlockF g
  pendingBlockPointer <- newForeignPtr freePendingBlockF p
  return PendingBlock{..}

---------------------------
-- * BlockPointer FFI calls
---------------------------

-- |Datatype representing a BlockPointer in the Rust side
data BlockPointerR
-- |Pointer to a BlockPointer in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockpointer-type
--
-- A BlockPointer in the Rust side must contain:
--
--    * Contents (pointer to Rust struct)
--    * Height
--    * TransactionCount
--    * Hash
--
-- Some other fields cannot be moved into the Rust side because they either
-- represent another BlockPointers, represent the BlockState or are Time values
data BlockPointer = BlockPointer {
  blockPointerPointer       :: ForeignPtr BlockPointerR,
  blockPointerParent        :: BlockPointer,
  blockPointerLastFinalized :: BlockPointer,
  blockPointerState         :: BlockState' BlockPointer,
  blockPointerReceiveTime   :: UTCTime,
  blockPointerArriveTime    :: UTCTime
  }

foreign import ccall unsafe "get_hash_from_block_pointer"
    blockPointerHashF :: Ptr BlockPointerR -> IO CString
foreign import ccall unsafe "get_height_from_block_pointer"
    blockPointerHeightF :: Ptr BlockPointerR -> IO Word64
foreign import ccall unsafe "get_transaction_count_from_block_pointer"
    blockPointerTransactionCountF :: Ptr BlockPointerR -> IO Word64
foreign import ccall unsafe "get_transaction_energy_cost_from_block_pointer"
    blockPointerTransactionEnergyF :: Ptr BlockPointerR -> IO Word64
foreign import ccall unsafe "get_transaction_size_from_block_pointer"
    blockPointerTransactionSizeF :: Ptr BlockPointerR -> IO Word64
foreign import ccall unsafe "get_contents_from_block_pointer"
    blockPointerContentsF :: Ptr BlockPointerR -> IO (Ptr BlockContentsR)

blockPointerHash :: BlockPointer -> BlockHash
blockPointerHash b = unsafePerformIO $ do
      p <- withForeignPtr (blockPointerPointer b) blockPointerHashF
      bh_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bh_str

blockPointerHeight :: BlockPointer -> BlockHeight
blockPointerHeight b = unsafePerformIO $ do
      withForeignPtr (blockPointerPointer b)
        (\bp -> (BlockHeight . fromIntegral) <$> blockPointerHeightF bp)

blockPointerTransactionCount :: BlockPointer -> Int
blockPointerTransactionCount b = fromIntegral . unsafePerformIO $ do
      withForeignPtr (blockPointerPointer b) blockPointerTransactionCountF

blockPointerTransactionEnergy :: BlockPointer -> Energy
blockPointerTransactionEnergy b = unsafePerformIO $ do
      withForeignPtr (blockPointerPointer b) (\bp -> Energy <$> blockPointerTransactionEnergyF bp)

blockPointerTransactionSize :: BlockPointer -> Int
blockPointerTransactionSize b = fromIntegral . unsafePerformIO $ do
      withForeignPtr (blockPointerPointer b) blockPointerTransactionSizeF

blockPointerExtractBlockFields :: BlockPointer -> Maybe BlockFields
blockPointerExtractBlockFields = blockContentsFields . blockPointerExtractBlockContents

blockPointerExtractBlockContents :: BlockPointer -> BlockContents
blockPointerExtractBlockContents b = BlockContents . unsafePerformIO $ do
      p <- withForeignPtr (blockPointerPointer b) blockPointerContentsF
      newForeignPtr blockContentsFreeF p

blockPointerExtractSignature :: BlockPointer -> Maybe BlockSignature
blockPointerExtractSignature = blockContentsSignature . blockPointerExtractBlockContents

-- BlockPointer is required to implement Eq and Ord, HashableTo BlockHash, BlockData
-- and BlockPointerData (requires Show)
instance Eq BlockPointer where
  a == b = (getHash a :: BlockHash) == (getHash b :: BlockHash)

instance Ord BlockPointer where
  a <= b = (getHash a :: BlockHash) <= (getHash b :: BlockHash)

instance HashableTo BlockHash BlockPointer where
  getHash = blockPointerHash

type instance BlockFieldType BlockPointer = BlockFields

instance BlockData BlockPointer where
  blockSlot = blockSlot . blockPointerExtractBlockContents
  blockFields = blockFields . blockPointerExtractBlockContents
  blockTransactions = blockTransactions . blockPointerExtractBlockContents
  verifyBlockSignature v = verifyBlockSignature v . blockPointerExtractBlockContents
  putBlock = putBlock . blockPointerExtractBlockContents

instance Show BlockPointer where
  show = show . blockPointerHash

instance BlockPointerData BlockPointer where
    type BlockState' BlockPointer = BBS.BlockState
    bpHash = blockPointerHash
    bpParent = blockPointerParent
    bpLastFinalized = blockPointerLastFinalized
    bpHeight = blockPointerHeight
    bpState = blockPointerState
    bpReceiveTime = blockPointerReceiveTime
    bpArriveTime = blockPointerArriveTime
    bpTransactionCount = blockPointerTransactionCount
    bpTransactionsEnergyCost = blockPointerTransactionEnergy
    bpTransactionsSize = blockPointerTransactionSize

-- |Create the BlockPointer for the GenesisBlock
-- This function must initialize the BlockPointerR of the Rust side
makeGenesisBlockPointer :: GlobalStatePtr -> GenesisData ->  BlockState' BlockPointer -> IO BlockPointer
makeGenesisBlockPointer gsptr genData blockPointerState = do
  let blockPointerReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
      blockPointerArriveTime = blockPointerReceiveTime
  p <- withForeignPtr gsptr $ getGenesisBlockPointer
  blockPointerPointer <- newForeignPtr freeBlockPointerF p
  let blockPtr = BlockPointer {blockPointerParent = blockPtr, blockPointerLastFinalized = blockPtr, ..}
  return blockPtr

-- |Creates the BlockPointer for a BakedBlock
-- This function must initialize the BlockPointerR of the Rust side
makeBlockPointer :: GlobalStatePtr ->
                    PendingBlock ->
                    BlockPointer ->
                    BlockPointer ->
                    BlockState'
                    BlockPointer ->
                    UTCTime ->
                    Energy ->
                    IO BlockPointer
makeBlockPointer gsptr b blockPointerParent blockPointerLastFinalized blockPointerState blockPointerArriveTime (Energy energy) =
  do
    p <-
      withForeignPtr gsptr $ \x ->
      withForeignPtr (pendingBlockPointer b) $ \y ->
      makeBlockPointerF x y ((+1) . theBlockHeight . bpHeight $ blockPointerParent) energy
    blockPointerPointer <- newForeignPtr freeBlockPointerF p
    let blockPointerReceiveTime = pendingBlockReceiveTime b
    return $ BlockPointer {..}

---------------------------
-- * FFI Types
---------------------------

-- |Datatype representing the GlobalState in Rust
data GlobalStateR
-- |Pointer to the GlobalState in Rust
type GlobalStatePtr = ForeignPtr GlobalStateR

makeEmptyGlobalState :: GenesisData -> IO GlobalStatePtr
makeEmptyGlobalState gendata = do
  gsptr <- unsafeUseAsCStringCSize (encode gendata) makeEmptyGlobalStateF
  newForeignPtr freeGlobalStateF gsptr
