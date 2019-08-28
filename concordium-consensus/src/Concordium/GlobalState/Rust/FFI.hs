{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, RecordWildCards #-}

module Concordium.GlobalState.Rust.FFI (
  -- * GlobalState
  GlobalStatePtr
  , getGenesisBlockPointer

  -- * BlockFields functions
  , BlockFields

  -- * BlockContents functions
  , BlockContents
  , blockContentsFields
  , blockContentsSignature

  -- * PendingBlock functions
  , PendingBlock(..)
  , makePendingBlock
  , pendingBlockUpdateTransactions

  -- * BlockPointer functions
  , BlockPointer(..)
  , makeGenesisBlockPointer
  , makeBlockPointer
  , blockPointerExtractBlockFields
  ) where

import qualified Concordium.Crypto.SHA256 as SHA256
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.SignatureScheme as SSCH
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Basic.Block (BakedBlock, Block (NormalBlock))
import qualified Concordium.GlobalState.Basic.BlockState as BBS hiding (BlockPointer, makeBlockPointer, makeGenesisBlockPointer)
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockState hiding (BlockPointer)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.Types
import Concordium.Types.HashableTo
import Data.ByteString hiding (intercalate, unpack)
import Data.ByteString.Short (toShort)
import Data.FixedByteString hiding (pack, unpack)
import Data.List
import Data.Maybe
import Data.Serialize
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Word
import Foreign.C
import Foreign.Ptr
import System.IO.Unsafe

---------------------------
-- * GlobalState FFI calls
---------------------------

foreign import ccall unsafe "get_genesis_block_pointer"
   getGenesisBlockPointerF :: GlobalStatePtr -> Ptr BlockPointerR
foreign import ccall unsafe "make_pending_block"
    makePendingBlockF :: GlobalStatePtr -> CString -> Int -> IO (Ptr PendingBlockR)
foreign import ccall unsafe "make_block_pointer"
    makeBlockPointerF :: GlobalStatePtr -> Ptr PendingBlockR -> Word64 -> Ptr BlockPointerR
foreign import ccall unsafe "make_genesis_data"
    makeGenesisDataF :: GlobalStatePtr -> CString -> Int -> IO (Ptr BlockPointerR)

-- This function should actually return a proper BlockPointer and for that we need somewhere to
-- get the State of the genesis from (or establish a state for the Genesis). The rest of the fields can
-- be retrieved without issues
getGenesisBlockPointer :: GlobalStatePtr -> Ptr BlockPointerR
getGenesisBlockPointer = getGenesisBlockPointerF

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
newtype BlockFields = BlockFields (Ptr BlockFieldsR)

-- Will always be 32b
foreign import ccall unsafe "block_fields_get_pointer"
    blockFieldsBlockPointerF :: BlockFields -> CString
foreign import ccall unsafe "block_fields_get_baker"
    blockFieldsBlockBakerF :: BlockFields -> Int
foreign import ccall unsafe "block_fields_get_proof"
    blockFieldsBlockProofF :: BlockFields -> CString
foreign import ccall unsafe "block_fields_get_proof_length"
    blockFieldsBlockProofLengthF :: BlockFields -> Word64
foreign import ccall unsafe "block_fields_get_nonce"
    blockFieldsBlockNonceF :: BlockFields -> CString
foreign import ccall unsafe "block_fields_get_nonce_length"
    blockFieldsBlockNonceLengthF :: BlockFields -> Word64
foreign import ccall unsafe "block_fields_get_last_finalized"
    blockFieldsBlockLastFinalizedF :: BlockFields -> CString

blockFieldsBlockPointer :: BlockFields -> BlockHash
blockFieldsBlockPointer b =
  let
    p = blockFieldsBlockPointerF b
  in
    unsafePerformIO $ do
      bh_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bh_str

blockFieldsBlockBaker :: BlockFields -> BakerId
blockFieldsBlockBaker = BakerId . fromIntegral . blockFieldsBlockBakerF

blockFieldsBlockProof :: BlockFields -> BlockProof
blockFieldsBlockProof b =
  let
    p = blockFieldsBlockProofF b
    l = blockFieldsBlockProofLengthF b
  in
    unsafePerformIO $ do
      bp_str <- curry packCStringLen p (fromIntegral l)
      return . VRF.Proof . fromByteString $ bp_str

blockFieldsBlockNonce :: BlockFields -> BlockNonce
blockFieldsBlockNonce b =
  let
    p = blockFieldsBlockNonceF b
    l = blockFieldsBlockNonceLengthF b
  in
    unsafePerformIO $ do
      bn_str <- curry packCStringLen p (fromIntegral l)
      return . VRF.Proof . fromByteString $ bn_str

blockFieldsBlockLastFinalized :: BlockFields -> BlockHash
blockFieldsBlockLastFinalized b =
  let
    p = blockFieldsBlockLastFinalizedF b
  in
    unsafePerformIO $ do
      bn_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bn_str

instance BlockMetadata BlockFields where
    blockPointer = blockFieldsBlockPointer
    blockBaker = blockFieldsBlockBaker
    blockProof = blockFieldsBlockProof
    blockNonce = blockFieldsBlockNonce
    blockLastFinalized = blockFieldsBlockLastFinalized

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
newtype BlockContents = BlockContents (Ptr BlockContentsR)

foreign import ccall unsafe "block_contents_fields"
    blockContentsFieldsF :: BlockContents -> Ptr BlockFieldsR
foreign import ccall unsafe "block_contents_slot"
    blockContentsSlotF :: BlockContents -> Int
foreign import ccall unsafe "block_contents_transactions"
    blockContentsTransactionsF :: BlockContents -> CString
foreign import ccall unsafe "block_contents_transactions_length"
    blockContentsTransactionsLengthF :: BlockContents -> Word64
foreign import ccall unsafe "block_contents_signature"
    blockContentsSignatureF :: BlockContents -> CString
foreign import ccall unsafe "block_contents_signature_length"
    blockContentsSignatureLengthF :: BlockContents -> Word64
foreign import ccall unsafe "block_contents_compare"
    blockContentsCompareF :: BlockContents -> BlockContents -> Bool

blockContentsFields :: BlockContents -> Maybe BlockFields
blockContentsFields bc = if p == nullPtr then Nothing else Just $ BlockFields p
  where
    p = blockContentsFieldsF bc

blockContentsSlot :: BlockContents -> Slot
blockContentsSlot = Slot . fromIntegral . blockContentsSlotF

blockContentsBlockTransactions :: BlockContents -> [Transaction]
blockContentsBlockTransactions b =
  let
    p = blockContentsTransactionsF b
    l = blockContentsTransactionsLengthF b
  in
    unsafePerformIO $ do
      bt_str <- curry packCStringLen p (fromIntegral l)
      case decode bt_str of
        Left e  -> fail $ "Couldn't deserialize txs " ++ show e
        Right v -> return v

blockContentsSignature :: BlockContents -> Maybe BlockSignature
blockContentsSignature bc = if p == nullPtr then Nothing
  else Just . SSCH.Signature . toShort . unsafePerformIO $
       curry packCStringLen p (fromIntegral l)
  where
    p = blockContentsSignatureF bc
    l = blockContentsSignatureLengthF bc

instance Eq BlockContents where
  a == b = blockContentsCompareF a b

type instance BlockFieldType BlockContents = BlockFields

instance BlockData BlockContents where
  blockSlot = blockContentsSlot
  blockFields = blockContentsFields
  blockTransactions = blockContentsBlockTransactions
  verifyBlockSignature key b = Sig.verify key (runPut $ blockBodySerialize b) (fromJust . blockContentsSignature $ b)
  putBlock b = blockBodySerialize b  >> (putByteString . encode . fromJust . blockContentsSignature $ b)

-- Improve for serializing also de genesis data
blockBodySerialize :: BlockContents -> Put
blockBodySerialize b = do
        put . blockSlot $ b
        put . blockPointer . fromJust . blockContentsFields $ b
        put . blockBaker . fromJust . blockContentsFields $ b
        put . blockProof . fromJust . blockContentsFields $ b
        put . blockNonce . fromJust . blockContentsFields $ b
        put . blockLastFinalized . fromJust . blockContentsFields $ b
        put . blockTransactions $ b


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
  pendingBlockPointer     :: Ptr PendingBlockR,
  pendingBlockReceiveTime:: UTCTime
  }

foreign import ccall unsafe "pending_block_get_contents"
    pendingBlockContentsF :: Ptr PendingBlockR -> Ptr BlockContentsR
foreign import ccall unsafe "pending_block_get_hash"
    pendingBlockHashF :: Ptr PendingBlockR -> CString
foreign import ccall unsafe "pending_block_update_transactions"
    pendingBlockUpdateTransactionsF :: Ptr PendingBlockR -> CString -> Int -> IO ()

pendingBlockHash :: PendingBlock -> BlockHash
pendingBlockHash b =
  let p = pendingBlockHashF . pendingBlockPointer $ b in
    unsafePerformIO $ do
      bh_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bh_str

pendingBlockExtractBlockFields :: PendingBlock -> BlockFields
pendingBlockExtractBlockFields = fromJust . blockContentsFields . pendingBlockExtractBlockContents

pendingBlockExtractBlockContents :: PendingBlock -> BlockContents
pendingBlockExtractBlockContents = BlockContents . pendingBlockContentsF . pendingBlockPointer

-- |Update the list of transactions held in the Rust side
pendingBlockUpdateTransactions :: [Transaction] -> PendingBlock -> IO ()
pendingBlockUpdateTransactions txs pb =
  useAsCStringLen (encode txs) $ uncurry (pendingBlockUpdateTransactionsF (pendingBlockPointer pb))

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

-- |Create a PendingBlock
-- This function must initialize the PendingBlockR of the Rust side
makePendingBlock :: GlobalStatePtr -> BakedBlock -> UTCTime -> PendingBlock
makePendingBlock gsptr bb pendingBlockReceiveTime = thePendingBlock
  where
    thePendingBlock = PendingBlock {..}
    pendingBlockPointer = unsafePerformIO $ useAsCStringLen (encode . NormalBlock $ bb) $ uncurry (makePendingBlockF gsptr)

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
  blockPointerPointer       :: Ptr BlockPointerR,
  blockPointerParent        :: BlockPointer,
  blockPointerLastFinalized :: BlockPointer,
  blockPointerState         :: BlockState' BlockPointer,
  blockPointerReceiveTime   :: UTCTime,
  blockPointerArriveTime    :: UTCTime
  }

foreign import ccall unsafe "block_pointer_get_hash"
    blockPointerHashF :: Ptr BlockPointerR -> CString
foreign import ccall unsafe "block_pointer_get_height"
    blockPointerHeightF :: Ptr BlockPointerR -> Int
foreign import ccall unsafe "block_pointer_get_transaction_count"
    blockPointerTransactionCountF :: Ptr BlockPointerR -> Int
foreign import ccall unsafe "block_pointer_get_contents"
    blockPointerContentsF :: Ptr BlockPointerR -> Ptr BlockContentsR

blockPointerHash :: BlockPointer -> BlockHash
blockPointerHash b =
  let
    p = blockPointerHashF . blockPointerPointer $ b
  in
    unsafePerformIO $ do
      bh_str <- curry packCStringLen p 32
      return . SHA256.Hash . fromByteString $ bh_str

blockPointerHeight :: BlockPointer -> BlockHeight
blockPointerHeight = BlockHeight . fromIntegral . blockPointerHeightF . blockPointerPointer

blockPointerTransactionCount :: BlockPointer -> Int
blockPointerTransactionCount = blockPointerTransactionCountF . blockPointerPointer

blockPointerExtractBlockFields :: BlockPointer -> Maybe BlockFields
blockPointerExtractBlockFields = blockContentsFields . blockPointerExtractBlockContents

blockPointerExtractBlockContents :: BlockPointer -> BlockContents
blockPointerExtractBlockContents = BlockContents . blockPointerContentsF . blockPointerPointer

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

-- |Create the BlockPointer for the GenesisBlock
-- This function must initialize the BlockPointerR of the Rust side
makeGenesisBlockPointer :: GlobalStatePtr -> GenesisData ->  BlockState' BlockPointer -> BlockPointer
makeGenesisBlockPointer gsptr genData blockPointerState = theBlockPointer
  where
    theBlockPointer = BlockPointer {..}
    blockPointerParent = theBlockPointer
    blockPointerLastFinalized = theBlockPointer
    blockPointerReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
    blockPointerArriveTime = blockPointerReceiveTime
    blockPointerPointer = unsafePerformIO $ useAsCStringLen (encode genData) . uncurry $ makeGenesisDataF gsptr


-- |Creates the BlockPointer for a BakedBlock
-- This function must initialize the BlockPointerR of the Rust side
makeBlockPointer :: GlobalStatePtr ->
                    PendingBlock ->
                    BlockPointer ->
                    BlockPointer ->
                    BlockState'
                    BlockPointer ->
                    UTCTime ->
                    BlockPointer
makeBlockPointer gsptr b blockPointerParent blockPointerLastFinalized blockPointerState blockPointerArriveTime =
  theBlockPointer
  where
    theBlockPointer = BlockPointer {..}
    blockPointerReceiveTime = pendingBlockReceiveTime b
    blockPointerPointer = makeBlockPointerF gsptr (pendingBlockPointer b) ((+1) . theBlockHeight . bpHeight $ blockPointerParent)

---------------------------
-- * FFI Types
---------------------------

-- |Datatype representing the GlobalState in Rust
data GlobalStateR
-- |Pointer to the GlobalState in Rust
type GlobalStatePtr = Ptr GlobalStateR
