{-# LANGUAGE ForeignFunctionInterface, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, RecordWildCards #-}

module Concordium.GlobalState.Rust.FFI (
  -- * GlobalState
  GlobalStatePtr
  , getGenesisBlockPointerR
  , getGenesisDataR

  -- * Finalized blocks functions
  , storeFinalizedBlockR
  , getFinalizedBlockR

  -- * BlockFields functions
  , BlockFields

  -- * PendingBlock functions
  , PendingBlock(..)

  -- * BlockPointer functions
  , BlockPointer(..)
  -- ** Constructor
  , makeGenesisBlockPointer
  , makeBlockPointer
  ) where

import Foreign.Ptr
import Concordium.GlobalState.Parameters
import Data.Serialize
import Foreign.C
import Data.ByteString hiding (unpack, intercalate)
import Concordium.Types
import Concordium.Crypto.SHA256

import System.IO.Unsafe
import Data.FixedByteString hiding (unpack, pack)
import qualified Concordium.Crypto.VRF as VRF
import Concordium.GlobalState.Transactions

import Concordium.GlobalState.BlockState hiding (BlockPointer)

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Concordium.GlobalState.Basic.BlockState as BBS
       hiding (makeGenesisBlockPointer, makeBlockPointer, BlockPointer)
import Concordium.GlobalState.Block 
import Concordium.Types.HashableTo
import Data.List

---------------------------
-- * RustSlice operations
---------------------------

-- |Used for casting a Slice as if it was a C *char
foreign import ccall unsafe "get_ptr" getPtr :: Ptr RustSlice -> CString
-- |Used for casting a Slice as if it was a C *char
foreign import ccall unsafe "get_length" getLength :: Ptr RustSlice -> Int

---------------------------
-- * GenesisBlock FFI calls
---------------------------

foreign import ccall unsafe "store_genesis_block"
   storeGenesisBlockPointerF :: GlobalStatePtr -> CString -> Int -> IO ()
foreign import ccall unsafe "get_genesis_block_pointer"
   getGenesisBlockPointerF :: GlobalStatePtr -> Ptr BlockPointerR
foreign import ccall unsafe "get_genesis_data"
   getGenesisDataF :: GlobalStatePtr -> Ptr RustSlice

storeGenesisBlockPointerR :: GlobalStatePtr -> BlockPointer -> IO ()
storeGenesisBlockPointerR gsptr bp = undefined

getGenesisBlockPointerR :: GlobalStatePtr -> Ptr BlockPointerR
getGenesisBlockPointerR = getGenesisBlockPointerF

getGenesisDataR :: GlobalStatePtr -> GenesisData
getGenesisDataR gsptr = undefined --do
--  let gdata = getGenesisDataF gsptr
  --bs <- peekCStringLen (getPtr gdata, getLength gdata)
  --case decode bs of
    --Left e -> fail $ "Couldn't get genesis data" ++ show e
    --Right v -> return v

---------------------------
-- * Finalized Blocks FFI calls
---------------------------

foreign import ccall unsafe "store_finalized_block"
   storeFinalizedBlockF :: GlobalStatePtr -> CString -> Int -> IO ()
foreign import ccall unsafe "get_finalized_block"
  getFinalizedBlockF :: GlobalStatePtr -> CString -> Ptr RustSlice
    
storeFinalizedBlockR :: GlobalStatePtr -> PendingBlock -> IO ()
storeFinalizedBlockR gsptr block = do
  let eb = undefined --encode block
  useAsCStringLen eb $ uncurry (storeFinalizedBlockF gsptr)

getFinalizedBlockR :: GlobalStatePtr -> BlockHash -> BlockPointer
getFinalizedBlockR gsptr bhash = unsafePerformIO $ do
  useAsCString (hashToByteString $ bhash) $ \bh -> do
    let bdata = getFinalizedBlockF gsptr bh 
    bs <- packCStringLen (getPtr bdata, getLength bdata)
    case undefined of --decode bs of
      Left e -> fail $ "couldn't get finalized block for hash " ++ show bhash ++ " because of " ++ e
      Right v -> return v

---------------------------
-- * BlockFields FFI calls
---------------------------

foreign import ccall unsafe "block_fields_get_pointer"
    bfBlockPointerF :: BlockFields -> Ptr RustSlice
foreign import ccall unsafe "block_fields_get_baker"
    bfBlockBakerF :: BlockFields -> Int
foreign import ccall unsafe "block_fields_get_proof"
    bfBlockProofF :: BlockFields -> Ptr RustSlice
foreign import ccall unsafe "block_fields_get_nonce"
    bfBlockNonceF :: BlockFields -> Ptr RustSlice
foreign import ccall unsafe "block_fields_get_last_finalized"
    bfBlockLastFinalizedF :: BlockFields -> Ptr RustSlice

bfBlockPointerR :: BlockFields -> BlockHash
bfBlockPointerR b =
  let bh = bfBlockPointerF b in 
    unsafePerformIO $ do
      bh_str <- curry packCStringLen (getPtr bh) (getLength bh)
      return . hash $ bh_str

bfBlockBakerR :: BlockFields -> BakerId
bfBlockBakerR b =
  BakerId . fromIntegral . bfBlockBakerF $ b
  
bfBlockProofR :: BlockFields -> BlockProof
bfBlockProofR b = 
  let bp = bfBlockProofF b in 
    unsafePerformIO $ do
      bp_str <- curry packCStringLen (getPtr bp) (getLength bp)
      return . VRF.Proof . fromByteString $ bp_str

bfBlockNonceR :: BlockFields -> BlockNonce
bfBlockNonceR b = 
  let bn = bfBlockNonceF b in 
    unsafePerformIO $ do
      bn_str <- curry packCStringLen (getPtr bn) (getLength bn)
      return . VRF.Proof . fromByteString $ bn_str
    
bfBlockLastFinalizedR :: BlockFields -> BlockHash
bfBlockLastFinalizedR b = 
  let bn = bfBlockLastFinalizedF b in 
    unsafePerformIO $ do
      bn_str <- curry packCStringLen (getPtr bn) (getLength bn)
      return . hash $ bn_str

---------------------------
-- * PendingBlock FFI calls
---------------------------

foreign import ccall unsafe "pending_block_get_slot"
    pbBlockSlotF :: Ptr PendingBlockR -> Int
foreign import ccall unsafe "pending_block_get_fields"
    pbBlockFieldsF :: Ptr PendingBlockR -> BlockFields
foreign import ccall unsafe "pending_block_get_transactions"
    pbBlockTransactionsF :: Ptr PendingBlockR -> Ptr RustSlice
--foreign import ccall unsafe "pending_block_verify_block_signature"
--pbVerifyBlockSignatureF :: VerifyKey -> PendingBlock -> Bool
foreign import ccall unsafe "pending_block_serialize_block"
    pbSerializeBlockF :: Ptr PendingBlockR -> Ptr RustSlice
foreign import ccall unsafe "pending_block_get_hash"
    pbGetHashF :: Ptr PendingBlockR -> Ptr RustSlice
foreign import ccall unsafe "pending_block_display"
    pbShowF :: Ptr PendingBlockR -> Ptr RustSlice
foreign import ccall unsafe "pending_block_update_transactions"
  pbUpdateTransactionsF :: Ptr PendingBlockR -> CString -> Int -> IO ()

pbBlockSlotR :: PendingBlock -> Slot
pbBlockSlotR = Slot . fromIntegral . pbBlockSlotF . thePBPointer

pbBlockFieldsR :: PendingBlock -> BlockFields
pbBlockFieldsR = pbBlockFieldsF . thePBPointer

pbBlockTransactionsR :: PendingBlock -> [Transaction]
pbBlockTransactionsR b =
  let bt = pbBlockTransactionsF . thePBPointer $ b in
    unsafePerformIO $ do
      bt_str <- curry packCStringLen (getPtr bt) (getLength bt)
      case decode bt_str of
        Left e -> fail $ "Couldn't deserialize txs " ++ show e
        Right v -> return v

pbVerifyBlockSignatureR = undefined

pbSerializeBlockR :: PendingBlock -> ByteString
pbSerializeBlockR b =
   let bt = pbSerializeBlockF . thePBPointer $ b in
    unsafePerformIO $ curry packCStringLen (getPtr bt) (getLength bt)

pbGetHashR :: PendingBlock -> BlockHash
pbGetHashR b =  
  let bh = pbGetHashF . thePBPointer $ b in 
    unsafePerformIO $ do
      bh_str <- curry packCStringLen (getPtr bh) (getLength bh)
      return . hash $ bh_str

pbShowR :: PendingBlock -> String
pbShowR b =
  let bh = pbShowF . thePBPointer $ b in 
    unsafePerformIO $  curry peekCStringLen (getPtr bh) (getLength bh)

pbUpdateTransactionsR :: [Transaction] -> PendingBlock -> IO ()
pbUpdateTransactionsR txs pb = do
  useAsCStringLen (encode txs) $ uncurry (pbUpdateTransactionsF (thePBPointer pb))
  return ()

---------------------------
-- * BlockPointer FFI calls
---------------------------

foreign import ccall unsafe "block_pointer_get_hash"
    bpGetHashF :: Ptr BlockPointerR -> Ptr RustSlice
foreign import ccall unsafe "block_pointer_get_slot"
    bpBlockSlotF :: Ptr BlockPointerR -> Int
foreign import ccall unsafe "block_pointer_get_fields"
    bpBlockFieldsF :: Ptr BlockPointerR -> BlockFields
foreign import ccall unsafe "block_pointer_get_transactions"
    bpBlockTransactionsF :: Ptr BlockPointerR -> Ptr RustSlice
-- foreign import ccall unsafe "block_pointer_verify_signature"
--     bpVerifyBlockSignatureF :: Ptr BlockPointerR ->
foreign import ccall unsafe "block_pointer_serialize"
    bpSerializeBlockF :: Ptr BlockPointerR -> Ptr RustSlice
foreign import ccall unsafe "block_pointer_display"
    bpShowF :: Ptr BlockPointerR -> Ptr RustSlice
foreign import ccall unsafe "block_pointer_get_height"
    bpGetHeightF :: Ptr BlockPointerR -> Int
foreign import ccall unsafe "block_pointer_get_transaction_count"
    bpGetTransactionCounF :: Ptr BlockPointerR -> Int

bpGetHashR :: BlockPointer -> BlockHash
bpGetHashR b =
  let bh = bpGetHashF . theBPPointer $ b in 
    unsafePerformIO $ do
      bh_str <- curry packCStringLen (getPtr bh) (getLength bh)
      return . hash $ bh_str

bpBlockSlotR :: BlockPointer -> Slot
bpBlockSlotR = Slot . fromIntegral . bpBlockSlotF . theBPPointer

bpBlockFieldsR :: BlockPointer -> BlockFields
bpBlockFieldsR = bpBlockFieldsF . theBPPointer

bpBlockTransactionsR :: BlockPointer -> [Transaction]
bpBlockTransactionsR b =
  let bt = bpBlockTransactionsF . theBPPointer $ b in
    unsafePerformIO $ do
      bt_str <- curry packCStringLen (getPtr bt) (getLength bt)
      case decode bt_str of
        Left e -> fail $ "Couldn't deserialize txs" ++ show e
        Right v -> return v
        
bpVerifyBlockSignatureR = undefined

bpSerializeBlockR :: BlockPointer -> ByteString
bpSerializeBlockR b =
   let bt = bpSerializeBlockF . theBPPointer $ b in
    unsafePerformIO $ curry packCStringLen (getPtr bt) (getLength bt)
    
bpShowR :: BlockPointer -> String
bpShowR b =
  let bh = bpShowF . theBPPointer $ b in 
    unsafePerformIO $  curry peekCStringLen (getPtr bh) (getLength bh)

bpGetHeightR :: BlockPointer -> BlockHeight
bpGetHeightR = BlockHeight . fromIntegral . bpGetHeightF . theBPPointer

bpGetTransactionCountR :: BlockPointer -> Int
bpGetTransactionCountR = bpGetTransactionCounF . theBPPointer

---------------------------
-- * FFI Types
---------------------------

-- |Datatype representing the GlobalState in Rust
data GlobalStateR
-- |Pointer to the GlobalState in Rust
type GlobalStatePtr = Ptr GlobalStateR

-- |Helper datatype to transfer `CStringLen`s through FFI
data RustSlice

-- |Datatype representing a BlockFields in the Rust side
data BlockFieldsR
-- |Pointer to a BlockFields in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockmetadata-class
--
-- BlockFields holds information for:
--
--    * BlockParent hash
--    * BlockBaker
--    * BlockProof
--    * BlockNonce
--    * BlockLastFinalized hash
--
-- BlockFields is required to implement BlockMetadata
newtype BlockFields = BlockFields (Ptr BlockFieldsR)

instance BlockMetadata BlockFields where
    blockPointer = bfBlockPointerR
    blockBaker = bfBlockBakerR
    blockProof = bfBlockProofR
    blockNonce = bfBlockNonceR
    blockLastFinalized = bfBlockLastFinalizedR

-- |Datatype representing a PendingBlock in the Rust side
data PendingBlockR
-- |Datatype holding the pointer to a PendingBlock in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#pendingblock-type
--
-- A PendingBlock in the Rust side must have:
--
--    * BlockFields
--    * BlockSlot
--    * BlockTransactions
--
-- PendingBlock is required to implement Eq, BlockMetadata, BlockData, HashableTo
-- BlockHash, Show and BlockPendingData
data PendingBlock = PendingBlock {
  thePBPointer :: Ptr PendingBlockR,
  theTime:: UTCTime
  }

instance Eq PendingBlock where
  a == b = (getHash a :: BlockHash) == (getHash b :: BlockHash)
  
instance BlockMetadata PendingBlock where
    blockPointer = bfBlockPointerR . pbBlockFieldsR
    blockBaker = bfBlockBakerR . pbBlockFieldsR
    blockProof = bfBlockProofR . pbBlockFieldsR
    blockNonce = bfBlockNonceR . pbBlockFieldsR
    blockLastFinalized = bfBlockLastFinalizedR . pbBlockFieldsR

type instance BlockFieldType PendingBlock = BlockFields

instance BlockData PendingBlock where
  blockSlot = pbBlockSlotR
  blockFields = Just . pbBlockFieldsR
  blockTransactions = pbBlockTransactionsR
  verifyBlockSignature = pbVerifyBlockSignatureR
  putBlock = putByteString . pbSerializeBlockR

instance HashableTo BlockHash PendingBlock where
  getHash = pbGetHashR

instance Show PendingBlock where
  show = pbShowR

instance BlockPendingData PendingBlock where
  blockReceiveTime = theTime

-- |Datatype representing a BlockPointer in the Rust side
data BlockPointerR
-- |Pointer to a BlockPointer in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockpointer-type
--
-- A BlockPointer in the Rust side must contain:
--
--    * Block (AKA GenesisData/BakedBlock)
--    * BlockHeight
--    * TransactionCount
--    * BlockHash
--
-- Some other fields cannot be moved into the Rust side because they either
-- represent another BlockPointers, represent the BlockState or are Time values
data BlockPointer = BlockPointer {
  theBPPointer :: Ptr BlockPointerR,
  theParent :: BlockPointer,
  theLastFinalized :: BlockPointer,
  theState :: BlockState' BlockPointer,
  theReceiveTime :: UTCTime,
  theArriveTime :: UTCTime
  }

-- BlockPointer is required to implement Eq and Ord, HashableTo BlockHash, BlockData
-- and BlockPointerData (requires Show)
instance Eq BlockPointer where
  a == b = (getHash a :: BlockHash) == (getHash b :: BlockHash)

instance Ord BlockPointer where
  a <= b = (getHash a :: BlockHash) <= (getHash b :: BlockHash)

instance HashableTo BlockHash BlockPointer where
  getHash = bpGetHashR

type instance BlockFieldType BlockPointer = BlockFields

instance BlockData BlockPointer where
  blockSlot = bpBlockSlotR
  blockFields = Just . bpBlockFieldsR
  blockTransactions = bpBlockTransactionsR
  verifyBlockSignature = bpVerifyBlockSignatureR
  putBlock = putByteString . bpSerializeBlockR --Wrong

instance Show BlockPointer where
  show bp = intercalate ", " $ bpShowR bp :
    [show . theParent $ bp
    , show . theLastFinalized $ bp
    , show . theState $ bp
    , show . theReceiveTime $ bp
    , show . theArriveTime $ bp]

instance BlockPointerData BlockPointer where
    type BlockState' BlockPointer = BBS.BlockState
    bpHash = bpGetHashR
    bpParent = theParent
    bpLastFinalized = theLastFinalized
    bpHeight = bpGetHeightR
    bpState = theState
    bpReceiveTime = theReceiveTime
    bpArriveTime = theArriveTime
    bpTransactionCount = bpGetTransactionCountR

-- |Create the BlockPointer for the GenesisBlock
--
-- This function must initialize the BlockPointerR of the Rust side
makeGenesisBlockPointer :: GlobalStatePtr -> GenesisData ->  BlockState' BlockPointer -> BlockPointer
makeGenesisBlockPointer gsptr genData theState = theBlockPointer
  where
    theBlockPointer = BlockPointer {..}
    theParent = theBlockPointer
    theLastFinalized = theBlockPointer
    theReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
    theArriveTime = theReceiveTime
    theBPPointer = makeBPGenesisPointerR gsptr genData

-- |Initialize the BlockPointerR in the Rust side
makeBPGenesisPointerR :: GlobalStatePtr -> GenesisData -> Ptr BlockPointerR
makeBPGenesisPointerR = undefined

-- |Creates the BlockPointer for a BakedBlock
--
-- This function must initialize the BlockPointerR of the Rust side
makeBlockPointer :: GlobalStatePtr ->
                    PendingBlock ->
                    BlockPointer ->
                    BlockPointer ->
                    BlockState'
                    BlockPointer ->
                    UTCTime ->
                    BlockPointer
makeBlockPointer gsptr b theParent theLastFinalized theState theArriveTime = theBlockPointer
  where
    theBlockPointer = BlockPointer {..}
    theReceiveTime = theTime b
    theBPPointer = makeBPPointerR gsptr b

makeBPPointerR :: GlobalStatePtr -> PendingBlock -> Ptr BlockPointerR
makeBPPointerR = undefined
