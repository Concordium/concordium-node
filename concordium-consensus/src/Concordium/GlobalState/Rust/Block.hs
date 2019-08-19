{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Concordium.GlobalState.Rust.Block (
  BlockFields(..)
  , PendingBlock(..)
  , BlockPointer(..)
  )where

import Foreign.Ptr
import Concordium.GlobalState.BlockState hiding (BlockState, BlockPointer)
import Concordium.GlobalState.Basic.BlockState (BlockState)
import Concordium.GlobalState.Block 
import Concordium.Types
import Concordium.GlobalState.Rust.FFI
import Concordium.Types.HashableTo
import Data.Serialize
import Data.List

-- BlockFields is required to implement BlockMetadata
instance BlockMetadata BlockFields where
    blockPointer = bfBlockPointerR
    blockBaker = bfBlockBakerR
    blockProof = bfBlockProofR
    blockNonce = bfBlockNonceR
    blockLastFinalized = bfBlockLastFinalizedR

-- PendingBlock is required to implement BlockMetadata, BlockData, HashableTo
-- BlockHash, Show and BlockPendingData
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
    type BlockState' BlockPointer = BlockState
    bpHash = bpGetHashR
    bpParent = theParent
    bpLastFinalized = theLastFinalized
    bpHeight = bpGetHeightR
    bpState = theState
    bpReceiveTime = theReceiveTime
    bpArriveTime = theArriveTime
    bpTransactionCount = bpGetTransactionCountR
