{-# LANGUAGE RecordWildCards #-}
module Concordium.GlobalState.Rust.FFI.Types (
  GlobalStatePtr
  , BlockPointer(..)
  , BlockPointerR
  , RustSlice
  , BlockFields(..)
  , PendingBlock(..)
  , PendingBlockR
  ) where

import Foreign.Ptr
import Data.Time.Clock
import Concordium.GlobalState.BlockState hiding (BlockPointer)
import Concordium.GlobalState.Parameters

import Data.Time.Clock.POSIX

-- |Datatype representing the GlobalState in Rust
data GlobalStateR
-- |Pointer to the GlobalState in Rust
type GlobalStatePtr = Ptr GlobalStateR

-- |Helper datatype to transfer `CStringLen`s through FFI
data RustSlice

-- |Datatype representing a BlockPointer in the Rust side
data BlockPointerR
-- |Pointer to a BlockPointer in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockpointer-type
data BlockPointer = BlockPointer {
  theBPPointer :: Ptr BlockPointerR,
  theParent :: BlockPointer,
  theLastFinalized :: BlockPointer,
  theState :: BlockState' BlockPointer,
  theReceiveTime :: UTCTime,
  theArriveTime :: UTCTime
  }

-- makeGenesisBlockPointer :: GenesisData -> Ptr BlockPointerR -> BlockState' BlockPointer -> BlockPointer
-- makeGenesisBlockPointer genData theBPPointer theState = theBlockPointer
--   where
--     theBlockPointer = BlockPointer {..}
--     theParent = theBlockPointer
--     theLastFinalized = theBlockPointer
--     theReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
--     theArriveTime = theReceiveTime

-- |Datatype representing a BlockFields in the Rust side
data BlockFieldsR
-- |Pointer to a BlockFields in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#blockmetadata-class
newtype BlockFields = BlockFields (Ptr BlockFieldsR)

-- |Datatype representing a PendingBlock in the Rust side
data PendingBlockR
-- |Pointer to a PendingBlock in the Rust side, check
-- https://gitlab.com/Concordium/notes-wiki/wikis/Global-state#pendingblock-type
data PendingBlock = PendingBlock {
  thePBPointer :: Ptr PendingBlockR,
  theTime:: UTCTime
  }
