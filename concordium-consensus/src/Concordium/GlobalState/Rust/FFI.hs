{-# LANGUAGE ForeignFunctionInterface #-}

module Concordium.GlobalState.Rust.FFI (
  GlobalStateR
  , GlobalStatePtr
  , getGenesisBlockPointerR
  , getGenesisDataR
  , storeFinalizedBlockR
  , getFinalizedBlockR
  ) where

import Foreign.Ptr
import Concordium.GlobalState.Parameters
import Data.Serialize
import Foreign.C
import Data.ByteString hiding (unpack)
import Data.ByteString.Char8 (unpack)
import Concordium.Types
import Concordium.Crypto.SHA256
import Concordium.GlobalState.Block

data GlobalStateR
type GlobalStatePtr = Ptr GlobalStateR

data BlockPointerR
newtype BlockPointerPtr = BlockPointerPtr (Ptr BlockPointerR)

data RustSlice
foreign import ccall unsafe "get_ptr" getPtr :: Ptr RustSlice -> CString
foreign import ccall unsafe "get_length" getLength :: Ptr RustSlice -> Int

foreign import ccall unsafe "get_genesis_block_pointer"
   getGenesisBlockPointerF :: GlobalStatePtr -> BlockPointerPtr
foreign import ccall unsafe "get_genesis_data"
   getGenesisDataF :: GlobalStatePtr -> Ptr RustSlice
foreign import ccall unsafe "store_finalized_block"
   storeFinalizedBlockF :: GlobalStatePtr -> CString -> Int -> IO ()
foreign import ccall unsafe "get_finalized_block"
   getFinalizedBlockF :: GlobalStatePtr -> CString -> Ptr RustSlice

getGenesisBlockPointerR :: GlobalStatePtr -> BlockPointerPtr
getGenesisBlockPointerR = getGenesisBlockPointerF

getGenesisDataR :: GlobalStatePtr -> IO GenesisData
getGenesisDataR gsptr = do
  let gdata = getGenesisDataF gsptr
  bs <- packCStringLen (getPtr gdata, getLength gdata)
  case decode bs of
    Left e -> fail $ "Couldn't get genesis data" ++ show e
    Right v -> return v
    
storeFinalizedBlockR :: GlobalStatePtr -> Block -> IO ()
storeFinalizedBlockR gsptr block = do
  let eb = encode block
  useAsCStringLen eb $ uncurry (storeFinalizedBlockF gsptr)

getFinalizedBlockR :: GlobalStatePtr -> BlockHash -> IO Block
getFinalizedBlockR gsptr bhash = do
  bh <- newCString . unpack . hashToByteString $ bhash
  let bdata = getFinalizedBlockF gsptr bh
  bs <- packCStringLen (getPtr bdata, getLength bdata)
  case decode bs of
    Left e -> fail $ "couldn't get finalized block for hash " ++ show bhash ++ " because of " ++ e
    Right v -> return v  
