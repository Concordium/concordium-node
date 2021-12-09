{-| Common types and functions used to support wasm module storage in block state. |-}
module Concordium.GlobalState.Wasm (
  -- ** Instrumented module
  --
  -- | An instrumented module is a processed module that is ready to be
  -- instantiated and run.
  ModuleArtifactV0(..),
  newModuleArtifactV0,
  withModuleArtifactV0,
  InstrumentedModule(..),

  -- *** Module interface
  ModuleInterface(..)
  )
  where

import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Foreign (ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.Ptr
import Foreign.C

import Concordium.Crypto.FFIHelpers (fromBytesHelper, toBytesHelper)
import Concordium.Utils.Serialization
import Concordium.Types
import Concordium.Wasm

foreign import ccall unsafe "&artifact_v0_free" freeArtifactV0 :: FunPtr (Ptr ModuleArtifactV0 -> IO ())
foreign import ccall unsafe "artifact_v0_to_bytes" toBytesArtifactV0 :: Ptr ModuleArtifactV0 -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "artifact_v0_from_bytes" fromBytesArtifactV0 :: Ptr Word8 -> CSize -> IO (Ptr ModuleArtifactV0)

-- | A processed module artifact ready for execution. The actual module is
-- allocated and stored on the Rust heap, in a reference counted pointer.
newtype ModuleArtifactV0 = ModuleArtifactV0 { maArtifactV0 :: ForeignPtr ModuleArtifactV0 }
  deriving(Eq, Show) -- the Eq and Show instances are only for debugging and compare and show pointers.

-- |Wrap the pointer to the module artifact together with a finalizer that will
-- deallocate it when the module is no longer used.
newModuleArtifactV0 :: Ptr ModuleArtifactV0 -> IO ModuleArtifactV0
newModuleArtifactV0 p = do
  maArtifactV0 <- newForeignPtr freeArtifactV0 p
  return ModuleArtifactV0{..}

-- |Use the module artifact temporarily. The pointer must not be leaked from the
-- computation.
withModuleArtifactV0 :: ModuleArtifactV0 -> (Ptr ModuleArtifactV0 -> IO a) -> IO a
withModuleArtifactV0 ModuleArtifactV0{..} = withForeignPtr maArtifactV0

instance Serialize ModuleArtifactV0 where
  get = do
    len <- getWord32be
    bs <- getByteString (fromIntegral len)
    case fromBytesHelper freeArtifactV0 fromBytesArtifactV0 bs of
      Nothing -> fail "Cannot decode module artifact."
      Just maArtifactV0 -> return ModuleArtifactV0{..}

  put ModuleArtifactV0{..} = 
    let bs = toBytesHelper toBytesArtifactV0 maArtifactV0
    in putWord32be (fromIntegral (BS.length bs)) <> putByteString bs

-- |Web assembly module in binary format, instrumented with whatever it needs to
-- be instrumented with, and preprocessed to an executable format, ready to be
-- instantiated and run.
data InstrumentedModule = InstrumentedWasmModule {
  -- |Version of the Wasm standard and on-chain API this module corresponds to.
  imWasmVersion :: !Word32,
  -- |Source in binary wasm format.
  imWasmArtifact :: !ModuleArtifactV0
  } deriving(Eq, Show)

instance Serialize InstrumentedModule where
  put InstrumentedWasmModule{..} = do
    putWord32be imWasmVersion
    put imWasmArtifact

  get = InstrumentedWasmModule <$> getWord32be <*> get

--------------------------------------------------------------------------------

-- |A Wasm module interface with exposed entry-points.
data ModuleInterface = ModuleInterface {
  -- |Reference of the module on the chain.
  miModuleRef :: !ModuleRef,
  -- |Init methods exposed by this module.
  -- They should each be exposed with a type Amount -> Word32
  miExposedInit :: !(Set.Set InitName),
  -- |Receive methods exposed by this module, indexed by contract name.
  -- They should each be exposed with a type Amount -> Word32
  miExposedReceive :: !(Map.Map InitName (Set.Set ReceiveName)),
  -- |Module source in binary format, instrumented with whatever it needs to be instrumented with.
  miModule :: !InstrumentedModule,
  miModuleSize :: !Word64
  } deriving(Eq, Show)

instance Serialize ModuleInterface where
  get = do
    miModuleRef <- get
    miExposedInit <- getSafeSetOf get
    miExposedReceive <- getSafeMapOf get (getSafeSetOf get)
    miModule <- get
    miModuleSize <- getWord64be
    return ModuleInterface {..}
  put ModuleInterface{..} = do
    put miModuleRef
    putSafeSetOf put miExposedInit
    putSafeMapOf put (putSafeSetOf put) miExposedReceive
    put miModule
    putWord64be miModuleSize
