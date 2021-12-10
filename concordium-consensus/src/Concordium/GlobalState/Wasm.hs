{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Common types and functions used to support wasm module storage in block state. |-}
module Concordium.GlobalState.Wasm (
  -- ** Instrumented module
  --
  -- | An instrumented module is a processed module that is ready to be
  -- instantiated and run.
  V0,
  V1,
  ModuleArtifactV0,
  ModuleArtifactV1,
  newModuleArtifactV0,
  newModuleArtifactV1,
  withModuleArtifact,
  InstrumentedModuleV(..),
  imWasmVersion,
  imWasmArtifact,
  -- *** Module interface
  ModuleInterface(..),
  ModuleInterfaceV(..),
  HasModuleRef(..),
  HasEntrypoints(..)
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

foreign import ccall unsafe "&artifact_v1_free" freeArtifactV1 :: FunPtr (Ptr ModuleArtifactV1 -> IO ())
foreign import ccall unsafe "artifact_v1_to_bytes" toBytesArtifactV1 :: Ptr ModuleArtifactV1 -> Ptr CSize -> IO (Ptr Word8)
foreign import ccall unsafe "artifact_v1_from_bytes" fromBytesArtifactV1 :: Ptr Word8 -> CSize -> IO (Ptr ModuleArtifactV1)

-- | A processed module artifact ready for execution. The actual module is
-- allocated and stored on the Rust heap, in a reference counted pointer.
newtype ModuleArtifact v = ModuleArtifact { maArtifact :: ForeignPtr (ModuleArtifact v) }
  deriving(Eq, Show) -- the Eq and Show instances are only for debugging and compare and show pointers.

data V0
data V1

type ModuleArtifactV0 = ModuleArtifact V0
type ModuleArtifactV1 = ModuleArtifact V1

-- |Wrap the pointer to the module artifact together with a finalizer that will
-- deallocate it when the module is no longer used.
newModuleArtifactV0 :: Ptr ModuleArtifactV0 -> IO ModuleArtifactV0
newModuleArtifactV0 p = do
  maArtifact <- newForeignPtr freeArtifactV0 p
  return ModuleArtifact{..}

-- |Wrap the pointer to the module artifact together with a finalizer that will
-- deallocate it when the module is no longer used.
newModuleArtifactV1 :: Ptr ModuleArtifactV1 -> IO ModuleArtifactV1
newModuleArtifactV1 p = do
  maArtifact <- newForeignPtr freeArtifactV1 p
  return ModuleArtifact{..}

-- |Use the module artifact temporarily. The pointer must not be leaked from the
-- computation.
withModuleArtifact :: ModuleArtifact v -> (Ptr (ModuleArtifact v) -> IO a) -> IO a
withModuleArtifact ModuleArtifact{..} = withForeignPtr maArtifact

instance Serialize ModuleArtifactV0 where
  get = do
    len <- getWord32be
    bs <- getByteString (fromIntegral len)
    case fromBytesHelper freeArtifactV0 fromBytesArtifactV0 bs of
      Nothing -> fail "Cannot decode module artifact."
      Just maArtifact -> return ModuleArtifact{..}

  put ModuleArtifact{..} = 
    let bs = toBytesHelper toBytesArtifactV0 maArtifact
    in putWord32be (fromIntegral (BS.length bs)) <> putByteString bs


instance Serialize ModuleArtifactV1 where
  get = do
    len <- getWord32be
    bs <- getByteString (fromIntegral len)
    case fromBytesHelper freeArtifactV1 fromBytesArtifactV1 bs of
      Nothing -> fail "Cannot decode module artifact."
      Just maArtifact -> return ModuleArtifact{..}

  put ModuleArtifact{..} = 
    let bs = toBytesHelper toBytesArtifactV1 maArtifact
    in putWord32be (fromIntegral (BS.length bs)) <> putByteString bs


-- |Web assembly module in binary format, instrumented with whatever it needs to
-- be instrumented with, and preprocessed to an executable format, ready to be
-- instantiated and run.
data InstrumentedModuleV v where
  InstrumentedWasmModuleV0 :: { imWasmArtifactV0 :: ModuleArtifact V0 } -> InstrumentedModuleV V0
  InstrumentedWasmModuleV1 :: { imWasmArtifactV1 :: ModuleArtifact V1 } -> InstrumentedModuleV V1

deriving instance Eq (InstrumentedModuleV v)
deriving instance Show (InstrumentedModuleV v)

-- This is just an internal helper 
data InstrumentedModule where
  IWMV0 :: { imwArtifactV0 :: ModuleArtifact V0 } -> InstrumentedModule
  IWMV1 :: { imwArtifactV1 :: ModuleArtifact V1 } -> InstrumentedModule

instance Serialize InstrumentedModule where
  put IWMV0{..} = do
    putWord32be 0
    put imwArtifactV0
  put IWMV1{..} = do
    putWord32be 1
    put imwArtifactV1

  get = getWord32be >>= \case
    0 -> IWMV0 <$> get
    1 -> IWMV1 <$> get
    _ -> fail "Unsupported Wasm module version."

imWasmVersion :: InstrumentedModuleV v -> Word32
imWasmVersion (InstrumentedWasmModuleV0 _) = 0
imWasmVersion (InstrumentedWasmModuleV1 _) = 1

instance Serialize (InstrumentedModuleV V0) where
  put InstrumentedWasmModuleV0{..} = do
    putWord32be 0
    put imWasmArtifactV0

  get = getWord32be >>= \case
    0 -> InstrumentedWasmModuleV0 <$> get
    _ -> fail "Unsupported Wasm module version."

instance Serialize (InstrumentedModuleV V1) where
  put InstrumentedWasmModuleV1{..} = do
    putWord32be 1
    put imWasmArtifactV1

  get = getWord32be >>= \case
    1 -> InstrumentedWasmModuleV1 <$> get
    _ -> fail "Unsupported Wasm module version."

--------------------------------------------------------------------------------

-- |A Wasm module interface with exposed entry-points.
data ModuleInterfaceV v = ModuleInterface {
  -- |Reference of the module on the chain.
  miModuleRef :: !ModuleRef,
  -- |Init methods exposed by this module.
  -- They should each be exposed with a type Amount -> Word32
  miExposedInit :: !(Set.Set InitName),
  -- |Receive methods exposed by this module, indexed by contract name.
  -- They should each be exposed with a type Amount -> Word32
  miExposedReceive :: !(Map.Map InitName (Set.Set ReceiveName)),
  -- |Module source in binary format, instrumented with whatever it needs to be instrumented with.
  miModule :: !(InstrumentedModuleV v),
  miModuleSize :: !Word64
  } deriving(Eq, Show)

imWasmArtifact :: ModuleInterfaceV v -> ModuleArtifact v
imWasmArtifact ModuleInterface{miModule = InstrumentedWasmModuleV0{..}} = imWasmArtifactV0
imWasmArtifact ModuleInterface{miModule = InstrumentedWasmModuleV1{..}} = imWasmArtifactV1

class HasModuleRef a where
  moduleReference :: a -> ModuleRef

class HasEntrypoints a where
  exposedInit :: a -> Set.Set InitName
  exposedReceive :: a -> Map.Map InitName (Set.Set ReceiveName)

instance HasEntrypoints (ModuleInterfaceV v) where
  exposedInit ModuleInterface{..} = miExposedInit
  exposedReceive ModuleInterface{..} = miExposedReceive

instance HasModuleRef (ModuleInterfaceV v) where
  {-# INLINE moduleReference #-}
  moduleReference = miModuleRef
  
data ModuleInterface where
  ModuleInterfaceV0 :: ModuleInterfaceV V0 -> ModuleInterface
  ModuleInterfaceV1 :: ModuleInterfaceV V1 -> ModuleInterface
  deriving (Eq, Show)

instance HasModuleRef ModuleInterface where
  {-# INLINE moduleReference #-}
  moduleReference (ModuleInterfaceV0 mi) = miModuleRef mi
  moduleReference (ModuleInterfaceV1 mi) = miModuleRef mi

instance HasEntrypoints ModuleInterface where
  exposedInit (ModuleInterfaceV0 m) = exposedInit m
  exposedInit (ModuleInterfaceV1 m) = exposedInit m
  exposedReceive (ModuleInterfaceV0 m) = miExposedReceive m
  exposedReceive (ModuleInterfaceV1 m) = miExposedReceive m

instance Serialize (InstrumentedModuleV v) => Serialize (ModuleInterfaceV v) where
  get = do
    miModuleRef <- get
    miExposedInit <- getSafeSetOf get
    miExposedReceive <- getSafeMapOf get (getSafeSetOf get)
    miModule <- get
    miModuleSize <- getWord64be
    return ModuleInterface{..}
  put ModuleInterface{..} = do
    put miModuleRef
    putSafeSetOf put miExposedInit
    putSafeMapOf put (putSafeSetOf put) miExposedReceive
    put miModule
    putWord64be miModuleSize

instance Serialize ModuleInterface where
  get = do
    miModuleRef <- get
    miExposedInit <- getSafeSetOf get
    miExposedReceive <- getSafeMapOf get (getSafeSetOf get)
    miModuleV <- get
    miModuleSize <- getWord64be
    case miModuleV of
      IWMV0 imWasmArtifactV0 -> return (ModuleInterfaceV0 ModuleInterface{miModule = InstrumentedWasmModuleV0 {..},..})
      IWMV1 imWasmArtifactV1 -> return (ModuleInterfaceV1 ModuleInterface{miModule = InstrumentedWasmModuleV1 {..},..})
  put (ModuleInterfaceV0 ModuleInterface{..}) = do
    put miModuleRef
    putSafeSetOf put miExposedInit
    putSafeMapOf put (putSafeSetOf put) miExposedReceive
    put miModule
    putWord64be miModuleSize
  put (ModuleInterfaceV1 ModuleInterface{..}) = do
    put miModuleRef
    putSafeSetOf put miExposedInit
    putSafeMapOf put (putSafeSetOf put) miExposedReceive
    put miModule
    putWord64be miModuleSize
