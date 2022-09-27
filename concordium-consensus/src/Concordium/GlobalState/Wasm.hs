{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  InstrumentedModuleV(..),
  imWasmArtifactBytes,
  instrumentedModuleFromBytes,
  -- *** Module interface
  ModuleInterface(..),
  ModuleInterfaceA(..),
  ModuleInterfaceV,
  BasicModuleInterface,
  HasModuleRef(..),
  HasEntrypoints(..)
  )
  where

import qualified Data.ByteString as BS
import Data.Kind
import Data.Serialize
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Concordium.Utils.Serialization
import Concordium.Types
import Concordium.Wasm

-- |A processed module artifact as a 'BS.ByteString', as returned by the @validate_and_process_v*@
-- Rust functions, and used by the @call_receive_v*@ and @call_init_v*@ functions.
newtype ModuleArtifactBytes (v :: WasmVersion) = ModuleArtifactBytes { maBytes :: BS.ByteString }
  deriving(Eq, Show)

-- This serialization instance does not add explicit versioning on its own. The
-- module artifact is always stored as part of another structure that has
-- versioning.
instance Serialize (ModuleArtifactBytes v) where
  get = do
    len <- getWord32be
    bs <- getByteString (fromIntegral len)
    return $! ModuleArtifactBytes bs

  put ModuleArtifactBytes{..} = 
    putWord32be (fromIntegral (BS.length maBytes)) <> putByteString maBytes

-- |Web assembly module in binary format, instrumented with whatever it needs to
-- be instrumented with, and preprocessed to an executable format, ready to be
-- instantiated and run.
data InstrumentedModuleV v where
  InstrumentedWasmModuleV0 :: { imWasmArtifactV0 :: !(ModuleArtifactBytes V0) } -> InstrumentedModuleV V0
  InstrumentedWasmModuleV1 :: { imWasmArtifactV1 :: !(ModuleArtifactBytes V1) } -> InstrumentedModuleV V1

deriving instance Eq (InstrumentedModuleV v)
deriving instance Show (InstrumentedModuleV v)

instance Serialize (InstrumentedModuleV V0) where
  put InstrumentedWasmModuleV0{..} = do
    putWord32be 0
    put imWasmArtifactV0

  get = get >>= \case
    V0 -> InstrumentedWasmModuleV0 <$> get
    V1 -> fail "Expected Wasm version 0, got 1."


instance Serialize (InstrumentedModuleV V1) where
  put InstrumentedWasmModuleV1{..} = do
    putWord32be 1
    put imWasmArtifactV1

  get = get >>= \case
    V0 -> fail "Expected Wasm version 1, got 0."
    V1 -> InstrumentedWasmModuleV1 <$> get

-- |Get the 'BS.ByteString' serialized module artifact.
imWasmArtifactBytes :: InstrumentedModuleV v -> BS.ByteString
imWasmArtifactBytes InstrumentedWasmModuleV0{..} = maBytes imWasmArtifactV0
imWasmArtifactBytes InstrumentedWasmModuleV1{..} = maBytes imWasmArtifactV1

-- |Construct an 'InstrumentedModuleV' from the serialized bytes.
-- (This does no checking of the 'BS.ByteString'.)
instrumentedModuleFromBytes :: SWasmVersion v -> BS.ByteString -> InstrumentedModuleV v
instrumentedModuleFromBytes SV0 = InstrumentedWasmModuleV0 . ModuleArtifactBytes
instrumentedModuleFromBytes SV1 = InstrumentedWasmModuleV1 . ModuleArtifactBytes

--------------------------------------------------------------------------------

-- |A Wasm module interface, parametrised by the type of the "Artifact" i.e. an instrumented module.
-- The instrumented module should be e.g. @InstrumentedModuleV v@.
data ModuleInterfaceA instrumentedModule = ModuleInterface {
  -- |Reference of the module on the chain.
  miModuleRef :: !ModuleRef,
  -- |Init methods exposed by this module.
  -- They should each be exposed with a type Amount -> Word32
  miExposedInit :: !(Set.Set InitName),
  -- |Receive methods exposed by this module, indexed by contract name.
  -- They should each be exposed with a type Amount -> Word32
  miExposedReceive :: !(Map.Map InitName (Set.Set ReceiveName)),
  -- |Module source processed into an efficiently executable format.
  -- For details see "Artifact" in smart-contracts/wasm-chain-integration
  miModule :: !instrumentedModule,
  -- |Size of the module as deployed in the transaction.
  miModuleSize :: !Word64
  } deriving(Eq, Show, Functor, Foldable, Traversable)

-- |A Wasm module interface, parametrised by the version of the instrumented module @v@.
type ModuleInterfaceV (v :: WasmVersion) = ModuleInterfaceA (InstrumentedModuleV v)

class HasModuleRef a where
  -- |Retrieve the module reference (the way a module is identified on the chain).
  moduleReference :: a -> ModuleRef

-- |A class that makes it more convenient to retrieve certain fields both from
-- versioned and unversioned modules.
class HasEntrypoints a where
  -- |Retrieve the set of contracts/init names from a module.
  exposedInit :: a -> Set.Set InitName
  -- |Retrieve the set of exposed entrypoints indexed by contract names.
  exposedReceive :: a -> Map.Map InitName (Set.Set ReceiveName)

instance HasEntrypoints (ModuleInterfaceA im) where
  exposedInit ModuleInterface{..} = miExposedInit
  exposedReceive ModuleInterface{..} = miExposedReceive

instance HasModuleRef (ModuleInterfaceA im) where
  {-# INLINE moduleReference #-}
  moduleReference = miModuleRef

-- This serialization instance relies on the versioning of the
-- InstrumentedModuleV for its own versioning.
instance Serialize im => Serialize (ModuleInterfaceA im) where
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

-- |A module interface in either version 0 or 1. This is generally only used
-- when looking up a module before an instance is created. Afterwards an
-- explicitly versioned module interface (ModuleInterfaceV) is used.
-- This is parametrised by the type (family) of the instrumented module
-- @im :: WasmVersion -> Type@.
data ModuleInterface (im :: WasmVersion -> Type) where
  ModuleInterfaceV0 :: !(ModuleInterfaceA (im V0)) -> ModuleInterface im
  ModuleInterfaceV1 :: !(ModuleInterfaceA (im V1)) -> ModuleInterface im

deriving instance (Show (im V0), Show (im V1)) => Show (ModuleInterface im)

instance HasModuleRef (ModuleInterface im) where
  {-# INLINE moduleReference #-}
  moduleReference (ModuleInterfaceV0 mi) = miModuleRef mi
  moduleReference (ModuleInterfaceV1 mi) = miModuleRef mi

instance HasEntrypoints (ModuleInterface im) where
  exposedInit (ModuleInterfaceV0 m) = exposedInit m
  exposedInit (ModuleInterfaceV1 m) = exposedInit m
  exposedReceive (ModuleInterfaceV0 m) = miExposedReceive m
  exposedReceive (ModuleInterfaceV1 m) = miExposedReceive m

type BasicModuleInterface = ModuleInterface InstrumentedModuleV

instance Serialize BasicModuleInterface where
  get = do
    miModuleRef <- get
    miExposedInit <- getSafeSetOf get
    miExposedReceive <- getSafeMapOf get (getSafeSetOf get)
    get >>= \case
      V0 -> do
        miModule <- InstrumentedWasmModuleV0 <$> get
        miModuleSize <- getWord64be
        return (ModuleInterfaceV0 ModuleInterface{..})
      V1 -> do
        miModule <- InstrumentedWasmModuleV1 <$> get
        miModuleSize <- getWord64be
        return (ModuleInterfaceV1 ModuleInterface{..})
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
