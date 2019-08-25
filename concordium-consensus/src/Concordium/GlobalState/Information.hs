{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Concordium.GlobalState.Information where

import qualified Data.Serialize as S
import GHC.Generics

import qualified Concordium.Types.Acorn.Core as Core
import Concordium.Types.Acorn.Core(Literal(..))
import Concordium.Types.Acorn.Interfaces
import Concordium.Types

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Text as AET
import Data.Aeson((.=))

import Data.String(fromString)
import Data.Word
import Data.ByteString.Builder(toLazyByteString, byteStringHex)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import Data.Foldable(toList)

import Data.Void

-- *Summary of global state to be sent over the network.

data InstanceInfo = InstanceInfo
    {
     messageType :: !(Core.Type Core.UA Core.ModuleRef)
    ,localState :: !(Value Void)  -- must be storable
    ,instanceAmount :: !Amount
    } deriving(Show)

instance S.Serialize InstanceInfo where
  put (InstanceInfo{..}) = S.put messageType <> putStorable localState <> S.put instanceAmount
  get = InstanceInfo <$> S.get <*> getStorable <*> S.get

data AccountInfo = AccountInfo
    {accountNonce :: !Nonce
    ,accountAmount :: !Amount
    } 
    deriving(Show, Generic)

instance S.Serialize AccountInfo

-- |Note that this on its own is lossy. If we also had the types then we can
-- recover the full term.
jsonLiteral :: Core.Literal -> JSON.Value
jsonLiteral l = case l of
  Int32 i -> JSON.toJSON i
  Int64 i -> JSON.toJSON i
  Int128 i -> JSON.toJSON (toInteger i)
  Int256 i -> JSON.toJSON i
  Word32 i -> JSON.toJSON i
  Word64 i -> JSON.toJSON i
  Word128 i -> JSON.toJSON (toInteger i)
  Word256 i -> JSON.toJSON i
  ByteStr32 bs -> JSON.String . TL.toStrict . EL.decodeUtf8 . toLazyByteString . byteStringHex $ bs
  CAddress addr -> JSON.toJSON addr
  AAddress addr -> JSON.String (fromString (show addr)) -- show instance for addresses shows them in base58
  Str bs -> JSON.String . TL.toStrict . EL.decodeUtf8 . toLazyByteString . byteStringHex $ bs


-- |The serialization instances for values are only for storable values. If you
-- try to serialize with a value which is not storable the methods will fail
-- raising an exception. 
jsonStorable :: Value Void -> JSON.Value
jsonStorable (VLiteral l) = jsonLiteral l
jsonStorable (VConstructor n vals) =
  JSON.object $ ["name" .= (fromIntegral n :: Word32)] ++
                  zipWith (\i v -> ("child-" <> fromString (show i)) .= jsonStorable v) [(0::Int)..] (toList vals)
jsonStorable _ = error "FATAL: Trying to serialize a non-storable value. This should not happen."

valueToJSONString :: Value Void -> String
valueToJSONString = TL.unpack . AET.encodeToLazyText . jsonStorable
