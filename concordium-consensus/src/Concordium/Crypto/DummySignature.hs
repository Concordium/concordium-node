{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- |This module provides a dummy signature scheme for
-- prototyping purposes.  It provides NO SECURITY and
-- obviously should be replaced with a real implementation.
module Concordium.Crypto.DummySignature(
    SignKey,
    VerifyKey,
    Signature,
    newKeypair,
    sign,
    verify,
    emptySignature
) where

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import Data.ByteString
import Data.ByteString.Builder
import Data.Word
import GHC.Generics
import System.Random

data SignKey = SignKey ByteString
    deriving (Eq, Generic)
data VerifyKey = VerifyKey ByteString
    deriving (Eq, Generic)
instance Serialize VerifyKey where

newtype Signature = Signature ByteString
    deriving (Eq, Generic, Serialize)

newKeypair :: IO (SignKey, VerifyKey)
newKeypair = do
        r <- randomIO :: IO (Word64)
        let key = L.toStrict $ toLazyByteString $ word64BE r
        return (SignKey key, VerifyKey key)


mySign :: ByteString -> ByteString -> Signature
mySign key doc = Signature $ Hash.hashLazy $ toLazyByteString $ (stringUtf8 "SIGN") <> byteString key <> byteString doc

sign :: SignKey -> ByteString -> Signature
sign (SignKey key) doc = mySign key doc

verify :: VerifyKey -> ByteString -> Signature -> Bool
verify (VerifyKey key) doc sig = sig == mySign key doc

emptySignature :: Signature
emptySignature = Signature empty