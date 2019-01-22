{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
-- |This module provides a dummy signature scheme for
-- prototyping purposes.  It provides NO SECURITY and
-- obviously should be replaced with a real implementation.
module Concordium.Crypto.DummySignature(
    SignKey,
    VerifyKey,
    KeyPair(..),
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
instance Serialize SignKey where

data VerifyKey = VerifyKey ByteString
    deriving (Eq, Generic)
instance Serialize VerifyKey where

newtype Signature = Signature Hash.Hash
    deriving (Eq, Generic, Serialize, Show)

data KeyPair = KeyPair {
    signKey :: SignKey,
    verifyKey :: VerifyKey
}

instance Random KeyPair where
    random gen = (key, gen')
        where
            (r, gen') = random gen
            akey = L.toStrict $ toLazyByteString $ word64BE r
            key = KeyPair (SignKey akey) (VerifyKey akey)

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
emptySignature = Signature (Hash.hash empty)
