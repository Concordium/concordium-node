{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{- | This module implements a dummy verifiable random function.
     The implementation is intended to immitate the behaviour of
     a real implementation, but does not provide any security.
-}
module Concordium.Crypto.DummyVRF(
    PublicKey,
    PrivateKey,
    KeyPair(..),
    Hash,
    Proof,
    newKeypair,
    hash,
    prove,
    proofToHash,
    verify,
    verifyKey,
    hashToDouble,
    hashToInt,
    emptyHash,
    emptyProof
) where

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Data.ByteString.Lazy as L
import Data.Serialize
import Data.ByteString as BS
import Data.ByteString.Builder
import Data.Word
import GHC.Generics
import System.Random

data PublicKey = PublicKey ByteString
    deriving (Eq, Generic)
instance Serialize PublicKey where

data PrivateKey = PrivateKey ByteString
    deriving (Eq, Generic)

instance Serialize PrivateKey where

newtype Hash = Hash ByteString
    deriving (Eq, Generic, Serialize)

newtype Proof = Proof ByteString
    deriving (Eq, Generic, Serialize, Show)

data KeyPair = KeyPair {
    privateKey :: PrivateKey,
    publicKey :: PublicKey
}

instance Random KeyPair where
    random gen = (key, gen')
        where
            (r, gen') = random gen
            akey = L.toStrict $ toLazyByteString $ word64BE r
            key = KeyPair (PrivateKey akey) (PublicKey akey)

newKeypair :: IO (PrivateKey, PublicKey)
newKeypair = do
        r <- randomIO :: IO (Word64)
        let key = L.toStrict $ toLazyByteString $ word64BE r
        return (PrivateKey key, PublicKey key)


myHash :: ByteString -> ByteString -> ByteString
myHash key doc = Hash.hashLazy $ toLazyByteString $ (stringUtf8 "SIGN") <> byteString key <> byteString doc

hash :: PrivateKey -> ByteString -> Hash
hash (PrivateKey key) = Hash . myHash key

prove :: PrivateKey -> ByteString -> Proof
prove (PrivateKey key) = Proof . myHash key

proofToHash :: Proof -> Hash
proofToHash (Proof p) = Hash p

verify :: PublicKey -> ByteString -> Proof -> Bool
verify (PublicKey key) doc (Proof pf) = myHash key doc == pf

verifyKey :: PublicKey -> Bool
verifyKey (PublicKey key) = BS.length key == 8

-- |Convert a 'Hash' into a 'Double' value in the range [0,1].
-- This implementation takes the first 64-bit word (big-endian) and uses it
-- as the significand, with an exponent of -64.  Since the precision of a
-- 'Double' is only 53 bits, there is inevitably some loss.  This also means
-- that the outcome 1 is not possible.
hashToDouble :: Hash -> Double
hashToDouble (Hash h) = case runGet getWord64be h of
    Left e -> error e
    Right w -> encodeFloat (toInteger w) (-64)

hashToInt :: Hash -> Int
hashToInt (Hash h) = case runGet getInt64be h of
    Left e -> error e
    Right i -> fromIntegral i

-- |An empty hash value (typically, not a valid hash)
emptyHash :: Hash
emptyHash = Hash empty

-- |An empty proof (typically, not a valid proof)
emptyProof :: Proof
emptyProof = Proof empty
