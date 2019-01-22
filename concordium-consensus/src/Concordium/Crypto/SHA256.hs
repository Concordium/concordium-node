{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Concordium.Crypto.SHA256 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Builder
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Serialize
import Data.Hashable

newtype Hash = Hash B.ByteString deriving (Eq, Ord, Serialize)

instance Show Hash where
    show (Hash h) = LC.unpack (toLazyByteString $ byteStringHex h)

instance Hashable Hash where
    hashWithSalt s (Hash b) = hashWithSalt s b
    hash (Hash b) = case decode b of
        Left _ -> hashWithSalt 0 b
        Right v -> v

hash :: B.ByteString -> Hash
hash = Hash . SHA256.hash

hashLazy :: L.ByteString -> Hash
hashLazy = Hash . SHA256.hashlazy