module Concordium.Crypto.SHA256 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA256

hash :: B.ByteString -> B.ByteString
hash = SHA256.hash

hashLazy :: L.ByteString -> B.ByteString
hashLazy = SHA256.hashlazy