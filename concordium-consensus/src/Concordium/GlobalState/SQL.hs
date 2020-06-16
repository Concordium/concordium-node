{-# LANGUAGE DerivingVia #-}
-- |This module provides helpers for interfacing with an SQL database.
module Concordium.GlobalState.SQL where

import Control.Monad
import Control.Arrow
import Database.Persist
import Database.Persist.Postgresql
import Data.Proxy
import Data.ByteString
import qualified Data.Text as Text
import qualified Data.Serialize as S

-- |Wraps a type for persistent storage via a serialization to a 'ByteString'.
newtype ByteStringSerialized a = ByteStringSerialized { unBSS :: a }
    deriving (S.Serialize, Eq, Ord, Show) via a

instance S.Serialize a => PersistField (ByteStringSerialized a) where
  toPersistValue = toPersistValue . S.encode
  fromPersistValue =
    fromPersistValue >=> left (Text.pack) . S.decode

instance S.Serialize a => PersistFieldSql (ByteStringSerialized a) where
  sqlType _ = sqlType (Proxy :: Proxy ByteString)
