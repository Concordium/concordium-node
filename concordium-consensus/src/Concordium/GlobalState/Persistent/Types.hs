{-# LANGUAGE OverloadedStrings, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Concordium.GlobalState.Persistent.Types where
{-
import Database.Persist.Sql
import Data.Serialize
import Data.Text (pack)
import qualified Data.ByteString.Short as SBS
import Data.Proxy
import Control.Monad.Trans.Reader

import Concordium.Crypto.SHA256 as SHA256
import Concordium.ID.Types
import Concordium.Crypto.SignatureScheme
import Concordium.Types

instance PersistField SHA256.Hash where
    toPersistValue = PersistByteString . encode
    fromPersistValue (PersistByteString bs) = case decode bs of
            Left err -> Left (pack err)
            Right v -> Right v
    fromPersistValue _ = Left "A hash must be persisted as a ByteString"

instance PersistFieldSql SHA256.Hash where
    sqlType _ = SqlString

instance PersistField AccountAddress where
    toPersistValue = PersistByteString . encode
    fromPersistValue (PersistByteString bs) = case decode bs of
            Left err -> Left (pack err)
            Right v -> Right v
    fromPersistValue _ = Left "An AccountAddress must be persisted as a ByteString"

instance PersistFieldSql AccountAddress where
    sqlType _ = SqlString

instance PersistField AccountEncryptionKey where
    toPersistValue (EncKeyAcc key) = PersistByteString (SBS.fromShort key)
    fromPersistValue (PersistByteString bs) = return $! EncKeyAcc (SBS.toShort bs)
    fromPersistValue _ = Left "An AccountEncryptionKey must be persisted as a ByteString"

instance PersistFieldSql AccountEncryptionKey where
    sqlType _ = SqlString

deriving instance PersistField Nonce
deriving instance PersistFieldSql Nonce

deriving instance PersistField Amount
deriving instance PersistFieldSql Amount

instance PersistField VerifyKey where
    toPersistValue (VerifyKey key) = PersistByteString (SBS.fromShort key)
    fromPersistValue (PersistByteString bs) = return $! VerifyKey (SBS.toShort bs)
    fromPersistValue _ = Left "A VerifyKey must be persisted as a ByteString"

instance PersistFieldSql VerifyKey where
    sqlType _ = SqlString

instance PersistField SchemeId where
    toPersistValue = toPersistValue . fromEnum
    -- FIXME: Don't use toEnum without bounds checking (may generate run-time error)
    fromPersistValue = fmap toEnum . fromPersistValue

instance PersistFieldSql SchemeId where
    sqlType _ = sqlType (Proxy :: Proxy Int)

deriving instance PersistField BakerId
deriving instance PersistFieldSql BakerId

type PersistM a = ReaderT SqlBackend IO a
-}