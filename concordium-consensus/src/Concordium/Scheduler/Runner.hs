{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Concordium.Scheduler.Runner where

import GHC.Generics(Generic)

import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as S
import qualified Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.Fail(MonadFail)

-- import qualified Data.Aeson as AE
-- import qualified Data.Aeson.TH as AETH
-- import Data.ByteString.Lazy(ByteString)
-- import Data.ByteString.Lazy(ByteString)

-- import Data.Aeson(ToJSON, FromJSON, Value(..), (.:), (.:?), parseJSON, parseJSONList)
-- import Data.Aeson.Types(typeMismatch)

import qualified Concordium.ID.AccountHolder as AH
-- import qualified Data.Base58String.Bitcoin as Base58

import Concordium.Crypto.SHA256(hash)
import Concordium.Crypto.SignatureScheme(KeyPair)

import Concordium.Types
import qualified Concordium.Scheduler.Types as Types

import Acorn.Parser.Runner

import Prelude hiding(mod, exp)

-- processTransaction :: MonadFail m => ByteString -> Context m Types.Transaction
-- processTransaction txt = do
--   case AE.eitherDecode txt of
--     Left err -> fail $ "Error decoding JSON: " ++ err
--     Right t -> transactionHelper t

signTx :: KeyPair -> Types.TransactionHeader -> EncodedPayload -> Types.Transaction
signTx kp th = Types.signTransaction kp th

transactionHelper :: MonadFail m => TransactionJSON -> Context m Types.Transaction
transactionHelper t = do
  case t of
    (TJSON meta (DeployModule mnameText) keys) ->
      (signTx keys meta . Types.encodePayload . Types.DeployModule) <$> getModule mnameText
    (TJSON meta (InitContract amount mnameText cNameText paramExpr) keys) -> do
      (mref, _, tys) <- getModuleTmsTys mnameText
      case Map.lookup cNameText tys of
        Just contName -> do
          params <- processTmInCtx mnameText paramExpr
          return $ signTx keys meta (Types.encodePayload (Types.InitContract amount mref contName params))
        Nothing -> error (show cNameText)
    (TJSON meta (Update mnameText amount address msgText) keys) -> do
      msg <- processTmInCtx mnameText msgText
      return $ signTx keys meta (Types.encodePayload (Types.Update amount address msg))
    (TJSON meta (Transfer to amount) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.Transfer to amount))
    (TJSON meta (CreateAccount verifyKey16) keys) -> do
      let d = S.decode (S.encode (hash (TE.encodeUtf8 verifyKey16))) -- NB: This is horrible, and a temporary hack
      case d of
        Left err -> fail err
        Right key -> return $ signTx keys meta (Types.encodePayload (Types.CreateAccount (AH.createAccount key)))

-- decodeAndProcessTransactions :: MonadFail m => ByteString -> Context m [Types.Transaction]
-- decodeAndProcessTransactions txt =
--   case AE.eitherDecode txt of
--     Left err -> fail $ "Error decoding JSON: " ++ err
--     Right t -> processTransactions t

processTransactions :: MonadFail m => [TransactionJSON]  -> Context m [Types.Transaction]
processTransactions = mapM transactionHelper

data PayloadJSON = DeployModule { moduleName :: Text }
                 | InitContract { amount :: Amount
                                , moduleName :: Text
                                , contractName :: Text
                                , parameter :: Text}
                 | Update { moduleName :: Text
                          , amount :: Amount
                          , address :: ContractAddress
                          , message :: Text
                          }
                 | Transfer { toaddress :: Address
                            , amount :: Amount
                            }
                 | CreateAccount { verifyKey16 :: Text }
  deriving(Show, Generic)

data TransactionJSON = TJSON { metadata :: Types.TransactionHeader
                             , payload :: PayloadJSON
                             , keypair :: KeyPair
                             }
  deriving(Generic)

-- instance AE.FromJSON TransactionJSON


-- -- auxiliary instances for testing

-- instance FromJSON Types.TransactionHeader where
--   parseJSON (Object v) = Types.TransactionHeader <$> (v .: "sender") <*> (v .: "nonce") <*> (v .: "gasAmount")
--   parseJSON invalid = typeMismatch "TransactionHeader" invalid

-- -- instance ToJSON Header where

-- instance FromJSON ContractAddress

-- instance ToJSON ContractAddress

-- -- inherid word64 instances
-- instance FromJSON Nonce where
--   parseJSON v = Nonce <$> parseJSON v
--   parseJSONList v = map Nonce <$> parseJSONList v

-- -- inherit text instances
-- instance FromJSON AccountAddress where
--   parseJSON v = AH.base58decodeAddr <$> parseJSON v
--   parseJSONList v = map AH.base58decodeAddr <$> parseJSONList v

-- -- inherit Word64 instances
-- instance FromJSON Amount where
--   parseJSON v = Amount <$> (parseJSON v)
--   parseJSONList v = map Amount <$> parseJSONList v

-- instance FromJSON Address where
--   parseJSON (Object v) =  do
--     r <- v .:? "accountAddress"
--     case r of
--       Nothing -> AddressContract <$> (v .: "contractAddress")
--       Just a -> return (AddressAccount a)

--   parseJSON invalid = typeMismatch "Address" invalid

-- instance IsString AccountAddress where
--   fromString = AH.base58decodeAddr . Base58.fromText . fromString

-- $(AETH.deriveFromJSON (AETH.defaultOptions { AETH.sumEncoding = AETH.TaggedObject "transactionType" "contents"}) ''PayloadJSON)

