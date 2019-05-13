{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Runner where

import GHC.Generics(Generic)

import Data.Text(Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Serialize as S
import qualified Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.Fail(MonadFail)

import qualified Concordium.ID.AccountHolder as AH

import Concordium.Crypto.SHA256(hash)
import Concordium.Crypto.SignatureScheme(KeyPair)

import Concordium.Types
import qualified Concordium.Scheduler.Types as Types

import Acorn.Parser.Runner

import Prelude hiding(mod, exp)

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
          return $ signTx keys meta (Types.encodePayload (Types.InitContract amount mref contName params 0)) -- NB: 0 is fine as size as that is not serialized
        Nothing -> error (show cNameText)
    (TJSON meta (Update mnameText amount address msgText) keys) -> do
      msg <- processTmInCtx mnameText msgText
      return $ signTx keys meta (Types.encodePayload (Types.Update amount address msg 0)) -- NB: 0 is fine as size as that is not serialized
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
