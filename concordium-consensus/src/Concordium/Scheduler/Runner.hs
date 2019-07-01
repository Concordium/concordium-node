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
import qualified Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.Fail(MonadFail)

import Concordium.Crypto.SignatureScheme(KeyPair)

import Concordium.Types
import Concordium.Types.Execution(Proof)
import Concordium.ID.Types
import qualified Concordium.Scheduler.Types as Types

import Acorn.Parser.Runner
import qualified Acorn.Core as Core

import Prelude hiding(mod, exp)

signTx :: KeyPair -> Types.TransactionHeader -> EncodedPayload -> Types.Transaction
signTx kp th = Types.signTransaction kp th

transactionHelper :: MonadFail m => TransactionJSON -> Context Core.UA m Types.Transaction
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
    (TJSON meta (DeployCredential c) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.DeployCredential c))
    (TJSON meta (AddBaker vkey sigkey acc proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.AddBaker vkey sigkey acc proof))
    (TJSON meta (RemoveBaker bid proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.RemoveBaker bid proof))
    (TJSON meta (UpdateBakerAccount bid address proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerAccount bid address proof))
    (TJSON meta (UpdateBakerSignKey bid key proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerSignKey bid key proof))
    (TJSON meta (DelegateStake bid) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.DelegateStake bid))
    (TJSON meta UndelegateStake keys) ->
      return $ signTx keys meta (Types.encodePayload Types.UndelegateStake)


-- decodeAndProcessTransactions :: MonadFail m => ByteString -> Context m [Types.Transaction]
-- decodeAndProcessTransactions txt =
--   case AE.eitherDecode txt of
--     Left err -> fail $ "Error decoding JSON: " ++ err
--     Right t -> processTransactions t

processTransactions :: MonadFail m => [TransactionJSON]  -> Context Core.UA m [Types.Transaction]
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
                 | DeployCredential {cdi :: CredentialDeploymentInformation}
                 | AddBaker {
                     bvfkey :: BakerElectionVerifyKey,
                     bsigvfkey :: BakerSignVerifyKey,
                     baccount :: AccountAddress,
                     bproof :: Proof }
                 | RemoveBaker {
                     rbId :: !BakerId,
                     rbProof :: !Proof
                     }
                 | UpdateBakerAccount {
                     ubaId :: !BakerId,
                     ubaAddress :: !AccountAddress,
                     ubaProof :: !Proof
                     }
                 | UpdateBakerSignKey {
                     ubsId :: !BakerId,
                     ubsKey :: !BakerSignVerifyKey,
                     ubsProof :: !Proof
                     }
                 | DelegateStake {
                     dsID :: !BakerId
                     }
                 | UndelegateStake
                 deriving(Show, Generic)

data TransactionJSON = TJSON { metadata :: Types.TransactionHeader
                             , payload :: PayloadJSON
                             , keypair :: KeyPair
                             }
  deriving(Show,Generic)
