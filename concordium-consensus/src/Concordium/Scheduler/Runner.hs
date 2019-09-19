{-# LANGUAGE DeriveGeneric #-}
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
import qualified Concordium.ID.Types as IDTypes

import Acorn.Parser.Runner
import qualified Acorn.Core as Core

import Prelude hiding(mod, exp)

signTx :: KeyPair -> TransactionHeader -> EncodedPayload -> Types.BareTransaction
signTx kp th encPayload = Types.signTransaction kp header encPayload
    where header = Types.makeTransactionHeader (thScheme th) (thSenderKey th) (Types.payloadSize encPayload) (thNonce th) (thGasAmount th)

transactionHelper :: MonadFail m => TransactionJSON -> Context Core.UA m Types.BareTransaction
transactionHelper t =
  case t of
    (TJSON meta (DeployModule mnameText) keys) ->
      (signTx keys meta . Types.encodePayload . Types.DeployModule) <$> getModule mnameText
    (TJSON meta (InitContract amnt mnameText cNameText paramExpr) keys) -> do
      (mref, _, tys) <- getModuleTmsTys mnameText
      case Map.lookup cNameText tys of
        Just contName -> do
          params <- processTmInCtx mnameText paramExpr
          return $ signTx keys meta (Types.encodePayload (Types.InitContract amnt mref contName params))
        Nothing -> error (show cNameText)
    (TJSON meta (Update mnameText amnt addr msgText) keys) -> do
      msg <- processTmInCtx mnameText msgText
      return $ signTx keys meta (Types.encodePayload (Types.Update amnt addr msg))
    (TJSON meta (Transfer to amnt) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.Transfer to amnt))
    (TJSON meta (DeployCredential c) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.DeployCredential c))
    (TJSON meta (AddBaker vkey sigkey acc proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.AddBaker vkey sigkey acc proof))
    (TJSON meta (RemoveBaker bid proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.RemoveBaker bid proof))
    (TJSON meta (UpdateBakerAccount bid addr proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerAccount bid addr proof))
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

processTransactions :: MonadFail m => [TransactionJSON]  -> Context Core.UA m [Types.BareTransaction]
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

data TransactionHeader = TransactionHeader {
    -- |Signature scheme used by the account.
    thScheme :: !SchemeId,
    -- |Verification key of the sender.
    thSenderKey :: !IDTypes.AccountVerificationKey,
    -- |Per account nonce, strictly increasing, no gaps.
    thNonce :: !Nonce,
    -- |Amount of gas dedicated for the execution of this transaction.
    thGasAmount :: !Energy
    } deriving (Show)

data TransactionJSON = TJSON { metadata :: TransactionHeader
                             , payload :: PayloadJSON
                             , keypair :: KeyPair
                             }
  deriving(Show,Generic)
