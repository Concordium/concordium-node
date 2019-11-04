{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Runner where

import GHC.Generics(Generic)

import Data.Text(Text)
import qualified Data.HashMap.Strict as Map

import Control.Monad.Except
import Control.Monad.Fail(MonadFail)

import Concordium.Crypto.SignatureScheme(KeyPair)
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.Crypto.Proofs as Proofs

import Concordium.Types
import Concordium.Types.Execution(Proof)
import Concordium.ID.Types
import qualified Concordium.ID.Account as AH
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.ID.Types as IDTypes

import Data.Serialize

import Acorn.Parser.Runner
import qualified Acorn.Core as Core

import Prelude hiding(mod, exp)

signTx :: KeyPair -> TransactionHeader -> EncodedPayload -> Types.BareTransaction
signTx kp th encPayload = Types.signTransaction kp header encPayload
    where header = Types.makeTransactionHeader (thSenderKey th) (Types.payloadSize encPayload) (thNonce th) (thGasAmount th)

transactionHelper :: (MonadFail m, MonadIO m) => TransactionJSON -> Context Core.UA m Types.BareTransaction
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
    (TJSON meta AddBaker{..} keys) ->
      let abElectionVerifyKey = bvfkey
          abSignatureVerifyKey = bsigvfkey
          abAggregationVerifyKey = baggvfkey
          abAccount = AH.accountAddress (Sig.correspondingVerifyKey baccountKeyPair)
          challenge = runPut (put abElectionVerifyKey <> put abSignatureVerifyKey <> put abAccount)
      in do
        Just abProofElection <- liftIO $ Proofs.proveDlog25519VRF challenge (VRF.KeyPair bvfSecretKey abElectionVerifyKey)
        Just abProofSig <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 bsigkey bsigvfkey)
        Just abProofAccount <- liftIO $ Proofs.proveDlog25519KP challenge baccountKeyPair
        return $ signTx keys meta (Types.encodePayload Types.AddBaker{..})
    (TJSON meta (RemoveBaker bid proof) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.RemoveBaker bid proof))
    (TJSON meta (UpdateBakerAccount bid kp) keys) ->
      let ubaAddress = AH.accountAddress (Sig.correspondingVerifyKey kp)
          challenge = runPut (put bid <> put ubaAddress)
      in do
        Just ubaProof <- liftIO $ Proofs.proveDlog25519KP challenge kp
        return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerAccount bid ubaAddress ubaProof))
    (TJSON meta (UpdateBakerSignKey bid ubsKey signkey) keys) ->
      let challenge = runPut (put bid <> put ubsKey)
      in do
        Just ubsProof <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 signkey ubsKey)
        return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerSignKey bid ubsKey ubsProof))
    (TJSON meta (DelegateStake bid) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.DelegateStake bid))
    (TJSON meta UndelegateStake keys) ->
      return $ signTx keys meta (Types.encodePayload Types.UndelegateStake)

processTransactions :: (MonadFail m, MonadIO m) => [TransactionJSON]  -> Context Core.UA m [Types.BareTransaction]
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
                     bvfSecretKey :: VRF.SecretKey,
                     bsigvfkey :: BakerSignVerifyKey,
                     baggvfkey :: BakerAggregationVerifyKey,
                     bsigkey :: BlockSig.SignKey,
                     baccountKeyPair :: Sig.KeyPair
                 }
                 | RemoveBaker {
                     rbId :: !BakerId,
                     rbProof :: !Proof
                     }
                 | UpdateBakerAccount {
                     ubaId :: !BakerId,
                     ubaKeyPair :: !Sig.KeyPair
                     }
                 | UpdateBakerSignKey {
                     ubsId :: !BakerId,
                     ubsKey :: !BakerSignVerifyKey,
                     ubsSecretKey :: !BlockSig.SignKey
                     }
                 | DelegateStake {
                     dsID :: !BakerId
                     }
                 | UndelegateStake
                 deriving(Show, Generic)

data TransactionHeader = TransactionHeader {
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
