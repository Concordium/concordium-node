{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Runner where

import GHC.Generics(Generic)

import Data.Maybe
import Data.Text(Text)
import Data.Word

import Control.Monad.Except

import Concordium.Crypto.SignatureScheme(KeyPair)
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.Crypto.Proofs as Proofs
import qualified Concordium.Crypto.BlsSignature as Bls

import Concordium.ID.Types
import Concordium.Types
import Concordium.Wasm(WasmModule(..))
import qualified Concordium.Wasm as Wasm
import qualified Concordium.Scheduler.Types as Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding(mod, exp)
import Concordium.Crypto.EncryptedTransfers

-- |Sign a transaction with the given list of keys.
signTx :: [(CredentialIndex, [(KeyIndex, KeyPair)])] -> TransactionHeader -> EncodedPayload -> Types.AccountTransaction
signTx keys TransactionHeader{..} encPayload = Types.signTransaction keys header encPayload
    where header = Types.TransactionHeader{thPayloadSize=Types.payloadSize encPayload,..}

signTxSingle :: KeyPair -> TransactionHeader -> EncodedPayload -> Types.AccountTransaction
signTxSingle key TransactionHeader{..} encPayload = Types.signTransactionSingle key header encPayload
    where header = Types.TransactionHeader{thPayloadSize=Types.payloadSize encPayload,..}

transactionHelper :: (MonadFail m, MonadIO m) => TransactionJSON -> m Types.AccountTransaction
transactionHelper t =
  case t of
    (TJSON meta (DeployModule version mnameText) keys) -> liftIO $ do
      BS.readFile mnameText >>= \wasmMod ->
        let modl = WasmModule version $ Wasm.ModuleSource wasmMod
        in return $ signTx keys meta . Types.encodePayload . Types.DeployModule $ modl
    (TJSON meta (InitContract icAmount version mnameText cNameText paramExpr) keys) -> liftIO $ do
      BS.readFile mnameText >>= \wasmMod ->
        let modl = WasmModule version $ Wasm.ModuleSource wasmMod
            payload = Types.InitContract{
              icModRef = Wasm.getModuleRef modl,
              icInitName = Wasm.InitName cNameText,
              icParam = Wasm.Parameter paramExpr,
              ..
              }
        in return . signTx keys meta . Types.encodePayload $ payload
    (TJSON meta (Update uAmount uAddress rNameText paramExpr) keys) -> return $
            let payload = Types.Update{
                  uReceiveName = Wasm.ReceiveName rNameText,
                  uMessage = Wasm.Parameter paramExpr,
                  ..
                  }
            in signTx keys meta . Types.encodePayload $ payload
    (TJSON meta (Transfer to amnt) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.Transfer to amnt))
    (TJSON meta AddBaker{..} keys) ->
      let abElectionVerifyKey = bElectionVerifyKey
          abSignatureVerifyKey = bSignVerifyKey
          abAggregationVerifyKey = bAggregateVerifyKey
          abBakingStake = bInitialStake
          abRestakeEarnings = bRestakeEarnings
          challenge = Types.addBakerChallenge (thSender meta) bElectionVerifyKey bSignVerifyKey bAggregateVerifyKey
      in do
        Just abProofElection <- liftIO $ Proofs.proveDlog25519VRF challenge (VRF.KeyPair bElectionSecretKey bElectionVerifyKey)
        Just abProofSig <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 bSignSecretKey bSignVerifyKey)
        abProofAggregation <- liftIO $ Bls.proveKnowledgeOfSK challenge bAggregateSecretKey -- TODO: Make sure enough context data is included that this proof can't be reused.
        return $ signTx keys meta (Types.encodePayload Types.AddBaker{..})
    (TJSON meta RemoveBaker keys) ->
      return $ signTx keys meta (Types.encodePayload Types.RemoveBaker)
    (TJSON meta UpdateBakerStake{..} keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerStake{..}))
    (TJSON meta UpdateBakerRestakeEarnings{..} keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerRestakeEarnings{..}))
    (TJSON meta UpdateBakerKeys{..} keys) ->
      let
          ubkElectionVerifyKey = bElectionVerifyKey
          ubkSignatureVerifyKey = bSignVerifyKey
          ubkAggregationVerifyKey = bAggregateVerifyKey
          challenge = Types.updateBakerKeyChallenge (thSender meta) bElectionVerifyKey bSignVerifyKey bAggregateVerifyKey
      in do
        Just ubkProofElection <- liftIO $ Proofs.proveDlog25519VRF challenge (VRF.KeyPair bElectionSecretKey bElectionVerifyKey)
        Just ubkProofSig <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 bSignSecretKey bSignVerifyKey)
        ubkProofAggregation <- liftIO $ Bls.proveKnowledgeOfSK challenge bAggregateSecretKey -- TODO: Make sure enough context data is included that this proof can't be reused.
        return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerKeys{..}))
    (TJSON meta UpdateCredentialKeys{..} keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateCredentialKeys{..}))
    (TJSON meta TransferToEncrypted{..} keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.TransferToEncrypted tteAmount))
    (TJSON meta EncryptedAmountTransfer{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.EncryptedAmountTransfer{..})
    (TJSON meta TransferToPublic{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.TransferToPublic{..})
    (TJSON meta TransferWithSchedule{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.TransferWithSchedule{..})
    (TJSON meta UpdateCredentials{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.UpdateCredentials{..})


processTransactions :: (MonadFail m, MonadIO m) => [TransactionJSON]  -> m [Types.AccountTransaction]
processTransactions = mapM transactionHelper

-- |For testing purposes: process transactions without grouping them by accounts
-- (i.e. creating one "group" per transaction).
-- Arrival time of transactions is taken to be 0.
processUngroupedTransactions ::
  (MonadFail m, MonadIO m) =>
  [TransactionJSON] ->
  m Types.GroupedTransactions
processUngroupedTransactions inpt = do
  txs <- processTransactions inpt
  return (Types.fromTransactions (map ((:[]) . Types.fromAccountTransaction 0) txs))

-- |For testing purposes: process transactions in the groups in which they came
-- The arrival time of all transactions is taken to be 0.
processGroupedTransactions ::
  (MonadFail m, MonadIO m) =>
  [[TransactionJSON]] ->
  m Types.GroupedTransactions
processGroupedTransactions = fmap (Types.fromTransactions . map (map (Types.fromAccountTransaction 0)))
                             . mapM processTransactions

data PayloadJSON = DeployModule { version :: Word32, moduleName :: FilePath }
                 | InitContract { amount :: Amount
                                , version :: Word32
                                , moduleName :: FilePath
                                , contractName :: Text
                                , parameter :: BSS.ShortByteString }
                 | Update { amount :: Amount
                          , address :: ContractAddress
                          , receiveName :: Text
                          , message :: BSS.ShortByteString
                          }
                 | Transfer { toaddress :: AccountAddress
                            , amount :: Amount
                            }
                 | AddBaker {
                   bElectionVerifyKey :: BakerElectionVerifyKey,
                   bElectionSecretKey :: VRF.SecretKey,
                   bSignVerifyKey :: BakerSignVerifyKey,
                   bSignSecretKey :: BlockSig.SignKey,
                   bAggregateVerifyKey :: BakerAggregationVerifyKey,
                   bAggregateSecretKey :: BakerAggregationPrivateKey,
                   bInitialStake :: Amount,
                   bRestakeEarnings :: Bool
                 }
                 | RemoveBaker
                 | UpdateBakerStake {
                     ubsStake :: !Amount
                     }
                 | UpdateBakerRestakeEarnings {
                     ubreRestakeEarnings :: !Bool
                     }
                 | UpdateBakerKeys {
                    bElectionVerifyKey :: BakerElectionVerifyKey,
                    bElectionSecretKey :: VRF.SecretKey,
                    bSignVerifyKey :: BakerSignVerifyKey,
                    bSignSecretKey :: BlockSig.SignKey,
                    bAggregateVerifyKey :: BakerAggregationVerifyKey,
                    bAggregateSecretKey :: BakerAggregationPrivateKey
                    }
                 | UpdateCredentialKeys {
                      -- | New set of credential keys to be replaced with the existing ones, including updating the threshold.
                      uckCredId :: CredentialRegistrationID,
                      uckKeys :: !CredentialPublicKeys
                    }
                 | TransferToEncrypted {
                     tteAmount :: !Amount
                     }
                 | EncryptedAmountTransfer {
                     eatTo :: !AccountAddress,
                     eatData :: !EncryptedAmountTransferData
                     }
                 | TransferToPublic {
                     ttpData :: !SecToPubAmountTransferData
                     }
                 | TransferWithSchedule {
                     twsTo :: !AccountAddress,
                     twsSchedule :: ![(Timestamp, Amount)]
                     }
                 | UpdateCredentials {
                     ucNewCredInfos :: !(Map.Map CredentialIndex CredentialDeploymentInformation),
                     ucRemoveCredIds :: ![CredentialRegistrationID],
                     ucNewThreshold :: !AccountThreshold
                     }
                 deriving(Show, Generic)

data TransactionHeader = TransactionHeader {
    -- |Sender account address.
    thSender :: !AccountAddress,
    -- |Per account nonce, strictly increasing, no gaps.
    thNonce :: !Nonce,
    -- |Amount of gas dedicated for the execution of this transaction.
    thEnergyAmount :: !Energy,
    -- |Expiration time after which transaction will not be executed
    thExpiry :: TransactionExpiryTime
    } deriving (Show)

data TransactionJSON = TJSON { metadata :: TransactionHeader
                             , payload :: PayloadJSON
                             , keys :: [(CredentialIndex, [(KeyIndex, KeyPair)])]
                             }
  deriving(Show,Generic)
