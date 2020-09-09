{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Runner where

import GHC.Generics(Generic)

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

import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding(mod, exp)
import Concordium.Crypto.EncryptedTransfers

-- |Sign a transaction with the given list of keys.
signTx :: [(KeyIndex, KeyPair)] -> TransactionHeader -> EncodedPayload -> Types.AccountTransaction
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
        let modl = WasmModule version wasmMod
        in return $ signTx keys meta . Types.encodePayload . Types.DeployModule $ modl
    (TJSON meta (InitContract icAmount version mnameText cNameText paramExpr) keys) -> liftIO $ do
      BS.readFile mnameText >>= \wasmMod ->
        let modl = WasmModule version wasmMod
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
      let abElectionVerifyKey = bvfkey
          abSignatureVerifyKey = bsigvfkey
          abAggregationVerifyKey = baggvfkey
          abAccount = baccountAddress
          challenge = runPut (put abElectionVerifyKey <> put abSignatureVerifyKey <> put abAggregationVerifyKey <> put abAccount)
      in do
        Just abProofElection <- liftIO $ Proofs.proveDlog25519VRF challenge (VRF.KeyPair bvfSecretKey abElectionVerifyKey)
        Just abProofSig <- liftIO $ Proofs.proveDlog25519KP challenge (Sig.KeyPairEd25519 bsigkey bsigvfkey)
        Just abProofAccount' <- liftIO $ Proofs.proveDlog25519KP challenge baccountKeyPair
        let abProofAccount = Types.singletonAOP abProofAccount' -- FIXME: This only works for simple accounts.
            abProofAggregation = Bls.proveKnowledgeOfSK challenge baggsigkey -- TODO: Make sure enough context data is included that this proof can't be reused.
        return $ signTx keys meta (Types.encodePayload Types.AddBaker{..})
    (TJSON meta (RemoveBaker bid) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.RemoveBaker bid))
    (TJSON meta (UpdateBakerAccount bid ubaAddress kp) keys) ->
      let challenge = runPut (put bid <> put ubaAddress)
      in do
        Just ubaProof' <- liftIO $ Proofs.proveDlog25519KP challenge kp
        let ubaProof = Types.singletonAOP ubaProof' -- FIXME: This only works for simple accounts.
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
    (TJSON meta (UpdateBakerAggregationVerifyKey bid publicKey secretKey) keys) ->
      let challenge = runPut (put bid <> put publicKey)
          proof = Bls.proveKnowledgeOfSK challenge secretKey
      in return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerAggregationVerifyKey bid publicKey proof))
    (TJSON meta (UpdateBakerElectionKey bid sk pk) keys) ->
      let challenge = runPut (put bid <> put pk)
      in do
        Just ubekProof <- liftIO $ Proofs.proveDlog25519VRF challenge (VRF.KeyPair sk pk)
        return $ signTx keys meta (Types.encodePayload (Types.UpdateBakerElectionKey bid pk ubekProof))
    (TJSON meta (UpdateAccountKeys keyUpdates) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.UpdateAccountKeys keyUpdates))
    (TJSON meta (RemoveAccountKeys keyIdxs threshold) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.RemoveAccountKeys keyIdxs threshold))
    (TJSON meta (AddAccountKeys newKeys threshold) keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.AddAccountKeys newKeys threshold))
    (TJSON meta TransferToEncrypted{..} keys) ->
      return $ signTx keys meta (Types.encodePayload (Types.TransferToEncrypted tteAmount))
    (TJSON meta EncryptedAmountTransfer{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.EncryptedAmountTransfer{..})
    (TJSON meta TransferToPublic{..} keys) ->
      return $ signTx keys meta (Types.encodePayload Types.TransferToPublic{..})


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
                 -- FIXME: These should be updated to support more than one keypair for the account.
                 -- Need to demonstrate knowledge of all the relevant keys.
                 | AddBaker {
                     bvfkey :: BakerElectionVerifyKey,
                     bvfSecretKey :: VRF.SecretKey,
                     bsigvfkey :: BakerSignVerifyKey,
                     baggvfkey :: BakerAggregationVerifyKey,
                     baggsigkey :: BakerAggregationPrivateKey,
                     bsigkey :: BlockSig.SignKey,
                     baccountAddress :: AccountAddress,
                     baccountKeyPair :: Sig.KeyPair
                 }
                 | RemoveBaker {
                     rbId :: !BakerId
                     }
                 -- FIXME: These should be updated to support more than one keypair.
                 | UpdateBakerAccount {
                     ubaId :: !BakerId,
                     ubaAddress :: !AccountAddress,
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
                 | UpdateBakerAggregationVerifyKey {
                     ubavkId :: !BakerId,
                     ubavkKey :: !BakerAggregationVerifyKey,
                     ubavkPrivateKey :: !BakerAggregationPrivateKey
                     }
                 | UpdateBakerElectionKey {
                     ubekId :: !BakerId,
                     ubekSecretKey :: !VRF.SecretKey,
                     ubekPublicKey :: !VRF.PublicKey
                     }
                 | UpdateAccountKeys {
                     uakUpdates :: !(Map.Map KeyIndex AccountVerificationKey)
                     }
                 | RemoveAccountKeys {
                     rakIndices :: !(Set.Set KeyIndex),
                     rakThreshold :: !(Maybe SignatureThreshold)
                     }
                 | AddAccountKeys {
                     aakKeys :: !(Map.Map KeyIndex AccountVerificationKey),
                     aakThreshold :: !(Maybe SignatureThreshold)
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
                             , keys :: [(KeyIndex, KeyPair)]
                             }
  deriving(Show,Generic)
