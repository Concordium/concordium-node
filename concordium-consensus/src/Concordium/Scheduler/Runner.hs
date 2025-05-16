{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Concordium.Scheduler.Runner where

import GHC.Generics (Generic)

import Data.Maybe
import Data.Text (Text)

import Control.Monad.IO.Class

import qualified Concordium.Crypto.BlockSignature as BlockSig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.Proofs as Proofs
import Concordium.Crypto.SignatureScheme (KeyPair)
import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Types.Updates as Updates

import Concordium.ID.Types
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types
import qualified Concordium.Wasm as Wasm

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map as Map

import Concordium.Crypto.EncryptedTransfers
import Prelude hiding (exp, mod)

-- | Sign a transaction with the given list of keys.
signTx :: [(CredentialIndex, [(KeyIndex, KeyPair)])] -> TransactionHeader -> EncodedPayload -> Types.AccountTransaction
signTx keys TransactionHeader{..} encPayload = Types.signTransaction keys header encPayload
  where
    header = Types.TransactionHeader{thPayloadSize = Types.payloadSize encPayload, ..}

signTxSingle :: KeyPair -> TransactionHeader -> EncodedPayload -> Types.AccountTransaction
signTxSingle key TransactionHeader{..} encPayload = Types.signTransactionSingle key header encPayload
  where
    header = Types.TransactionHeader{thPayloadSize = Types.payloadSize encPayload, ..}

signChainTx :: ChainTransaction -> Types.UpdateInstruction
signChainTx
    ChainTransaction
        { ctSeqNumber = ruiSeqNumber,
          ctEffectiveTime = ruiEffectiveTime,
          ctTimeout = ruiTimeout,
          ctPayload = ruiPayload,
          ..
        } = Updates.makeUpdateInstruction Updates.RawUpdateInstruction{..} $ Map.fromList ctKeys

transactionHelper :: (MonadFail m, MonadIO m) => TransactionJSON -> m Types.AccountTransaction
transactionHelper t =
    case t of
        (TJSON meta (DeployModule version mnameText) keys) -> liftIO $ do
            BS.readFile mnameText >>= \wasmMod ->
                let modl = case version of
                        Wasm.V0 -> Wasm.WasmModuleV0 . Wasm.WasmModuleV . Wasm.ModuleSource $ wasmMod
                        Wasm.V1 -> Wasm.WasmModuleV1 . Wasm.WasmModuleV . Wasm.ModuleSource $ wasmMod
                in  return $ signTx keys meta . Types.encodePayload . Types.DeployModule $ modl
        (TJSON meta (InitContract icAmount version mnameText cNameText paramExpr) keys) -> liftIO $ do
            BS.readFile mnameText >>= \wasmMod ->
                let icModRef = case version of
                        Wasm.V0 -> Wasm.getModuleRef @Wasm.V0 . Wasm.WasmModuleV . Wasm.ModuleSource $ wasmMod
                        Wasm.V1 -> Wasm.getModuleRef @Wasm.V1 . Wasm.WasmModuleV . Wasm.ModuleSource $ wasmMod
                    payload =
                        Types.InitContract
                            { icInitName = Wasm.InitName cNameText,
                              icParam = Wasm.Parameter paramExpr,
                              ..
                            }
                in  return . signTx keys meta . Types.encodePayload $ payload
        (TJSON meta (Update uAmount uAddress rNameText paramExpr) keys) ->
            return $
                let payload =
                        Types.Update
                            { uReceiveName = Wasm.ReceiveName rNameText,
                              uMessage = Wasm.Parameter paramExpr,
                              ..
                            }
                in  signTx keys meta . Types.encodePayload $ payload
        (TJSON meta (Transfer to amnt) keys) ->
            return $ signTx keys meta (Types.encodePayload (Types.Transfer to amnt))
        (TJSON meta AddBaker{..} keys) ->
            let abElectionVerifyKey = bElectionVerifyKey
                abSignatureVerifyKey = bSignVerifyKey
                abAggregationVerifyKey = bAggregateVerifyKey
                abBakingStake = bInitialStake
                abRestakeEarnings = bRestakeEarnings
                challenge = Types.addBakerChallenge (thSender meta) bElectionVerifyKey bSignVerifyKey bAggregateVerifyKey
            in  do
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
            in
                do
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
        (TJSON meta (TransferWithMemo to memo amnt) keys) ->
            return $ signTx keys meta (Types.encodePayload (Types.TransferWithMemo to memo amnt))
        (TJSON meta EncryptedAmountTransferWithMemo{..} keys) ->
            return $ signTx keys meta (Types.encodePayload Types.EncryptedAmountTransferWithMemo{..})
        (TJSON meta TransferWithScheduleAndMemo{..} keys) ->
            return $ signTx keys meta (Types.encodePayload Types.TransferWithScheduleAndMemo{..})
        (TJSON meta ConfigureBaker{..} keys) ->
            return $ signTx keys meta (Types.encodePayload Types.ConfigureBaker{..})
        (TJSON meta ConfigureDelegation{..} keys) ->
            return $ signTx keys meta (Types.encodePayload Types.ConfigureDelegation{..})
        (TJSON meta TokenHolder{..} keys) ->
            return $ signTx keys meta (Types.encodePayload Types.TokenHolder{..})

processTransactions :: (MonadFail m, MonadIO m) => [TransactionJSON] -> m [Types.AccountTransaction]
processTransactions = mapM transactionHelper

processAnyTransactions :: (MonadFail m, MonadIO m) => [AnyTransaction] -> m [Types.BareBlockItem]
processAnyTransactions = mapM anyTxHelper

-- | This is a special case of `processAnyUngroupedTransactions` below that
-- only support account transactions. Kept for compatibility with existing tests that only use
-- account transactions.
processUngroupedTransactions ::
    (MonadFail m, MonadIO m) =>
    [TransactionJSON] ->
    m Types.GroupedTransactions
processUngroupedTransactions inpt = do
    txs <- processTransactions inpt
    -- We just attach a `Nothing` to the transaction such that it will be verified by the scheduler.
    return (map (\x -> Types.TGAccountTransactions [(Types.fromAccountTransaction 0 x, Nothing)]) txs)

-- | For testing purposes: process transactions without grouping them by accounts
--  (i.e. creating one "group" per transaction).
--  Arrival time of transactions is taken to be 0.
processAnyUngroupedTransactions ::
    (MonadFail m, MonadIO m) =>
    [AnyTransaction] ->
    m Types.GroupedTransactions
processAnyUngroupedTransactions inpt = do
    txs <- processAnyTransactions inpt
    -- We just attach a `Nothing` to the transaction such that it will be verified by the scheduler.
    return (map txMap txs)
  where
    txMap (Types.NormalTransaction x) = Types.TGAccountTransactions [(Types.fromAccountTransaction 0 x, Nothing)]
    txMap (Types.CredentialDeployment x) = Types.TGCredentialDeployment (Types.addMetadata Types.CredentialDeployment 0 x, Nothing)
    txMap (Types.ChainUpdate x) = Types.TGUpdateInstructions [(Types.addMetadata Types.ChainUpdate 0 x, Nothing)]

-- | For testing purposes: process transactions in the groups in which they came
--  The arrival time of all transactions is taken to be 0.
processGroupedTransactions ::
    (MonadFail m, MonadIO m) =>
    [[TransactionJSON]] ->
    m Types.GroupedTransactions
-- We just attach a `Nothing` to the transaction such that it will be verified by the scheduler.
processGroupedTransactions =
    fmap (Types.fromTransactions . map (map (\x -> (Types.fromAccountTransaction 0 x, Nothing))))
        . mapM processTransactions

data PayloadJSON
    = DeployModule {wasmVersion :: Wasm.WasmVersion, moduleName :: FilePath}
    | InitContract
        { amount :: Amount,
          version :: Wasm.WasmVersion,
          moduleName :: FilePath,
          initFunctionName :: Text,
          parameter :: BSS.ShortByteString
        }
    | Update
        { amount :: Amount,
          address :: ContractAddress,
          receiveName :: Text,
          message :: BSS.ShortByteString
        }
    | Transfer
        { toaddress :: AccountAddress,
          amount :: Amount
        }
    | AddBaker
        { bElectionVerifyKey :: BakerElectionVerifyKey,
          bElectionSecretKey :: VRF.SecretKey,
          bSignVerifyKey :: BakerSignVerifyKey,
          bSignSecretKey :: BlockSig.SignKey,
          bAggregateVerifyKey :: BakerAggregationVerifyKey,
          bAggregateSecretKey :: BakerAggregationPrivateKey,
          bInitialStake :: Amount,
          bRestakeEarnings :: Bool
        }
    | RemoveBaker
    | UpdateBakerStake
        { ubsStake :: !Amount
        }
    | UpdateBakerRestakeEarnings
        { ubreRestakeEarnings :: !Bool
        }
    | UpdateBakerKeys
        { bElectionVerifyKey :: BakerElectionVerifyKey,
          bElectionSecretKey :: VRF.SecretKey,
          bSignVerifyKey :: BakerSignVerifyKey,
          bSignSecretKey :: BlockSig.SignKey,
          bAggregateVerifyKey :: BakerAggregationVerifyKey,
          bAggregateSecretKey :: BakerAggregationPrivateKey
        }
    | UpdateCredentialKeys
        { -- | New set of credential keys to be replaced with the existing ones, including updating the threshold.
          uckCredId :: CredentialRegistrationID,
          uckKeys :: !CredentialPublicKeys
        }
    | TransferToEncrypted
        { tteAmount :: !Amount
        }
    | EncryptedAmountTransfer
        { eatTo :: !AccountAddress,
          eatData :: !EncryptedAmountTransferData
        }
    | TransferToPublic
        { ttpData :: !SecToPubAmountTransferData
        }
    | TransferWithSchedule
        { twsTo :: !AccountAddress,
          twsSchedule :: ![(Timestamp, Amount)]
        }
    | UpdateCredentials
        { ucNewCredInfos :: !(Map.Map CredentialIndex CredentialDeploymentInformation),
          ucRemoveCredIds :: ![CredentialRegistrationID],
          ucNewThreshold :: !AccountThreshold
        }
    | TransferWithMemo
        { twmToAddress :: !AccountAddress,
          twmMemo :: !Memo,
          twmAmount :: !Amount
        }
    | EncryptedAmountTransferWithMemo
        { eatwmTo :: !AccountAddress,
          eatwmMemo :: !Memo,
          eatwmData :: !EncryptedAmountTransferData
        }
    | TransferWithScheduleAndMemo
        { twswmTo :: !AccountAddress,
          twswmMemo :: !Memo,
          twswmSchedule :: ![(Timestamp, Amount)]
        }
    | ConfigureBaker
        { -- | The equity capital of the baker
          cbCapital :: !(Maybe Amount),
          -- | Whether the baker's earnings are restaked
          cbRestakeEarnings :: !(Maybe Bool),
          -- | Whether the pool is open for delegators
          cbOpenForDelegation :: !(Maybe Types.OpenStatus),
          -- | The key/proof pairs to verify baker.
          cbKeysWithProofs :: !(Maybe Types.BakerKeysWithProofs),
          -- | The URL referencing the baker's metadata.
          cbMetadataURL :: !(Maybe UrlText),
          -- | The commission the pool owner takes on transaction fees.
          cbTransactionFeeCommission :: !(Maybe AmountFraction),
          -- | The commission the pool owner takes on baking rewards.
          cbBakingRewardCommission :: !(Maybe AmountFraction),
          -- | The commission the pool owner takes on finalization rewards.
          cbFinalizationRewardCommission :: !(Maybe AmountFraction),
          -- | Whether to suspend/resume the validator.
          cbSuspend :: !(Maybe Bool)
        }
    | ConfigureDelegation
        { -- | The capital delegated to the pool.
          cdCapital :: !(Maybe Amount),
          -- | Whether the delegator's earnings are restaked.
          cdRestakeEarnings :: !(Maybe Bool),
          -- | The target of the delegation.
          cdDelegationTarget :: !(Maybe Types.DelegationTarget)
        }
    | -- \| An update for a protocol level token.
      TokenHolder
        { -- | Identifier of the token type to which the transaction refers.
          thTokenSymbol :: !Types.TokenId,
          -- | The CBOR-encoded operations to perform.
          thOperations :: !Types.TokenParameter
        }
    deriving (Show, Generic)

data TransactionHeader = TransactionHeader
    { -- | Sender account address.
      thSender :: !AccountAddress,
      -- | Per account nonce, strictly increasing, no gaps.
      thNonce :: !Nonce,
      -- | Amount of gas dedicated for the execution of this transaction.
      thEnergyAmount :: !Energy,
      -- | Expiration time after which transaction will not be executed
      thExpiry :: TransactionExpiryTime
    }
    deriving (Show)

data TransactionJSON = TJSON
    { metadata :: TransactionHeader,
      payload :: PayloadJSON,
      keys :: [(CredentialIndex, [(KeyIndex, KeyPair)])]
    }
    deriving (Show, Generic)

data ChainTransaction = ChainTransaction
    { ctSeqNumber :: Updates.UpdateSequenceNumber,
      ctEffectiveTime :: Types.TransactionTime,
      ctTimeout :: Types.TransactionTime,
      ctPayload :: Updates.UpdatePayload,
      ctKeys :: [(Updates.UpdateKeyIndex, KeyPair)]
    }
    deriving (Show, Generic)

data AnyTransaction = AccountTx TransactionJSON | ChainTx ChainTransaction
    deriving (Show, Generic)

anyTxHelper :: (MonadFail m, MonadIO m) => AnyTransaction -> m Types.BareBlockItem
anyTxHelper (ChainTx tx) = return $ Types.ChainUpdate $ signChainTx tx
anyTxHelper (AccountTx tx) = Types.NormalTransaction <$> transactionHelper tx
