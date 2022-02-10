{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-|
The scheduler executes transactions (including credential deployment), updating the current block state.
It can

  * Execute a given list of transactions until failure ('runTransactions' / 'execTransactions'), used to execute a given block.
  * Select transactions to create a new block ('filterTransactions').

= Processing of transactions

  * Processing happens in the 'SchedulerMonad'.

  * The processing of a transaction can end in three different ways (see also 'TxResult'):

      1. The transaction is invalid and can not be part of a block. The block state is thus not
         changed. This can for example be because the transaction has an invalid
         header (e.g. incorrect signatures). For all possible kinds of this failure see 'FailureKind'.
      2. The transaction is valid and can be part of a block. The block state is updated with the effects
         of the transaction, including the sender being charged for execution. A 'ValidResult' is
         returned.

          2a. The transaction is executed successfully - 'TxSuccess' with a list of events is returned.

          2b. Execution of the transaction fails - 'TxReject' with the reason (see 'RejectReason')
              is returned.
              This can for example happen when the deposited energy is not sufficient to cover the
              execution cost of the transaction ('OutOfEnergy') or some specific conditions of the
              respective transaction are not satisfied.

-}
module Concordium.Scheduler
  (filterTransactions
  ,runTransactions
  ,execTransactions
  ,handleContractUpdateV1
  ,handleContractUpdateV0
  ,checkAndGetBalanceInstanceV1
  ,checkAndGetBalanceInstanceV0
  ,checkAndGetBalanceAccountV1
  ,checkAndGetBalanceAccountV0
  ,FilteredTransactions(..)
  ) where
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Wasm as Wasm
import qualified Concordium.Scheduler.WasmIntegration as WasmV0
import qualified Concordium.Scheduler.WasmIntegration.V1 as WasmV1
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment
import Data.Time
import Concordium.TimeMonad
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.BlockState (AccountOperations(..), AccountAllowance (..))
import qualified Concordium.GlobalState.BakerInfo as BI
import qualified Concordium.GlobalState.Instance as Ins
import Concordium.GlobalState.Types
import qualified Concordium.Cost as Cost
import Concordium.Crypto.EncryptedTransfers

import Control.Applicative
import Concordium.Logger
import Control.Monad.Except
import Data.Function (on)
import Data.List (find, foldl')
import qualified Data.Map.Strict as OrdMap
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

import qualified Concordium.Crypto.Proofs as Proofs
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.TransactionVerification as TVer

import Lens.Micro.Platform

import Prelude hiding (exp, mod)
import Concordium.Scheduler.WasmIntegration.V1 (ReceiveResultData(rrdCurrentState))


-- |The function asserts the following
--  * the transaction has a valid sender,
--  * the amount corresponding to the deposited energy is on the sender account,
--  * the transaction is not expired,
--  * the transaction nonce is the account's next nonce,
--  * the transaction is signed with the account's verification keys.
-- "Valid sender" means that the sender account exists and has at least one valid credential,
-- where currently valid means non-expired.
--
-- Throws 'Nothing' if the remaining block energy is not sufficient to cover the cost of checking the
-- header and @Just fk@ if any of the checks fail, with the respective 'FailureKind'.
--
-- Important! If @mVerRes@ is `Just VerificationResult` then it MUST be the `VerificationResult` matching the provided transaction.
--
-- Returns the sender account and the cost to be charged for checking the header.
checkHeader :: forall pv msg m . (TransactionData msg, SchedulerMonad pv m) => msg -> Maybe TVer.VerificationResult -> ExceptT (Maybe FailureKind) m (IndexedAccount m, Energy)
checkHeader meta mVerRes = do
  unless (validatePayloadSize (protocolVersion @pv) (thPayloadSize (transactionHeader meta))) $ throwError $ Just InvalidPayloadSize
  -- Before even checking the header we calculate the cost that will be charged for this
  -- and check that at least that much energy is deposited and remaining from the maximum block energy.
  let cost = Cost.baseCost (getTransactionHeaderPayloadSize $ transactionHeader meta) (getTransactionNumSigs (transactionSignature meta))
  remainingBlockEnergy <- lift getRemainingEnergy
  -- check that enough energy is remaining for the block.
  unless (remainingBlockEnergy >= cost) $ throwError Nothing
  
  -- check that the transaction is not expired
  cm <- lift getChainMetadata
  when (transactionExpired (thExpiry $ transactionHeader meta) $ slotTime cm) $ throwError . Just $ ExpiredTransaction
  
  let addr = transactionSender meta
  miacc <- lift (getStateAccount addr)
  case miacc of
    -- check if the sender is present on the chain.
    Nothing -> throwError (Just $ UnknownAccount addr)
    Just iacc -> do
      -- The sender exists and thus we continue verifying the transaction.
      
      -- We check if we previously have deemed the transaction valid and check if the
      -- current account information matches with one at the point of verification.
      -- Also we check that the nonce is valid and that the sender has enough funds to cover his transfer.
      let acc = snd iacc
      case mVerRes of
        Just (TVer.Ok (TVer.NormalTransactionSuccess keysHash _)) -> do
          currentKeys <- lift (TVer.getAccountVerificationKeys acc)
          -- Check that the keys match from initial verification.
          -- If they match we skip checking the signature as it has already been verified.
          if ID.matchesAccountInformation currentKeys keysHash then do
            checkNonceAndFunds acc
            return (iacc, cost)
            -- the account information has changed, so we re-verify the signature.
          else do
            unless (verifyTransaction currentKeys meta) (throwError $ Just IncorrectSignature)
            checkNonceAndFunds acc
            return (iacc, cost)
        -- An invalid verification result or `Nothing` was supplied to this function.
        -- In either case we verify the transaction now.
        _ -> do 
              newVerRes <- lift (TVer.verifyNormalTransaction meta)
              case checkTransactionVerificationResult newVerRes of
                Left failure -> throwError . Just $ failure
                Right _ -> return (iacc, cost)
  where
    -- check that the nonce is ok and that the sender has enough funds to cover the transaction fee deposit.
    checkNonceAndFunds acc = do
      -- Check that the nonce is still 'Ok'.
      nextNonce <- lift (TVer.getNextAccountNonce acc)
      let nonce = transactionNonce meta
      unless (nonce == nextNonce) $ throwError (Just $ NonSequentialNonce nonce)
      -- Check that the account still has enough funds to cover the deposit
      amnt <- lift (TVer.getAccountAvailableAmount acc)
      depositedAmount <- lift (TVer.energyToCcd (transactionGasAmount meta))
      unless (depositedAmount <= amnt) $ throwError $ Just InsufficientFunds

-- |Maps transaction verification results into Either `FailureKind`s. or `OkResult`s
checkTransactionVerificationResult :: TVer.VerificationResult -> Either FailureKind TVer.OkResult
-- 'Ok' mappings
checkTransactionVerificationResult (TVer.Ok res) = Right res
-- 'MaybeOK' mappings
checkTransactionVerificationResult (TVer.MaybeOk (TVer.CredentialDeploymentInvalidIdentityProvider ipid)) = Left $ NonExistentIdentityProvider ipid
checkTransactionVerificationResult (TVer.MaybeOk TVer.CredentialDeploymentInvalidAnonymityRevokers) = Left UnsupportedAnonymityRevokers
checkTransactionVerificationResult (TVer.MaybeOk (TVer.ChainUpdateInvalidNonce expectedSequenceNumber)) = Left $ NonSequentialNonce expectedSequenceNumber
checkTransactionVerificationResult (TVer.MaybeOk TVer.ChainUpdateInvalidSignatures) = Left IncorrectSignature
checkTransactionVerificationResult (TVer.MaybeOk (TVer.NormalTransactionInvalidSender aaddr)) = Left $ UnknownAccount aaddr
checkTransactionVerificationResult (TVer.MaybeOk (TVer.NormalTransactionInvalidNonce nonce)) = Left $ NonSequentialNonce nonce
checkTransactionVerificationResult (TVer.MaybeOk TVer.NormalTransactionInvalidSignatures) = Left IncorrectSignature
checkTransactionVerificationResult (TVer.MaybeOk TVer.NormalTransactionInsufficientFunds) = Left InsufficientFunds
-- 'NotOk' mappings
checkTransactionVerificationResult (TVer.NotOk (TVer.CredentialDeploymentDuplicateAccountRegistrationID regId)) = Left $ DuplicateAccountRegistrationID regId
checkTransactionVerificationResult (TVer.NotOk TVer.CredentialDeploymentInvalidSignatures) = Left AccountCredentialInvalid
checkTransactionVerificationResult (TVer.NotOk TVer.CredentialDeploymentExpired) = Left AccountCredentialInvalid
checkTransactionVerificationResult (TVer.NotOk (TVer.ChainUpdateSequenceNumberTooOld nonce)) = Left $ NonSequentialNonce nonce
checkTransactionVerificationResult (TVer.NotOk TVer.ChainUpdateEffectiveTimeBeforeTimeout) = Left InvalidUpdateTime
checkTransactionVerificationResult (TVer.NotOk TVer.NormalTransactionDepositInsufficient) = Left DepositInsufficient
checkTransactionVerificationResult (TVer.NotOk TVer.NormalTransactionEnergyExceeded) = Left ExceedsMaxBlockEnergy
checkTransactionVerificationResult (TVer.NotOk (TVer.NormalTransactionDuplicateNonce nonce)) = Left $ NonSequentialNonce nonce
checkTransactionVerificationResult (TVer.NotOk TVer.Expired) = Left ExpiredTransaction
checkTransactionVerificationResult (TVer.NotOk TVer.InvalidPayloadSize) = Left InvalidPayloadSize


-- | Execute a transaction on the current block state, charging valid accounts
-- for the resulting energy cost.
--
-- First checks the meta data in the header of the transaction, then decodes the
-- payload and lets the respective handler execute the actual transaction.
-- 
-- The payload is accompanied by its `VerificationResult` from when the transaction was received.
-- As per above the transaction is verified prior to execution.
-- This verification process is leveraged by this accompanying `VerificationResult`.
-- Hence if the transaction has been successfully verified already then some verification steps
-- can be skipped.
-- 
-- Returns
--
-- * @Nothing@ if the transaction would exceed the remaining block energy.
-- * @Just result@ if the transaction failed ('TxInvalid') or was successfully committed
--  ('TxValid', with either 'TxSuccess' or 'TxReject').
dispatch :: forall msg pv m. (TransactionData msg, SchedulerMonad pv m) => (msg, Maybe TVer.VerificationResult) -> m (Maybe TxResult)
dispatch (msg, mVerRes) = do
  let meta = transactionHeader msg
  validMeta <- runExceptT (checkHeader msg mVerRes)
  case validMeta of
    Left (Just fk) -> return $ Just (TxInvalid fk)
    Left Nothing -> return Nothing
    Right (senderAccount, checkHeaderCost) -> do
      -- At this point the transaction is going to be committed to the block.
      -- It could be that the execution exceeds maximum block energy allowed, but in that case
      -- the whole block state will be removed, and thus this operation will have no effect anyhow.
      -- Hence we can increase the account nonce of the sender account.
      increaseAccountNonce senderAccount

      let psize = payloadSize (transactionPayload msg)

      tsIndex <- bumpTransactionIndex
      case decodePayload (protocolVersion @pv) psize (transactionPayload msg) of
        Left _ -> do
          -- In case of serialization failure we charge the sender for checking
          -- the header and reject the transaction; we have checked that the amount
          -- exists on the account with 'checkHeader'.
          payment <- energyToGtu checkHeaderCost
          chargeExecutionCost senderAccount payment
          return $ Just $ TxValid $ TransactionSummary{
            tsEnergyCost = checkHeaderCost,
            tsCost = payment,
            tsSender = Just (thSender meta), -- the sender of the transaction is as specified in the transaction.
            tsResult = TxReject SerializationFailure,
            tsHash = transactionHash msg,
            tsType = TSTAccountTransaction Nothing,
            ..
            }
        Right payload -> do
          usedBlockEnergy <- getUsedEnergy
          let mkWTC _wtcTransactionType = WithDepositContext{
                _wtcSenderAccount = senderAccount,
                _wtcTransactionHash = transactionHash msg,
                _wtcTransactionHeader = meta,
                _wtcTransactionCheckHeaderCost = checkHeaderCost,
                -- NB: We already account for the cost we used here.
                _wtcCurrentlyUsedBlockEnergy = usedBlockEnergy + checkHeaderCost,
                _wtcTransactionIndex = tsIndex,
                ..}
          -- Now pass the decoded payload to the respective transaction handler which contains
          -- the main transaction logic.
          -- During processing of transactions the amount on the sender account is decreased by the
          -- amount corresponding to the deposited energy, i.e., the maximum amount that can be charged
          -- for execution. The amount corresponding to the unused energy is refunded at the end of
          -- processing; see `withDeposit`.
          res <- case payload of
                   DeployModule mod ->
                     handleDeployModule (mkWTC TTDeployModule) mod

                   InitContract{..} ->
                     handleInitContract (mkWTC TTInitContract) icAmount icModRef icInitName icParam

                   Transfer toaddr amount ->
                     handleSimpleTransfer (mkWTC TTTransfer) toaddr amount Nothing

                   Update{..} ->
                     handleUpdateContract (mkWTC TTUpdate) uAmount uAddress uReceiveName uMessage

                   AddBaker{..} ->
                     handleAddBaker (mkWTC TTAddBaker) abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abProofSig abProofElection abProofAggregation abBakingStake abRestakeEarnings

                   RemoveBaker ->
                     handleRemoveBaker (mkWTC TTRemoveBaker)

                   UpdateBakerStake{..} ->
                     handleUpdateBakerStake (mkWTC TTUpdateBakerStake) ubsStake

                   UpdateBakerRestakeEarnings{..} ->
                     handleUpdateBakerRestakeEarnings (mkWTC TTUpdateBakerRestakeEarnings) ubreRestakeEarnings

                   UpdateBakerKeys{..} ->
                     handleUpdateBakerKeys (mkWTC TTUpdateBakerKeys) ubkElectionVerifyKey ubkSignatureVerifyKey ubkAggregationVerifyKey ubkProofSig ubkProofElection ubkProofAggregation

                   UpdateCredentialKeys{..} ->
                     handleUpdateCredentialKeys (mkWTC TTUpdateCredentialKeys) uckCredId uckKeys (transactionSignature msg)

                   EncryptedAmountTransfer{..} ->
                     handleEncryptedAmountTransfer (mkWTC TTEncryptedAmountTransfer) eatTo eatData Nothing

                   TransferToEncrypted{..} ->
                     handleTransferToEncrypted (mkWTC TTTransferToEncrypted) tteAmount

                   TransferToPublic{..} ->
                     handleTransferToPublic (mkWTC TTTransferToPublic) ttpData

                   TransferWithSchedule{..} ->
                     handleTransferWithSchedule (mkWTC TTTransferWithSchedule) twsTo twsSchedule Nothing

                   UpdateCredentials{..} ->
                     handleUpdateCredentials (mkWTC TTUpdateCredentials) ucNewCredInfos ucRemoveCredIds ucNewThreshold

                   RegisterData {..} ->
                     handleRegisterData (mkWTC TTRegisterData) rdData

                   TransferWithMemo toaddr memo amount ->
                     handleSimpleTransfer (mkWTC TTTransferWithMemo) toaddr amount $ Just memo

                   EncryptedAmountTransferWithMemo{..} ->
                     handleEncryptedAmountTransfer (mkWTC TTEncryptedAmountTransferWithMemo) eatwmTo eatwmData $ Just eatwmMemo

                   TransferWithScheduleAndMemo{..} ->
                     handleTransferWithSchedule (mkWTC TTTransferWithScheduleAndMemo) twswmTo twswmSchedule $ Just twswmMemo

          case res of
            -- The remaining block energy is not sufficient for the handler to execute the transaction.
            Nothing -> return Nothing
            Just summary -> return $ Just $ TxValid summary

handleTransferWithSchedule :: forall pv m .
  SchedulerMonad pv m
  => WithDepositContext m
  -> AccountAddress
  -> [(Timestamp, Amount)]
  -> Maybe Memo  -- ^Nothing in case of a TransferWithSchedule and Just in case of a TransferWithScheduleAndMemo
  -> m (Maybe TransactionSummary)
handleTransferWithSchedule wtc twsTo twsSchedule maybeMemo = withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        c = do
          -- After we've checked all of that, we charge.
          tickEnergy (Cost.scheduledTransferCost $ length twsSchedule)

          -- we do not allow for self scheduled transfers
          -- (This is checked later for protocol P3 and up, to ensure that the
          -- addresses are not aliases for the same account.)
          when ((demoteProtocolVersion (protocolVersion @pv) <= P2) && twsTo == senderAddress) $
              rejectTransaction (ScheduledSelfTransfer twsTo)

          -- Get the amount available for the account
          senderAmount <- getCurrentAccountAvailableAmount senderAccount

          -- check that we are not going to send an empty schedule
          case twsSchedule of
            [] -> rejectTransaction ZeroScheduledAmount
            (firstRelease@(firstTimestamp, firstReleaseAmount) : restOfReleases) -> do
              when (firstReleaseAmount == 0) $ rejectTransaction ZeroScheduledAmount
              -- check that the first timestamp has not yet passed
              cm <- getChainMetadata
              when (firstTimestamp < slotTime cm) $! rejectTransaction FirstScheduledReleaseExpired

              -- Check that the release schedule is strictly increasing, and that all amounts are non-zero, while also computing the sum
              (_, transferAmount) <- foldM (\(prev, acc) (i,v) -> if prev >= i
                                                                then rejectTransaction NonIncreasingSchedule
                                                                else if v == 0
                                                                     then rejectTransaction ZeroScheduledAmount
                                                                     else return (i, acc + v)) firstRelease restOfReleases

              -- check that this is not sending 0 tokens
              unless (transferAmount > 0) $! rejectTransaction ZeroScheduledAmount

              -- check if the available amount in the origin account is enough
              unless (senderAmount >= transferAmount) $! rejectTransaction (AmountTooLarge (AddressAccount senderAddress) transferAmount)

              -- check the target account
              targetAccount <- getStateAccount twsTo `rejectingWith` InvalidAccountReference twsTo
              -- In protocol version P3 account addresses are no longer in 1-1
              -- correspondence with accounts. Thus to check that a scheduled
              -- transfer is not a self transfer we need to check canonical
              -- account references. We use account indices for that here. The
              -- check above for scheduled self transfer using twsTo and
              -- senderAddress is thus redundant, however it must be kept there
              -- to keep protocol compatibility with P1 and P2 protocols.
              -- Rejected transactions's rejection reason is part of block
              -- hashes.
              when (demoteProtocolVersion (protocolVersion @pv) >= P3) $
                when (fst targetAccount == fst senderAccount) $ rejectTransaction . ScheduledSelfTransfer $ senderAddress

              withScheduledAmount senderAccount targetAccount transferAmount twsSchedule txHash $ return ()

        k ls () = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          commitChanges (ls ^. changeSet)
          let eventList = TransferredWithSchedule{etwsFrom = senderAddress, etwsTo = twsTo, etwsAmount = twsSchedule}
                  : (TransferMemo <$> maybeToList maybeMemo)
          return (TxSuccess eventList,
                    energyCost,
                    usedEnergy)


handleTransferToPublic ::
  SchedulerMonad pv m
  => WithDepositContext m
  -> SecToPubAmountTransferData
  -> m (Maybe TransactionSummary)
handleTransferToPublic wtc transferData@SecToPubAmountTransferData{..} = do
  cryptoParams <- TVer.getCryptographicParameters
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        c cryptoParams = do
          -- the expensive operations start now, so we charge.
          tickEnergy Cost.transferToPublicCost

          senderAllowed <- checkAccountIsAllowed (snd senderAccount) AllowedEncryptedTransfers
          unless senderAllowed $ rejectTransaction NotAllowedToHandleEncrypted

          -- Get the encrypted amount at the index that the transfer claims to be using.
          senderAmount <- getAccountEncryptedAmountAtIndex (snd senderAccount) stpatdIndex `rejectingWith` InvalidIndexOnEncryptedTransfer

          -- and then we start validating the proof. This is the most expensive
          -- part of the validation by far, the rest only being lookups and a little bit of addition.
          senderPK <- getAccountEncryptionKey (snd senderAccount)
          let valid = verifySecretToPublicTransferProof cryptoParams senderPK senderAmount transferData

          unless valid $ rejectTransaction InvalidTransferToPublicProof

          -- if the proof is valid we need to
          -- - add the decrypted amount to the balance
          -- - replace some encrypted amounts on the sender's account
          addAmountFromEncrypted senderAccount stpatdTransferAmount stpatdIndex stpatdRemainingAmount

          return senderAmount

        k ls senderAmount = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          notifyEncryptedBalanceChange $ amountDiff 0 stpatdTransferAmount
          commitChanges (ls ^. changeSet)
          return (TxSuccess [EncryptedAmountsRemoved{
                                earAccount = senderAddress,
                                earUpToIndex = stpatdIndex,
                                earInputAmount = senderAmount,
                                earNewAmount = stpatdRemainingAmount
                                },
                              AmountAddedByDecryption{
                                aabdAccount = senderAddress,
                                aabdAmount = stpatdTransferAmount
                                }],
                   energyCost,
                   usedEnergy)


handleTransferToEncrypted ::
  SchedulerMonad pv m
  => WithDepositContext m
  -> Amount
  -> m (Maybe TransactionSummary)
handleTransferToEncrypted wtc toEncrypted = do
  cryptoParams <- TVer.getCryptographicParameters
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta

        c cryptoParams = do

          tickEnergy Cost.transferToEncryptedCost

          senderAllowed <- checkAccountIsAllowed (snd senderAccount) AllowedEncryptedTransfers
          unless senderAllowed $ rejectTransaction NotAllowedToHandleEncrypted

          -- check that the sender actually owns the amount it claims to be transferred
          senderamount <- getCurrentAccountAvailableAmount senderAccount
          unless (senderamount >= toEncrypted) $! rejectTransaction (AmountTooLarge (AddressAccount senderAddress) toEncrypted)

          -- compute the encrypted amount
          let encryptedAmount = encryptAmountZeroRandomness cryptoParams toEncrypted

          -- We have to subtract the amount and update the self encrypted amount
          addSelfEncryptedAmount senderAccount toEncrypted encryptedAmount

          return encryptedAmount

        k ls encryptedAmount = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          notifyEncryptedBalanceChange $ amountToDelta toEncrypted
          commitChanges (ls ^. changeSet)

          return (TxSuccess [EncryptedSelfAmountAdded{
                                eaaAccount = senderAddress,
                                eaaNewAmount = encryptedAmount,
                                eaaAmount = toEncrypted
                                }],
                   energyCost,
                   usedEnergy)

handleEncryptedAmountTransfer :: forall pv m .
  SchedulerMonad pv m
  => WithDepositContext m
  -> AccountAddress -- ^ Receiver address.
  -> EncryptedAmountTransferData
  -> Maybe Memo -- ^Nothing in case of an EncryptedAmountTransfer and Just in case of an EncryptedAmountTransferWithMemo
  -> m (Maybe TransactionSummary)
handleEncryptedAmountTransfer wtc toAddress transferData@EncryptedAmountTransferData{..} maybeMemo = do
  cryptoParams <- TVer.getCryptographicParameters
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta

        c cryptoParams = do

          -- We charge as soon as we can even if we could in principle do some
          -- checks that are cheaper.
          tickEnergy Cost.encryptedTransferCost

          -- We do not allow sending encrypted transfers from an account to itself.
          -- There is no reason to do so in the current setup, and it causes some technical
          -- complications.
          -- (This is checked later for protocol P3 and up, to ensure that the
          -- addresses are not aliases for the same account.)
          when ((demoteProtocolVersion (protocolVersion @pv) <= P2) && toAddress == senderAddress)
              $ rejectTransaction (EncryptedAmountSelfTransfer toAddress)

          senderAllowed <- checkAccountIsAllowed (snd senderAccount) AllowedEncryptedTransfers
          unless senderAllowed $ rejectTransaction NotAllowedToHandleEncrypted

          -- Look up the receiver account first, and don't charge if it does not exist
          -- and does not have a valid credential.
          targetAccount <- getStateAccount toAddress `rejectingWith` InvalidAccountReference toAddress
          -- Check that the account is not transferring to itself since that
          -- causes technical complications. In protocol versions 1 and 2
          -- account addresses and accounts were in 1-1 correspondence. In
          -- protocol version 3 an account may have multiple addresses and thus
          -- to check a self transfer we must check with canonical account
          -- identifiers. We use account indices for that.
          when ((demoteProtocolVersion (protocolVersion @pv) >= P3) && fst targetAccount == fst senderAccount)
              $ rejectTransaction . EncryptedAmountSelfTransfer $ senderAddress


          receiverAllowed <- checkAccountIsAllowed (snd targetAccount) AllowedEncryptedTransfers
          unless receiverAllowed $ rejectTransaction NotAllowedToReceiveEncrypted

          -- Get the encrypted amount at the index that the transfer claims to be using.
          senderAmount <- getAccountEncryptedAmountAtIndex (snd senderAccount) eatdIndex `rejectingWith` InvalidIndexOnEncryptedTransfer
          -- and then we start validating the proof. This is the most expensive
          -- part of the validation by far, the rest only being lookups.
          receiverPK <- getAccountEncryptionKey (snd targetAccount)
          senderPK <- getAccountEncryptionKey (snd senderAccount)
          let valid = verifyEncryptedTransferProof cryptoParams receiverPK senderPK senderAmount transferData

          unless valid $ rejectTransaction InvalidEncryptedAmountTransferProof

          -- if the proof is valid we need to
          -- - update the receiver account with an additional amount
          -- - replace some encrypted amounts on the sender's account
          -- We do this by first replacing on the sender's account, and then adding.
          -- The order does not matter since we disallow encrypted transfer from
          -- the account to itself.

          replaceEncryptedAmount senderAccount eatdIndex eatdRemainingAmount
          -- The index that the new amount on the receiver's account will get
          targetAccountEncryptedAmountIndex <- addEncryptedAmount targetAccount eatdTransferAmount

          return (targetAccountEncryptedAmountIndex, senderAmount)

        k ls (targetAccountEncryptedAmountIndex, senderAmount) = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          commitChanges (ls ^. changeSet)
          let eventList = [EncryptedAmountsRemoved{
                                earAccount = senderAddress,
                                earUpToIndex = eatdIndex,
                                earInputAmount = senderAmount,
                                earNewAmount = eatdRemainingAmount
                                },
                             NewEncryptedAmount{
                                neaAccount = toAddress,
                                neaNewIndex = targetAccountEncryptedAmountIndex,
                                neaEncryptedAmount = eatdTransferAmount
                                }
                            ] ++ (TransferMemo <$> maybeToList maybeMemo)

          return (TxSuccess eventList,
                   energyCost,
                   usedEnergy)

-- | Handle the deployment of a module.
handleDeployModule :: forall pv m .
  SchedulerMonad pv m
  => WithDepositContext m
  -> Wasm.WasmModule -- ^The module to deploy.
  -> m (Maybe TransactionSummary)
handleDeployModule wtc mod =
  withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    meta = wtc ^. wtcTransactionHeader

    c = do
      case mod of
        Wasm.WasmModuleV0 moduleV0 -> do
          tickEnergy (Cost.deployModuleCost (Wasm.moduleSourceLength (Wasm.wmvSource moduleV0)))
          case WasmV0.processModule moduleV0 of
              Nothing -> rejectTransaction ModuleNotWF
              Just iface -> do
                let mhash = GSWasm.moduleReference iface
                exists <- isJust <$> getModuleInterfaces mhash
                when exists $ rejectTransaction (ModuleHashAlreadyExists mhash)
                return (Left (iface, moduleV0), mhash)
        Wasm.WasmModuleV1 moduleV1 | demoteProtocolVersion (protocolVersion @pv) >= P4 -> do
          tickEnergy (Cost.deployModuleCost (Wasm.moduleSourceLength (Wasm.wmvSource moduleV1)))
          case WasmV1.processModule moduleV1 of
              Nothing -> rejectTransaction ModuleNotWF
              Just iface -> do
                let mhash = GSWasm.moduleReference iface
                exists <- isJust <$> getModuleInterfaces mhash
                when exists $ rejectTransaction (ModuleHashAlreadyExists mhash)
                return (Right (iface, moduleV1), mhash)
        _ -> rejectTransaction ModuleNotWF

    k ls (toCommit, mhash) = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost senderAccount energyCost
      -- Add the module to the global state (module interface, value interface and module itself).
      -- We know the module does not exist at this point, so we can ignore the return value.
      case toCommit of
        Left v0 -> () <$ commitModule v0
        Right v1 -> () <$ commitModule v1
      return (TxSuccess [ModuleDeployed mhash], energyCost, usedEnergy)

-- | Tick energy for storing the given contract state.
tickEnergyStoreState ::
  TransactionMonad pv m
  => Wasm.ContractState
  -> m ()
tickEnergyStoreState cs =
  -- Compute the size of the value and charge for storing based on this size.
  -- This uses the 'ResourceMeasure' instance for 'ByteSize' to determine the cost for storage.
  tickEnergy (Cost.toEnergy (Wasm.contractStateSize cs))

-- | Get the current contract state and charge for its lookup.
-- NB: In principle we should look up the state size, then charge, and only then lookup the full state
-- But since state size is limited to be small it is acceptable to look it up and then charge for it.
getCurrentContractInstanceTicking ::
  TransactionMonad pv m
  => ContractAddress
  -> m Instance
getCurrentContractInstanceTicking cref = getCurrentContractInstanceTicking' cref `rejectingWith` InvalidContractAddress cref

-- | Get the current contract state and charge for its lookup.
-- NB: In principle we should look up the state size, then charge, and only then lookup the full state
-- But since state size is limited to be small it is acceptable to look it up and then charge for it.
-- TODO: This function will be replaced once the state changes for V1 are in. Then it will only handle V0 instances.
getCurrentContractInstanceTicking' ::
  TransactionMonad pv m
  => ContractAddress
  -> m (Maybe Instance)
getCurrentContractInstanceTicking' cref = do
  getCurrentContractInstance cref >>= \case
    Nothing -> return Nothing
    Just inst -> do
      -- Compute the size of the contract state value and charge for the lookup based on this size.
      -- This uses the 'ResourceMeasure' instance for 'Cost.LookupByteSize' to determine the cost for lookup.
      case inst of
        InstanceV0 iv -> tickEnergy (Cost.lookupContractState $ Wasm.contractStateSize (Ins._instanceVModel iv))
        InstanceV1 iv -> tickEnergy (Cost.lookupContractState $ Wasm.contractStateSize (Ins._instanceVModel iv)) -- FIXME: This will be revised
      return (Just inst)


-- | Handle the initialization of a contract instance.
handleInitContract ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> Amount   -- ^The amount to initialize the contract instance with.
    -> ModuleRef  -- ^The module to initialize a contract from.
    -> Wasm.InitName -- ^Name of the init method to invoke.
    -> Wasm.Parameter  -- ^Parameter expression to initialize with.
    -> m (Maybe TransactionSummary)
handleInitContract wtc initAmount modref initName param =
  withDeposit wtc c k
    where senderAccount = wtc ^. wtcSenderAccount
          meta = wtc ^. wtcTransactionHeader
          -- The contract gets the address that was used when signing the
          -- transactions, as opposed to the canonical one.
          senderAddress = thSender meta
          c = do
            -- charge for base administrative cost
            tickEnergy Cost.initializeContractInstanceBaseCost

            -- Check whether the sender account's amount can cover the amount to initialize the contract
            -- with. Note that the deposit is already deducted at this point.
            senderAmount <- getCurrentAccountAvailableAmount senderAccount

            unless (senderAmount >= initAmount) $! rejectTransaction (AmountTooLarge (AddressAccount (thSender meta)) initAmount)

            -- First try to get the module interface of the parent module of the contract.
            viface <- liftLocal (getModuleInterfaces modref) `rejectingWith` InvalidModuleReference modref
            case viface of
              GSWasm.ModuleInterfaceV0 iface -> do
                let iSize = GSWasm.miModuleSize iface
                tickEnergy $ Cost.lookupModule iSize
    
                -- Then get the particular contract interface (in particular the type of the init method).
                unless (Set.member initName (GSWasm.miExposedInit iface)) $ rejectTransaction $ InvalidInitMethod modref initName
    
                cm <- liftLocal getChainMetadata
                -- Finally run the initialization function of the contract, resulting in an initial state
                -- of the contract. This ticks energy during execution, failing when running out of energy.
                -- NB: At this point the amount to initialize with has not yet been deducted from the
                -- sender account. Thus if the initialization function were to observe the current balance it would
                -- be amount - deposit. Currently this is in any case not exposed in contracts, but in case it
                -- is in the future we should be mindful of which balance is exposed.
                senderCredentials <- getAccountCredentials (snd senderAccount)
                let initCtx = Wasm.InitContext{
                      initOrigin = senderAddress,
                      icSenderPolicies = map (Wasm.mkSenderPolicy . snd) (OrdMap.toAscList senderCredentials)
                   }
                result <- runInterpreter (return . WasmV0.applyInitFun iface cm initCtx initName param initAmount)
                           `rejectingWith'` wasmRejectToRejectReasonInit
    
                -- Charge for storing the contract state.
                tickEnergyStoreState (Wasm.newState result)
                -- And for storing the instance.
                tickEnergy Cost.initializeContractInstanceCreateCost
    
                return (Left (iface, result))
                
              GSWasm.ModuleInterfaceV1 iface -> do
                let iSize = GSWasm.miModuleSize iface
                tickEnergy $ Cost.lookupModule iSize
    
                -- Then get the particular contract interface (in particular the type of the init method).
                unless (Set.member initName (GSWasm.miExposedInit iface)) $ rejectTransaction $ InvalidInitMethod modref initName
    
                cm <- liftLocal getChainMetadata
                -- Finally run the initialization function of the contract, resulting in an initial state
                -- of the contract. This ticks energy during execution, failing when running out of energy.
                -- NB: At this point the amount to initialize with has not yet been deducted from the
                -- sender account. Thus if the initialization function were to observe the current balance it would
                -- be amount - deposit. Currently this is in any case not exposed in contracts, but in case it
                -- is in the future we should be mindful of which balance is exposed.
                senderCredentials <- getAccountCredentials (snd senderAccount)
                let initCtx = Wasm.InitContext{
                      initOrigin = senderAddress,
                      icSenderPolicies = map (Wasm.mkSenderPolicy . snd) (OrdMap.toAscList senderCredentials)
                   }
                result <- runInterpreter (return . WasmV1.applyInitFun iface cm initCtx initName param initAmount)
                           `rejectingWith'` WasmV1.cerToRejectReasonInit
    
                -- Charge for storing the contract state.
                tickEnergyStoreState (WasmV1.irdNewState result)
                -- And for storing the instance.
                tickEnergy Cost.initializeContractInstanceCreateCost
    
                return (Right (iface, result))

          k ls (Left (iface, result)) = do
            let model = Wasm.newState result
            (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
            chargeExecutionCost senderAccount energyCost

            -- Withdraw the amount the contract is initialized with from the sender account.
            cs' <- addAmountToCS senderAccount (amountDiff 0 initAmount) (ls ^. changeSet)

            let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive iface)
            let ins = makeInstance initName receiveMethods iface model initAmount senderAddress
            addr <- putNewInstance ins

            -- add the contract initialization to the change set and commit the changes
            commitChanges $ addContractInitToCS (ins addr) cs'

            return (TxSuccess [ContractInitialized{ecRef=modref,
                                                   ecAddress=addr,
                                                   ecAmount=initAmount,
                                                   ecInitName=initName,
                                                   ecContractVersion=Wasm.V0,
                                                   ecEvents=Wasm.logs result
                                                   }], energyCost, usedEnergy
                                                   )
          k ls (Right (iface, result)) = do
            let model = WasmV1.irdNewState result
            (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
            chargeExecutionCost senderAccount energyCost

            -- Withdraw the amount the contract is initialized with from the sender account.
            cs' <- addAmountToCS senderAccount (amountDiff 0 initAmount) (ls ^. changeSet)

            let receiveMethods = OrdMap.findWithDefault Set.empty initName (GSWasm.miExposedReceive iface)
            let ins = makeInstance initName receiveMethods iface model initAmount senderAddress
            addr <- putNewInstance ins

            -- add the contract initialization to the change set and commit the changes
            commitChanges $ addContractInitToCS (ins addr) cs'

            return (TxSuccess [ContractInitialized{ecRef=modref,
                                                   ecAddress=addr,
                                                   ecAmount=initAmount,
                                                   ecInitName=initName,
                                                   ecContractVersion=Wasm.V1,
                                                   ecEvents=WasmV1.irdLogs result
                                                   }], energyCost, usedEnergy
                                                   )

handleSimpleTransfer ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> AccountAddress -- ^Address to send the amount to, either account or contract.
    -> Amount -- ^The amount to transfer.
    -> Maybe Memo -- ^Nothing in case of a Transfer and Just in case of a TransferWithMemo
    -> m (Maybe TransactionSummary)
handleSimpleTransfer wtc toAddr transferamount maybeMemo =
  withDeposit wtc c (defaultSuccess wtc)
    where senderAccount = wtc ^. wtcSenderAccount
          meta = wtc ^. wtcTransactionHeader
          senderAddress = thSender meta
          c = do
            -- charge at the beginning, successful and failed transfers will have the same cost.
            tickEnergy Cost.simpleTransferCost
            -- Check whether the sender has the amount to be transferred and reject the transaction if not.
            senderamount <- getCurrentAccountAvailableAmount senderAccount
            unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge (AddressAccount senderAddress) transferamount)

            -- Check whether target account exists and get it.
            targetAccount <- getStateAccount toAddr `rejectingWith` InvalidAccountReference toAddr

            -- Add the transfer to the current changeset and return the corresponding event.
            withAccountToAccountAmount senderAccount targetAccount transferamount $
                return $ Transferred (AddressAccount senderAddress) transferamount (AddressAccount toAddr) : (TransferMemo <$> maybeToList maybeMemo)

-- | Handle a top-level update transaction to a contract.
handleUpdateContract ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Wasm.ReceiveName -- ^Name of the receive method to invoke.
    -> Wasm.Parameter -- ^Message to send to the receive method.
    -> m (Maybe TransactionSummary)
handleUpdateContract wtc uAmount uAddress uReceiveName uMessage =
  withDeposit wtc c (defaultSuccess wtc)
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        checkAndGetBalanceV1 = checkAndGetBalanceAccountV1 senderAddress senderAccount
        checkAndGetBalanceV0 = checkAndGetBalanceAccountV0 senderAddress senderAccount
        c = do
          getCurrentContractInstanceTicking uAddress >>= \case
            InstanceV0 ins -> 
                -- Now invoke the general handler for contract messages.
                handleContractUpdateV0 senderAddress
                              ins
                              checkAndGetBalanceV0
                              uAmount
                              uReceiveName
                              uMessage
            InstanceV1 ins -> do
              handleContractUpdateV1 senderAddress ins checkAndGetBalanceV1 uAmount uReceiveName uMessage >>= \case
                Left cer -> rejectTransaction (WasmV1.cerToRejectReasonReceive uAddress uReceiveName uMessage cer)
                Right (_, events) -> return (reverse events)

-- |Check that the account has sufficient balance, and construct credentials of the account.
checkAndGetBalanceAccountV1 :: (TransactionMonad pv m, AccountOperations m)
    => AccountAddress -- ^Used address
    -> IndexedAccount m
    -> Amount
    -> m (Either WasmV1.ContractCallFailure (Address, [ID.AccountCredential], (Either ContractAddress IndexedAccountAddress)))
checkAndGetBalanceAccountV1 usedAddress senderAccount transferAmount = do
  (senderAddr, senderCredentials) <- mkSenderAddrCredentials (Right (usedAddress, senderAccount))
  senderamount <- getCurrentAccountAvailableAmount senderAccount
  if senderamount >= transferAmount then do
    canonicalAddr <- getAccountCanonicalAddress (snd senderAccount)
    return (Right (senderAddr, senderCredentials, Right (fst senderAccount, canonicalAddr)))
  else
    return (Left (WasmV1.EnvFailure (WasmV1.AmountTooLarge senderAddr transferAmount)))

-- |Check that the account has sufficient balance, and construct credentials of the account.
-- In contrast to the V1 version above this one uses the TransactionMonad's error handling
-- to raise an error, instead of returning it.
checkAndGetBalanceAccountV0 :: (TransactionMonad pv m, AccountOperations m)
    => AccountAddress -- ^Used address
    -> IndexedAccount m
    -> Amount
    -> m (Address, [ID.AccountCredential], (Either ContractAddress IndexedAccountAddress))
checkAndGetBalanceAccountV0 usedAddress senderAccount transferAmount = do
  (senderAddr, senderCredentials) <- mkSenderAddrCredentials (Right (usedAddress, senderAccount))
  senderamount <- getCurrentAccountAvailableAmount senderAccount
  if senderamount >= transferAmount then do
    canonicalAddr <- getAccountCanonicalAddress (snd senderAccount)
    return (senderAddr, senderCredentials, Right (fst senderAccount, canonicalAddr))
  else
    rejectTransaction (AmountTooLarge senderAddr transferAmount)


-- |Check that the instance has sufficient balance, and construct credentials of the owner account.
checkAndGetBalanceInstanceV1 :: (TransactionMonad pv m, AccountOperations m)
    => IndexedAccount m
    -> InstanceV vOrigin
    -> Amount
    -> m (Either WasmV1.ContractCallFailure (Address, [ID.AccountCredential], (Either ContractAddress IndexedAccountAddress)))
checkAndGetBalanceInstanceV1 ownerAccount istance transferAmount = do
  (senderAddr, senderCredentials) <- mkSenderAddrCredentials (Left (ownerAccount, instanceAddress istance))
  senderamount <- getCurrentContractAmount istance
  if senderamount >= transferAmount then
    return (Right (senderAddr, senderCredentials, (Left (instanceAddress istance))))
  else
    return (Left (WasmV1.EnvFailure (WasmV1.AmountTooLarge senderAddr transferAmount)))

-- |Check that the instance has sufficient balance, and construct credentials of the owner account.
-- In contrast to the V1 version above this one uses the TransactionMonad's error handling
-- to raise an error, instead of returning it.
checkAndGetBalanceInstanceV0 :: (TransactionMonad pv m, AccountOperations m)
    => IndexedAccount m
    -> InstanceV vOrigin
    -> Amount
    -> m (Address, [ID.AccountCredential], Either ContractAddress IndexedAccountAddress)
checkAndGetBalanceInstanceV0 ownerAccount istance transferAmount = do
  (senderAddr, senderCredentials) <- mkSenderAddrCredentials (Left (ownerAccount, instanceAddress istance))
  senderamount <- getCurrentContractAmount istance
  if senderamount >= transferAmount then
    return (senderAddr, senderCredentials, Left (instanceAddress istance))
  else
    rejectTransaction (AmountTooLarge senderAddr transferAmount)

-- |Handle updating a V1 contract.
-- In contrast to most other methods in this file this one does not use the
-- error handling facilities of the transaction monad. Instead it explicitly returns an Either type.
-- The reason for this is that the possible errors are exposed back to the smart
-- contract in case a contract A invokes contract B's entrypoint.
handleContractUpdateV1 :: forall pv r m.
  (IsProtocolVersion pv, StaticInformation m, AccountOperations m)
  => AccountAddress -- ^The address that was used to send the top-level transaction.
  -> InstanceV GSWasm.V1 -- ^The current state of the target contract of the transaction, which must exist.
  -> (Amount -> LocalT pv r m (Either WasmV1.ContractCallFailure (Address, [ID.AccountCredential], Either ContractAddress IndexedAccountAddress)))
  -- ^Check that the sender has sufficient amount to cover the given amount and return a triple of
  -- - used address
  -- - credentials of the address, either account or owner of the contract
  -- - resolved address. In case this is an account
  -- (i.e., this is called from a top-level transaction) the value is a pair of the address that was used
  -- as the sender address of the transaction, and the account to which it points.
  -> Amount -- ^The amount to be transferred from the sender of the message to the contract upon success.
  -> Wasm.ReceiveName -- ^Name of the contract to invoke.
  -> Wasm.Parameter -- ^Message to invoke the receive method with.
  -> LocalT pv r m (Either WasmV1.ContractCallFailure (WasmV1.ReturnValue, [Event]))
  -- ^The events resulting from processing the message and all recursively processed messages. For efficiency
  -- reasons the events are in **reverse order** of the actual effects.
handleContractUpdateV1 originAddr istance checkAndGetSender transferAmount receiveName parameter = do
  -- Cover administrative costs.
  tickEnergy Cost.updateContractInstanceBaseCost
  let model = _instanceVModel istance
  let iParams = _instanceVParameters istance
  let cref = instanceAddress iParams
  let receivefuns = instanceReceiveFuns . _instanceVParameters $ istance
  let ownerAccountAddress = instanceOwner iParams
  -- The invariants maintained by global state should ensure that an owner account always exists.
  -- However we are defensive here and reject the transaction instead of panicking in case it does not.
  ownerCheck <- getStateAccount ownerAccountAddress
  senderCheck <- checkAndGetSender transferAmount

  case (Set.member receiveName receivefuns, ownerCheck, senderCheck) of
    (False, _, _) -> return (Left (WasmV1.EnvFailure (WasmV1.InvalidEntrypoint (GSWasm.miModuleRef . instanceModuleInterface $ iParams) receiveName)))
    (_, Nothing, _) -> return (Left (WasmV1.EnvFailure (WasmV1.MissingAccount ownerAccountAddress)))
    (_, _, Left err) -> return (Left err)
    (True, Just ownerAccount, Right (senderAddr, senderCredentials, sender)) -> do
      cm <- getChainMetadata
      let receiveCtx = Wasm.ReceiveContext {
            invoker = originAddr,
            selfAddress = cref,
            -- NB: This means that the contract observes the balance **with** the incoming one
            -- which is different from the V0 contracts. The reason for this choice is that
            -- in V1 contracts, since they execute in one go, it is necessary for some uses to
            -- make the incoming amount immediately available. Otherwise the contract cannot, for example,
            -- forward the incoming amount. Since that is necessary, the updated semantics is the most natural one.
            selfBalance = _instanceVAmount istance + transferAmount,
            sender = senderAddr,
            owner = instanceOwner iParams,
            rcSenderPolicies = map Wasm.mkSenderPolicy senderCredentials
            }
      -- Now run the receive function on the message. This ticks energy during execution, failing when running out of energy.
      let iface = instanceModuleInterface iParams
      -- charge for looking up the module
      tickEnergy $ Cost.lookupModule (GSWasm.miModuleSize iface)
    
      -- we've covered basic administrative costs now.
      -- The @go@ function iterates until the end of execution, handling any interrupts by dispatching
      -- to appropriate handlers.
      let go :: [Event]
              -> Either WasmV1.ContractExecutionReject WasmV1.ReceiveResultData
              -- ^Result of invoking an operation
              -> LocalT pv r m (Either WasmV1.ContractCallFailure (WasmV1.ReturnValue, [Event]))
          go _ (Left cer) = return (Left (WasmV1.ExecutionReject cer)) -- contract execution failed.
          go events (Right rrData) = do
            -- balance at present before handling out calls or transfers.
            entryBalance <- getCurrentContractAmount istance
            case rrData of
              WasmV1.ReceiveSuccess{..} -> do
                -- execution terminated, commit the new state
                withInstanceStateV1 istance rrdNewState $ \_modifiedIndex ->
                  let event = Updated{euAddress=instanceAddress istance,
                                      euInstigator=senderAddr,
                                      euAmount=transferAmount,
                                      euMessage=parameter,
                                      euReceiveName=receiveName,
                                      euContractVersion=Wasm.V1,
                                      euEvents = rrdLogs
                                     }
                  in return (Right (rrdReturnValue, event:events))
              WasmV1.ReceiveInterrupt{..} -> do
                -- execution invoked an operation. Dispatch and continue.
                let interruptEvent = Interrupted{
                      iAddress = instanceAddress istance,
                      iEvents = rrdLogs
                      }
                    resumeEvent rSuccess = Resumed{
                      rAddress = instanceAddress istance,
                      ..
                      }
                case rrdMethod of
                  -- the operation is an account transfer, so we handle it.
                  WasmV1.Transfer{..} ->
                    runExceptT (transferAccountSync imtTo istance imtAmount) >>= \case
                      Left errCode -> do
                        go (resumeEvent False:interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig Nothing entryBalance (WasmV1.Error (WasmV1.EnvFailure errCode)) Nothing)
                      Right transferEvents -> do
                        newBalance <- getCurrentContractAmount istance
                        go (resumeEvent True:transferEvents ++ interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig Nothing newBalance WasmV1.Success Nothing)
                  WasmV1.Call{..} ->
                    -- the operation is a call to another contract. There is a bit of complication because the contract could be a V0
                    -- or V1 one, and the behaviour is different depending on which one it is.
                    -- First, commit the current state of the contract.
                    -- TODO: With the new state, only do this if the state has actually changed.
                    withInstanceStateV1 istance rrdCurrentState $ \modificationIndex -> do
                       -- lookup the instance to invoke
                       getCurrentContractInstanceTicking' imcTo >>= \case
                         -- we could not find the instance, return this to the caller and continue
                         Nothing -> go events =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig Nothing entryBalance (WasmV1.Error (WasmV1.EnvFailure (WasmV1.MissingContract imcTo))) Nothing)
                         Just (InstanceV0 targetInstance) -> do
                           -- we are invoking a V0 instance.
                           -- in this case we essentially treat this as a top-level transaction invoking that contract.
                           -- That is, we execute the entire tree that is potentially generated.
                           let rName = Wasm.uncheckedMakeReceiveName (instanceInitName (_instanceVParameters targetInstance)) imcName
                               runSuccess = handleContractUpdateV0 originAddr targetInstance (checkAndGetBalanceInstanceV0 ownerAccount istance) imcAmount rName imcParam
                           -- If execution of the contract succeeds resume.
                           -- Otherwise rollback the state and report that to the caller.
                           runInnerTransaction runSuccess >>= \case
                              Left err -> do -- execution failed, ignore the reject reason since V0 contract cannot return useful information
                                go (resumeEvent False:interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig Nothing entryBalance WasmV1.MessageSendFailed Nothing)
                              Right evs -> do
                                -- Execution of the contract might have changed our own state. If so, we need to resume in the new state, otherwise
                                -- we can keep the old one.
                                (lastModifiedIndex, newState) <- getCurrentContractInstanceState istance
                                let resumeState = if lastModifiedIndex == modificationIndex then Nothing else Just newState
                                newBalance <- getCurrentContractAmount istance
                                go (resumeEvent True:evs ++ interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig resumeState newBalance WasmV1.Success Nothing)
                         Just (InstanceV1 targetInstance) -> do
                           -- invoking a V1 instance is easier. We recurse on the update function.
                           -- If this returns Right _ it is successful, and we pass this, and the returned return value
                           -- to the caller.
                           -- Otherwise we roll back all the changes and return the return value, and the error code to the caller.
                           let rName = Wasm.uncheckedMakeReceiveName (instanceInitName (_instanceVParameters targetInstance)) imcName
                           withRollback (handleContractUpdateV1 originAddr targetInstance (checkAndGetBalanceInstanceV1 ownerAccount istance) imcAmount rName imcParam) >>= \case
                              Left cer -> do
                                go (resumeEvent False:interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig Nothing entryBalance (WasmV1.Error cer) (WasmV1.ccfToReturnValue cer))
                              Right (rVal, callEvents) -> do
                                (lastModifiedIndex, newState) <- getCurrentContractInstanceState istance
                                let resumeState = if lastModifiedIndex == modificationIndex then Nothing else Just newState
                                newBalance <- getCurrentContractAmount istance
                                go (resumeEvent True:callEvents ++ interruptEvent:events) =<< runInterpreter (return . WasmV1.resumeReceiveFun rrdInterruptedConfig resumeState newBalance WasmV1.Success (Just rVal))

      -- start contract execution.
      -- transfer the amount from the sender to the contract at the start. This is so that the contract may immediately use it
      -- for, e.g., forwarding.
      withToContractAmount sender istance transferAmount $
        go [] =<< runInterpreter (return . WasmV1.applyReceiveFun iface cm receiveCtx receiveName parameter transferAmount model)
          
   where  transferAccountSync :: AccountAddress -- ^The target account address.
                              -> InstanceV GSWasm.V1 -- ^The sender of this transfer.
                              -> Amount -- ^The amount to transfer.
                              -> ExceptT WasmV1.EnvFailure (LocalT pv r m) [Event] -- ^The events resulting from the transfer.
          transferAccountSync accAddr senderInstance tAmount = do
            -- charge at the beginning, successful and failed transfers will have the same cost.
            -- Check whether the sender has the amount to be transferred and reject the transaction if not.
            senderamount <- lift $ do
              tickEnergy Cost.simpleTransferCost
              getCurrentContractAmount senderInstance
            let addr = AddressContract (instanceAddress senderInstance)
            unless (senderamount >= tAmount) $! throwError (WasmV1.AmountTooLarge addr tAmount)
            -- Check whether target account exists and get it.
            lift (getStateAccount accAddr) >>=  \case
              Nothing -> throwError (WasmV1.MissingAccount accAddr)
              Just targetAccount -> 
                -- Add the transfer to the current changeset and return the corresponding event.
                lift (withContractToAccountAmount (instanceAddress senderInstance) targetAccount tAmount $
                      return [Transferred addr transferAmount (AddressAccount accAddr)])
           

-- | Invoke a V0 contract and process any generated messages.
-- This includes the transfer of an amount from the sending account or instance.
-- Recursively do the same for new messages created by contracts (from left to right, depth first).
-- The target contract must exist, so that its state can be looked up.
handleContractUpdateV0 :: forall pv r m.
  (IsProtocolVersion pv, StaticInformation m, AccountOperations m)
  => AccountAddress -- ^The address that was used to send the top-level transaction.
  -> InstanceV GSWasm.V0 -- ^The current state of the target contract of the transaction, which must exist.
  -> (Amount -> LocalT pv r m (Address, [ID.AccountCredential], (Either ContractAddress IndexedAccountAddress)))
  -- ^Check that the sender has sufficient amount to cover the given amount and return a triple of
  -- - used address
  -- - credentials of the address, either account or owner of the contract
  -- - resolved address. In case this is an account
  -- (i.e., this is called from a top-level transaction) the value is a pair of the address that was used
  -- as the sender address of the transaction, and the account to which it points.
  -> Amount -- ^The amount to be transferred from the sender of the message to the receiver.
  -> Wasm.ReceiveName -- ^Name of the contract to invoke.
  -> Wasm.Parameter -- ^Message to invoke the receive method with.
  -> LocalT pv r m [Event] -- ^The events resulting from processing the message and all recursively processed messages.
handleContractUpdateV0 originAddr istance checkAndGetSender transferAmount receiveName parameter = do
  -- Cover administrative costs.
  tickEnergy Cost.updateContractInstanceBaseCost

  let model = _instanceVModel istance
  -- Check whether the sender of the message has enough on its account/instance for the transfer.
  -- If the amount is not sufficient, the top-level transaction is rejected.
  -- Note that this returns the address that was used in the top-level transaction, or the contract address.
  -- In the former case the credentials are credentials of the account, in the
  -- latter they are credentials of the owner account.
  (senderAddr, senderCredentials, sender) <- checkAndGetSender transferAmount

  let iParams = _instanceVParameters istance
  let cref = instanceAddress iParams
  let receivefuns = instanceReceiveFuns . _instanceVParameters $ istance
  unless (Set.member receiveName receivefuns) $ rejectTransaction $
      InvalidReceiveMethod (GSWasm.miModuleRef . instanceModuleInterface $ iParams) receiveName
  -- Now we also check that the owner account of the receiver instance has at least one valid credential
  -- and reject the transaction if not.
  let ownerAccountAddress = instanceOwner iParams
  -- The invariants maintained by global state should ensure that an owner account always exists.
  -- However we are defensive here and reject the transaction instead of panicking in case it does not.
  ownerAccount <- getStateAccount ownerAccountAddress `rejectingWith` InvalidAccountReference ownerAccountAddress
  cm <- getChainMetadata

  -- We have established that the owner account of the receiver instance has at least one valid credential.
  let receiveCtx = Wasm.ReceiveContext {
        invoker = originAddr,
        selfAddress = cref,
        selfBalance = _instanceVAmount istance,
        sender = senderAddr,
        owner = instanceOwner iParams,
        rcSenderPolicies = map Wasm.mkSenderPolicy senderCredentials
        }
  -- Now run the receive function on the message. This ticks energy during execution, failing when running out of energy.
  -- FIXME: Once errors can be caught in smart contracts update this to not terminate the transaction.
  let iface = instanceModuleInterface iParams
  -- charge for looking up the module
  tickEnergy $ Cost.lookupModule (GSWasm.miModuleSize iface)

  result <- runInterpreter (return . WasmV0.applyReceiveFun iface cm receiveCtx receiveName parameter transferAmount model)
             `rejectingWith'` wasmRejectToRejectReasonReceive cref receiveName parameter

  -- If we reach here the contract accepted the message and returned a new state as well as outgoing messages.
  let newModel = Wasm.newState result
      txOut = Wasm.messages result
  -- If we reached here we will charge for attempting to store this state. If
  -- this message is part of a larger `and` composition then it is possible that
  -- the contract will not actually be stored. We are not doing that because it
  -- increases complexity and makes tracking of energy even less local than it
  -- is now.
  -- TODO We might want to change this behaviour to prevent charging for storage that is not done.
  tickEnergyStoreState newModel

  -- Process the generated messages in the new context (transferred amount, updated state) in
  -- sequence from left to right, depth first.
  withToContractAmount sender istance transferAmount $
    withInstanceStateV0 istance newModel $ do
      let initEvent = Updated{euAddress=cref,
                              euInstigator=senderAddr,
                              euAmount=transferAmount,
                              euMessage=parameter,
                              euReceiveName=receiveName,
                              euContractVersion=Wasm.V0,
                              euEvents = Wasm.logs result
                               }
      foldEvents originAddr (ownerAccount, istance) initEvent txOut

-- Cost of a step in the traversal of the actions tree. We need to charge for
-- this separately to prevent problems with exponentially sized trees
-- (exponential in the size of their representation). Based on benchmarks we can
-- handle the worst case, which is a tree consisting solely of Accept and And
-- nodes, of 300000 accepts well in about half the time we have for making a
-- block. Thus we set the cost to 10 so that it both prevents abuse, but also
-- so that it is cheap enough not be negligible compared to transfer costs
-- for trees that have genuine utility.
traversalStepCost :: Energy
traversalStepCost = 10

foldEvents :: (IsProtocolVersion pv, StaticInformation m, AccountOperations m)
           => AccountAddress -- ^Address that was used in the top-level transaction.
           -> (IndexedAccount m, InstanceV GSWasm.V0) -- ^Instance that generated the events.
           -> Event -- ^Event generated by the invocation of the instance.
           -> Wasm.ActionsTree -- ^Actions to perform
           -> LocalT pv r m [Event] -- ^List of events in order that transactions were traversed.
foldEvents originAddr istance initEvent = fmap (initEvent:) . go
  where go Wasm.TSend{..} = do
          getCurrentContractInstanceTicking erAddr >>= \case
            InstanceV0 cinstance -> handleContractUpdateV0 originAddr
                                   cinstance
                                   (uncurry checkAndGetBalanceInstanceV0 istance)
                                   erAmount
                                   erName
                                   erParameter
            InstanceV1 cinstance ->
              let c = handleContractUpdateV1
                      originAddr
                      cinstance
                      (uncurry checkAndGetBalanceInstanceV1 istance)
                      erAmount
                      erName
                      erParameter
              in snd <$> (c `rejectingWith'` WasmV1.cerToRejectReasonReceive erAddr erName erParameter)
        go Wasm.TSimpleTransfer{..} = do
          handleTransferAccount erTo (snd istance) erAmount
        go (Wasm.And l r) = do
          tickEnergy traversalStepCost
          resL <- go l
          resR <- go r
          return (resL ++ resR)
        -- FIXME: This will not retain logs from the left run if it fails.
        -- We might want to include the information on why the right-side
        -- was run in the output event list.
        go (Wasm.Or l r) = do
          tickEnergy traversalStepCost
          go l `orElse` go r
        go Wasm.Accept = return []

-- |Construct the address and a list of credentials of the sender. If the sender
-- is an account, this is the address of the account that was used in the
-- transaction, together with the list of credentials of that account, ordered
-- by credential index. If the sender is a smart contract the returned address
-- will be a contract address, and the credentials will be of the owner account.
mkSenderAddrCredentials :: AccountOperations m
    => Either (IndexedAccount m, ContractAddress) (AccountAddress, IndexedAccount m)
    -> m (Address, [ID.AccountCredential])
mkSenderAddrCredentials sender =
    case sender of
      Left (ownerAccount, iaddr) -> do
        credentials <- getAccountCredentials (snd ownerAccount)
        return (AddressContract iaddr, map snd (OrdMap.toAscList credentials))
      Right (usedAddress, (_, acc)) -> do
        let addr = AddressAccount usedAddress
        credentials <- getAccountCredentials acc
        return (addr, map snd (OrdMap.toAscList credentials))


-- | Handle the transfer of an amount from a contract instance to an account.
handleTransferAccount ::
  (TransactionMonad pv m, HasInstanceAddress a, HasInstanceFields a)
  => AccountAddress -- ^The target account address.
  -> a -- ^The sender of this transfer.
  -> Amount -- ^The amount to transfer.
  -> m [Event] -- ^The events resulting from the transfer.
handleTransferAccount accAddr senderInstance transferamount = do
  -- charge at the beginning, successful and failed transfers will have the same cost.
  tickEnergy Cost.simpleTransferCost
  -- Check whether the sender has the amount to be transferred and reject the transaction if not.
  senderamount <- getCurrentContractAmount senderInstance
  let addr = AddressContract (instanceAddress senderInstance)
  unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge addr transferamount)

  -- Check whether target account exists and get it.
  targetAccount <- getStateAccount accAddr `rejectingWith` InvalidAccountReference accAddr

  -- Add the transfer to the current changeset and return the corresponding event.
  withContractToAccountAmount (instanceAddress senderInstance) targetAccount transferamount $
      return [Transferred addr transferamount (AddressAccount accAddr)]

-- |Run the interpreter with the remaining amount of energy. If the interpreter
-- runs out of energy set the remaining gas to 0 and reject the transaction,
-- otherwise decrease the consumed amount of energy and return the result.
{-# INLINE runInterpreter #-}
runInterpreter :: TransactionMonad pv m => (Wasm.InterpreterEnergy -> m (Maybe (a, Wasm.InterpreterEnergy))) -> m a
runInterpreter f = withExternal $ \availableEnergy -> do
  f availableEnergy >>= \case
    Nothing -> return Nothing
    Just (result, remainingEnergy) -> do
      -- the following relies on the interpreter ensuring
      -- remainingEnergy <= availableEnergy. Since both of these
      -- values are unsigned the computation will otherwise overflow.
      -- Even if this happens it is likely to simply cause an 'OutOfEnergy'
      -- or outOfBlockEnergy termination.
      let usedEnergy = availableEnergy - remainingEnergy
      return (Just (result, usedEnergy))

-- |A simple sigma protocol to check knowledge of secret key.
checkElectionKeyProof :: BS.ByteString -> BakerElectionVerifyKey -> Proofs.Dlog25519Proof -> Bool
checkElectionKeyProof = Proofs.checkDlog25519ProofVRF

-- |A simple sigma protocol to check knowledge of secret key.
checkSignatureVerifyKeyProof :: BS.ByteString -> BakerSignVerifyKey -> Proofs.Dlog25519Proof -> Bool
checkSignatureVerifyKeyProof = Proofs.checkDlog25519ProofBlock

-- |Add a baker for the sender account. The logic is as follows:
--
--  * The transaction fails ('InsufficientBalanceForBakerStake') if the balance on the account
--    (less the energy deposit) is below the amount to stake.
--  * The transaction fails ('InvalidProof') if any of the key ownership proofs is invalid.
--  * The transaction fails ('AlreadyBaker') if the account is already a baker.
--  * The transaction fails ('DuplicateAggregationKey') if the aggregation key is used by another baker.
--  * The transaction succeeds ('BASuccess') if it passes all the above checks; the baker is
--    then recorded on the account, its stake becoming locked, and registered as a baker.
--    It will be able to start in the epoch after next.
--
-- It is assumed that the sender account has been verified to exist and
-- have sufficient balance to cover the deposit. If it does not exist, then
-- the transaction could fail (after checking the proofs) with 'InvalidAccountReference'.
-- If the balance check has not been made, the behaviour is undefined. (Most likely,
-- this will lead to an underflow and an invariant violation.)
handleAddBaker ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> BakerElectionVerifyKey
    -> BakerSignVerifyKey
    -> BakerAggregationVerifyKey
    -> Proofs.Dlog25519Proof
    -> Proofs.Dlog25519Proof
    -> BakerAggregationProof
    -> Amount
    -- ^Initial stake amount
    -> Bool
    -- ^Whether to restake the baker's earnings
    -> m (Maybe TransactionSummary)
handleAddBaker wtc abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abProofSig abProofElection abProofAggregation abBakingStake abRestakeEarnings =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        c = do
          tickEnergy Cost.addBakerCost
          -- Get the total amount on the account, including locked amounts,
          -- less the deposit.
          getCurrentAccountTotalAmount senderAccount
        k ls accountBalance = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          let challenge = addBakerChallenge senderAddress abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey
              electionP = checkElectionKeyProof challenge abElectionVerifyKey abProofElection
              signP = checkSignatureVerifyKeyProof challenge abSignatureVerifyKey abProofSig
              aggregationP = Bls.checkProofOfKnowledgeSK challenge abProofAggregation abAggregationVerifyKey

          if accountBalance < abBakingStake then
            -- The balance is insufficient.
            return (TxReject InsufficientBalanceForBakerStake, energyCost, usedEnergy)
          else if electionP && signP && aggregationP then do
            -- The proof validates that the baker owns all the private keys,
            -- thus we can try to create the baker.
            res <- addBaker (fst senderAccount) BI.BakerAdd {
                  baKeys = BI.BakerKeyUpdate {
                    bkuSignKey = abSignatureVerifyKey,
                    bkuAggregationKey = abAggregationVerifyKey,
                    bkuElectionKey = abElectionVerifyKey
                  },
                  baStake = abBakingStake,
                  baStakeEarnings = abRestakeEarnings
                }
            case res of
              BI.BASuccess bid -> do
                let baddEvt = BakerAdded {
                          ebaBakerId = bid,
                          ebaAccount = senderAddress,
                          ebaSignKey = abSignatureVerifyKey,
                          ebaElectionKey = abElectionVerifyKey,
                          ebaAggregationKey = abAggregationVerifyKey,
                          ebaStake = abBakingStake,
                          ebaRestakeEarnings = abRestakeEarnings
                        }
                return (TxSuccess [baddEvt], energyCost, usedEnergy)
              BI.BAInvalidAccount -> -- This case should not be possible because the account was already resolved
                return (TxReject (InvalidAccountReference senderAddress), energyCost, usedEnergy)
              BI.BAAlreadyBaker bid -> return (TxReject (AlreadyABaker bid), energyCost, usedEnergy)
              BI.BADuplicateAggregationKey -> return (TxReject (DuplicateAggregationKey abAggregationVerifyKey), energyCost, usedEnergy)
              BI.BAStakeUnderThreshold -> return (TxReject StakeUnderMinimumThresholdForBaking, energyCost, usedEnergy)
          else return (TxReject InvalidProof, energyCost, usedEnergy)

-- |Remove the baker for an account. The logic is as follows:
--
--  * If the account is not a baker, the transaction fails ('NotABaker').
--  * If the account is the cool-down period for another baker change, the transaction fails ('BakerInCooldown').
--  * Otherwise, the baker is removed, which takes effect after the cool-down period.
handleRemoveBaker ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> m (Maybe TransactionSummary)
handleRemoveBaker wtc =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        c = tickEnergy Cost.removeBakerCost
        k ls () = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          res <- removeBaker (fst senderAccount)
          case res of
            BI.BRRemoved bid _ -> do
              let brEvt = BakerRemoved {
                        ebrBakerId = bid,
                        ebrAccount = senderAddress
                      }
              return (TxSuccess [brEvt], energyCost, usedEnergy)
            BI.BRInvalidBaker -> return (TxReject (NotABaker senderAddress), energyCost, usedEnergy)
            BI.BRChangePending _ -> return (TxReject BakerInCooldown, energyCost, usedEnergy)

handleUpdateBakerStake ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> Amount
    -- ^new stake
    -> m (Maybe TransactionSummary)
handleUpdateBakerStake wtc newStake =
    withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    meta = wtc ^. wtcTransactionHeader
    senderAddress = thSender meta
    c = do
      tickEnergy Cost.updateBakerStakeCost
      -- Get the total amount on the account, including locked amounts,
      -- less the deposit.
      getCurrentAccountTotalAmount senderAccount
    k ls accountBalance = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost senderAccount energyCost
      if accountBalance < newStake then
        -- The balance is insufficient.
        return (TxReject InsufficientBalanceForBakerStake, energyCost, usedEnergy)
      else do
        res <- updateBakerStake (fst senderAccount) newStake
        case res of
          BI.BSUChangePending _ -> return (TxReject BakerInCooldown, energyCost, usedEnergy)
          BI.BSUStakeIncreased bid ->
            return (TxSuccess [BakerStakeIncreased bid senderAddress newStake], energyCost, usedEnergy)
          BI.BSUStakeReduced bid _ ->
            return (TxSuccess [BakerStakeDecreased bid senderAddress newStake], energyCost, usedEnergy)
          BI.BSUStakeUnchanged _ ->
            return (TxSuccess [], energyCost, usedEnergy)
          BI.BSUInvalidBaker -> -- Since we resolved the account already, this happens only if the account is not a baker.
            return (TxReject (NotABaker senderAddress), energyCost, usedEnergy)
          BI.BSUStakeUnderThreshold ->
            return (TxReject StakeUnderMinimumThresholdForBaking, energyCost, usedEnergy)

handleUpdateBakerRestakeEarnings ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> Bool
    -- ^Whether to restake earnings
    -> m (Maybe TransactionSummary)
handleUpdateBakerRestakeEarnings wtc newRestakeEarnings = withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    meta = wtc ^. wtcTransactionHeader
    senderAddress = thSender meta
    c = tickEnergy Cost.updateBakerRestakeCost
    k ls () = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost senderAccount energyCost

      res <- updateBakerRestakeEarnings (fst senderAccount) newRestakeEarnings
      case res of
        BI.BREUUpdated bid ->
          return (TxSuccess [BakerSetRestakeEarnings bid senderAddress newRestakeEarnings], energyCost, usedEnergy)
        BI.BREUInvalidBaker -> -- Since we resolved the account already, this happens only if the account is not a baker.
          return (TxReject (NotABaker senderAddress), energyCost, usedEnergy)

-- |Update a baker's keys. The logic is as follows:
--
--  * The transaction fails ('InvalidProof') if any of the key ownership proofs is invalid.
--  * The transaction fails ('DuplicateAggregationKey') if the aggregation key is used by another baker.
--  * The transaction succeeds ('BASuccess') if it passes all the above checks; the baker is
--    then recorded on the account, its stake becoming locked, and registered as a baker.
--    It will be able to start in the epoch after next.
--
-- It is assumed that the sender account has been verified to exist and
-- have sufficient balance to cover the deposit. If it does not exist, then
-- the transaction could fail (after checking the proofs) with 'InvalidAccountReference'.
-- If the balance check has not been made, the behaviour is undefined. (Most likely,
-- this will lead to an underflow and an invariant violation.)
handleUpdateBakerKeys ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> BakerElectionVerifyKey
    -> BakerSignVerifyKey
    -> BakerAggregationVerifyKey
    -> Proofs.Dlog25519Proof
    -> Proofs.Dlog25519Proof
    -> BakerAggregationProof
    -> m (Maybe TransactionSummary)
handleUpdateBakerKeys wtc bkuElectionKey bkuSignKey bkuAggregationKey bkuProofSig bkuProofElection bkuProofAggregation =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        meta = wtc ^. wtcTransactionHeader
        senderAddress = thSender meta
        c = tickEnergy Cost.updateBakerKeysCost
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          let challenge = updateBakerKeyChallenge senderAddress bkuElectionKey bkuSignKey bkuAggregationKey
              electionP = checkElectionKeyProof challenge bkuElectionKey bkuProofElection
              signP = checkSignatureVerifyKeyProof challenge bkuSignKey bkuProofSig
              aggregationP = Bls.checkProofOfKnowledgeSK challenge bkuProofAggregation bkuAggregationKey

          if electionP && signP && aggregationP then do
            -- The proof validates that the baker owns all the private keys,
            -- thus we can try to create the baker.
            res <- updateBakerKeys (fst senderAccount) BI.BakerKeyUpdate{..}
            case res of
              BI.BKUSuccess bid -> do
                let bupdEvt = BakerKeysUpdated{
                  ebkuBakerId = bid,
                  ebkuAccount = senderAddress,
                  ebkuSignKey = bkuSignKey,
                  ebkuElectionKey = bkuElectionKey,
                  ebkuAggregationKey = bkuAggregationKey
                }
                return (TxSuccess [bupdEvt], energyCost, usedEnergy)
              BI.BKUInvalidBaker -> -- Since we resolved the account already, this happens only if the account is not a baker.
                return (TxReject (NotABaker senderAddress), energyCost, usedEnergy)
              BI.BKUDuplicateAggregationKey -> return (TxReject (DuplicateAggregationKey bkuAggregationKey), energyCost, usedEnergy)
          else return (TxReject InvalidProof, energyCost, usedEnergy)

-- |Credential deployments (transactions without a sender)
-- The logic is as follows:
-- * The transaction fails ('ExpiredTransaction') if the transaction was expired.
-- * The transaction fails ('Nothing') if the block ran out of energy.
--   Note this function returns `Nothing` iff. the block ran out of energy.
-- * The transaction fails ('DuplicateAccountRegistrationID') if the regid already exists on chain.
-- * The transaction fails ('AccountCredentialInvalid') if the credential deployment was expired.
-- * The transaction fails ('AccountCredentialInvalid') if the account already exists on chain.
-- * The transaction fails ('AccountCredentialInvalid') if the credential deployment was malformed (the commitments could not be extracted).
--   Note this can only happen for 'normal' account creations.
-- * The transactions fails ('NonExistentIdentityProvider') if an invalid identity provider id was contained in the credential deployment.
-- * The transaction fails ('UnsupportedAnonymityRevokers') if invalid anonymity revokers were contained in the credential deployment.
-- * The transaction fails ('AccountCredentialInvalid') if the signatures could not be verified.
--
-- If the `CredentialDeployment` was valid then return `Just TxSuccess`
-- 
-- Note that the function only fails with `TxInvalid` and thus failed transactions are not committed to chain.
handleDeployCredential ::
  SchedulerMonad pv m =>
  -- |Credentials to deploy with the current verification status.
  TVer.CredentialDeploymentWithStatus ->
  TransactionHash ->
  m (Maybe TxResult)
handleDeployCredential (WithMetadata{wmdData=cred@AccountCreation{messageExpiry=messageExpiry, credential=cdi}}, mVerRes) cdiHash = do
    res <- runExceptT $ do
     cm <- lift getChainMetadata
     let ts = slotTime cm
     -- check that the transaction is not expired
     when (transactionExpired messageExpiry ts) $ throwError (Just ExpiredTransaction)
     remainingEnergy <- lift getRemainingEnergy
     when (remainingEnergy < theCost) $ throwError Nothing
     case mVerRes of
       Just (TVer.Ok _) -> do
         -- check that the credential deployment has not expired since we last verified it.
         unless (isTimestampBefore ts (ID.validTo cdi)) $ throwError (Just AccountCredentialInvalid)
         -- We always need to make sure that the account was not created in between
         -- the transaction was received and the actual execution.
         -- Check that the registration id does not exist
         regIdEx <- lift (TVer.registrationIdExists regId)
         when regIdEx $ throwError $ Just (DuplicateAccountRegistrationID regId)
         -- Create the account
         newAccount
       -- An invalid verification result or `Nothing` was supplied to this function.
       -- In either case we verify the transaction now.
       _ -> do
         newVerRes <- lift (TVer.verifyCredentialDeployment ts cred)
         case checkTransactionVerificationResult newVerRes of
           Left failure -> throwError . Just $ failure
           Right _ -> newAccount
    case res of
      Left err -> return (TxInvalid <$> err)
      Right ts -> return (Just ts)
  where
    regId = ID.credId cdi
    aaddr = ID.addressFromRegId regId
    newAccount = do
      -- Check that the address would not clash with an existing one.
      accExistsAlready <- lift (addressWouldClash aaddr)
      when accExistsAlready $ throwError $ Just AccountCredentialInvalid
      cryptoParams <- lift TVer.getCryptographicParameters
      cdv <- case cdi of
        ID.InitialACWP icdi -> return (ID.InitialAC (ID.icdiValues icdi))
        ID.NormalACWP _ -> case ID.values cdi of
            Just cdv -> return cdv
            Nothing -> throwError $ Just AccountCredentialInvalid
      _ <- lift (createAccount cryptoParams aaddr cdv)
      tsIndex <- lift bumpTransactionIndex
      let tsResult = TxSuccess [AccountCreated aaddr, CredentialDeployed{ecdRegId=regId,ecdAccount=aaddr}]
      return $ TxValid $ TransactionSummary{
              tsSender = Nothing,
              tsHash = cdiHash,
              tsCost = 0,
              tsEnergyCost = theCost,
              tsType = TSTCredentialDeploymentTransaction (ID.credentialType cdi),
              ..
              }
    theCost = Cost.deployCredential (ID.credentialType cdi) (ID.credNumKeys . ID.credPubKeys $ cdi)

-- |Updates the credential keys in the credential with the given Credential ID.
-- It rejects if there is no credential with the given Credential ID.
handleUpdateCredentialKeys ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> ID.CredentialRegistrationID
    -- ^Registration ID of the credential we are updating.
    -> ID.CredentialPublicKeys
    -- ^New public keys of the credential..
    -> TransactionSignature
    -- ^Signatures on the transaction. This is needed to check that a specific credential signed.
    -> m (Maybe TransactionSummary)
handleUpdateCredentialKeys wtc cid keys sigs =
  withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    meta = wtc ^. wtcTransactionHeader

    c = do
      existingCredentials <- getAccountCredentials (snd senderAccount)
      tickEnergy $ Cost.updateCredentialKeysCost (OrdMap.size existingCredentials) $ length $ ID.credKeys keys

      let credIndex = fst <$> find (\(_, v) -> ID.credId v == cid) (OrdMap.toList existingCredentials)
      -- check that the new threshold is no more than the number of credentials
      let thresholdCheck = toInteger (OrdMap.size (ID.credKeys keys)) >= toInteger (ID.credThreshold keys)

      unless thresholdCheck $ rejectTransaction InvalidCredentialKeySignThreshold

      case credIndex of
        Nothing -> rejectTransaction NonExistentCredentialID
        Just index -> do
          -- We check that the credential whose keys we are updating has indeed signed this transaction.
          let ownerCheck = OrdMap.member index $ tsSignatures sigs
          unless ownerCheck $ rejectTransaction CredentialHolderDidNotSign
          return index
    k ls index = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost senderAccount energyCost
      updateCredentialKeys (fst senderAccount) index keys
      return (TxSuccess [CredentialKeysUpdated cid], energyCost, usedEnergy)


-- * Chain updates

-- |Handle a chain update message
handleChainUpdate :: forall pv m .
  SchedulerMonad pv m
  => TVer.ChainUpdateWithStatus
  -> m TxResult
handleChainUpdate (WithMetadata{wmdData = ui@UpdateInstruction{..}, ..}, mVerRes) = do
  cm <- getChainMetadata
  -- check that payload si
  if not (validatePayloadSize (protocolVersion @pv) (updatePayloadSize uiHeader)) then
    return (TxInvalid InvalidPayloadSize)
  -- check that the transaction is not expired
  else if transactionExpired (updateTimeout uiHeader) (slotTime cm) then
    return (TxInvalid ExpiredTransaction)
  else do
    -- Check that the sequence number is correct
    expectedSequenceNumber <- getNextUpdateSequenceNumber (updateType uiPayload)
    if updateSeqNumber uiHeader /= expectedSequenceNumber then
      return (TxInvalid (NonSequentialNonce expectedSequenceNumber))
    else do
      -- Convert the payload to an update
      case uiPayload of
        ProtocolUpdatePayload u -> checkSigAndEnqueue $ UVProtocol u
        ElectionDifficultyUpdatePayload u -> checkSigAndEnqueue $ UVElectionDifficulty u
        EuroPerEnergyUpdatePayload u -> checkSigAndEnqueue $ UVEuroPerEnergy u
        MicroGTUPerEuroUpdatePayload u -> checkSigAndEnqueue $ UVMicroGTUPerEuro u
        FoundationAccountUpdatePayload u -> getAccountIndex u >>= \case
          Just ai -> checkSigAndEnqueue $ UVFoundationAccount ai
          Nothing -> return (TxInvalid (UnknownAccount u))
        MintDistributionUpdatePayload u -> checkSigAndEnqueue $ UVMintDistribution u
        TransactionFeeDistributionUpdatePayload u -> checkSigAndEnqueue $ UVTransactionFeeDistribution u
        GASRewardsUpdatePayload u -> checkSigAndEnqueue $ UVGASRewards u
        BakerStakeThresholdUpdatePayload u -> checkSigAndEnqueue $ UVBakerStakeThreshold u
        AddAnonymityRevokerUpdatePayload u -> checkSigAndEnqueue $ UVAddAnonymityRevoker u
        AddIdentityProviderUpdatePayload u -> checkSigAndEnqueue $ UVAddIdentityProvider u
        RootUpdatePayload (RootKeysRootUpdate u) -> checkSigAndEnqueue $ UVRootKeys u
        RootUpdatePayload (Level1KeysRootUpdate u) -> checkSigAndEnqueue $ UVLevel1Keys u
        RootUpdatePayload (Level2KeysRootUpdate u) -> checkSigAndEnqueue $ UVLevel2Keys u
        Level1UpdatePayload (Level1KeysLevel1Update u) -> checkSigAndEnqueue $ UVLevel1Keys u
        Level1UpdatePayload (Level2KeysLevel1Update u) -> checkSigAndEnqueue $ UVLevel2Keys u
  where
    checkSigAndEnqueue change = do
      case mVerRes of
        Just (TVer.Ok (TVer.ChainUpdateSuccess keysHash _)) -> do
          currentKeys <- getUpdateKeyCollection
          -- If the keys have not changed then the signature remains valid.
          if matchesUpdateKeysCollection currentKeys keysHash then do
            enqueue change
          else do
            -- keys might have changed and as such we try to verify the signature again.
            if not (checkAuthorizedUpdate currentKeys ui) then return $ TxInvalid IncorrectSignature
            else enqueue change
        -- An invalid verification result or `Nothing` was supplied to this function.
        -- In either case we verify the transaction now.
        _ -> do
          newVerRes <- TVer.verifyChainUpdate ui
          case checkTransactionVerificationResult newVerRes of
            Left failure -> return $ TxInvalid failure
            Right _ -> enqueue change
    enqueue change = do
      enqueueUpdate (updateEffectiveTime uiHeader) change
      tsIndex <- bumpTransactionIndex
      return $ TxValid TransactionSummary {
            tsSender = Nothing,
            tsHash = wmdHash,
            tsCost = 0,
            tsEnergyCost = 0,
            tsType = TSTUpdateTransaction $ updateType uiPayload,
            tsResult = TxSuccess [UpdateEnqueued (updateEffectiveTime uiHeader) uiPayload],
            ..
          }

handleUpdateCredentials ::
  SchedulerMonad pv m
    => WithDepositContext m
    -> OrdMap.Map ID.CredentialIndex ID.CredentialDeploymentInformation
    -> [ID.CredentialRegistrationID]
    -> ID.AccountThreshold
    -> m (Maybe TransactionSummary)
handleUpdateCredentials wtc cdis removeRegIds threshold =
  withDeposit wtc c k
   where
    senderAccount = wtc ^. wtcSenderAccount
    meta = wtc ^. wtcTransactionHeader
    senderAddress = thSender meta

    c = do
      tickEnergy Cost.updateCredentialsBaseCost
      creds <- getAccountCredentials (snd senderAccount)
      tickEnergy $ Cost.updateCredentialsVariableCost (OrdMap.size creds) (map (ID.credNumKeys . ID.credPubKeys) . OrdMap.elems $ cdis)
      cm <- lift getChainMetadata
      -- ensure none of the credentials have expired yet
      forM_ cdis $ \cdi -> do
        let expiry = ID.validTo cdi
        unless (isTimestampBefore (slotTime cm) expiry) $ rejectTransaction InvalidCredentials
      -- and ensure that this account is allowed to have multiple credentials
      allowed <- checkAccountIsAllowed (snd senderAccount) AllowedMultipleCredentials
      unless allowed $ rejectTransaction NotAllowedMultipleCredentials
      return creds

    k ls existingCredentials = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost senderAccount energyCost
      cryptoParams <- TVer.getCryptographicParameters

      -- check that all credentials that are to be removed actually exist.
      -- This produces:
      --  * a list of credential regIds that were supposed to be removed, but don't exist on the account,
      --    in reverse order
      --  * a set of the credential indices to remove
      --  * a list of the credential indices to remove, without duplicates, in reverse order
      let (nonExistingRegIds, indicesToRemove, revListIndicesToRemove) =
            -- Here we do not care whether the list of credentials to remove has unique elements
            -- or not. As long as the lists are limited in length this should not matter, i.e.,
            -- this is not an abusable property.
            -- Because we should never have duplicate regids on the chain, each of them will only
            -- map to the unique key index. Thus the following map is well-defined.
            let existingCredIds = OrdMap.fromList . map (\(ki, v) -> (ID.credId v, ki)) . OrdMap.toList $ existingCredentials
            in foldl' (\(nonExisting, existing, remList) rid ->
                         case rid `OrdMap.lookup` existingCredIds of
                           Nothing -> (rid:nonExisting, existing, remList)
                           Just ki -> (nonExisting, Set.insert ki existing, if Set.member ki existing then remList else ki : remList)
                      ) ([], Set.empty, []) removeRegIds

      -- check that the indices after removal are disjoint from the indices that we are about to add
      let removalCheck = null nonExistingRegIds &&
                         let existingIndices = OrdMap.keysSet existingCredentials
                             newIndices = OrdMap.keysSet cdis
                         in Set.intersection existingIndices newIndices `Set.isSubsetOf` indicesToRemove

      -- check that the new threshold is no more than the number of credentials
      --
      let thresholdCheck = toInteger (OrdMap.size existingCredentials) + toInteger (OrdMap.size cdis) >= toInteger (Set.size indicesToRemove) + toInteger threshold

      let firstCredNotRemoved = 0 `notElem` indicesToRemove

      -- make sure that the credentials that are being added have not yet been used on the chain.
      -- This is a list of credential registration ids that already exist. The list is used for error
      -- reporting.
      (existingCredIds, newCredIds) <- foldM (\(acc, newRids) cdi -> do
                                       let rid = ID.cdvCredId . ID.cdiValues $ cdi
                                       exists <- TVer.registrationIdExists rid
                                       if exists || Set.member rid newRids then
                                         return (rid:acc, rid `Set.insert` newRids)
                                       else return (acc, rid `Set.insert` newRids)
                                    ) ([], Set.empty) cdis

      let getIP cdi = TVer.getIdentityProvider $ ID.ipId $ ID.NormalACWP cdi
          getAR cdi = TVer.getAnonymityRevokers $ OrdMap.keys $ ID.cdvArData $ ID.cdiValues cdi
          -- verify the cryptographic proofs
          checkCDI cdi = do
              ip <- getIP cdi
              ar <- getAR cdi
              case ip of
                Nothing -> return False
                Just ipInfo -> case ar of
                  Nothing -> return False
                  Just arInfos -> return $ AH.verifyCredential cryptoParams ipInfo arInfos (S.encode cdi) (Right senderAddress)
      -- check all the credential proofs.
      -- This is only done if all the previous checks have succeeded since this is by far the most computationally expensive part.
      checkProofs <- foldM (\check cdi -> if check then checkCDI cdi else return False) (firstCredNotRemoved && thresholdCheck && removalCheck && null existingCredIds) cdis
      if checkProofs then do -- check if stuff is correct
        let creds = traverse (ID.values . ID.NormalACWP) cdis
        case creds of
          Nothing -> return (TxReject InvalidCredentials, energyCost, usedEnergy)
          Just newCredentials -> do
            updateAccountCredentials (fst senderAccount) (reverse revListIndicesToRemove) newCredentials threshold
            return (TxSuccess [CredentialsUpdated {
                                  cuAccount = senderAddress,
                                  cuNewCredIds = Set.toList newCredIds,
                                  cuRemovedCredIds = removeRegIds,
                                  cuNewThreshold = threshold
                                  }], energyCost, usedEnergy)
      else
        -- try to provide a more fine-grained error by analyzing what went wrong
        -- at some point we should refine the scheduler monad to support cleaner error
        -- handling by adding MonadError capability to it. Until that is done this
        -- is a pretty clean alternative to avoid deep nesting.
        if not firstCredNotRemoved then
          return (TxReject RemoveFirstCredential, energyCost, usedEnergy)
        else if not thresholdCheck then
          return (TxReject InvalidAccountThreshold, energyCost, usedEnergy)
        else if not (null nonExistingRegIds) then
          return (TxReject (NonExistentCredIDs nonExistingRegIds), energyCost, usedEnergy)
        else if not removalCheck then
          return (TxReject KeyIndexAlreadyInUse, energyCost, usedEnergy)
        else if not (null existingCredIds) then
          return (TxReject (DuplicateCredIDs existingCredIds), energyCost, usedEnergy)
        else return (TxReject InvalidCredentials, energyCost, usedEnergy)

-- |Charges energy based on payload size and emits a 'DataRegistered' event.
handleRegisterData ::
  SchedulerMonad pv m
  => WithDepositContext m
  -> RegisteredData -- ^The data to register.
  -> m (Maybe TransactionSummary)
handleRegisterData wtc regData =
  withDeposit wtc c (defaultSuccess wtc)
  where
    c = do
      tickEnergy Cost.registerDataCost
      return [DataRegistered regData]


-- * Exposed methods.

-- |Make a valid block out of a list of transactions, respecting the given
-- maximum block size and block energy limit.
--
-- The preconditions of this function (which are not checked) are:
--
-- * The transactions appear grouped by their associated account address,
--   and the transactions in each group are ordered by increasing transaction nonce.
-- * Each transaction's nonce is equal or higher than the next nonce of the specified sender
--   account (if the account it exists).
--
-- The 'GroupedTransactions' ('perAccountTransactions' and 'credentialDeployments') are processed in
-- order of their arrival time, assuming that both lists are ordered by arrival time from earliest to
-- latest. For each group in 'perAccountTransactions', only the time of the first transaction in the
-- group is considered and the entire group is processed
-- in one sequence.
--
-- = Processing of transactions
--
-- Processing starts with an initial remaining block energy being the maximum block energy
-- (birk parameter) and the remaining block size being as specified by the parameter to this function.
--
-- Each transaction or credential deployment is processed as follows:
--
-- * It is checked whether the deposited energy (or in case of credential deployment the respective
--   energy cost) is not greater than the maximum block energy (in which case the transaction fails
--   with 'ExceedsMaxBlockEnergy').
-- * It is checked whether the deposited energy (or, in case of credential deployment, the respective
--   energy cost) and the transaction size is not greater than the remaining block energy / block size
--   (in which case the transaction is skipped and added to the list of unprocessed
--   transactions/credentials).
-- * If the previous checks passed, the transaction is executed.
--
--     * If execution fails with another 'FailureKind', the transaction / credential deployment is added
--       to the list of failed transactions/credentials.
--     * If execution succeeds ('TxValid'), the transaction / credential deployment is added to the list
--       of added transactions and the actual energy used by the transaction as well as the transaction
--       size is deducted from the remaining block energy / block size.
--
-- Only added transactions have an effect on the block state.
--
-- = Transaction groups
-- Groups allow early failure of transactions with a repeated nonce after a successful transaction
-- as well as a special failure kind ('SuccessorOfInvalidTransaction') for transactions which cannot
-- be accepted because a predecessor transaction (with a lower nonce) failed and no other transaction
-- with the same nonce was added.
--
-- The processing of transactions within a group has the following additional properties:
--
-- * After an added transactions, all following transactions with the same nonce directly fail with
--   'NonSequentialNonce' (whereas when processed individually, it might fail for another reason).
--   The next transaction with a higher nonce is processed normally.
-- * If a transaction fails and the next transaction has the same nonce, it is processed normally.
-- * If a transaction fails and the next transaction has a higher nonce all remaining transactions in
--   the group will fail with 'SuccessorOfInvalidTransaction' instead of 'NonSequentialNonce' (or the
--   failure it would fail with if processed individually). Note that because transactions are ordered
--   by nonce, all those remaining transactions are invalid at least because of a non-sequential nonce.
--
-- Note that this behaviour relies on the precondition of transactions within a group coming from the
-- same account and being ordered by increasing nonce.
--
-- = Result
-- The order of transactions in 'ftAdded' (this includes credential deployments) corresponds to the
-- order the transactions should appear on the block (i.e., the order they were executed on the current
-- block state).
-- There is no guarantee for any order in `ftFailed`, `ftFailedCredentials`, `ftUnprocessed`
-- and `ftUnprocessedCredentials`.
filterTransactions :: forall m pv. (SchedulerMonad pv m, TimeMonad m)
                   => Integer -- ^Maximum block size in bytes.
                   -> UTCTime -- ^Timeout for block construction.
                             -- This is the absolute time after which we should stop trying to add new transctions to the block.
                   -> [TransactionGroup] -- ^Transactions to make a block out of.
                   -> m FilteredTransactions
filterTransactions maxSize timeout groups0 = do
  maxEnergy <- getMaxBlockEnergy
  credLimit <- getAccountCreationLimit
  ftTrans <- runNext maxEnergy 0 credLimit False emptyFilteredTransactions groups0
  -- For each type of invalid message we only log the first 10 items so as to
  -- not to spend too much time in the logging phase. This makes it useful for debugging,
  -- but not a potential problem when a lot of invalid transactions are being sent.
  let (toReportTrans, restTrans) = splitAt 10 (ftFailed ftTrans)
  forM_ toReportTrans $ uncurry logInvalidTransaction
  unless (null restTrans) $ logEvent Scheduler LLWarning "Too many invalid transactions. Suppressing reporting the remaining ones."
  let (toReportCredentials, restCredentials) = splitAt 10 (ftFailedCredentials ftTrans)
  forM_ toReportCredentials $ uncurry logInvalidCredential
  unless (null restCredentials) $ logEvent Scheduler LLWarning "Too many invalid credentials. Suppressing reporting the remaining ones."
  let (toReportUpdates, restUpdates) = splitAt 10 (ftFailedUpdates ftTrans)
  forM_ toReportUpdates $ uncurry logInvalidChainUpdate
  unless (null restUpdates) $ logEvent Scheduler LLWarning "Too many invalid updates. Suppressing reporting the remaining ones."
  return ftTrans
  where
        -- Run next credential deployment or transaction group, depending on arrival time.
        runNext :: Energy -- ^Maximum block energy
                -> Integer -- ^Current size of transactions in the block.
                -> CredentialsPerBlockLimit -- ^Number of credentials until limit.
                -> Bool -- ^Whether or not the block timeout is reached
                -> FilteredTransactions -- ^Currently accumulated result
                -> [TransactionGroup] -- ^Grouped transactions to process
                -> m FilteredTransactions
        -- All block items are processed. We accumulate the added items
        -- in reverse order, so reverse the list before returning.
        runNext _ _ _ _ fts []= return fts{ftAdded = reverse (ftAdded fts)}
        runNext maxEnergy size credLimit True fts (g : groups) = case g of -- Time is up. Mark subsequent groups as unprocessed.
          TGAccountTransactions group -> let newFts = fts { ftUnprocessed = group ++ ftUnprocessed fts }
                                         in runNext maxEnergy size credLimit True newFts groups
          TGCredentialDeployment c -> let newFts = fts { ftUnprocessedCredentials = c : ftUnprocessedCredentials fts }
                                      in runNext maxEnergy size credLimit True newFts groups
          TGUpdateInstructions group -> let newFts = fts { ftUnprocessedUpdates = group ++ ftUnprocessedUpdates fts }
                                        in runNext maxEnergy size credLimit True newFts groups
        runNext maxEnergy size credLimit False fts (g : groups) = case g of -- Time isn't up yet. Process groups until time is up.
          TGAccountTransactions group -> runTransactionGroup size fts group
          TGCredentialDeployment c -> runCredential c
          TGUpdateInstructions group -> runUpdateInstructions size fts group
          where
            -- Run a group of update instructions of one type
            runUpdateInstructions currentSize currentFts [] = runNext maxEnergy currentSize credLimit False currentFts groups
            runUpdateInstructions currentSize currentFts (ui : uis) = do
              -- Update instructions use no energy, so we only consider size
              let csize = currentSize + fromIntegral (wmdSize (fst ui))
              now <- currentTime
              if now >= timeout then do -- Time is up. Mark this and all subsequent groups as unprocessed.
                logEvent Scheduler LLWarning "Timeout reached for block construction."
                let newFts = currentFts{ftUnprocessedUpdates = ui : uis ++ ftUnprocessedUpdates currentFts}
                runNext maxEnergy currentSize credLimit True newFts groups
              else if csize <= maxSize then
                -- Chain updates have no account footprint
                handleChainUpdate ui >>= \case
                  TxInvalid reason -> case uis of
                    (nui : _) | ((==) `on` (updateSeqNumber . uiHeader . wmdData)) (fst ui) (fst nui) ->
                      -- If there is another update with the same sequence number, we want to try that
                      runUpdateInstructions currentSize currentFts{ftFailedUpdates = (ui, reason) : ftFailedUpdates currentFts} uis
                    _ -> do
                      -- Otherwise, any subsequent updates are also treated as failures
                      let newFts = currentFts{
                              ftFailedUpdates = (ui, reason) : ((, SuccessorOfInvalidTransaction) <$> uis)
                                                ++ ftFailedUpdates currentFts
                            }
                      runNext maxEnergy currentSize credLimit False newFts groups
                  TxValid summary -> do
                    let (invalid, rest) = span (((==) `on` (updateSeqNumber . uiHeader . wmdData . fst)) ui) uis
                        curSN = updateSeqNumber $ uiHeader $ wmdData (fst ui)
                        newFts = currentFts{
                            ftFailedUpdates = ((, NonSequentialNonce (curSN + 1)) <$> invalid) ++ ftFailedUpdates currentFts,
                            ftAdded = (ui & _1 %~ chainUpdate, summary) : ftAdded currentFts
                          }
                    runUpdateInstructions csize newFts rest
              else -- The cumulative block size with this update is too high.
                case uis of
                  (nui : _) | ((==) `on` (updateSeqNumber . uiHeader . wmdData . fst)) ui nui ->
                    -- There is another update with the same sequence number, so try that
                    let newFts = currentFts{ftUnprocessedUpdates = ui : ftUnprocessedUpdates currentFts}
                    in runUpdateInstructions currentSize newFts uis
                  _ ->
                    -- Otherwise, there's no chance of processing remaining updates
                    let newFts = currentFts{ftUnprocessedUpdates = ui : uis ++ ftUnprocessedUpdates currentFts}
                    in runNext maxEnergy currentSize credLimit False newFts groups

            -- Run a single credential and continue with 'runNext'.
            runCredential :: TVer.CredentialDeploymentWithStatus -> m FilteredTransactions
            runCredential cws@(c@WithMetadata{..}, verRes) = do
              totalEnergyUsed <- getUsedEnergy
              let csize = size + fromIntegral wmdSize
                  energyCost = Cost.deployCredential (ID.credentialType c) (ID.credNumKeys . ID.credPubKeys $ c)
                  cenergy = totalEnergyUsed + fromIntegral energyCost
              now <- currentTime
              if now >= timeout then do -- Time is up. Mark this and all subsequent groups as unprocessed.
                logEvent Scheduler LLWarning "Timeout reached for block construction."
                let newFts = fts { ftUnprocessedCredentials = cws : ftUnprocessedCredentials fts}
                runNext maxEnergy size credLimit True newFts groups
              -- NB: be aware that credLimit is of an unsigned type, so it is crucial that we never wrap around
              else if credLimit > 0 && csize <= maxSize && cenergy <= maxEnergy then
                observeTransactionFootprint (handleDeployCredential cws wmdHash) >>= \case
                    (Just (TxInvalid reason), _) -> do
                      let newFts = fts { ftFailedCredentials = (cws, reason) : ftFailedCredentials fts}
                      runNext maxEnergy size (credLimit - 1) False newFts groups -- NB: We keep the old size
                    (Just (TxValid summary), fp) -> do
                      markEnergyUsed (tsEnergyCost summary)
                      tlNotifyAccountEffect fp summary
                      let newFts = fts { ftAdded = ((credentialDeployment c, verRes), summary) : ftAdded fts}
                      runNext maxEnergy csize (credLimit - 1) False newFts groups
                    (Nothing, _) -> error "Unreachable due to cenergy <= maxEnergy check."
              else if Cost.deployCredential (ID.credentialType c) (ID.credNumKeys . ID.credPubKeys $ c) > maxEnergy then
                -- this case should not happen (it would mean we set the parameters of the chain wrong),
                -- but we keep it just in case.
                 let newFts = fts { ftFailedCredentials = (cws, ExceedsMaxBlockEnergy) : ftFailedCredentials fts}
                 in runNext maxEnergy size credLimit False newFts groups
              else
                 let newFts = fts { ftUnprocessedCredentials = cws : ftUnprocessedCredentials fts}
                 in runNext maxEnergy size credLimit False newFts groups

            -- Run all transactions in a group and continue with 'runNext'.
            runTransactionGroup :: Integer -- ^Current size of transactions in the block.
                                -> FilteredTransactions
                                -> [TVer.TransactionWithStatus] -- ^Current group to process.
                                -> m FilteredTransactions
            runTransactionGroup currentSize currentFts (t:ts) = do
              totalEnergyUsed <- getUsedEnergy
              let csize = currentSize + fromIntegral (transactionSize (fst t))
                  tenergy = transactionGasAmount $ fst t
                  cenergy = totalEnergyUsed + tenergy
              now <- currentTime
              if now >= timeout then do -- Time is up. Mark this and all subsequent groups as unprocessed.
                logEvent Scheduler LLWarning "Timeout reached for block construction."
                let newFts = currentFts { ftUnprocessed = t : ts ++ ftUnprocessed currentFts }
                runNext maxEnergy currentSize credLimit True newFts groups
              else if csize <= maxSize && cenergy <= maxEnergy then
                -- The transaction fits regarding both block energy limit and max transaction size.
                -- Thus try to add the transaction by executing it.
                observeTransactionFootprint (dispatch t) >>= \case
                   -- The transaction was committed, add it to the list of added transactions.
                   (Just (TxValid summary), fp) -> do
                     (newFts, rest) <- validTs t summary fp currentFts ts
                     runTransactionGroup csize newFts rest
                   -- The transaction failed, add it to the list of failed transactions and
                   -- determine whether the following transactions have to fail as well.
                   (Just (TxInvalid reason), _) ->
                     let (newFts, rest) = invalidTs t reason currentFts ts
                     in runTransactionGroup currentSize newFts rest
                   (Nothing, _) -> error "Unreachable. Dispatch honors maximum transaction energy."
              -- If the stated energy of a single transaction exceeds the block energy limit the
              -- transaction is invalid. Add it to the list of failed transactions and
              -- determine whether following transactions have to fail as well.
              else if tenergy > maxEnergy then
                let (newFts, rest) = invalidTs t ExceedsMaxBlockEnergy currentFts ts
                in runTransactionGroup currentSize newFts rest
              else -- otherwise still try the remaining transactions in the group to avoid deadlocks from
                   -- one single too-big transaction (with same nonce).
                case ts of
                  (nt : _) | transactionNonce (fst nt) == transactionNonce (fst t) ->
                    let newFts = currentFts { ftUnprocessed = t : ftUnprocessed currentFts }
                    in runTransactionGroup currentSize newFts ts
                  _ ->
                    let newFts = currentFts { ftUnprocessed = t : ts ++ ftUnprocessed currentFts }
                    in runNext maxEnergy currentSize credLimit False newFts groups

            -- Group processed, continue with the next group or credential
            runTransactionGroup currentSize currentFts [] =
              runNext maxEnergy currentSize credLimit False currentFts groups

            -- Add a valid transaction to the list of added transactions, mark used energy and
            -- notify about the account effects. Then add all following transactions with the
            -- same nonce to the list of failed transactions, as they are invalid.
            -- NOTE: It is necessary that we process those invalid transactions directly,
            -- because while 'invalidTs' would classify them as 'NonSequentialNonce' as well,
            -- it would reject following valid transactions with a higher but correct nonce.
            -- The next transaction with a higher nonce (head of ts') should thus be processed
            -- with 'runNext'.
            validTs t summary fp currentFts ts = do
              markEnergyUsed (tsEnergyCost summary)
              tlNotifyAccountEffect fp summary
              let (invalid, rest) = span ((== transactionNonce (fst t)) . transactionNonce . fst) ts
              let nextNonce = transactionNonce (fst t) + 1
              let newFts =
                    currentFts { ftFailed = map (, NonSequentialNonce nextNonce) invalid
                                            ++ ftFailed currentFts
                               , ftAdded = ((normalTransaction (fst t), snd t), summary) : ftAdded currentFts
                               }
              return (newFts, rest)

            -- Add a failed transaction (t, failure) to the list of failed transactions and
            -- check whether the remaining transactions from this group (ts) have a nonce
            -- greater than that of the failed transaction, in which case these are also added to the
            -- list of failed transactions with a 'SuccessorOfInvalidTransaction' failure.
            -- Returns the updated 'FilteredTransactions' and the yet to be processed transactions.
            invalidTs t failure currentFts ts =
              let newFailedEntry = (t, failure) in
                -- NOTE: Following transactions with the same nonce could be valid. Therefore,
                -- if the next transaction has the same nonce as the failed, we continue with 'runNext'.
                -- Note that we rely on the precondition of transactions being ordered by nonce.
                if not (null ts) && transactionNonce (fst (head ts)) > transactionNonce (fst t)
                then let failedSuccessors = map (, SuccessorOfInvalidTransaction) ts in
                       (currentFts { ftFailed = newFailedEntry : failedSuccessors ++ ftFailed currentFts }
                     , [])
                else (currentFts { ftFailed = newFailedEntry : ftFailed currentFts }, ts)

-- |Execute transactions in sequence, collecting the outcomes of each transaction.
-- This is meant to execute the transactions of a given block.
--
-- Returns
--
-- * @Left Nothing@ if maximum block energy limit was exceeded.
-- * @Left (Just fk)@ if a transaction failed, with the failure kind of the first failed transaction.
-- * @Right outcomes@ if all transactions are successful, with the given outcomes.
runTransactions :: forall m pv. (SchedulerMonad pv m)
                => [TVer.BlockItemWithStatus]
                -> m (Either (Maybe FailureKind) [(BlockItem, TransactionSummary)])
runTransactions = go []
    where go valid (bi:ts) =
            observeTransactionFootprint (predispatch bi) >>= \case
              (Just (TxValid summary), fp) -> do
                markEnergyUsed (tsEnergyCost summary)
                tlNotifyAccountEffect fp summary
                go ((bi, summary):valid) ts
              (Just (TxInvalid reason), _) -> do
                logInvalidBlockItem (fst bi) reason
                return (Left (Just reason))
              (Nothing, _) -> return (Left Nothing)

          go valid [] = return (Right (reverse $ map (\(x,y) -> (fst x, y)) valid))

          predispatch :: TVer.BlockItemWithStatus -> m (Maybe TxResult)
          predispatch (WithMetadata{wmdData=NormalTransaction tr,..}, verRes) = dispatch (WithMetadata{wmdData=tr,..}, verRes)
          predispatch (WithMetadata{wmdData=CredentialDeployment cred,..}, verRes) = handleDeployCredential (WithMetadata{wmdData=cred,..}, verRes) wmdHash
          predispatch (WithMetadata{wmdData=ChainUpdate cu,..}, verRes) = Just <$> handleChainUpdate (WithMetadata{wmdData=cu,..}, verRes)

-- |Execute transactions in sequence. Like 'runTransactions' but only for side-effects on global state.
--
-- Returns
--
-- * @Left Nothing@ if maximum block energy limit was exceeded.
-- * @Left (Just fk)@ if a transaction failed, with the failure kind of the first failed transaction
-- * @Right ()@ if all transactions are successful.
--
-- This is more efficient than 'runTransactions' since it does not have to build a list
-- of results.
execTransactions :: forall m pv. (SchedulerMonad pv m)
                 => [TVer.BlockItemWithStatus]
                 -> m (Either (Maybe FailureKind) ())
execTransactions = go
  -- Same implementation as 'runTransactions', just that valid block items
  -- and transaction summaries are not collected.
  where go (bi:ts) =
          observeTransactionFootprint (predispatch bi) >>= \case
            (Nothing, _) -> return (Left Nothing)
            (Just (TxValid summary), fp) -> do
              markEnergyUsed (tsEnergyCost summary)
              tlNotifyAccountEffect fp summary
              go ts
            (Just (TxInvalid reason), _) -> do
              logInvalidBlockItem (fst bi) reason
              return (Left (Just reason))
        go [] = return (Right ())

        predispatch :: TVer.BlockItemWithStatus -> m (Maybe TxResult)
        predispatch (WithMetadata{wmdData=NormalTransaction tr,..}, verRes) = dispatch (WithMetadata{wmdData=tr,..}, verRes)
        predispatch (WithMetadata{wmdData=CredentialDeployment cred,..}, verRes) = handleDeployCredential (WithMetadata{wmdData=cred,..}, verRes) wmdHash
        predispatch (WithMetadata{wmdData=ChainUpdate cu,..}, verRes) = Just <$> handleChainUpdate (WithMetadata{wmdData=cu,..}, verRes)
