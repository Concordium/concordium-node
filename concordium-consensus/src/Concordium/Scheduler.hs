{-# LANGUAGE TupleSections #-}
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
  ,FilteredTransactions(..)
  ) where

import qualified Concordium.Wasm as Wasm
import qualified Concordium.Scheduler.WasmIntegration as Wasm
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment

import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.BlockState (AccountOperations(..))
import qualified Concordium.GlobalState.BakerInfo as BI
import qualified Concordium.GlobalState.Instance as Ins
import Concordium.GlobalState.Types
import qualified Concordium.Scheduler.Cost as Cost
import Concordium.Crypto.EncryptedTransfers

import Control.Applicative
import Control.Monad.Except
import Data.Function (on)
import qualified Data.Map.Strict as OrdMap
import Data.Maybe
import Data.Ord
import Data.List hiding (group)
import qualified Data.Set as Set

import qualified Concordium.Crypto.Proofs as Proofs
import qualified Concordium.Crypto.BlsSignature as Bls

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

-- |Check that there exists a valid credential in the context of the given chain
-- metadata.
existsValidCredential :: AccountOperations m => ChainMetadata -> Account m -> m Bool
existsValidCredential cm acc = do
  -- check that the sender has at least one still valid credential.
  expiry <- getAccountMaxCredentialValidTo acc
  -- If the credential is still valid at the beginning of this slot then
  -- we consider it valid. Otherwise we fail the transaction.
  return $ isTimestampBefore (slotTime cm) expiry

-- |Check that
--  * the transaction has a valid sender,
--  * the amount corresponding to the deposited energy is on the sender account,
--  * the transaction is not expired,
--  * the transaction nonce is the account's next nonce,
--  * the transaction is signed with the account's verification keys.
-- "Valid sender" means that the sender account exists and has at least one valid credential,
-- where currently valid means non-expired.
--
-- Before any other checks this checks whether the amount deposited is enough to cover
-- the cost that will be charged for checking the header.
--
-- Throws 'Nothing' if the remaining block energy is not sufficient to cover the cost of checking the
-- header and @Just fk@ if any of the checks fails, with the respective 'FailureKind'.
--
-- Returns the sender account and the cost to be charged for checking the header.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> ExceptT (Maybe FailureKind) m (Account m, Energy)
checkHeader meta = do
  -- Before even checking the header we calculate the cost that will be charged for this
  -- and check that at least that much energy is deposited and remaining from the maximum block energy.
  let cost = Cost.checkHeader (getTransactionHeaderPayloadSize $ transactionHeader meta) (getTransactionNumSigs (transactionSignature meta))
  unless (transactionGasAmount meta >= cost) $ throwError (Just DepositInsufficient)
  remainingBlockEnergy <- lift getRemainingEnergy
  unless (remainingBlockEnergy >= cost) $ throwError Nothing

  -- Now check whether the specified sender exists, and only then do all remaining checks.
  macc <- lift (getAccount (transactionSender meta))
  case macc of
    Nothing -> throwError . Just $ (UnknownAccount (transactionSender meta))
    Just acc -> do
      amnt <- getAccountAvailableAmount acc
      nextNonce <- getAccountNonce acc
      let txnonce = transactionNonce meta
      let expiry = thExpiry $ transactionHeader meta

      cm <- lift getChainMetadata
      when (transactionExpired expiry $ slotTime cm) $ throwError . Just $ ExpiredTransaction
      validCredExists <- existsValidCredential cm acc
      unless validCredExists $ throwError . Just $ NoValidCredential

      -- After the successful credential check we check that the sender account
      -- has enough GTU to cover the deposited energy.
      depositedAmount <- lift (energyToGtu (transactionGasAmount meta))
      unless (depositedAmount <= amnt) (throwError . Just $ InsufficientFunds)

      unless (txnonce == nextNonce) (throwError . Just $ (NonSequentialNonce nextNonce))

      -- Finally do the signature verification, the computationally most expensive part.
      keys <- getAccountVerificationKeys acc
      let sigCheck = verifyTransaction keys meta
      unless sigCheck (throwError . Just $ IncorrectSignature)

      return (acc, cost)

-- TODO: When we have policies checking one sensible approach to rewarding
-- identity providers would be as follows.
--
-- - Each time we need to check a policy on an account all the identity
-- providers that have valid credentials deployed on that account are counted.
-- This means that if the same account is involved multiple times inside one
-- transaction then the identity providers on that account would be rewarded
-- multiple times.
--
-- An alternative design is that each identity provider involved in one
-- transaction is rewarded only once. To allow for this we will need to keep
-- track of the identity providers inside the transaction monad.
--
-- Another important point to resolve is how identity providers should be
-- rewarded in case of a rejected transaction. Should they be, or should they
-- not be. In particular when a transaction is rejected based on transaction
-- logic that is unrelated to identities.

-- | Execute a transaction on the current block state, charging valid accounts
-- for the resulting energy cost.
--
-- First checks the meta data in the header of the transaction, then decodes the
-- payload and lets the respective handler execute the actual transaction.
--
-- Returns
--
-- * @Nothing@ if the transaction would exceed the remaining block energy.
-- * @Just result@ if the transaction failed ('TxInvalid') or was successfully committed
--  ('TxValid', with either 'TxSuccess' or 'TxReject').
dispatch :: (TransactionData msg, SchedulerMonad m) => msg -> m (Maybe TxResult)
dispatch msg = do
  let meta = transactionHeader msg
  validMeta <- runExceptT (checkHeader msg)
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
      -- TODO: Check whether the cost for deserializing the transaction is sufficiently covered
      -- by the cost for checking the header (which is linear in the transaction size).

      tsIndex <- bumpTransactionIndex
      case decodePayload psize (transactionPayload msg) of
        Left _ -> do
          -- In case of serialization failure we charge the sender for checking
          -- the header and reject the transaction; we have checked that the amount
          -- exists on the account with 'checkHeader'.
          payment <- energyToGtu checkHeaderCost
          chargeExecutionCost (transactionHash msg) senderAccount payment
          addr <- getAccountAddress senderAccount
          return $ Just $ TxValid $ TransactionSummary{
            tsEnergyCost = checkHeaderCost,
            tsCost = payment,
            tsSender = Just addr,
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
                     handleDeployModule (mkWTC TTDeployModule) psize mod

                   InitContract{..} ->
                     handleInitContract (mkWTC TTInitContract) icAmount icModRef icInitName icParam
                   -- FIXME: This is only temporary for now.
                   -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
                   Transfer toaddr amount ->
                     handleSimpleTransfer (mkWTC TTTransfer) toaddr amount

                   Update{..} ->
                     -- the payload size includes amount + address + message, but since the first two fields are
                     -- fixed size this is OK.
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

                   UpdateAccountKeys{..} ->
                     handleUpdateAccountKeys (mkWTC TTUpdateAccountKeys) uakKeys

                   AddAccountKeys{..} ->
                     handleAddAccountKeys (mkWTC TTAddAccountKeys) aakKeys aakThreshold

                   RemoveAccountKeys{..} ->
                     handleRemoveAccountKeys (mkWTC TTRemoveAccountKeys) rakIndices rakThreshold

                   EncryptedAmountTransfer{..} ->
                     handleEncryptedAmountTransfer (mkWTC TTEncryptedAmountTransfer) eatTo eatData

                   TransferToEncrypted{..} ->
                     handleTransferToEncrypted (mkWTC TTTransferToEncrypted) tteAmount

                   TransferToPublic{..} ->
                     handleTransferToPublic (mkWTC TTTransferToPublic) ttpData

                   TransferWithSchedule{..} ->
                     handleTransferWithSchedule (mkWTC TTTransferWithSchedule) twsTo twsSchedule

          case res of
            -- The remaining block energy is not sufficient for the handler to execute the transaction.
            Nothing -> return Nothing
            Just summary -> return $ Just $ TxValid summary

handleTransferWithSchedule ::
  SchedulerMonad m
  => WithDepositContext m
  -> AccountAddress
  -> [(Timestamp, Amount)]
  -> m (Maybe TransactionSummary)
handleTransferWithSchedule wtc twsTo twsSchedule = withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader

        c = do
          senderAddress <- getAccountAddress senderAccount
          -- the expensive operations start now, so we charge.

          -- After we've checked all of that, we charge.
          tickEnergy (Cost.transferWithSchedule $ length twsSchedule)

          -- we do not allow for self scheduled transfers
          when (twsTo == senderAddress) $ rejectTransaction (ScheduledSelfTransfer twsTo)

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
              -- Check that the account has a valid credential and reject the transaction if not
              -- (it is not allowed to send to accounts without valid credential).
              validCredExists <- existsValidCredential cm targetAccount
              unless validCredExists $ rejectTransaction (ReceiverAccountNoCredential twsTo)

              withScheduledAmount senderAccount targetAccount transferAmount twsSchedule txHash $ return senderAddress

        k ls senderAddress = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          commitChanges (ls ^. changeSet)
          return (TxSuccess [TransferredWithSchedule{etwsFrom = senderAddress,
                                                     etwsTo = twsTo,
                                                     etwsAmount = twsSchedule}],
                    energyCost,
                    usedEnergy)


handleTransferToPublic ::
  SchedulerMonad m
  => WithDepositContext m
  -> SecToPubAmountTransferData
  -> m (Maybe TransactionSummary)
handleTransferToPublic wtc transferData@SecToPubAmountTransferData{..} = do
  cryptoParams <- getCryptoParams
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader

        c cryptoParams = do
          senderAddress <- getAccountAddress senderAccount
          -- the expensive operations start now, so we charge.

          -- After we've checked all of that, we charge.
          tickEnergy Cost.secToPubTransfer

          -- Get the encrypted amount at the index that the transfer claims to be using.
          senderAmount <- getAccountEncryptedAmountAtIndex senderAccount stpatdIndex `rejectingWith` InvalidIndexOnEncryptedTransfer

          -- and then we start validating the proof. This is the most expensive
          -- part of the validation by far, the rest only being lookups and a little bit of addition.
          senderPK <- getAccountEncryptionKey senderAccount
          let valid = verifySecretToPublicTransferProof cryptoParams senderPK senderAmount transferData

          unless valid $ rejectTransaction InvalidTransferToPublicProof

          -- if the proof is valid we need to
          -- - add the decrypted amount to the balance
          -- - replace some encrypted amounts on the sender's account
          addAmountFromEncrypted senderAccount stpatdTransferAmount stpatdIndex stpatdRemainingAmount

          return (senderAddress, senderAmount)

        k ls (senderAddress, senderAmount) = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
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
  SchedulerMonad m
  => WithDepositContext m
  -> Amount
  -> m (Maybe TransactionSummary)
handleTransferToEncrypted wtc toEncrypted = do
  cryptoParams <- getCryptoParams
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader

        c cryptoParams = do
          senderAddress <- getAccountAddress senderAccount

          tickEnergy Cost.pubToSecTransfer

          -- check that the sender actually owns the amount it claims to be transferred
          senderamount <- getCurrentAccountAvailableAmount senderAccount
          unless (senderamount >= toEncrypted) $! rejectTransaction (AmountTooLarge (AddressAccount senderAddress) toEncrypted)

          -- compute the encrypted amount
          let encryptedAmount = encryptAmountZeroRandomness cryptoParams toEncrypted

          -- We have to subtract the amount and update the self encrypted amount
          addSelfEncryptedAmount senderAccount toEncrypted encryptedAmount

          return (senderAddress, encryptedAmount)

        k ls (senderAddress, encryptedAmount) = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          notifyEncryptedBalanceChange $ amountToDelta toEncrypted
          commitChanges (ls ^. changeSet)

          return (TxSuccess [EncryptedSelfAmountAdded{
                                eaaAccount = senderAddress,
                                eaaNewAmount = encryptedAmount,
                                eaaAmount = toEncrypted
                                }],
                   energyCost,
                   usedEnergy)

handleEncryptedAmountTransfer ::
  SchedulerMonad m
  => WithDepositContext m
  -> AccountAddress -- ^ Receiver address.
  -> EncryptedAmountTransferData
  -> m (Maybe TransactionSummary)
handleEncryptedAmountTransfer wtc toAddress transferData@EncryptedAmountTransferData{..} = do
  cryptoParams <- getCryptoParams
  withDeposit wtc (c cryptoParams) k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader

        c cryptoParams = do
          senderAddress <- getAccountAddress senderAccount
          -- We do not allow sending encrypted transfers from an account to itself.
          -- There is no reason to do so in the current setup, and it causes some technical
          -- complications.
          when (toAddress == senderAddress) $ rejectTransaction (EncryptedAmountSelfTransfer toAddress)

          -- Look up the receiver account first, and don't charge if it does not exist
          -- and does not have a valid credential.
          targetAccount <- getStateAccount toAddress `rejectingWith` InvalidAccountReference toAddress
          cm <- getChainMetadata
          validCredExists <- existsValidCredential cm targetAccount
          unless validCredExists $ rejectTransaction (ReceiverAccountNoCredential toAddress)

          -- the expensive operations start now, so we charge.

          -- After we've checked all of that, we charge.
          tickEnergy Cost.encryptedAmountTransfer

          -- Get the encrypted amount at the index that the transfer claims to be using.
          senderAmount <- getAccountEncryptedAmountAtIndex senderAccount eatdIndex `rejectingWith` InvalidIndexOnEncryptedTransfer
          -- and then we start validating the proof. This is the most expensive
          -- part of the validation by far, the rest only being lookups.
          receiverPK <- getAccountEncryptionKey targetAccount
          senderPK <- getAccountEncryptionKey senderAccount
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
          targetAccountIndex <- addEncryptedAmount targetAccount eatdTransferAmount

          return (senderAddress, targetAccountIndex, senderAmount)

        k ls (senderAddress, targetAccountIndex, senderAmount) = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          commitChanges (ls ^. changeSet)

          return (TxSuccess [EncryptedAmountsRemoved{
                                earAccount = senderAddress,
                                earUpToIndex = eatdIndex,
                                earInputAmount = senderAmount,
                                earNewAmount = eatdRemainingAmount
                                },
                             NewEncryptedAmount{
                                neaAccount = toAddress,
                                neaNewIndex = targetAccountIndex,
                                neaEncryptedAmount = eatdTransferAmount
                                }
                            ],
                   energyCost,
                   usedEnergy)

-- | Handle the deployment of a module.
handleDeployModule ::
  SchedulerMonad m
  => WithDepositContext m
  -> PayloadSize -- ^Serialized size of the module. Used for charging execution cost.
  -> Wasm.WasmModule -- ^The module to deploy.
  -> m (Maybe TransactionSummary)
handleDeployModule wtc psize mod =
  withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader

    c = do
      tickEnergy (Cost.deployModule (fromIntegral psize))
      case Wasm.processModule mod of
        Nothing -> rejectTransaction ModuleNotWF
        Just iface -> do
          let mhash = Wasm.miModuleRef iface
          exists <- isJust <$> getModuleInterfaces mhash
          when exists $ rejectTransaction (ModuleHashAlreadyExists mhash)
          return ((iface, mod), mhash)

    k ls (iface, mhash) = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      -- Add the module to the global state (module interface, value interface and module itself).
      -- We know the module does not exist at this point, so we can ignore the return value.
      _ <- commitModule iface
      return (TxSuccess [ModuleDeployed mhash], energyCost, usedEnergy)

-- | Tick energy for storing the given 'Value'.
-- Calculates the size of the value and rejects with 'OutOfEnergy' when reaching a size
-- that cannot be paid for.
tickEnergyValueStorage ::
  TransactionMonad m
  => Wasm.ContractState
  -> m ()
tickEnergyValueStorage cs =
  -- Compute the size of the value and charge for storing based on this size.
  -- This uses the 'ResourceMeasure' instance for 'ByteSize' to determine the cost for storage.
  withExternalPure_ $ (Wasm.contractStateSize @ Wasm.ByteSize) cs

-- | Tick energy for looking up a contract instance, then do the lookup.
-- FIXME: Currently we do not know the size of the instance before looking it up.
-- Therefore we charge a "pre-lookup cost" before the lookup and the actual cost after.
getCurrentContractInstanceTicking ::
  TransactionMonad m
  => ContractAddress
  -> m Instance
getCurrentContractInstanceTicking cref = do
  tickEnergy Cost.lookupBytesPre
  inst <- getCurrentContractInstance cref `rejectingWith` (InvalidContractAddress cref)
  -- Compute the size of the contract state value and charge for the lookup based on this size.
  -- This uses the 'ResourceMeasure' instance for 'Cost.LookupByteSize' to determine the cost for lookup.
  withExternalPure_ $ Wasm.contractStateSize @ Cost.LookupByteSize (Ins.instanceModel inst)
  return inst

-- | Handle the initialization of a contract instance.
handleInitContract ::
  SchedulerMonad m
    => WithDepositContext m
    -> Amount   -- ^The amount to initialize the contract instance with.
    -> ModuleRef  -- ^The module to initialize a contract from.
    -> Wasm.InitName -- ^Name of the init method to invoke.
    -> Wasm.Parameter  -- ^Parameter expression to initialize with.
    -> m (Maybe TransactionSummary)
handleInitContract wtc initAmount modref initName param =
  withDeposit wtc c k
    where senderAccount = wtc ^. wtcSenderAccount
          txHash = wtc ^. wtcTransactionHash
          meta = wtc ^. wtcTransactionHeader
          c = do
            -- Check whether the sender account's amount can cover the amount to initialize the contract
            -- with. Note that the deposit is already deducted at this point.
            senderAmount <- getCurrentAccountAvailableAmount senderAccount

            unless (senderAmount >= initAmount) $! rejectTransaction (AmountTooLarge (AddressAccount (thSender meta)) initAmount)

            -- First try to get the module interface of the parent module of the contract.
            -- TODO We currently first charge the full amount after the lookup, as we do not have the
            -- size available before.
            tickEnergy Cost.lookupBytesPre
            iface <- liftLocal (getModuleInterfaces modref) `rejectingWith` InvalidModuleReference modref
            let iSize = Wasm.miModuleSize $ iface
            tickEnergy $ Cost.lookupModule iSize

            -- Then get the particular contract interface (in particular the type of the init method).
            unless (Set.member initName (Wasm.miExposedInit iface)) $ rejectTransaction $ InvalidInitMethod modref initName

            cm <- liftLocal getChainMetadata
            -- Finally run the initialization function of the contract, resulting in an initial state
            -- of the contract. This ticks energy during execution, failing when running out of energy.
            -- NB: At this point the amount to initialize with has not yet been deducted from the
            -- sender account. Thus if the initialization function were to observe the current balance it would
            -- be amount - deposit. Currently this is in any case not exposed in contracts, but in case it
            -- is in the future we should be mindful of which balance is exposed.
            senderCredentials <- getAccountCredentials senderAccount
            let initCtx = Wasm.InitContext{
                  initOrigin = thSender meta,
                  icSenderPolicies = map Wasm.mkSenderPolicy senderCredentials
               }
            result <- runInterpreter (return . Wasm.applyInitFun iface cm initCtx initName param initAmount)
                       `rejectingWith'` wasmRejectToRejectReason

            -- Charge for storing the contract state.
            tickEnergyValueStorage (Wasm.newState result)

            return (iface, result)

          k ls (iface, result) = do
            let model = Wasm.newState result
            (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
            chargeExecutionCost txHash senderAccount energyCost

            -- Withdraw the amount the contract is initialized with from the sender account.
            cs' <- addAmountToCS senderAccount (amountDiff 0 initAmount) (ls ^. changeSet)

            let receiveMethods = OrdMap.findWithDefault Set.empty initName (Wasm.miExposedReceive iface)
            let ins = makeInstance modref initName receiveMethods iface model initAmount (thSender meta)
            addr <- putNewInstance ins

            -- add the contract initialization to the change set and commit the changes
            commitChanges $ addContractInitToCS (ins addr) cs'

            return (TxSuccess [ContractInitialized{ecRef=modref,
                                                   ecAddress=addr,
                                                   ecAmount=initAmount,
                                                   ecInitName=initName,
                                                   ecEvents=Wasm.logs result
                                                   }], energyCost, usedEnergy
                                                   )

handleSimpleTransfer ::
  SchedulerMonad m
    => WithDepositContext m
    -> AccountAddress -- ^Address to send the amount to, either account or contract.
    -> Amount -- ^The amount to transfer.
    -> m (Maybe TransactionSummary)
handleSimpleTransfer wtc toaddr amount =
  withDeposit wtc c (defaultSuccess wtc)
    where senderAccount = wtc ^. wtcSenderAccount
          c = handleTransferAccount senderAccount toaddr (Right senderAccount) amount

-- | Handle a top-level update transaction to a contract.
handleUpdateContract ::
  SchedulerMonad m
    => WithDepositContext m
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Wasm.ReceiveName -- ^Name of the receive method to invoke.
    -> Wasm.Parameter -- ^Message to send to the receive method.
    -> m (Maybe TransactionSummary)
handleUpdateContract wtc uAmount uAddress uReceiveName uMessage =
  withDeposit wtc c (defaultSuccess wtc)
  where senderAccount = wtc ^. wtcSenderAccount
        c = do
          ins <- getCurrentContractInstanceTicking uAddress
          -- Now invoke the general handler for contract messages.
          handleMessage senderAccount
                        ins
                        (Right senderAccount)
                        uAmount
                        uReceiveName
                        uMessage


-- | Process a message to a contract.
-- This includes the transfer of an amount from the sending account or instance.
-- Recursively do the same for new messages created by contracts (from left to right, depth first).
-- The target contract must exist, so that its state can be looked up.
handleMessage ::
  (TransactionMonad m, AccountOperations m)
  => Account m -- ^The account that sent the top-level transaction.
  -> Instance -- ^The current state of the target contract of the transaction, which must exist.
  -> Either (Account m, Instance) (Account m)
  -- ^The sender of the message (contract instance or account). In case this is
  -- a contract the first parameter the owner account of the instance.
  -- On the first invocation of this function this will be the sender of the
  -- top-level transaction, and in recursive calls the respective contract
  -- instance that produced the message.
  -> Amount -- ^The amount to be transferred from the sender of the message to the receiver.
  -> Wasm.ReceiveName -- ^Name of the contract to invoke.
  -> Wasm.Parameter -- ^Message to invoke the receive method with.
  -> m [Event] -- ^The events resulting from processing the message and all recursively processed messages.
handleMessage origin istance sender transferAmount receiveName parameter = do
  let model = instanceModel istance
  -- Check whether the sender of the message has enough on its account/instance for the transfer.
  -- If the amount is not sufficient, the top-level transaction is rejected.
  -- TODO: For now there is no exception handling in smart contracts and contracts cannot check
  -- amounts on other instances or accounts. Possibly this will need to be changed.
  (senderAddr, senderCredentials) <- mkSenderAddrCredentials sender
  senderamount <- getCurrentAvailableAmount sender
  unless (senderamount >= transferAmount) $ rejectTransaction (AmountTooLarge senderAddr transferAmount)
  originAddr <- getAccountAddress origin

  let iParams = instanceParameters istance
  let cref = instanceAddress iParams
  let receivefuns = instanceReceiveFuns . instanceParameters $ istance
  unless (Set.member receiveName receivefuns) $ rejectTransaction $
      InvalidReceiveMethod (Wasm.miModuleRef . instanceModuleInterface $ iParams) receiveName
  -- Now we also check that the owner account of the receiver instance has at least one valid credential
  -- and reject the transaction if not.
  let ownerAccountAddress = instanceOwner iParams
  -- The invariants maintained by global state should ensure that an owner account always exists.
  -- However we are defensive here and reject the transaction, acting as if there is no credential.
  ownerAccount <- getStateAccount ownerAccountAddress `rejectingWith` ReceiverContractNoCredential cref
  cm <- getChainMetadata
  validCredExists <- existsValidCredential cm ownerAccount
  unless validCredExists $ rejectTransaction (ReceiverContractNoCredential cref)

  -- We have established that the owner account of the receiver instance has at least one valid credential.
  let receiveCtx = Wasm.ReceiveContext {
        invoker = originAddr,
        selfAddress = cref,
        selfBalance = instanceAmount istance,
        sender = senderAddr,
        owner = instanceOwner iParams,
        rcSenderPolicies = map Wasm.mkSenderPolicy senderCredentials
        }
  -- Now run the receive function on the message. This ticks energy during execution, failing when running out of energy.
  -- FIXME: Once errors can be caught in smart contracts update this to not terminate the transaction.
  let iface = instanceModuleInterface iParams
  result <- runInterpreter (return . Wasm.applyReceiveFun iface cm receiveCtx receiveName parameter transferAmount model)
             `rejectingWith'` wasmRejectToRejectReason
  -- If we reach here the contract accepted the message and returned a new state as well as outgoing messages.
  let newModel = Wasm.newState result
      txOut = Wasm.messages result
      -- Charge for eventually storing the new contract state (even if it might not be stored
      -- in the end because the transaction fails).
      -- TODO We might want to change this behaviour to prevent charging for storage that is not done.
  tickEnergyValueStorage newModel
  -- Process the generated messages in the new context (transferred amount, updated state) in
  -- sequence from left to right, depth first.
  withToContractAmount sender istance transferAmount $
    withInstanceState istance newModel $ do
      let initEvent = Updated{euAddress=cref,
                              euInstigator=senderAddr,
                              euAmount=transferAmount,
                              euMessage=parameter,
                              euReceiveName=receiveName,
                              euEvents = Wasm.logs result
                               }
      foldEvents origin (ownerAccount, istance) initEvent txOut

foldEvents :: (TransactionMonad m, AccountOperations m)
           =>  Account m -- ^Account that originated the top-level transaction
           -> (Account m, Instance) -- ^Instance that generated the events.
           -> Event -- ^Event generated by the invocation of the instance.
           -> Wasm.ActionsTree -- ^Actions to perform
           -> m [Event] -- ^List of events in order that transactions were traversed.
foldEvents origin istance initEvent = fmap (initEvent:) . go
  where go Wasm.TSend{..} = do
          cinstance <- getCurrentContractInstanceTicking erAddr
          handleMessage origin
                              cinstance
                              (Left istance)
                              erAmount
                              erName
                              erParameter
        go Wasm.TSimpleTransfer{..} = do
          handleTransferAccount origin erTo (Left istance) erAmount
        go (Wasm.And l r) = do
          resL <- go l
          resR <- go r
          return (resL ++ resR)
        -- FIXME: This will not retain logs from the left run if it fails.
        -- We might want to include the information on why the right-side
        -- was run in the output event list.
        go (Wasm.Or l r) = go l `orElse` go r
        go Wasm.Accept = return []

mkSenderAddrCredentials :: AccountOperations m => Either (Account m, Instance) (Account m) -> m (Address, [ID.AccountCredential])
mkSenderAddrCredentials sender =
    case sender of
      Left (ownerAccount, istance) -> do
        credentials <- getAccountCredentials ownerAccount
        return (AddressContract (instanceAddress (instanceParameters istance)), credentials)
      Right acc -> do
        addr <- AddressAccount <$> getAccountAddress acc
        credentials <- getAccountCredentials acc
        return (addr, credentials)


-- | Handle the transfer of an amount from an account or contract instance to an account.
-- TODO: Figure out whether we need the origin information in here (i.e.,
-- whether an account can observe it).
handleTransferAccount ::
  (TransactionMonad m, AccountOperations m)
  => Account m -- ^The account that sent the top-level transaction.
  -> AccountAddress -- The target account address.
  -> Either (Account m, Instance) (Account m) -- The sender of this transfer (contract instance or account).
  -> Amount -- The amount to transfer.
  -> m [Event] -- The events resulting from the transfer.
handleTransferAccount _origin accAddr sender transferamount = do
  tickEnergy Cost.transferAccount
  -- Check whether the sender has the amount to be transferred and reject the transaction if not.
  senderamount <- getCurrentAvailableAmount sender
  (addr, _) <- mkSenderAddrCredentials sender
  unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge addr transferamount)

  -- Check whether target account exists and get it.
  targetAccount <- getStateAccount accAddr `rejectingWith` InvalidAccountReference accAddr
  -- Check that the account has a valid credential and reject the transaction if not
  -- (it is not allowed to send to accounts without valid credential).
  cm <- getChainMetadata
  validCredExists <- existsValidCredential cm targetAccount
  unless validCredExists $ rejectTransaction (ReceiverAccountNoCredential accAddr)

  -- Add the transfer to the current changeset and return the corresponding event.
  withToAccountAmount sender targetAccount transferamount $
      return [Transferred addr transferamount (AddressAccount accAddr)]

-- |Run the interpreter with the remaining amount of energy. If the interpreter
-- runs out of energy set the remaining gas to 0 and reject the transaction,
-- otherwise decrease the consumed amount of energy and return the result.
{-# INLINE runInterpreter #-}
runInterpreter :: TransactionMonad m => (Wasm.InterpreterEnergy -> m (Maybe (a, Wasm.InterpreterEnergy))) -> m a
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

-- FIXME: The baker handling is purely proof-of-concept. In particular the
-- precise logic for when a baker can be added and removed should be analyzed
-- from a security perspective.

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
  SchedulerMonad m
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
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = do
          tickEnergy Cost.addBaker
          -- Get the total amount on the account, including locked amounts,
          -- less the deposit.
          getCurrentAccountTotalAmount senderAccount
        k ls accountBalance = do
          senderAddress <- getAccountAddress senderAccount
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

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
            res <- addBaker senderAddress BI.BakerAdd {
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
          else return (TxReject InvalidProof, energyCost, usedEnergy)
          
-- |Remove the baker for an account. The logic is as follows:
--
--  * If the account is not a baker, the transaction fails ('NotABaker').
--  * If the account is the cool-down period for another baker change, the transaction fails ('BakerInCooldown').
--  * Otherwise, the baker is removed, which takes effect after the cool-down period.
handleRemoveBaker ::
  SchedulerMonad m
    => WithDepositContext m
    -> m (Maybe TransactionSummary)
handleRemoveBaker wtc =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.removeBaker
        k ls _ = do
          senderAddress <- getAccountAddress senderAccount
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

          res <- removeBaker senderAddress
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
  SchedulerMonad m
    => WithDepositContext m
    -> Amount
    -- ^new stake
    -> m (Maybe TransactionSummary)
handleUpdateBakerStake wtc newStake =
    withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    c = do
      tickEnergy Cost.updateBakerStake
      -- Get the total amount on the account, including locked amounts,
      -- less the deposit.
      getCurrentAccountTotalAmount senderAccount
    k ls accountBalance = do
      senderAddress <- getAccountAddress senderAccount
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      if accountBalance < newStake then
        -- The balance is insufficient.
        return (TxReject InsufficientBalanceForBakerStake, energyCost, usedEnergy)
      else do
        res <- updateBakerStake senderAddress newStake
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

handleUpdateBakerRestakeEarnings ::
  SchedulerMonad m
    => WithDepositContext m
    -> Bool
    -- ^Whether to restake earnings
    -> m (Maybe TransactionSummary)
handleUpdateBakerRestakeEarnings wtc newRestakeEarnings = withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    c = tickEnergy Cost.updateBakerRestakeEarnings
    k ls _ = do
      senderAddress <- getAccountAddress senderAccount
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost

      res <- updateBakerRestakeEarnings senderAddress newRestakeEarnings
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
  SchedulerMonad m
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
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.updateBakerKeys
        k ls _ = do
          senderAddress <- getAccountAddress senderAccount
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

          let challenge = updateBakerKeyChallenge senderAddress bkuElectionKey bkuSignKey bkuAggregationKey
              electionP = checkElectionKeyProof challenge bkuElectionKey bkuProofElection
              signP = checkSignatureVerifyKeyProof challenge bkuSignKey bkuProofSig
              aggregationP = Bls.checkProofOfKnowledgeSK challenge bkuProofAggregation bkuAggregationKey

          if electionP && signP && aggregationP then do
            -- The proof validates that the baker owns all the private keys,
            -- thus we can try to create the baker.
            res <- updateBakerKeys senderAddress BI.BakerKeyUpdate{..}
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

-- *Transactions without a sender
handleDeployCredential ::
  SchedulerMonad m =>
  -- |Credentials to deploy.
  ID.AccountCredentialWithProofs ->
  TransactionHash ->
  m (Maybe TxResult)
handleDeployCredential cdi cdiHash = do
    remainingEnergy <- getRemainingEnergy
    let cost = Cost.deployCredential (ID.credentialType cdi)
    if remainingEnergy < cost then return Nothing
    else do
      let mkSummary tsResult = do
            tsIndex <- bumpTransactionIndex
            return $ Just . TxValid $ TransactionSummary{
              tsSender = Nothing,
              tsHash = cdiHash,
              tsCost = 0,
              tsEnergyCost = cost,
              tsType = TSTCredentialDeploymentTransaction (ID.credentialType cdi),
              ..
              }

      cm <- getChainMetadata
      let expiry = ID.validTo cdi

      -- check that a registration id does not yet exist
      let regId = ID.regId cdi
      regIdEx <- accountRegIdExists regId
      if not (isTimestampBefore (slotTime cm) expiry) then
        return $ Just (TxInvalid AccountCredentialInvalid)
      else if regIdEx then
        return $ Just (TxInvalid (DuplicateAccountRegistrationID (ID.regId cdi)))
      else do
        -- We now look up the identity provider this credential is derived from.
        -- Of course if it does not exist we reject the transaction.
        let credentialIP = ID.ipId cdi
        getIPInfo credentialIP >>= \case
          Nothing -> return $ Just (TxInvalid (NonExistentIdentityProvider (ID.ipId cdi)))
          Just ipInfo -> do
            case cdi of
              ID.InitialACWP icdi -> do
                   let aaddr = ID.addressFromRegId regId
                   accExistsAlready <- isJust <$> getAccount aaddr
                   if accExistsAlready then
                     return $ Just (TxInvalid AccountCredentialInvalid)
                   else do
                     if AH.verifyInitialAccountCreation ipInfo (S.encode icdi) then do
                       -- Create the account with the credential, but don't yet add it to the state
                       cryptoParams <- getCryptoParams
                       let initialAccountInfo = ID.icdvAccount (ID.icdiValues icdi)
                       let accountKeys = ID.makeAccountKeys (ID.icaKeys initialAccountInfo) (ID.icaThreshold initialAccountInfo)
                       -- Creation is guaranteed to succeed since an account with the address
                       -- does not already exist.
                       _ <- createAccount cryptoParams accountKeys aaddr (ID.InitialAC (ID.icdiValues icdi))
                       mkSummary (TxSuccess [AccountCreated aaddr, CredentialDeployed{ecdRegId=regId,ecdAccount=aaddr}])
                     else return $ Just (TxInvalid AccountCredentialInvalid)


              ID.NormalACWP ncdi -> do
                let cdiBytes = S.encode ncdi
                let ncdv = ID.cdiValues ncdi
                getArInfos (OrdMap.keys (ID.cdvArData ncdv)) >>= \case
                  Nothing -> return $ Just (TxInvalid UnsupportedAnonymityRevokers)
                  Just arsInfos -> do
                    cryptoParams <- getCryptoParams
                    -- we have two options. One is that we are deploying a credential on an existing account.
                    case ID.cdvAccount ncdv of
                      ID.NewAccount keys threshold ->
                        -- account does not yet exist, so create it, but we need to be careful
                        if null keys || length keys > 255 then
                          return $ Just (TxInvalid AccountCredentialInvalid)
                        else do
                          let accountKeys = ID.makeAccountKeys keys threshold
                          let aaddr = ID.addressFromRegId regId
                          case ID.values cdi of
                            Nothing -> return $ Just (TxInvalid AccountCredentialInvalid)
                            Just cdv -> do
                              -- this check is extremely unlikely to fail (it would amount to a hash collision since
                              -- we checked regIdEx above already).
                              accExistsAlready <- isJust <$> getAccount aaddr
                              let check = AH.verifyCredential cryptoParams ipInfo arsInfos Nothing cdiBytes
                              if not accExistsAlready && check then do
                                -- Add the account to the state, but only if the credential was valid and the account does not exist
                                _ <- createAccount cryptoParams accountKeys aaddr cdv

                                mkSummary (TxSuccess [AccountCreated aaddr, CredentialDeployed{ecdRegId=regId,ecdAccount=aaddr}])
                              else return $ Just (TxInvalid AccountCredentialInvalid)

-- |Updates the account keys. For each (index, key) pair, updates the key at
-- the specified index to the specified key. Is valid when the given indices all point
-- to an existing key.
handleUpdateAccountKeys ::
  SchedulerMonad m
    => WithDepositContext m
    -> OrdMap.Map ID.KeyIndex AccountVerificationKey
    -> m (Maybe TransactionSummary)
handleUpdateAccountKeys wtc keys =
  withDeposit wtc cost k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    cost = tickEnergy $ Cost.updateAccountKeys $ length keys
    k ls _ = do
      accountKeys <- getAccountVerificationKeys senderAccount
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      if Set.isSubsetOf (OrdMap.keysSet keys) (ID.getKeyIndices accountKeys) then do
        senderAddr <- getAccountAddress senderAccount
        updateAccountKeys senderAddr keys
        return (TxSuccess [AccountKeysUpdated], energyCost, usedEnergy)
      else
        return (TxReject NonExistentAccountKey, energyCost, usedEnergy)


-- |Removes the account keys at the supplied indices and, optionally, updates
-- the signature threshold. Is valid when the indices supplied are already in use
-- and the new threshold does not exceed the new total amount of keys.
handleRemoveAccountKeys ::
  SchedulerMonad m
    => WithDepositContext m
    -> Set.Set ID.KeyIndex
    -> Maybe ID.SignatureThreshold
    -> m (Maybe TransactionSummary)
handleRemoveAccountKeys wtc indices threshold =
  withDeposit wtc cost k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    cost = tickEnergy $ Cost.removeAccountKeys $ length indices
    k ls _ = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      accountKeys <- getAccountVerificationKeys senderAccount
      let numOfKeys = length (ID.akKeys accountKeys)
      let currentThreshold = ID.akThreshold accountKeys
      if Set.isSubsetOf indices (ID.getKeyIndices accountKeys) then
        case threshold of
          Just newThreshold ->
            -- the subtraction is safe here because we have checked that indices is a subset of keys.
            if newThreshold <= (fromIntegral (numOfKeys - (length indices))) then do
              senderAddr <- getAccountAddress senderAccount
              removeAccountKeys senderAddr indices threshold
              return (TxSuccess [AccountKeysRemoved, AccountKeysSignThresholdUpdated], energyCost, usedEnergy)
            else
              return (TxReject InvalidAccountKeySignThreshold, energyCost, usedEnergy)
          Nothing -> do
            if currentThreshold <= (fromIntegral (numOfKeys - (length indices))) then do
              senderAddr <- getAccountAddress senderAccount
              removeAccountKeys senderAddr indices Nothing
              return (TxSuccess [AccountKeysRemoved], energyCost, usedEnergy)
            else
              return (TxReject InvalidAccountKeySignThreshold, energyCost, usedEnergy)
      else return (TxReject NonExistentAccountKey, energyCost, usedEnergy)


-- |Adds keys to the account at the supplied indices and, optionally, updates
-- the signature threshold. Is valid when the indices supplied are not already in use
-- and the new threshold doesn’t exceed the new total amount of keys.
handleAddAccountKeys ::
  SchedulerMonad m
    => WithDepositContext m
    -> OrdMap.Map ID.KeyIndex AccountVerificationKey
    -> Maybe ID.SignatureThreshold
    -> m (Maybe TransactionSummary)
handleAddAccountKeys wtc keys threshold =
  withDeposit wtc cost k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    cost = tickEnergy $ Cost.addAccountKeys $ length keys
    k ls _ = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      -- all the keys must be new.
      accountKeys <- getAccountVerificationKeys senderAccount
      let numOfKeys = length (ID.akKeys accountKeys)
      if Set.disjoint (OrdMap.keysSet keys) (ID.getKeyIndices accountKeys) then
        case threshold of
          Just newThreshold ->
            if newThreshold <= fromIntegral (numOfKeys + (length keys)) then do
              senderAddr <- getAccountAddress senderAccount
              addAccountKeys senderAddr keys threshold
              return (TxSuccess [AccountKeysAdded, AccountKeysSignThresholdUpdated], energyCost, usedEnergy)
            else
              return (TxReject InvalidAccountKeySignThreshold, energyCost, usedEnergy)
          Nothing -> do
            senderAddr <- getAccountAddress senderAccount
            addAccountKeys senderAddr keys Nothing
            return (TxSuccess [AccountKeysAdded], energyCost, usedEnergy)
      else
        return (TxReject KeyIndexAlreadyInUse, energyCost, usedEnergy)

-- * Chain updates

-- |Handle a chain update message
handleChainUpdate ::
  SchedulerMonad m
  => WithMetadata UpdateInstruction
  -> m TxResult
handleChainUpdate WithMetadata{wmdData = ui@UpdateInstruction{..}, ..} = do
  -- Check that the timeout is not in the past
  cm <- getChainMetadata
  if transactionExpired (updateTimeout uiHeader) (slotTime cm) then
    return (TxInvalid ExpiredTransaction)
  else do
    -- Check that the timeout is no later than the effective time,
    -- or the update is immediate
    if updateTimeout uiHeader >= updateEffectiveTime uiHeader && updateEffectiveTime uiHeader /= 0 then
      return (TxInvalid InvalidUpdateTime)
    else do
      -- Check that the sequence number is correct
      expectedSequenceNumber <- getNextUpdateSequenceNumber (updateType uiPayload)
      if updateSeqNumber uiHeader /= expectedSequenceNumber then
        return (TxInvalid (NonSequentialNonce expectedSequenceNumber))
      else do
        -- Convert the payload to an update
        case uiPayload of
          AuthorizationUpdatePayload u -> checkSigAndUpdate $ UVAuthorization u
          ProtocolUpdatePayload u -> checkSigAndUpdate $ UVProtocol u
          ElectionDifficultyUpdatePayload u -> checkSigAndUpdate $ UVElectionDifficulty u
          EuroPerEnergyUpdatePayload u -> checkSigAndUpdate $ UVEuroPerEnergy u
          MicroGTUPerEuroUpdatePayload u -> checkSigAndUpdate $ UVMicroGTUPerEuro u
          FoundationAccountUpdatePayload u -> getAccountIndex u >>= \case
            Just ai -> checkSigAndUpdate $ UVFoundationAccount ai
            Nothing -> return (TxInvalid (UnknownAccount u))
          MintDistributionUpdatePayload u -> checkSigAndUpdate $ UVMintDistribution u
          TransactionFeeDistributionUpdatePayload u -> checkSigAndUpdate $ UVTransactionFeeDistribution u
          GASRewardsUpdatePayload u -> checkSigAndUpdate $ UVGASRewards u
  where
    checkSigAndUpdate change = do
      -- Check that the signatures use the appropriate keys and are valid.
      auths <- getUpdateAuthorizations
      if checkAuthorizedUpdate auths ui then do
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
      else
        return (TxInvalid IncorrectSignature)



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
--
-- TODO: We might need to add a real-time timeout at which point we stop processing transactions.
filterTransactions :: forall m . (SchedulerMonad m)
                   => Integer -- ^Maximum block size in bytes.
                   -> [TransactionGroup] -- ^Transactions to make a block out of.
                   -> m FilteredTransactions
filterTransactions maxSize groups0 = do
  maxEnergy <- getMaxBlockEnergy
  credLimit <- getAccountCreationLimit
  ftTrans <- runNext maxEnergy 0 credLimit emptyFilteredTransactions groups0
  forM_ (ftFailed ftTrans) $ uncurry logInvalidTransaction
  forM_ (ftFailedCredentials ftTrans) $ uncurry logInvalidCredential
  forM_ (ftFailedUpdates ftTrans) $ uncurry logInvalidChainUpdate
  return ftTrans
  where
        -- Run next credential deployment or transaction group, depending on arrival time.
        runNext :: Energy -- ^Maximum block energy
                -> Integer -- ^Current size of transactions in the block.
                -> CredentialsPerBlockLimit -- ^Number of credentials until limit.
                -> FilteredTransactions -- ^Currently accumulated result
                -> [TransactionGroup] -- ^Grouped transactions to process
                -> m FilteredTransactions
        -- All block items are processed. We accumulate the added items
        -- in reverse order, so reverse the list before returning.
        runNext _ _ _ fts [] = return fts{ftAdded = reverse (ftAdded fts)}
        runNext maxEnergy size credLimit fts (g : groups) = case g of
          TGAccountTransactions group -> runTransactionGroup size fts group
          TGCredentialDeployment c -> runCredential c
          TGUpdateInstructions group -> runUpdateInstructions size fts group
          where
            -- Run a group of update instructions of one type
            runUpdateInstructions currentSize currentFts [] = runNext maxEnergy currentSize credLimit currentFts groups
            runUpdateInstructions currentSize currentFts (ui : uis) = do
              -- Update instructions use no energy, so we only consider size
              let csize = currentSize + fromIntegral (wmdSize ui)
              if csize <= maxSize then
                -- Chain updates have no account footprint
                handleChainUpdate ui >>= \case
                  TxInvalid reason -> case uis of
                    (nui : _) | ((==) `on` (updateSeqNumber . uiHeader . wmdData)) ui nui ->
                      -- If there is another update with the same sequence number, we want to try that
                      runUpdateInstructions currentSize currentFts{ftFailedUpdates = (ui, reason) : ftFailedUpdates currentFts} uis
                    _ -> do
                      -- Otherwise, any subsequent updates are also treated as failures
                      let newFts = currentFts{
                              ftFailedUpdates = (ui, reason) : ((, SuccessorOfInvalidTransaction) <$> uis)
                                                ++ ftFailedUpdates currentFts
                            }
                      runNext maxEnergy currentSize credLimit newFts groups
                  TxValid summary -> do
                    let (invalid, rest) = span (((==) `on` (updateSeqNumber . uiHeader . wmdData)) ui) uis
                        curSN = updateSeqNumber $ uiHeader $ wmdData ui
                        newFts = currentFts{
                            ftFailedUpdates = ((, NonSequentialNonce (curSN + 1)) <$> invalid) ++ ftFailedUpdates currentFts,
                            ftAdded = (chainUpdate ui, summary) : ftAdded currentFts
                          }
                    runUpdateInstructions csize newFts rest
              else -- The cumulative block size with this update is too high.
                case uis of
                  (nui : _) | ((==) `on` (updateSeqNumber . uiHeader . wmdData)) ui nui ->
                    -- There is another update with the same sequence number, so try that
                    let newFts = currentFts{ftUnprocessedUpdates = ui : ftUnprocessedUpdates currentFts}
                    in runUpdateInstructions currentSize newFts uis
                  _ ->
                    -- Otherwise, there's no chance of processing remaining updates
                    let newFts = currentFts{ftUnprocessedUpdates = ui : uis ++ ftUnprocessedUpdates currentFts}
                    in runNext maxEnergy currentSize credLimit newFts groups

            -- Run a single credential and continue with 'runNext'.
            runCredential c@WithMetadata{..} = do
              totalEnergyUsed <- getUsedEnergy
              let csize = size + fromIntegral wmdSize
                  energyCost = Cost.deployCredential (ID.credentialType wmdData)
                  cenergy = totalEnergyUsed + fromIntegral energyCost
              if 0 <= credLimit && csize <= maxSize && cenergy <= maxEnergy then
                observeTransactionFootprint (handleDeployCredential wmdData wmdHash) >>= \case
                    (Just (TxInvalid reason), _) -> do
                      let newFts = fts { ftFailedCredentials = (c, reason) : ftFailedCredentials fts}
                      runNext maxEnergy size (credLimit - 1) newFts groups -- NB: We keep the old size
                    (Just (TxValid summary), fp) -> do
                      markEnergyUsed (tsEnergyCost summary)
                      tlNotifyAccountEffect fp summary
                      let newFts = fts { ftAdded = (credentialDeployment c, summary) : ftAdded fts}
                      runNext maxEnergy csize (credLimit - 1) newFts groups
                    (Nothing, _) -> error "Unreachable due to cenergy <= maxEnergy check."
              else if Cost.deployCredential (ID.credentialType wmdData) > maxEnergy then
                -- this case should not happen (it would mean we set the parameters of the chain wrong),
                -- but we keep it just in case.
                 let newFts = fts { ftFailedCredentials = (c, ExceedsMaxBlockEnergy) : ftFailedCredentials fts}
                 in runNext maxEnergy size credLimit newFts groups
              else
                 let newFts = fts { ftUnprocessedCredentials = c : ftUnprocessedCredentials fts}
                 in runNext maxEnergy size credLimit newFts groups

            -- Run all transactions in a group and continue with 'runNext'.
            runTransactionGroup :: Integer -- ^Current size of transactions in the block.
                                -> FilteredTransactions
                                -> [Transaction] -- ^Current group to process.
                                -> m FilteredTransactions
            runTransactionGroup currentSize currentFts (t:ts) = do
              totalEnergyUsed <- getUsedEnergy
              let csize = currentSize + fromIntegral (transactionSize t)
                  tenergy = transactionGasAmount t
                  cenergy = totalEnergyUsed + tenergy
              if csize <= maxSize && cenergy <= maxEnergy then
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
                  (nt : _) | transactionNonce nt == transactionNonce t ->
                    let newFts = currentFts { ftUnprocessed = t : ftUnprocessed currentFts }
                    in runTransactionGroup currentSize newFts ts
                  _ ->
                    let newFts = currentFts { ftUnprocessed = t : ts ++ ftUnprocessed currentFts }
                    in runNext maxEnergy currentSize credLimit newFts groups

            -- Group processed, continue with the next group or credential
            runTransactionGroup currentSize currentFts [] =
              runNext maxEnergy currentSize credLimit currentFts groups

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
              let (invalid, rest) = span ((== transactionNonce t) . transactionNonce) ts
              let nextNonce = transactionNonce t + 1
              let newFts =
                    currentFts { ftFailed = map (, NonSequentialNonce nextNonce) invalid
                                            ++ ftFailed currentFts
                               , ftAdded = (normalTransaction t, summary) : ftAdded currentFts
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
                if not (null ts) && transactionNonce (head ts) > transactionNonce t
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
runTransactions :: forall m . (SchedulerMonad m)
                => [BlockItem]
                -> m (Either (Maybe FailureKind) [(BlockItem, TransactionSummary)])
runTransactions = go []
    where go valid (bi:ts) =
            observeTransactionFootprint (predispatch bi) >>= \case
              (Just (TxValid summary), fp) -> do
                markEnergyUsed (tsEnergyCost summary)
                tlNotifyAccountEffect fp summary
                go ((bi, summary):valid) ts
              (Just (TxInvalid reason), _) -> do
                logInvalidBlockItem bi reason
                return (Left (Just reason))
              (Nothing, _) -> return (Left Nothing)

          go valid [] = return (Right (reverse valid))

          predispatch :: BlockItem -> m (Maybe TxResult)
          predispatch WithMetadata{wmdData=NormalTransaction tr,..} = dispatch WithMetadata{wmdData=tr,..}
          predispatch WithMetadata{wmdData=CredentialDeployment cred,..} = handleDeployCredential cred wmdHash
          predispatch WithMetadata{wmdData=ChainUpdate cu,..} = Just <$> handleChainUpdate WithMetadata{wmdData=cu,..}

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
execTransactions :: forall m . (SchedulerMonad m)
                 => [BlockItem]
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
              logInvalidBlockItem bi reason
              return (Left (Just reason))
        go [] = return (Right ())

        predispatch :: BlockItem -> m (Maybe TxResult)
        predispatch WithMetadata{wmdData=NormalTransaction tr,..} = dispatch WithMetadata{wmdData=tr,..}
        predispatch WithMetadata{wmdData=CredentialDeployment cred,..} = handleDeployCredential cred wmdHash
        predispatch WithMetadata{wmdData=ChainUpdate cu,..} = Just <$> handleChainUpdate WithMetadata{wmdData=cu,..}
