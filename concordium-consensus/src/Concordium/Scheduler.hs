{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

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

import qualified Acorn.TypeCheck as TC
import qualified Acorn.Interpreter as I
import qualified Acorn.Core as Core
import Acorn.Types(compile, InterpreterEnergy)
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment

import Data.Word
import qualified Data.Serialize as S
import qualified Data.ByteString as BS

import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Bakers(bakerAccount)
import qualified Concordium.GlobalState.Bakers as Bakers
import qualified Concordium.GlobalState.Instance as Ins
import qualified Concordium.Scheduler.Cost as Cost

import Control.Applicative
import Control.Monad.Except
import Control.Exception
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust, isJust)
import Data.Ord
import Data.List hiding (group)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet
import qualified Data.PQueue.Prio.Max as Queue

import qualified Concordium.Crypto.Proofs as Proofs
import qualified Concordium.Crypto.BlsSignature as Bls

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

-- |Check that there exists a valid credential in the context of the given chain
-- metadata.
existsValidCredential :: ChainMetadata -> Account -> Bool
existsValidCredential cm acc = do
  let credentials = acc ^. accountCredentials
  -- check that the sender has at least one still valid credential.
  case Queue.getMax credentials of
    Nothing -> False
    -- Note that the below relies on the invariant that the key is
    -- the same as the expiry date of the credential.
    -- If the credential is still valid at the beginning of this slot then
    -- we consider it valid. Otherwise we fail the transaction.
    Just (expiry, _) -> isTimestampBefore (slotTime cm) expiry

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
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> ExceptT (Maybe FailureKind) m (Account, Energy)
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
      let amnt = acc ^. accountAmount
      let nextNonce = acc ^. accountNonce
      let txnonce = transactionNonce meta
      let expiry = thExpiry $ transactionHeader meta

      cm <- lift getChainMetadata
      when (transactionExpired expiry $ slotTime cm) $ throwError . Just $ ExpiredTransaction
      unless (existsValidCredential cm acc) $ throwError . Just $ NoValidCredential

      -- After the successful credential check we check that the sender account
      -- has enough GTU to cover the deposited energy.
      depositedAmount <- lift (energyToGtu (transactionGasAmount meta))
      unless (depositedAmount <= amnt) (throwError . Just $ InsufficientFunds)

      unless (txnonce == nextNonce) (throwError . Just $ (NonSequentialNonce nextNonce))

      -- Finally do the signature verification, the computationally most expensive part.
      let sigCheck = verifyTransaction (acc ^. accountVerificationKeys) meta
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

      -- then we notify the block state that all the identity issuers on the sender account should be rewarded
      -- TODO: Check for existence of valid identity provider.
      -- TODO: Alternative design would be to only reward them if the transaction is successful/committed, or
      -- to add additional parameters (such as deposited amount)
      -- FIXME: Only consider non-expired credentials.
      mapM_ (notifyIdentityProviderCredential . ID.cdvIpId) (senderAccount ^. accountCredentials)

      let psize = payloadSize (transactionPayload msg)
      -- TODO: Check whether the cost for deserializing the transaction is sufficiently covered
      -- by the cost for checking the header (which is linear in the transaction size).

      tsIndex <- bumpTransactionIndex
      case decodePayload (transactionPayload msg) of
        Left _ -> do
          -- In case of serialization failure we charge the sender for checking
          -- the header and reject the transaction; we have checked that the amount
          -- exists on the account with 'checkHeader'.
          payment <- energyToGtu checkHeaderCost
          chargeExecutionCost (transactionHash msg) senderAccount payment
          return $! Just $! TxValid $! TransactionSummary{
            tsEnergyCost = checkHeaderCost,
            tsCost = payment,
            tsSender = Just (senderAccount ^. accountAddress),
            tsResult = TxReject SerializationFailure,
            tsHash = transactionHash msg,
            tsType = Nothing,
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

                   InitContract amount modref cname param ->
                     -- the payload size includes amount + address of module + name of
                     -- contract + parameters, but since the first three fields are
                     -- fixed size this is OK.
                     let paramSize = fromIntegral (thPayloadSize meta)
                     in handleInitContract (mkWTC TTInitContract) amount modref cname param paramSize
                   -- FIXME: This is only temporary for now.
                   -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
                   Transfer toaddr amount ->
                     handleSimpleTransfer (mkWTC TTTransfer) toaddr amount

                   Update amount cref maybeMsg ->
                     -- the payload size includes amount + address + message, but since the first two fields are
                     -- fixed size this is OK.
                     let msgSize = fromIntegral (thPayloadSize meta)
                     in handleUpdateContract (mkWTC TTUpdate) cref amount maybeMsg msgSize

                   AddBaker{..} ->
                     handleAddBaker (mkWTC TTAddBaker) abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount abProofSig abProofElection abProofAccount abProofAggregation

                   RemoveBaker{..} ->
                     handleRemoveBaker (mkWTC TTRemoveBaker) rbId

                   UpdateBakerAccount{..} ->
                     handleUpdateBakerAccount (mkWTC TTUpdateBakerAccount) ubaId ubaAddress ubaProof

                   UpdateBakerSignKey{..} ->
                     handleUpdateBakerSignKey (mkWTC TTUpdateBakerSignKey) ubsId ubsKey ubsProof

                   DelegateStake{..} ->
                     handleDelegateStake (mkWTC TTDelegateStake) (Just dsID)

                   UndelegateStake ->
                     handleDelegateStake (mkWTC TTUndelegateStake) Nothing

                   UpdateElectionDifficulty{..} ->
                     handleUpdateElectionDifficulty (mkWTC TTUpdateElectionDifficulty) uedDifficulty

                   UpdateBakerAggregationVerifyKey{..} ->
                     handleUpdateBakerAggregationVerifyKey (mkWTC TTUpdateBakerAggregationVerifyKey) ubavkId ubavkKey ubavkProof

                   UpdateBakerElectionKey{..} ->
                     handleUpdateBakerElectionKey (mkWTC TTUpdateBakerElectionKey) ubekId ubekKey ubekProof
          case res of
            -- The remaining block energy is not sufficient for the handler to execute the transaction.
            Nothing -> return Nothing
            Just summary -> return $! Just $! TxValid summary

-- | Handle the deployment of a module.
handleDeployModule ::
  SchedulerMonad m
  => WithDepositContext
  -> PayloadSize -- ^Serialized size of the module. Used for charging execution cost.
  -> Module -- ^The module to deploy.
  -> m (Maybe TransactionSummary)
handleDeployModule wtc psize mod =
  withDeposit wtc c k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader

    c = do
      tickEnergy (Cost.deployModule (fromIntegral psize))
      let mhash = Core.moduleHash mod
      imod <- pure (runExcept (Core.makeInternal mhash (fromIntegral psize) mod)) `rejectingWith'` (const MissingImports)
      -- Before typechecking, we charge for loading the dependencies of the to-be-typechecked module
      -- (as typechecking will load these if used). For now, we do so by loading the interface of
      -- each dependency one after the other and then charging based on its size.
      -- TODO We currently first charge the full amount after each lookup, as we do not have the
      -- module sizes available before.
      let imports = Map.elems $ Core.imImports imod
      forM_ imports $ \ref -> do
        tickEnergy $ Cost.lookupBytesPre
        -- As the given module is not typechecked yet, it might contain imports of
        -- non-existing modules.
        iface <- getInterface ref `rejectingWith` ModuleNotWF
        tickEnergy $ Cost.lookupModule (iSize iface)

      -- Typecheck the module, resulting in the module 'Interface'.
      -- The cost of type-checking is dependent on the size of the module.
      iface <- typeHidingErrors (TC.typeModule imod) `rejectingWith` ModuleNotWF
      -- Create the 'ValueInterface' of the module (compiles all terms).
      let viface = I.evalModule imod
      return (mhash, iface, viface)

    k ls (mhash, iface, viface) = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      -- Add the module to the global state (module interface, value interface and module itself).
      b <- commitModule mhash iface viface mod
      if b then
        return $! (TxSuccess [ModuleDeployed mhash], energyCost, usedEnergy)
          else
        -- FIXME: Check whether the module exists already at the beginning of this handler.
        -- Doing it before typechecking will often save effort in the failure case, e.g. in case
        -- typechecking results in module lookups anyway.
        -- With checking the transaction type even before fully deserializing the payload,
        -- this check can be done even earlier, since the module hash is the hash of module serialization.
        return $! (TxReject (ModuleHashAlreadyExists mhash), energyCost, usedEnergy)

-- | Tick energy for storing the given 'Value'.
-- Calculates the size of the value and rejects with 'OutOfEnergy' when reaching a size
-- that cannot be paid for.
tickEnergyValueStorage ::
  TransactionMonad m
  => Value
  -> m ()
tickEnergyValueStorage val =
  -- Compute the size of the value and charge for storing based on this size.
  -- This uses the 'ResourceMeasure' instance for 'ByteSize' to determine the cost for storage.
  withExternalPure_ $ storableSizeWithLimit @ ByteSize val

-- | Tick energy for looking up a contract instance, then do the lookup.
-- FIXME: Currently we do not know the size of the instance before looking it up.
-- Therefore we charge a "pre-lookup cost" before the lookup and the actual cost after.
getCurrentContractInstanceTicking ::
  TransactionMonad m
  => ContractAddress
  -> m Instance
getCurrentContractInstanceTicking cref = do
  tickEnergy $ Cost.lookupBytesPre
  inst <- getCurrentContractInstance cref `rejectingWith` (InvalidContractAddress cref)
  -- Compute the size of the contract state value and charge for the lookup based on this size.
  -- This uses the 'ResourceMeasure' instance for 'Cost.LookupByteSize' to determine the cost for lookup.
  withExternalPure_ $ storableSizeWithLimit @ Cost.LookupByteSize (Ins.instanceModel inst)
  return inst

-- | Handle the initialization of a contract instance.
handleInitContract ::
  SchedulerMonad m
    => WithDepositContext
    -> Amount   -- ^The amount to initialize the contract instance with.
    -> ModuleRef  -- ^The module to initialize a contract from.
    -> Core.TyName  -- ^Name of the contract to initialize from the given module.
    -> Core.Expr Core.UA Core.ModuleName  -- ^Parameter expression to initialize the contract with.
    -> Word64 -- ^Serialized size of the parameter expression. Used for computing typechecking cost.
    -> m (Maybe TransactionSummary)
handleInitContract wtc amount modref cname param paramSize =
  withDeposit wtc c k
    where senderAccount = wtc ^. wtcSenderAccount
          txHash = wtc ^. wtcTransactionHash
          meta = wtc ^. wtcTransactionHeader
          c = do
            -- Check whether the sender account's amount can cover the amount to initialize the contract
            -- with. Note that the deposit is already deducted at this point.
            senderAmount <- getCurrentAccountAmount senderAccount
            unless (senderAmount >= amount) $! rejectTransaction (AmountTooLarge (AddressAccount (thSender meta)) amount)

            -- First try to get the module interface of the parent module of the contract.
            -- TODO We currently first charge the full amount after the lookup, as we do not have the
            -- size available before.
            tickEnergy $ Cost.lookupBytesPre
            (iface, viface) <- getModuleInterfaces modref `rejectingWith` InvalidModuleReference modref
            tickEnergy $ Cost.lookupModule $ iSize iface

            -- Then get the particular contract interface (in particular the type of the init method).
            ciface <- pure (Map.lookup cname (exportedContracts iface)) `rejectingWith` InvalidContractReference modref cname
            -- Now typecheck the parameter expression (whether it has the parameter type specified
            -- in the contract). The cost of type-checking is dependent on the size of the term.
            -- TODO Here we currently do not account for possible dependent modules looked up
            -- when typechecking the term. We might want to tick energy on demand while typechecking.
            tickEnergy (Cost.initParamsTypecheck paramSize)
            qparamExp <- typeHidingErrors (TC.checkTyInCtx' iface param (paramTy ciface)) `rejectingWith` ParamsTypeError
            -- Link the contract, i.e., its init and receive functions as well as the constraint
            -- implementations. This ticks energy for the size of the linked expressions,
            -- failing if running out of energy in the process.
            -- NB: The unsafe Map.! is safe here because if the contract is part of the interface 'iface'
            -- it must also be part of the 'ValueInterface' returned by 'getModuleInterfaces'.
            linkedContract <- linkContract (uniqueName iface) cname (viContracts viface Map.! cname)
            let (initFun, _) = cvInitMethod linkedContract

            -- First compile the parameter expression, then link it, which ticks energy for the size of
            -- the linked expression, failing when running out of energy in the process.
            params' <- linkExpr (uniqueName iface) (compile qparamExp)

            cm <- getChainMetadata
            -- Finally run the initialization function of the contract, resulting in an initial state
            -- of the contract. This ticks energy during execution, failing when running out of energy.
            -- NB: At this point the amount to initialize with has not yet been deducted from the
            -- sender account. Thus if the initialization function were to observe the current balance it would
            -- be amount - deposit. Currently this is in any case not exposed in contracts, but in case it
            -- is in the future we should be mindful of which balance is exposed.
            model <- runInterpreter (I.applyInitFun cm (InitContext (thSender meta)) initFun params' amount)

            -- Charge for storing the contract state.
            tickEnergyValueStorage model

            return (linkedContract, iface, viface, (msgTy ciface), model, amount)

          k ls (linkedContract, iface, viface, msgty, model, initamount) = do
            (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
            chargeExecutionCost txHash senderAccount energyCost

            -- Withdraw the amount the contract is initialized with from the sender account.
            commitChanges (addAmountToCS senderAccount (amountDiff 0 initamount) (ls ^. changeSet))

            -- Finally create the new instance. This stores the linked functions in the
            -- global state (serialized).
            -- 'ins' is an instance whose address will be determined when we store it; this is what
            -- 'putNewInstance' does.
            let ins = makeInstance modref cname linkedContract msgty iface viface model initamount (thSender meta)
            addr <- putNewInstance ins
            return $! (TxSuccess [ContractInitialized{ecRef=modref,ecName=cname,ecAddress=addr,ecAmount=initamount}], energyCost, usedEnergy)

handleSimpleTransfer ::
  SchedulerMonad m
    => WithDepositContext
    -> Address -- ^Address to send the amount to, either account or contract.
    -> Amount -- ^The amount to transfer.
    -> m (Maybe TransactionSummary)
handleSimpleTransfer wtc toaddr amount =
  withDeposit wtc c (defaultSuccess wtc)
    where senderAccount = wtc ^. wtcSenderAccount
          c = case toaddr of
                AddressContract cref -> do
                  i <- getCurrentContractInstanceTicking cref
                  -- Send a Nothing message to the contract with the amount to be transferred.
                  let qmsgExpLinked = I.mkNothingE
                  handleMessage senderAccount
                                i
                                (Right senderAccount)
                                amount
                                (ExprMessage qmsgExpLinked)
                AddressAccount toAccAddr ->
                  handleTransferAccount senderAccount toAccAddr (Right senderAccount) amount

-- | Handle a top-level update transaction to a contract.
handleUpdateContract ::
  SchedulerMonad m
    => WithDepositContext
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> Core.Expr Core.UA Core.ModuleName -- ^Message to send to the receive method.
    -> Word64  -- ^Serialized size of the message.
    -> m (Maybe TransactionSummary)
handleUpdateContract wtc cref amount maybeMsg msgSize =
  withDeposit wtc c (defaultSuccess wtc)
  where senderAccount = wtc ^. wtcSenderAccount
        c = do
          i <- getCurrentContractInstanceTicking cref
          let msgType = Ins.imsgTy i
              (iface, _) = Ins.iModuleIface i
          -- TODO Here we currently do not account for possible dependent modules looked up
          -- when typechecking the term. We might want to tick energy on demand while typechecking.
          tickEnergy (Cost.updateMessageTypecheck msgSize)
          -- Type check the message expression, as coming from a top-level transaction it can be
          -- an arbitrary expression. The cost of type-checking is dependent on the size of the term.
          qmsgExp <- typeHidingErrors (TC.checkTyInCtx' iface maybeMsg msgType) `rejectingWith` MessageTypeError
          -- First compile the message expression, then link it, which ticks energy for the size of the
          -- linked expression, failing when running out of energy in the process.
          qmsgExpLinked <- linkExpr (uniqueName iface) (compile qmsgExp)
          -- Now invoke the general handler for contract messages.
          handleMessage senderAccount
                        i
                        (Right senderAccount)
                        amount
                        (ExprMessage (I.mkJustE qmsgExpLinked))

-- | Process a message to a contract.
-- This includes the transfer of an amount from the sending account or instance.
-- Recursively do the same for new messages created by contracts (from left to right, depth first).
-- The target contract must exist, so that its state can be looked up.
handleMessage ::
  (TransactionMonad m, InterpreterMonad NoAnnot m)
  => Account -- ^The account that sent the top-level transaction.
  -> Instance -- ^The current state of the target contract of the transaction, which must exist.
  -> Either Instance Account -- ^The sender of the message (contract instance or account).
                             -- On the first invocation of this function this will be the sender of the
                             -- top-level transaction, and in recursive calls the respective contract
                             -- instance that produced the message.
  -> Amount -- ^The amount to be transferred from the sender of the message to the receiver.
  -> MessageFormat -- ^Message sent to the contract. On the first invocation of this function this will
                   -- be an Acorn expression, and in nested calls an Acorn value.
  -> m [Event] -- The events resulting from processing the message and all recursively processed messages.
handleMessage origin istance sender transferamount maybeMsg = do
  let receivefun = ireceiveFun istance
      model = instanceModel istance
  -- Check whether the sender of the message has enough on its account/instance for the transfer.
  -- If the amount is not sufficient, the top-level transaction is rejected.
  -- TODO: For now there is no exception handling in smart contracts and contracts cannot check
  -- amounts on other instances or accounts. Possibly this will need to be changed.
  let senderAddr = mkSenderAddr sender
  senderamount <- getCurrentAmount sender
  unless (senderamount >= transferamount) $ rejectTransaction (AmountTooLarge senderAddr transferamount)

  let iParams = instanceParameters istance
  let cref = instanceAddress iParams

  -- Now we also check that the owner account of the receiver instance has at least one valid credential
  -- and reject the transaction if not.
  let ownerAccountAddress = instanceOwner iParams
  -- The invariants maintained by global state should ensure that an owner account always exists.
  -- However we are defensive here and reject the transaction, acting as if there is no credential.
  ownerAccount <- getCurrentAccount ownerAccountAddress `rejectingWith` (ReceiverContractNoCredential cref)
  cm <- getChainMetadata
  unless (existsValidCredential cm ownerAccount) $ rejectTransaction (ReceiverContractNoCredential cref)
  -- We have established that the owner account of the receiver instance has at least one valid credential.

  -- Now run the receive function on the message. This ticks energy during execution, failing when running out of energy.
  let originAddr = origin ^. accountAddress
  let receiveCtx = ReceiveContext { invoker = originAddr,
                                    selfAddress = cref,
                                    selfBalance = instanceAmount istance }
  result <- case maybeMsg of
              ValueMessage m -> runInterpreter (I.applyReceiveFunVal cm receiveCtx receivefun model senderAddr transferamount m)
              ExprMessage m ->  runInterpreter (I.applyReceiveFun cm receiveCtx receivefun model senderAddr transferamount m)
  case result of
    -- The contract rejected the message. Thus reject the top-level transaction, i.e., no changes
    -- are made to any account or contract state except for charging the sender of the top-level
    -- transaction for the execution cost ticked so far.
    Nothing -> rejectTransaction Rejected
    -- The contract accepted the message and returned a new state as well as outgoing messages.
    Just (newmodel, txout) -> do
        -- Charge for eventually storing the new contract state (even if it might not be stored
        -- in the end because the transaction fails).
        -- TODO We might want to change this behaviour to prevent charging for storage that is not done.
        tickEnergyValueStorage newmodel
        -- Process the generated messages in the new context (transferred amount, updated state) in
        -- sequence from left to right, depth first.
        withToContractAmount sender istance transferamount $
          withInstanceState istance newmodel $
            foldM (\res tx -> combineProcessing res $ do
                        -- Charge a small amount just for the fact that a message was generated.
                        tickEnergy Cost.interContractMessage
                        -- NB: The sender of all the newly generated messages is the contract instance 'istance'.
                        case tx of
                          TSend cref' transferamount' message' -> do
                            -- NB: Acorn only allows the creation of messages with addresses of existing
                            -- instances. If the instance does however not exist, this rejects the
                            -- transaction.
                            cinstance <- getCurrentContractInstanceTicking cref'
                            handleMessage origin
                                          cinstance
                                          (Left istance)
                                          transferamount'
                                          (ValueMessage (I.aJust message'))
                          -- A transfer to a contract is defined to be an Acorn @Nothing@ message
                          -- with the to be transferred amount.
                          TSimpleTransfer (AddressContract cref') transferamount' -> do
                            -- We can make a simple transfer without checking existence of a contract.
                            -- The following rejects the transaction in case the instance does not exist.
                            cinstance <- getCurrentContractInstanceTicking cref'
                            handleMessage origin
                                          cinstance
                                          (Left istance)
                                          transferamount'
                                          (ValueMessage I.aNothing)
                          TSimpleTransfer (AddressAccount acc) transferamount' ->
                            -- FIXME: This is temporary until accounts have their own functions
                            handleTransferAccount origin acc (Left istance) transferamount'
                            )
                  [Updated{euAddress=cref,euInstigator=senderAddr,euAmount=transferamount,euMessage=maybeMsg}] txout

-- | Combine two processing steps that each result in a list of events, concatenating the event lists.
combineProcessing :: Monad m => [Event] -> m [Event] -> m [Event]
combineProcessing x ma = (x ++) <$> ma

mkSenderAddr :: Either Instance Account -> Address
mkSenderAddr sender =
    case sender of
      Left istance -> AddressContract (instanceAddress (instanceParameters istance))
      Right acc -> AddressAccount (acc ^. accountAddress)


-- | Handle the transfer of an amount from an account or contract instance to an account.
-- TODO: Figure out whether we need the origin information in here (i.e.,
-- whether an account can observe it).
handleTransferAccount ::
  TransactionMonad m
  => Account -- ^The account that sent the top-level transaction.
  -> AccountAddress -- The target account address.
  -> Either Instance Account -- The sender of this transfer (contract instance or account).
  -> Amount -- The amount to transfer.
  -> m [Event] -- The events resulting from the transfer.
handleTransferAccount _origin accAddr sender transferamount = do
  tickEnergy Cost.transferAccount
  -- Check whether the sender has the amount to be transferred and reject the transaction if not.
  senderamount <- getCurrentAmount sender
  unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge (mkSenderAddr sender) transferamount)

  -- Check whether target account exists and get it.
  targetAccount <- getCurrentAccount accAddr `rejectingWith` InvalidAccountReference accAddr
  -- Check that the account has a valid credential and reject the transaction if not
  -- (it is not allowed to send to accounts without valid credential).
  cm <- getChainMetadata
  unless (existsValidCredential cm targetAccount) $ rejectTransaction (ReceiverAccountNoCredential accAddr)

  -- Add the transfer to the current changeset and return the corresponding event.
  withToAccountAmount sender targetAccount transferamount $
      return [Transferred (mkSenderAddr sender) transferamount (AddressAccount accAddr)]

-- |Run the interpreter with the remaining amount of energy. If the interpreter
-- runs out of energy set the remaining gas to 0 and reject the transaction,
-- otherwise decrease the consumed amount of energy and return the result.
{-# INLINE runInterpreter #-}
runInterpreter :: TransactionMonad m => (InterpreterEnergy -> m (Maybe (a, InterpreterEnergy))) -> m a
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

-- |A simple sigma protocol to check knowledge of secret key.
checkAccountOwnership :: BS.ByteString -> ID.AccountKeys -> AccountOwnershipProof -> Bool
checkAccountOwnership challenge keys (AccountOwnershipProof proofs) =
    enoughProofs && allProofsValid
  where -- the invariant on akThreshold should also guarantee there is at least one
        enoughProofs = length proofs >= fromIntegral (ID.akThreshold keys)
        -- this is not necessary (we only need the threshold), but safe
        allProofsValid = all checkProof proofs
        checkProof (idx, proof) =
          case ID.getAccountKey idx keys of
            Nothing -> False
            Just key -> Proofs.checkDlog25519ProofSig challenge key proof

-- |Add a baker to the baker pool. The current logic for when this is allowed is as follows.
--
--  * The account to which the baker wants to be rewarded must exist.
--  * The sender account can be any other account. It does not have to be the baker's account.
--  * The baker needs to provide a cryptographic proof that
--
--    - they own the private key corresponding to the reward account's key
--    - they own the private key corresponding to the election verification key
--    - they own the private key corresponding to the signature verification key
--
-- Upon successful completion of this transaction a new baker is added to the
-- baking pool (birk parameters). Initially the baker has 0 lottery power. If
-- they wish to gain lotter power they need some stake delegated to them, which
-- is a separate transaction.

-- |TODO: Figure out whether we need the sender account to verify validity of this transaction.
-- We might use it in checking validity of the proofs.
handleAddBaker ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerElectionVerifyKey
    -> BakerSignVerifyKey
    -> BakerAggregationVerifyKey
    -> AccountAddress
    -> Proofs.Dlog25519Proof
    -> Proofs.Dlog25519Proof
    -> AccountOwnershipProof
    -> BakerAggregationProof
    -> m (Maybe TransactionSummary)
handleAddBaker wtc abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount abProofSig abProofElection abProofAccount abProofAggregation =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.addBaker
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

          getAccount abAccount >>=
              \case Nothing -> return $! (TxReject (NonExistentRewardAccount abAccount), energyCost, usedEnergy)
                    Just Account{..} ->
                      let challenge = S.runPut (S.put abElectionVerifyKey <> S.put abSignatureVerifyKey <> S.put abAggregationVerifyKey <> S.put abAccount)
                          electionP = checkElectionKeyProof challenge abElectionVerifyKey abProofElection
                          signP = checkSignatureVerifyKeyProof challenge abSignatureVerifyKey abProofSig
                          accountP = checkAccountOwnership challenge _accountVerificationKeys abProofAccount
                          aggregationP = Bls.checkProofOfKnowledgeSK challenge abProofAggregation abAggregationVerifyKey
                      in if electionP && signP && accountP && aggregationP then do
                        -- the proof validates that the baker owns all the private keys.
                        -- Moreover at this point we know the reward account exists and belongs
                        -- to the baker.
                        -- Thus we can create the baker, starting it off with 0 lottery power.
                        mbid <- addBaker (BakerCreationInfo abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount)
                        case mbid of
                          Left Bakers.DuplicateSignKey -> return $ (TxReject (DuplicateSignKey abSignatureVerifyKey), energyCost, usedEnergy)
                          Left Bakers.DuplicateAggregationKey -> return $ (TxReject (DuplicateAggregationKey abAggregationVerifyKey), energyCost, usedEnergy)
                          Right bid -> return $ (TxSuccess [BakerAdded bid], energyCost, usedEnergy)
                      else return $ (TxReject InvalidProof, energyCost, usedEnergy)

-- |Remove a baker from the baker pool.
-- The current logic is that if the proof validates that the sender of the
-- transaction is the reward account of the baker.
handleRemoveBaker ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> m (Maybe TransactionSummary)
handleRemoveBaker wtc rbId =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.removeBaker
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

          getBakerInfo rbId >>=
              \case Nothing ->
                      return $ (TxReject (RemovingNonExistentBaker rbId), energyCost, usedEnergy)
                    Just binfo ->
                      if senderAccount ^. accountAddress == binfo ^. bakerAccount then do
                        -- only the baker itself can remove themselves from the pool
                        removeBaker rbId
                        return $ (TxSuccess [BakerRemoved rbId], energyCost, usedEnergy)
                      else
                        return $ (TxReject (InvalidBakerRemoveSource (senderAccount ^. accountAddress)), energyCost, usedEnergy)

-- |Update the baker's reward account. The transaction is considered valid if
--
--  * The transaction is coming from the baker's current reward account.
--  * The account they wish to set as their reward account exists.
--  * They own the account (meaning they know the private key corresponding to
--    the public key of the account)
-- TODO: Figure out (same as previous transaction) whether we need the sender account here
-- to validate the transaction.
handleUpdateBakerAccount ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> AccountAddress
    -> AccountOwnershipProof
    -> m (Maybe TransactionSummary)
handleUpdateBakerAccount wtc ubaId ubaAddress ubaProof =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.updateBakerAccount
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost

          getBakerInfo ubaId >>= \case
            Nothing ->
                return $ (TxReject (UpdatingNonExistentBaker ubaId), energyCost, usedEnergy)
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                  -- the transaction is coming from the current baker's account.
                  -- now check the account exists and the baker owns it
                  getAccount ubaAddress >>= \case
                    Nothing -> return (TxReject (NonExistentRewardAccount ubaAddress), energyCost, usedEnergy)
                    Just Account{..} ->
                      let challenge = S.runPut (S.put ubaId <> S.put ubaAddress)
                          accountP = checkAccountOwnership challenge _accountVerificationKeys ubaProof
                      in if accountP then do
                        _ <- updateBakerAccount ubaId ubaAddress
                        return $ (TxSuccess [BakerAccountUpdated ubaId ubaAddress], energyCost, usedEnergy)
                      else return $ (TxReject InvalidProof, energyCost, usedEnergy)
                else
                  return $ (TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)), energyCost, usedEnergy)

-- |Update the baker's public signature key. The transaction is considered valid if
--
--  * The transaction is coming from the baker's current reward account.
--  * The transaction proves that they own the private key corresponding to the __NEW__
--    signature verification key.
handleUpdateBakerSignKey ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> BakerSignVerifyKey
    -> Proofs.Dlog25519Proof
    -> m (Maybe TransactionSummary)
handleUpdateBakerSignKey wtc ubsId ubsKey ubsProof =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.updateBakerKey
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          getBakerInfo ubsId >>= \case
            Nothing ->
              return (TxReject (UpdatingNonExistentBaker ubsId), energyCost, usedEnergy)
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                -- only the baker itself can update its own keys
                -- now also check that they own the private key for the new signature key
                let challenge = S.runPut (S.put ubsId <> S.put ubsKey)
                    signP = checkSignatureVerifyKeyProof challenge ubsKey ubsProof
                in if signP then do
                     success <- updateBakerSignKey ubsId ubsKey
                     if success then
                       return (TxSuccess [BakerKeyUpdated ubsId ubsKey], energyCost, usedEnergy)
                     else return (TxReject (DuplicateSignKey ubsKey), energyCost, usedEnergy)
                   else return (TxReject InvalidProof, energyCost, usedEnergy)
              else
                return (TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)), energyCost, usedEnergy)

-- |Update an account's stake delegate.
handleDelegateStake ::
  SchedulerMonad m
    => WithDepositContext
    -> Maybe BakerId
    -> m (Maybe TransactionSummary)
handleDelegateStake wtc targetBaker =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy delegateCost
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          res <- delegateStake (thSender meta) targetBaker
          if res then
            let addr = senderAccount ^. accountAddress
                currentDelegate = senderAccount ^. accountStakeDelegate
            in return (TxSuccess [maybe (StakeUndelegated addr currentDelegate) (StakeDelegated addr) targetBaker], energyCost, usedEnergy)
          else
            return (TxReject (InvalidStakeDelegationTarget $ fromJust targetBaker), energyCost, usedEnergy)
        delegateCost = Cost.updateStakeDelegate (Set.size $ senderAccount ^. accountInstances)

-- |Update the election difficulty birk parameter.
-- The given difficulty must be valid (see 'isValidElectionDifficulty').
-- This precondition is ensured by the transaction (de)serialization.
handleUpdateElectionDifficulty
  :: SchedulerMonad m
  => WithDepositContext
  -> ElectionDifficulty
  -> m (Maybe TransactionSummary)
handleUpdateElectionDifficulty wtc uedDifficulty =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        senderAddr = senderAccount ^. accountAddress
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.updateElectionDifficulty
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          specialBetaAccounts <- getSpecialBetaAccounts
          if HashSet.member senderAddr specialBetaAccounts
          then do
            assert (isValidElectionDifficulty uedDifficulty) $ return ()
            updateElectionDifficulty uedDifficulty
            return (TxSuccess [ElectionDifficultyUpdated uedDifficulty], energyCost, usedEnergy)
          else return (TxReject NotFromSpecialAccount, energyCost, usedEnergy)

-- *Transactions without a sender
handleDeployCredential ::
  SchedulerMonad m =>
  -- |Credentials to deploy.
  ID.CredentialDeploymentInformation ->
  TransactionHash ->
  m (Maybe TxResult)
handleDeployCredential cdi cdiHash = do
    remainingEnergy <- getRemainingEnergy
    let cost = Cost.deployCredential
    if remainingEnergy < cost then return Nothing
    else do
      let mkSummary tsResult = do
            tsIndex <- bumpTransactionIndex
            return $ Just . TxValid $ TransactionSummary{
              tsSender = Nothing,
              tsHash = cdiHash,
              tsCost = 0,
              tsEnergyCost = cost,
              tsType = Nothing,
              ..
              }

      let cdiBytes = S.encode cdi
      let cdv = ID.cdiValues cdi

      cm <- getChainMetadata
      let expiry = ID.pValidTo (ID.cdvPolicy cdv)

      -- check that a registration id does not yet exist
      let regId = ID.cdvRegId cdv
      regIdEx <- accountRegIdExists regId
      if not (isTimestampBefore (slotTime cm) expiry) then
        return $ Just (TxInvalid AccountCredentialInvalid)
      else if regIdEx then
        return $ (Just (TxInvalid (DuplicateAccountRegistrationID (ID.cdvRegId cdv))))
      else do
        -- We now look up the identity provider this credential is derived from.
        -- Of course if it does not exist we reject the transaction.
        let credentialIP = ID.cdvIpId cdv
        getIPInfo credentialIP >>= \case
          Nothing -> return $! Just (TxInvalid (NonExistentIdentityProvider (ID.cdvIpId cdv)))
          Just ipInfo -> do
            cryptoParams <- getCrypoParams
            -- we have two options. One is that we are deploying a credential on an existing account.
            case ID.cdvAccount cdv of
              ID.ExistingAccount aaddr ->
                -- first check whether an account with the address exists in the global store
                -- if it does not we cannot deploy the credential.
                getAccount aaddr >>= \case
                  Nothing -> return $! Just (TxInvalid (NonExistentAccount aaddr))
                  Just account -> do
                        -- otherwise we just try to add a credential to the account
                        -- but only if the credential is from the same identity provider
                        -- as the existing ones on the account.
                        -- Since we always maintain this invariant it is sufficient to check
                        -- for one credential only.
                        let credentials = account ^. accountCredentials
                        let sameIP = maybe True (\(_, cred) -> ID.cdvIpId cred == credentialIP) (Queue.getMax credentials)
                        if sameIP && AH.verifyCredential cryptoParams ipInfo (Just (account ^. accountVerificationKeys)) cdiBytes then do
                          addAccountCredential account cdv
                          mkSummary (TxSuccess [CredentialDeployed{ecdRegId=regId,ecdAccount=aaddr}])
                        else
                          return $ (Just (TxInvalid AccountCredentialInvalid))
              ID.NewAccount keys threshold ->
                -- account does not yet exist, so create it, but we need to be careful
                if null keys || length keys > 255 then
                  return $ Just (TxInvalid AccountCredentialInvalid)
                else do
                  let accountKeys = ID.makeAccountKeys keys threshold
                  let aaddr = ID.addressFromRegId regId
                  let account = newAccount accountKeys aaddr regId
                  -- this check is extremely unlikely to fail (it would amount to a hash collision since
                  -- we checked regIdEx above already).
                  accExistsAlready <- isJust <$> getAccount aaddr
                  let check = AH.verifyCredential cryptoParams ipInfo Nothing cdiBytes
                  if not accExistsAlready && check then do
                    _ <- putNewAccount account -- first create new account, but only if credential was valid.
                                               -- We know the address does not yet exist.
                    addAccountCredential account cdv  -- and then add the credentials
                    mkSummary (TxSuccess [AccountCreated aaddr, CredentialDeployed{ecdRegId=regId,ecdAccount=aaddr}])
                  else return $ Just (TxInvalid AccountCredentialInvalid)

-- |Update the baker's public aggregation key. The transaction is considered valid if
--
--  * The transaction is coming from the baker's current reward account.
--  * The transaction proves that they own the private key corresponding to the __NEW__
--    aggregation verification key.
-- TODO: It might be valuable to include the old key in the challenge for the proof,
-- at the cost of complicating the uses of this transaction.
handleUpdateBakerAggregationVerifyKey ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> BakerAggregationVerifyKey
    -> BakerAggregationProof
    -> m (Maybe TransactionSummary)
handleUpdateBakerAggregationVerifyKey wtc ubavkId ubavkKey ubavkProof =
  withDeposit wtc c k
  where senderAccount = wtc ^. wtcSenderAccount
        txHash = wtc ^. wtcTransactionHash
        meta = wtc ^. wtcTransactionHeader
        c = tickEnergy Cost.updateBakerAggregationVerifyKey
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost txHash senderAccount energyCost
          getBakerInfo ubavkId >>= \case
            Nothing ->
              return (TxReject (UpdatingNonExistentBaker ubavkId), energyCost, usedEnergy)
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                -- only the baker itself can update its own keys
                -- now also check that they own the private key for the new aggregation key
                let challenge = S.runPut (S.put ubavkId <> S.put ubavkKey)
                    keyProof = Bls.checkProofOfKnowledgeSK challenge ubavkProof ubavkKey
                in if keyProof then do
                     success <- updateBakerAggregationKey ubavkId ubavkKey
                     if success then
                       return (TxSuccess [BakerAggregationKeyUpdated ubavkId ubavkKey], energyCost, usedEnergy)
                     else return (TxReject (DuplicateAggregationKey ubavkKey), energyCost, usedEnergy)
                   else return (TxReject InvalidProof, energyCost, usedEnergy)
              else
                return (TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)), energyCost, usedEnergy)

-- |Update the baker's VRF key.
-- The transaction is valid if it proves knowledge of the secret key,
-- and if it is coming from the baker's reward account.
-- TODO: It might be valuable to include the old VRF key in the challenge,
-- at the cost of complicating the uses of this transaction.
handleUpdateBakerElectionKey ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> BakerElectionVerifyKey
    -> Proofs.Dlog25519Proof
    -> m (Maybe TransactionSummary)
handleUpdateBakerElectionKey wtc ubekId ubekKey ubekProof =
  withDeposit wtc cost k
  where
    senderAccount = wtc ^. wtcSenderAccount
    txHash = wtc ^. wtcTransactionHash
    meta = wtc ^. wtcTransactionHeader
    cost = tickEnergy Cost.updateBakerElectionKey
    k ls _ = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      getBakerInfo ubekId >>= \case
        Nothing ->
          return (TxReject (UpdatingNonExistentBaker ubekId), energyCost, usedEnergy)
        Just binfo ->
          -- The transaction to update the election key of the baker must come
          -- from the account of the baker
          if binfo ^. bakerAccount == senderAccount ^. accountAddress then
            -- check that the baker supplied a valid proof of knowledge of the election key
            let challenge = S.runPut (S.put ubekId <> S.put ubekKey)
                keyProof = checkElectionKeyProof challenge ubekKey ubekProof
            in if keyProof then do
              updateBakerElectionKey ubekId ubekKey
              return (TxSuccess [BakerElectionKeyUpdated ubekId ubekKey], energyCost, usedEnergy)
            else return (TxReject InvalidProof, energyCost, usedEnergy)
          else
            return (TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)), energyCost, usedEnergy)


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
filterTransactions :: forall m . (SchedulerMonad m)
                   => Integer -- ^Maximum block size in bytes.
                   -> GroupedTransactions Transaction -- ^Transactions to make a block out of.
                   -> m FilteredTransactions
filterTransactions maxSize GroupedTransactions{..} = do
  maxEnergy <- getMaxBlockEnergy

  runNext maxEnergy 0 emptyFilteredTransactions credentialDeployments perAccountTransactions
  where
        -- Run next credential deployment or transaction group, depending on arrival time.
        runNext :: Energy -- ^Maximum block energy
                -> Integer -- ^Current size of transactions in the block.
                -> FilteredTransactions -- ^Currently accumulated result
                -> [CredentialDeploymentWithMeta] -- ^Credentials to process
                -> [[Transaction]] -- ^Transactions to process, grouped per account.
                -> m FilteredTransactions
        runNext maxEnergy size fts credentials remainingTransactions =
          case (credentials, remainingTransactions) of
            -- All credentials and transactions processed; Before returning,
            -- need to reverse because we accumulated in reverse (for performance reasons)
            ([], []) -> return fts{ ftAdded = reverse (ftAdded fts )}
            -- Further credentials or transactions to process
            ([], group : groups) -> runTransactionGroup size fts groups group
            (c:creds, []) -> runCredential creds c
            (cs@(c:creds), group : groups) ->
              case group of
                [] -> runNext maxEnergy size fts cs groups
                (t:_) ->
                  if wmdArrivalTime c <= wmdArrivalTime t
                  then runCredential creds c
                  else runTransactionGroup size fts groups group

          where
            -- Run a single credential and continue with 'runNext'.
            runCredential remainingCreds c@WithMetadata{..} = do
              totalEnergyUsed <- getUsedEnergy
              let csize = size + fromIntegral wmdSize
                  energyCost = Cost.deployCredential
                  cenergy = totalEnergyUsed + fromIntegral energyCost
              if csize <= maxSize && cenergy <= maxEnergy then
                observeTransactionFootprint (handleDeployCredential wmdData wmdHash) >>= \case
                    (Just (TxInvalid reason), _) -> do
                      let newFts = fts { ftFailedCredentials = (c, reason) : ftFailedCredentials fts}
                      runNext maxEnergy size newFts remainingCreds remainingTransactions -- NB: We keep the old size
                    (Just (TxValid summary), fp) -> do
                      markEnergyUsed (tsEnergyCost summary)
                      tlNotifyAccountEffect fp summary
                      let newFts = fts { ftAdded = (credentialDeployment c, summary) : ftAdded fts}
                      runNext maxEnergy csize newFts remainingCreds remainingTransactions
                    (Nothing, _) -> error "Unreachable due to cenergy <= maxEnergy check."
              else if Cost.deployCredential > maxEnergy then
                -- this case should not happen (it would mean we set the parameters of the chain wrong),
                -- but we keep it just in case.
                 let newFts = fts { ftFailedCredentials = (c, ExceedsMaxBlockEnergy) : ftFailedCredentials fts}
                 in runNext maxEnergy size newFts remainingCreds remainingTransactions
              else
                 let newFts = fts { ftUnprocessedCredentials = c : ftUnprocessedCredentials fts}
                 in runNext maxEnergy size newFts remainingCreds remainingTransactions

            -- Run all transactions in a group and continue with 'runNext'.
            runTransactionGroup :: Integer -- ^Current size of transactions in the block.
                                -> FilteredTransactions
                                -> [[Transaction]] -- ^Remaining groups to process.
                                -> [Transaction] -- ^Current group to process.
                                -> m FilteredTransactions
            runTransactionGroup currentSize currentFts remainingGroups (t:ts) = do
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
                     runTransactionGroup csize newFts remainingGroups rest
                   -- The transaction failed, add it to the list of failed transactions and
                   -- determine whether following transaction have to fail as well.
                   (Just (TxInvalid reason), _) ->
                     let (newFts, rest) = invalidTs t reason currentFts ts
                     in runTransactionGroup size newFts remainingGroups rest
                   (Nothing, _) -> error "Unreachable. Dispatch honors maximum transaction energy."
              -- If the stated energy of a single transaction exceeds the block energy limit the
              -- transaction is invalid. Add it to the list of failed transactions and
              -- determine whether following transactions have to fail as well.
              else if tenergy > maxEnergy then
                let (newFts, rest) = invalidTs t ExceedsMaxBlockEnergy currentFts ts
                in runTransactionGroup size newFts remainingGroups rest
              else -- otherwise still try the remaining transactions in the group to avoid deadlocks from
                   -- one single too-big transaction (with same nonce).
                let newFts = currentFts { ftUnprocessed = t : (ftUnprocessed currentFts) }
                in runTransactionGroup size newFts remainingGroups ts

            -- Group processed, continue with the next group or credential
            runTransactionGroup currentSize currentFts remainingGroups [] =
              runNext maxEnergy currentSize currentFts credentials remainingGroups

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
              (Just (TxInvalid reason), _) -> return (Left (Just reason))
              (Nothing, _) -> return (Left Nothing)

          go valid [] = return (Right (reverse valid))

          predispatch :: BlockItem -> m (Maybe TxResult)
          predispatch WithMetadata{wmdData=NormalTransaction tr,..} = dispatch WithMetadata{wmdData=tr,..}
          predispatch WithMetadata{wmdData=CredentialDeployment cred,..} = handleDeployCredential cred wmdHash

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
            (Just (TxInvalid reason), _) -> return (Left (Just reason))
        go [] = return (Right ())

        predispatch :: BlockItem -> m (Maybe TxResult)
        predispatch WithMetadata{wmdData=NormalTransaction tr,..} = dispatch WithMetadata{wmdData=tr,..}
        predispatch WithMetadata{wmdData=CredentialDeployment cred,..} = handleDeployCredential cred wmdHash
