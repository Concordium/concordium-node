{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler
  (filterTransactions
  ,runTransactions
  ,execTransactions
  ,FilteredTransactions(..)
  ) where

import qualified Acorn.TypeCheck as TC
import qualified Acorn.Interpreter as I
import qualified Acorn.Core as Core
import Acorn.Types(compile)
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment

import qualified Data.Serialize as S
import qualified Data.ByteString as BS
import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Bakers(bakerAccount)
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
    Just (expiry, _) -> expiry >= slotTime cm


-- |Check that
--  * the transaction has a valid sender,
--  * the amount they have deposited is on their account,
--  * the transaction is not expired.
-- The valid sender means that the sender account has at least one valid credential,
-- where currently valid means non-expired.
--
-- Before any other checks this checks wheter the amount deposited is enough to cover
-- the cost that will be charged for checking the header.
--
-- Returns the sender account and the cost to be charged for checking the header.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> ExceptT (Maybe FailureKind) m (Account, Energy)
checkHeader meta = do
  -- Before even checking the header we calculate the cost that will be charged to do so
  -- and check that at least that much energy is deposited.
  let cost = Cost.checkHeader (getTransactionHeaderPayloadSize $ transactionHeader meta) (getTransactionNumSigs (transactionSignature meta))
  unless (transactionGasAmount meta >= cost) $ throwError (Just DepositInsufficient)
  remainingBlockEnergy <- lift getRemainingEnergy
  unless (remainingBlockEnergy >= cost) $ throwError Nothing
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

      -- after the credential check is done we check the amount
      depositedAmount <- lift (energyToGtu (transactionGasAmount meta))

      -- check they have enough funds to cover the deposit
      unless (depositedAmount <= amnt) (throwError . Just $ InsufficientFunds)
      unless (txnonce == nextNonce) (throwError . Just $ (NonSequentialNonce nextNonce))
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

-- This method returns either a Just TxResult if transaction either failed, or
-- was successfully commited to a block, or Nothing, in case the transaction
-- would have pushed the block execution over the limit.
dispatch :: (TransactionData msg, SchedulerMonad m) => msg -> m (Maybe TxResult)
dispatch msg = do
  let meta = transactionHeader msg
  validMeta <- runExceptT (checkHeader msg)
  case validMeta of
    Left (Just fk) -> return $ Just (TxInvalid fk)
    Left Nothing -> return Nothing
    Right (senderAccount, cost) -> do
      -- at this point the transaction is going to be commited to the block.
      -- It could be that the execution exceeds maximum block energy allowed, but in that case
      -- the whole block state will be removed, and thus this operation will have no effect anyhow.
      -- Hence we can increase the account nonce of the sender account.
      increaseAccountNonce senderAccount

      -- then we notify the block state that all the identity issuers on the sender's account should be rewarded
      -- TODO: Check for existence of valid identity provider.
      -- TODO: Alternative design would be to only reward them if the transaction is successful/committed, or
      -- to add additional parameters (such as deposited amount)
      -- FIXME: Only consider non-expired credentials.
      mapM_ (notifyIdentityProviderCredential . ID.cdvIpId) (senderAccount ^. accountCredentials)

      -- available for execution remaining amount available on the sender's
      -- account. This is deducted prior to execution and refunded at the end,
      -- if there is any left.
      let psize = payloadSize (transactionPayload msg)
      -- TODO: Check whether the cost for deserializing the transaction is sufficiently covered
      -- by the cost for checking the header (which is linear in the transaction size).

      tsIndex <- bumpTransactionIndex
      case decodePayload (transactionPayload msg) of
        Left _ -> do
          -- in case of serialization failure we charge the sender for checking
          -- the header and reject the transaction
          payment <- energyToGtu cost
          chargeExecutionCost (transactionHash msg) senderAccount payment
          return $! Just $! TxValid $! TransactionSummary{
            tsEnergyCost = cost,
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
                _wtcTransactionCheckHeaderCost = cost,
                -- NB: We already account for the cost we used here.
                _wtcCurrentlyUsedBlockEnergy = usedBlockEnergy + cost,
                _wtcTransactionIndex = tsIndex,
                ..}
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

                   DeployEncryptionKey encKey ->
                     handleDeployEncryptionKey (mkWTC TTDeployEncryptionKey) encKey

                   AddBaker{..} ->
                     handleAddBaker (mkWTC TTAddBaker) abElectionVerifyKey abSignatureVerifyKey abAggregationVerifyKey abAccount abProofSig abProofElection abProofAccount abProofAggregation

                   RemoveBaker{..} ->
                     handleRemoveBaker (mkWTC TTRemoveBaker) rbId rbProof

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
          case res of
            Nothing -> return Nothing
            Just summary -> return $! Just $! TxValid summary

-- |Process the deploy module transaction.
handleDeployModule ::
  SchedulerMonad m
  => WithDepositContext
  -> PayloadSize -- ^Serialized size of the module. Used for charging execution cost.
  -> Module -- ^The module to deploy
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
      iface <- typeHidingErrors (TC.typeModule imod) `rejectingWith` ModuleNotWF
      let viface = I.evalModule imod
      return (mhash, iface, viface)

    k ls (mhash, iface, viface) = do
      (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
      chargeExecutionCost txHash senderAccount energyCost
      b <- commitModule mhash iface viface mod
      if b then
        return $! (TxSuccess [ModuleDeployed mhash], energyCost, usedEnergy)
          else
        -- FIXME:
        -- we should reject the transaction immediately if we figure out that the module with the hash already exists.
        -- otherwise we can waste some effort in checking before reaching this point.
        -- This could be checked immediately even before we reach the dispatch since module hash is the hash of module serialization.
        return $! (TxReject (ModuleHashAlreadyExists mhash), energyCost, usedEnergy)

-- |Handle the top-level initialize contract.
handleInitContract ::
  SchedulerMonad m
    => WithDepositContext
    -> Amount   -- ^The amount to initialize the contract with.
    -> ModuleRef  -- ^Module reference of the contract to initialize.
    -> Core.TyName  -- ^Name of the contract in a module.
    -> Core.Expr Core.UA Core.ModuleName  -- ^Parameters of the contract.
    -> Int -- ^Serialized size of the parameters. Used for computing typechecking cost.
    -> m (Maybe TransactionSummary)
handleInitContract wtc amount modref cname param paramSize =
  withDeposit wtc c k
    where senderAccount = wtc ^. wtcSenderAccount
          txHash = wtc ^. wtcTransactionHash
          meta = wtc ^. wtcTransactionHeader
          c = do
            -- decrease available energy and start processing. This will reject the transaction if not enough is available.
            tickEnergy Cost.initPreprocess

            -- if the sender does not have the funds available we fail the transaction immediately
            -- NB: This checks the sender amount __after__ the deposit is reserved
            senderAmount <- getCurrentAccountAmount senderAccount
            unless (senderAmount >= amount) $! rejectTransaction (AmountTooLarge (AddressAccount (thSender meta)) amount)

            -- otherwise we proceed with normal execution.
            -- first try to get the module interface of the parent module of the contract
            (iface, viface) <- getModuleInterfaces modref `rejectingWith` InvalidModuleReference modref
            -- and then the particular contract interface (in particular the type of the init method)
            ciface <- pure (Map.lookup cname (exportedContracts iface)) `rejectingWith` InvalidContractReference modref cname
            -- first typecheck the parameters, whether they have the expected type
            -- the cost of type-checking is dependent on the size of the term
            tickEnergy (Cost.initParamsTypecheck paramSize)
            qparamExp <- typeHidingErrors (TC.checkTyInCtx' iface param (paramTy ciface)) `rejectingWith` ParamsTypeError
            -- NB: The unsafe Map.! is safe here because we do know the contract exists by the invariant on viface and iface
            linkedContract <- linkContract (uniqueName iface) cname (viContracts viface Map.! cname)
            let (initFun, _) = cvInitMethod linkedContract
            -- link the parameters, and account for the size of the linked parameters, failing if running out of energy
            -- in the process.
            (params', _) <- linkExpr (uniqueName iface) (compile qparamExp)
            cm <- getChainMetadata
            res <- runInterpreter (I.applyInitFun cm (InitContext (thSender meta)) initFun params' (thSender meta) amount)
            return (linkedContract, iface, viface, (msgTy ciface), res, amount)

          k ls (contract, iface, viface, msgty, model, initamount) = do
            (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
            chargeExecutionCost txHash senderAccount energyCost

            -- we make a new changeset that also withdraws the amount from the sender's account
            -- this way of doing it means that if the contract observes current balance it will observe
            -- the value before the initialization of the contract
            commitChanges (addAmountToCS senderAccount (amountDiff 0 amount) (ls ^. changeSet))
            let ins = makeInstance modref cname contract msgty iface viface model initamount (thSender meta)
            addr <- putNewInstance ins
            return $! (TxSuccess [ContractInitialized{ecRef=modref,ecName=cname,ecAddress=addr,ecAmount=amount}], energyCost, usedEnergy)

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
                  i <- getCurrentContractInstance cref `rejectingWith` InvalidContractAddress cref
                  let rf = Ins.ireceiveFun i
                      model = Ins.instanceModel i
                      -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
                      qmsgExpLinked = I.mkNothingE -- give Nothing as argument to the receive method
                  handleTransaction senderAccount
                                    i
                                    rf
                                    (Right senderAccount)
                                    amount
                                    (ExprMessage (I.mkJustE qmsgExpLinked))
                                    model
                AddressAccount toAccAddr ->
                  handleTransferAccount senderAccount toAccAddr (Right senderAccount) amount

handleUpdateContract ::
  SchedulerMonad m
    => WithDepositContext
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> Core.Expr Core.UA Core.ModuleName -- ^Message to send to the receive method.
    -> Int  -- ^Serialized size of the message.
    -> m (Maybe TransactionSummary)
handleUpdateContract wtc cref amount maybeMsg msgSize =
  withDeposit wtc c (defaultSuccess wtc)
  where senderAccount = wtc ^. wtcSenderAccount
        c = do
          tickEnergy Cost.updatePreprocess
          i <- getCurrentContractInstance cref `rejectingWith` InvalidContractAddress cref
          let rf = Ins.ireceiveFun i
              msgType = Ins.imsgTy i
              (iface, _) = Ins.iModuleIface i
              model = Ins.instanceModel i
              -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
          tickEnergy (Cost.updateMessageTypecheck msgSize)
          qmsgExp <- typeHidingErrors (TC.checkTyInCtx' iface maybeMsg msgType) `rejectingWith` MessageTypeError
          (qmsgExpLinked, _) <- linkExpr (uniqueName iface) (compile qmsgExp)
          handleTransaction senderAccount
                            i
                            rf
                            (Right senderAccount)
                            amount
                            (ExprMessage (I.mkJustE qmsgExpLinked))
                            model

-- this will always be run when we know that the contract exists and we can lookup its local state
handleTransaction ::
  (TransactionMonad m, InterpreterMonad NoAnnot m)
  => Account -- ^the origin account of the top-level transaction
  -> Instance -- ^The target contract of the transaction
  -> LinkedReceiveMethod NoAnnot -- ^the receive function of the contract
  -> Either Instance Account -- ^The invoker of this particular transaction, in general different from the origin
  -> Amount -- ^Amount that was sent to the contract in the transaction
  -> MessageFormat -- ^message wrapped in a Maybe, at top level it will be an expression, and in nested calls a value
  -> Value -- ^current local state of the target contract
  -> m [Event]
handleTransaction origin istance receivefun txsender transferamount maybeMsg model = do
  -- a transaction is rejected in case we try to transfer amounts we don't have.
  -- This rejection is different from rejection by a contract, but the effect is the same.
  -- FIXME: Possibly this will need to be changed.
  let txsenderAddr = mkSenderAddr txsender
  senderamount <- getCurrentAmount txsender

  unless (senderamount >= transferamount) $ rejectTransaction (AmountTooLarge txsenderAddr transferamount)

  let iParams = instanceParameters istance
  let cref = instanceAddress iParams

  -- Now we also check that the owner account of the receiver instance has at least one valid credential.
  let ownerAccountAddress = instanceOwner iParams
  -- The invariants maintained by global state should ensure that an owner account always exists.
  -- However we are defensive here and reject the transaction, acting as if there is no credential.
  ownerAccount <- getCurrentAccount ownerAccountAddress `rejectingWith` (ReceiverContractNoCredential cref)
  cm <- getChainMetadata
  unless (existsValidCredential cm ownerAccount) $ rejectTransaction (ReceiverContractNoCredential cref)
  -- we have established that the credential exists.

  let originAddr = origin ^. accountAddress
  let receiveCtx = ReceiveContext { invoker = originAddr, selfAddress = cref }
  result <- case maybeMsg of
              ValueMessage m -> runInterpreter (I.applyReceiveFunVal cm receiveCtx receivefun model txsenderAddr transferamount m)
              ExprMessage m ->  runInterpreter (I.applyReceiveFun cm receiveCtx receivefun model txsenderAddr transferamount m)
  case result of
    Nothing -> -- transaction rejected, no other changes were recorder in the global state (in particular the amount was not transferred)
      rejectTransaction Rejected -- transaction rejected due to contract logic
    Just (newmodel, txout) ->
        -- transfer the amount from the sender to the receiving contract in our state.
        withToContractAmount txsender istance transferamount $
          withInstanceState istance newmodel $
            -- and then process the generated messages in the new context in
            -- sequence from left to right, depth first.
            foldM (\res tx -> combineTx res $ do
                        tickEnergy Cost.interContractMessage -- Charge a small amount just for the fact that a message was generated.
                        -- we need to get the fresh amount each time since it might have changed for each execution
                        -- NB: The sender of all the newly generated messages is the contract instance 'istance'
                        case tx of
                          TSend cref' transferamount' message' -> do
                            -- the only way to send is to first check existence, so this must succeed
                            cinstance <- fromJust <$> getCurrentContractInstance cref'
                            let receivefun' = Ins.ireceiveFun cinstance
                            let model' = Ins.instanceModel cinstance
                            handleTransaction origin
                                              cinstance
                                              receivefun'
                                              (Left istance)
                                              transferamount'
                                              (ValueMessage (I.aJust message'))
                                              model'
                          -- simple transfer to a contract is the same as a call to update with Nothing
                          TSimpleTransfer (AddressContract cref') transferamount' -> do
                            -- We can make a simple transfer without checking existence of a contract.
                            -- Hence we need to handle the failure case here.
                            cinstance <- getCurrentContractInstance cref' `rejectingWith` (InvalidContractAddress cref')
                            let receivefun' = Ins.ireceiveFun cinstance
                            let model' = Ins.instanceModel cinstance
                            handleTransaction origin
                                              cinstance
                                              receivefun'
                                              (Left istance)
                                              transferamount'
                                              (ValueMessage I.aNothing)
                                              model'
                          TSimpleTransfer (AddressAccount acc) transferamount' ->
                            -- FIXME: This is temporary until accounts have their own functions
                            handleTransferAccount origin acc (Left istance) transferamount'
                            )
                  [Updated{euAddress=cref,euInstigator=txsenderAddr,euAmount=transferamount,euMessage=maybeMsg}] txout

combineTx :: Monad m => [Event] -> m [Event] -> m [Event]
combineTx x ma = (x ++) <$> ma

mkSenderAddr :: Either Instance Account -> Address
mkSenderAddr txsender =
    case txsender of
      Left istance -> AddressContract (instanceAddress (instanceParameters istance))
      Right acc -> AddressAccount (acc ^. accountAddress)


-- |TODO: Figure out whether we need the origin information in here (i.e.,
-- whether an account can observe it).
handleTransferAccount ::
  TransactionMonad m
  => Account -- the origin account of the top-level transaction
  -> AccountAddress -- the target account address
  -> Either Instance Account -- the invoker of this particular transaction, in general different from the origin
  -> Amount -- amount that was sent in the transaction
  -> m [Event]
handleTransferAccount _origin accAddr txsender transferamount = do
  tickEnergy Cost.transferAccount
  -- the sender must have the amount available.
  -- Otherwise we reject the transaction immediately.
  senderamount <- getCurrentAmount txsender

  unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge (mkSenderAddr txsender) transferamount)

  -- check if target account exists and get it
  targetAccount <- getCurrentAccount accAddr `rejectingWith` InvalidAccountReference accAddr
  -- and check that the account has a valid credential
  cm <- getChainMetadata
  -- Cannot send funds to accounts which have no valid credentials.
  unless (existsValidCredential cm targetAccount) $ rejectTransaction (ReceiverAccountNoCredential accAddr)

  -- FIXME: Should pay for execution here as well.
  withToAccountAmount txsender targetAccount transferamount $
      return [Transferred (mkSenderAddr txsender) transferamount (AddressAccount accAddr)]

-- |Run the interpreter with the remaining amount of energy. If the interpreter
-- runs out of gas set the remaining gas to 0 and reject the transaction,
-- otherwise decrease the consumed amount of gas and return the result.
{-# INLINE runInterpreter #-}
runInterpreter :: TransactionMonad m => (Energy -> m (Maybe (a, Energy))) -> m a
runInterpreter f =
  getEnergy >>= f >>= \case Just (x, energy') -> x <$ putEnergy energy'
                            Nothing -> putEnergy 0 >> rejectTransaction OutOfEnergy

handleDeployEncryptionKey ::
  SchedulerMonad m
    => WithDepositContext
    -> ID.AccountEncryptionKey -- ^The encryption key.
    -> m (Maybe TransactionSummary)
handleDeployEncryptionKey wtc encKey =
  withDeposit wtc c (defaultSuccess wtc)
  where senderAccount = wtc ^. wtcSenderAccount
        c = do
          tickEnergy Cost.deployEncryptionKey
          let aaddr = senderAccount ^. accountAddress
          case senderAccount ^. accountEncryptionKey of
            Nothing -> do
              addAccountEncryptionKey senderAccount encKey
              return [AccountEncryptionKeyDeployed encKey aaddr]
            Just encKey' -> rejectTransaction (AccountEncryptionKeyAlreadyExists aaddr encKey')


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
                          Nothing -> return $! (TxReject (DuplicateSignKey abSignatureVerifyKey), energyCost, usedEnergy)
                          Just bid -> return $! (TxSuccess [BakerAdded bid], energyCost, usedEnergy)
                      else return $ (TxReject InvalidProof, energyCost, usedEnergy)

-- |Remove a baker from the baker pool.
-- The current logic is that if the proof validates that the sender of the
-- transaction is the reward account of the baker.
-- TODO: Need to make sure that this proof is not duplicable (via the challenge prefix I suppose).
handleRemoveBaker ::
  SchedulerMonad m
    => WithDepositContext
    -> BakerId
    -> Proof
    -> m (Maybe TransactionSummary)
handleRemoveBaker wtc rbId _rbProof =
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
                    Nothing -> return $! (TxReject (NonExistentRewardAccount ubaAddress), energyCost, usedEnergy)
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
              return $! (TxReject (UpdatingNonExistentBaker ubsId), energyCost, usedEnergy)
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                -- only the baker itself can update its own keys
                -- now also check that they own the private key for the new signature key
                let challenge = S.runPut (S.put ubsId <> S.put ubsKey)
                    signP = checkSignatureVerifyKeyProof challenge ubsKey ubsProof
                in if signP then do
                     success <- updateBakerSignKey ubsId ubsKey
                     if success then
                       return $! (TxSuccess [BakerKeyUpdated ubsId ubsKey], energyCost, usedEnergy)
                     else return $! (TxReject (DuplicateSignKey ubsKey), energyCost, usedEnergy)
                   else return $ (TxReject InvalidProof, energyCost, usedEnergy)
              else
                return $! (TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)), energyCost, usedEnergy)

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
            in return $! (TxSuccess [maybe (StakeUndelegated addr currentDelegate) (StakeDelegated addr) targetBaker], energyCost, usedEnergy)
          else
            return $! (TxReject (InvalidStakeDelegationTarget $! fromJust targetBaker), energyCost, usedEnergy)
        delegateCost = Cost.updateStakeDelegate (Set.size $! senderAccount ^. accountInstances)

-- |Update the election difficulty birk parameter.
-- The given difficulty
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
            return $! (TxSuccess [ElectionDifficultyUpdated uedDifficulty], energyCost, usedEnergy)
          else return $! (TxReject NotFromSpecialAccount, energyCost, usedEnergy)

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
      -- check that a registration id does not yet exist
      let regId = ID.cdvRegId cdv
      regIdEx <- accountRegIdExists regId
      if regIdEx then
        return $! (Just (TxInvalid (DuplicateAccountRegistrationID (ID.cdvRegId cdv))))
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
                  let account = newAccount accountKeys aaddr
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


-- * Exposed methods.

-- |Make a valid block out of a list of transactions, respecting the given
-- maximum block size and block energy limit.
--
-- The preconditions of this function (which are not checked) are:
--
-- * The transactions appear grouped by their associated account address,
--   and the transactions in each group are ordered by increasing transaction nonce.
-- * Each transaction's nonce is equal or higher than the next nonce of the specified sender's
--   account (if the account it exists).
--
-- The 'GroupedTransactions' ('perAccountTransactions'
-- and 'credentialDeployments') are processed in order of their arrival time, assuming that both lists
-- are ordered by arrival time from earliest to latest. For each group in 'perAccountTransactions', only the time of the first transaction in the group is considered and the entire group is processed in one sequence.
--
-- = Processing of transactions
--
-- Processing starts with an initial remaining block energy being the maximum block energy
-- (birk parameter) and the remaining block size being as specified by the parameter to this function.
--
-- Each transaction or credential deployment is processed as follows:
--
-- * It is checked whether the deposited energy (or in case of credential deployment the respective energy cost) is not greater than the maximum block energy (in which case the transaction fails with 'ExceedsMaxBlockEnergy').
-- * It is checked whether the deposited energy (or, in case of credential deployment, the respective
--   energy cost) and the transaction size is not greater than the remaining block energy / block size
-- (in which case the transaction is skipped and added to the list of unprocessed transactions/credentials).
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
-- * If a transaction fails with a different failure kind than 'NonSequentialNonce', and none
--   of the following transactions replace it (having the same nonce and being valid),
--   all transactions with a higher nonce than that of the failed
--   will fail with 'SuccessorOfInvalidTransaction' instead of 'NonSequentialNonce'.
-- * If a transaction fails with 'NonSequentialNonce' (that is, by precondition,
--   it has a higher nonce than the expected nonce), all following transactions with an
--   incorrect nonce will also fail with 'NonSequentialNonce'.
-- * Transactions with the same nonce as a previously failed transaction are processed normally.
--
-- Note that this behaviour relies on the precondition of transactions within a group coming from the same account and being ordered by increasing nonce.
--
-- = Result
-- The order of transactions in 'ftAdded' (this includes credential deployments) is in the order of processing as described above.
-- However, there is no guarantee for any order in `ftFailed`, `ftFailedCredentials`, `ftUnprocessed` and `ftUnprocessedCredentials`.
filterTransactions :: forall m . (SchedulerMonad m)
                   => Integer -- ^Maximum block size in bytes.
                   -> GroupedTransactions Transaction -- ^Transactions to make a block out of.
                   -> m FilteredTransactions
filterTransactions maxSize GroupedTransactions{..} = do
  maxEnergy <- getMaxBlockEnergy

  runNext maxEnergy 0 emptyFilteredTransactions credentialDeployments perAccountTransactions
  where
        runNext :: Energy -- ^Maximum block energy
                -> Integer -- ^Current size of transactions in the block.
                -> FilteredTransactions -- ^Currently accummulated result
                -> [CredentialDeploymentWithMeta] -- ^Credentials to process
                -> [[Transaction]] -- ^Transactions to process, grouped per account.
                -> m FilteredTransactions
        runNext maxEnergy size fts credentials remainingTransactions =
          case (credentials, remainingTransactions) of
            -- need to reverse because we accummulated in reverse (for performance reasons)
            ([], []) -> return fts{ ftAdded = reverse (ftAdded fts )}
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
            -- run a single credential and continue
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
                      let newFts = fts { ftAdded = (fmap CredentialDeployment c, summary) : ftAdded fts}
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

            -- run all transactions in a group
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
              if csize <= maxSize && cenergy <= maxEnergy then -- if the next transaction can fit into a block then add it.
                observeTransactionFootprint (dispatch t) >>= \case
                   (Just (TxValid summary), fp) -> do
                     markEnergyUsed (tsEnergyCost summary)
                     tlNotifyAccountEffect fp summary
                     let newFts = currentFts { ftAdded = (fmap NormalTransaction t, summary) : ftAdded currentFts}
                     runTransactionGroup csize newFts remainingGroups ts
                   (Just (TxInvalid reason), _) ->
                     let (newFts, rest) = invalidTs t reason currentFts ts
                     in runTransactionGroup size newFts remainingGroups rest
                   (Nothing, _) -> error "Unreachable. Dispatch honors maximum transaction energy."
              -- if the stated energy of a single transaction exceeds the block energy limit the transaction is invalid
              -- don't process any of the rest of the group
              else if tenergy > maxEnergy then
                let (newFts, rest) = invalidTs t ExceedsMaxBlockEnergy currentFts ts
                in runTransactionGroup size newFts remainingGroups rest
              else -- otherwise still try the remaining transactions in the group to avoid deadlocks from
                   -- one single too-big transaction.
                let newFts = currentFts { ftUnprocessed = t : (ftUnprocessed currentFts) }
                in runTransactionGroup size newFts remainingGroups ts

            -- group processed, continue with the next group or credential
            runTransactionGroup currentSize currentFts remainingGroups [] =
              runNext maxEnergy currentSize currentFts credentials remainingGroups

            -- determine whether there is any point in processing more transactions from this group.
            invalidTs t failure currentFts ts =
              -- FIXME: relying on laziness for the following to not be very expensive
              case takeWhile ((== transactionNonce t) . transactionNonce) ts of
                [] -> -- all remaining transactions have higher nonce, all will fail
                 (currentFts { ftFailed =  (t, failure) : map (, SuccessorOfInvalidTransaction) ts ++ ftFailed currentFts}, [])
                _ -> (currentFts { ftFailed = (t, failure) : ftFailed currentFts }, ts)

-- |Execute transactions in sequence. Returns
--
-- * 'Left Nothing' if maximum block energy limit was exceeded
-- * 'Left (Just fk)' if a transaction failed with the given failure kind
-- * 'Right outcomes' if all transactions are successful, with given outcomes.
runTransactions :: forall m .
                (SchedulerMonad m)
                => [BlockItem]
                -> m (Either (Maybe FailureKind) [(BlockItem, TransactionSummary)])
runTransactions = go []
    where go valid (bi:ts) = do
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

-- |Execute transactions in sequence only for sideffects on global state.
-- Returns @Right energy@ if block executed successfully (where energy is the
-- used energy), and 'Left' 'FailureKind' at first failed transaction. This is
-- more efficient than 'runTransactions' since it does not have to build a list
-- of results.
execTransactions :: forall m . (SchedulerMonad m)
                 => [BlockItem]
                 -> m (Either (Maybe FailureKind) ())
execTransactions = go
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
