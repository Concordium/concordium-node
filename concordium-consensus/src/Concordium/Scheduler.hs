{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler
  (filterTransactions
  ,runTransactions
  ,execTransactions
  ) where

import qualified Acorn.TypeCheck as TC
import qualified Acorn.Interpreter as I
import qualified Acorn.Core as Core
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment

import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Bakers(bakerSignatureVerifyKey)
import qualified Concordium.GlobalState.Instances as Ins
import qualified Concordium.Scheduler.Cost as Cost

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)
import qualified Data.Set as Set

import Control.Exception(assert)

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

-- |Check that the transaction has a valid sender, and that the amount they have
-- deposited is on their account.
-- Return the sender account as well as the deposited amount converted from the dedicated gas amount.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> m (Either FailureKind (Amount, Account))
checkHeader meta =
  if transactionGasAmount meta < Cost.minimumDeposit then return (Left DepositInsufficient)
  else do
    macc <- getAccount (transactionSender meta)
    case macc of
      Nothing -> return (Left (UnknownAccount (transactionSender meta)))
      Just acc ->
        let amnt = acc ^. accountAmount
            nextNonce = acc ^. accountNonce
            txnonce = transactionNonce meta
        -- NB: checking txnonce = nextNonce should also make sure that the nonce >= minNonce
        in do depositedAmount <- energyToGtu (transactionGasAmount meta)
              return (runExcept $! do
                       -- check they have enough funds to cover the deposit
                       unless (depositedAmount <= amnt) (throwError InsufficientFunds)
                       unless (txnonce == nextNonce) (throwError (NonSequentialNonce nextNonce))
                       let sigCheck = verifyTransactionSignature' (acc ^. accountVerificationKey) -- the signature is correct.
                                                                  meta
                                                                  (transactionSignature meta)
                       assert sigCheck (return (depositedAmount, acc))) -- only use assert because we rely on the signature being valid in the transaction table
                       -- unless sigCheck (throwError IncorrectSignature))
        -- TODO: If we are going to check that the signature is correct before adding the transaction to the table then this check can be removed,
        -- but only for transactions for which this was done.
        -- One issue is that if we don't include the public key with the transaction then we cannot do this, which is especially problematic for transactions
        -- which come as part of blocks.


-- |Given the deposited amount and the remaining amount of gas compute how much
-- the sender of the transaction should be charged. Used for rejected transactions.
computeRejectedCharge :: SchedulerMonad m => TransactionHeader -> Energy -> m Amount
computeRejectedCharge meta energy = energyToGtu (thGasAmount meta - energy)

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

dispatch :: (TransactionData msg, SchedulerMonad m) => msg -> m TxResult
dispatch msg = do
  let meta = transactionHeader msg
  validMeta <- checkHeader msg
  case validMeta of
    Left fk -> return $ TxInvalid fk
    Right (depositedAmount, senderAccount) -> do
      let oldAmount = senderAccount ^. accountAmount
      -- at this point the transaction is going to be commited to the block. Hence we can increase the
      -- account nonce of the sender account.
      increaseAccountNonce (thSender meta)

      -- then we notify the block state that all the identity issuers on the sender's account should be rewarded
      -- TODO: Alternative design would be to only reward them if the transaction is successful/committed, or
      -- to add additional parameters (such as deposited amount)
      mapM_ (notifyIdentityProviderCredential . ID.cdvIpId) (senderAccount ^. accountCredentials)

      let cost = Cost.checkHeader
      let energy = thGasAmount meta - cost  -- the remaining gas (after subtracting cost to process the header)
      -- available for execution remaining amount available on the sender's
      -- account. This is deducted prior to execution and refunded at the end,
      -- if there is any left.
      let remainingAmount = oldAmount - depositedAmount
      let psize = payloadSize (transactionPayload msg)
      case decodePayload (transactionPayload msg) of
        Left err -> do
          -- in case of serialization failure we charge the sender for checking the header and reject the transaction
          payment <- energyToGtu cost
          chargeExecutionCost (thSender meta) payment
          return $ TxValid $ TxReject (SerializationFailure err)
        Right payload -> 
          case payload of
            DeployModule mod ->
              handleDeployModule meta remainingAmount psize mod energy

            InitContract amount modref cname param paramSize ->
              handleInitContract meta remainingAmount amount modref cname param paramSize energy
            -- FIXME: This is only temporary for now.
            -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
            Transfer toaddr amount ->
              handleSimpleTransfer meta remainingAmount toaddr amount energy
 
            Update amount cref maybeMsg msgSize ->
              handleUpdateContract meta remainingAmount cref amount maybeMsg msgSize energy
           
            DeployCredential cdi ->
              handleDeployCredential meta (payloadBodyBytes (transactionPayload msg)) cdi energy
            
            DeployEncryptionKey encKey ->
              handleDeployEncryptionKey meta senderAccount encKey energy

            AddBaker{..} ->
              handleAddBaker meta senderAccount abElectionVerifyKey abSignatureVerifyKey abAccount abProof energy

            RemoveBaker{..} ->
              handleRemoveBaker meta senderAccount rbId rbProof energy

            UpdateBakerAccount{..} ->
              handleUpdateBakerAccount meta senderAccount ubaId ubaAddress ubaProof energy

            UpdateBakerSignKey{..} ->
              handleUpdateBakerSignKey meta senderAccount ubsId ubsKey ubsProof energy
            
            DelegateStake{..} ->
              handleDelegateStake meta senderAccount (Just dsID) energy
            
            UndelegateStake ->
              handleDelegateStake meta senderAccount Nothing energy

-- |Process the deploy module transaction.
handleDeployModule ::
  SchedulerMonad m
  => TransactionHeader -- ^Header of the transaction.
  -> Amount -- ^Amount remaining on sender's account (sender being the account initiating the transaction).
  -> Int -- ^Serialized size of the module. Used for charging execution cost.
  -> Module -- ^The module to deploy
  -> Energy -- ^The amount of energy this transaction is allowed to consume.
  -> m TxResult
handleDeployModule meta remainingAmount psize mod energy = do
  (res, (energy', cs)) <- runLocalTWithAmount (thSender meta) remainingAmount (handleModule meta psize mod) energy
  case res of
    Left reason -> do payment <- computeRejectedCharge meta energy'
                      chargeExecutionCost (thSender meta) payment
                      return (TxValid (TxReject reason))
    Right (mhash, iface, viface) -> do
      refund <- energyToGtu energy'
      let execCost = thGasAmount meta - energy'
      energyToGtu execCost >>= notifyExecutionCost
      commitStateAndAccountChanges (increaseAmountCS cs (thSender meta) refund)
      b <- commitModule mhash iface viface mod
      if b then do
        return $ (TxValid $ TxSuccess [ModuleDeployed mhash])
      else do
        -- FIXME:
        -- we should reject the transaction immediately if we figure out that the module with the hash already exists.
        -- otherwise we can waste some effort in checking before reaching this point.
        -- This could be chedked immediately even before we reach the dispatch since module hash is the hash of module serialization.
        return $ TxValid (TxReject (ModuleHashAlreadyExists mhash))


-- |TODO: Figure out whether we need the metadata or not here.
handleModule :: TransactionMonad m => TransactionHeader -> Int -> Module -> m (Core.ModuleRef, Interface, ValueInterface)
handleModule _meta msize mod = do
  -- Consume the gas amount required for processing.
  -- This is done even if the transaction is rejected in the end.
  -- NB: The next line will reject the transaction in case there are not enough funds.
  tickEnergy (Cost.deployModule msize)
  imod <- pure (runExcept (Core.makeInternal mod)) `rejectingWith'` MissingImports
  iface <- runExceptT (TC.typeModule imod) `rejectingWith'` ModuleNotWF
  let mhash = Core.imRef imod
  viface <- runMaybeT (I.evalModule imod) `rejectingWith` EvaluationError
  return (mhash, iface, viface)

-- |Handle the top-level initialize contract.
handleInitContract ::
  SchedulerMonad m
    => TransactionHeader -- ^Header of the transaction.
    -> Amount   -- ^Remaining amount on the sender's account (sender being the invoker of the top-level transaction).
    -> Amount   -- ^The amount to initialize the contract with.
    -> ModuleRef  -- ^Module reference of the contract to initialize.
    -> Core.TyName  -- ^Name of the contract in a module.
    -> Core.Expr Core.UA Core.ModuleName  -- ^Parameters of the contract.
    -> Int -- ^Serialized size of the parameters. Used for computing typechecking cost.
    -> Energy -- ^Amount of energy this transaction can consume
    -> m TxResult
handleInitContract meta remainingAmount amount modref cname param paramSize energy = do
  (result, (energy', cs)) <- runLocalTWithAmount (thSender meta) remainingAmount (handleInit meta remainingAmount amount modref cname param paramSize) energy
  case result of
    Left reason -> do 
      payment <- computeRejectedCharge meta energy'
      chargeExecutionCost (thSender meta) payment
      return $ TxValid (TxReject reason)
    Right (contract, iface, viface, msgty, model, initamount) -> do
        refund <- energyToGtu energy'
        let execCost = thGasAmount meta - energy'
        energyToGtu execCost >>= notifyExecutionCost
        -- The sender is paid the refund, but charged the initalamount.
        commitStateAndAccountChanges (modifyAmountCS cs (thSender meta) (amountDiff refund initamount))
        let ins = makeInstance modref cname contract msgty iface viface model initamount (thSender meta)
        addr <- putNewInstance ins
        return (TxValid $ TxSuccess [ContractInitialized modref cname addr])

handleInit
  :: (TransactionMonad m, InterpreterMonad NoAnnot m)
     => TransactionHeader
     -> Amount
     -> Amount
     -> Core.ModuleRef
     -> Core.TyName
     -> Core.Expr Core.UA Core.ModuleName
     -> Int
     -> m (ContractValue, Interface, ValueInterface, Core.Type Core.UA Core.ModuleRef, Value, Amount)
handleInit meta senderAmount amount modref cname param paramsize = do
  -- decrease available energy and start processing. This will reject the transaction if not enough is available.
  tickEnergy Cost.initPreprocess

  -- if the sender does not have the funds available we fail the transaction immediately
  unless (senderAmount >= amount) $! rejectTransaction (AmountTooLarge (AddressAccount (thSender meta)) amount)

  -- otherwise we proceed with normal execution.
  -- first try to get the module interface of the parent module of the contract
  (iface, viface) <- getModuleInterfaces modref `rejectingWith` InvalidModuleReference modref
  -- and then the particular contract interface (in particular the type of the init method)
  ciface <- pure (Map.lookup cname (exportedContracts iface)) `rejectingWith` InvalidContractReference modref cname
  -- first typecheck the parameters, whether they have the expected type
  -- the cost of type-checking is dependent on the size of the term
  tickEnergy (Cost.initParamsTypecheck paramsize)
  qparamExp <- runExceptT (TC.checkTyInCtx iface param (paramTy ciface)) `rejectingWith'` ParamsTypeError
  let contract = (exportedDefsConts viface) Map.! cname -- NB: The unsafe Map.! is safe here because we do know the contract exists by the invariant on viface and iface
      initFun = initMethod contract
  params' <- fromJust <$> runMaybeT (link (exportedDefsVals viface) qparamExp) -- linking must succeed because type-checking succeeded
  cm <- getChainMetadata
  res <- runInterpreter (I.applyInitFun cm (InitContext (thSender meta)) initFun params' (thSender meta) amount)
  return (contract, iface, viface, (msgTy ciface), res, amount)

handleSimpleTransfer ::
  SchedulerMonad m
    => TransactionHeader -- ^Header of the transaction.
    -> Amount -- ^Remaing amount on the sender's account.
    -> Address -- ^Address to send the amount to, either account or contract.
    -> Amount -- ^The amount to transfer.
    -> Energy -- ^Maximum amount of energy allowed for this transaction.
    -> m TxResult
handleSimpleTransfer meta remainingAmount toaddr amount energy = do
  (res, (energy', changeSet)) <- runLocalTWithAmount (thSender meta) remainingAmount (handleTransfer meta remainingAmount amount toaddr) energy
  case res of
    Right events -> do
      refund <- energyToGtu energy'
      let execCost = thGasAmount meta - energy'
      energyToGtu execCost >>= notifyExecutionCost
      commitStateAndAccountChanges (increaseAmountCS changeSet (thSender meta) refund)
      return $ TxValid $ TxSuccess events
    Left reason -> do
      payment <- computeRejectedCharge meta energy'
      chargeExecutionCost (thSender meta) payment
      return $ TxValid (TxReject reason)

handleUpdateContract ::
  SchedulerMonad m
    => TransactionHeader -- ^Header of the transaction.
    -> Amount -- ^Remaing amount on the sender's account.
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> Core.Expr Core.UA Core.ModuleName -- ^Message to send to the receive method.
    -> Int  -- ^Serialized size of the message.
    -> Energy -- ^Amount of energy this transaction is allowed to consume.
    -> m TxResult
handleUpdateContract meta remainingAmount cref amount maybeMsg msgSize energy = do
  (result, (energy', changeSet)) <- runLocalTWithAmount (thSender meta) remainingAmount (handleUpdate meta remainingAmount amount cref maybeMsg msgSize) energy
  case result of
    Right events -> do
       refund <- energyToGtu energy'
       let execCost = thGasAmount meta - energy'
       energyToGtu execCost >>= notifyExecutionCost
       commitStateAndAccountChanges (increaseAmountCS changeSet (thSender meta) refund)
       return $ TxValid $ TxSuccess events
    Left reason -> do
        payment <- computeRejectedCharge meta energy'
        chargeExecutionCost (thSender meta) payment
        return $ TxValid (TxReject reason)
 
handleUpdate
  :: (TransactionMonad m, InterpreterMonad NoAnnot m)
     => TransactionHeader
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> ContractAddress
     -> Core.Expr Core.UA Core.ModuleName
     -> Int
     -> m [Event]
handleUpdate meta accountamount amount cref msg msgSize = do

  tickEnergy Cost.updatePreprocess

  i <- getCurrentContractInstance cref `rejectingWith` InvalidContractAddress cref
  let rf = Ins.ireceiveFun i
      msgType = Ins.imsgTy i
      (iface, viface) = Ins.iModuleIface i
      model = Ins.instanceModel i
      contractamount = Ins.instanceAmount i
      -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
  do tickEnergy (Cost.updateMessageTypecheck msgSize)
     qmsgExp <- runExceptT (TC.checkTyInCtx iface msg msgType) `rejectingWith'` MessageTypeError
     qmsgExpLinked <- fromJust <$> runMaybeT (link (exportedDefsVals viface) qmsgExp)
     handleTransaction (thSender meta)
                       cref
                       rf
                       (AddressAccount (thSender meta))
                       accountamount
                       amount
                       (ExprMessage (I.mkJustE qmsgExpLinked))
                       model
                       contractamount

-- this will always be run when we know that the contract exists and we can lookup its local state
handleTransaction ::
  (TransactionMonad m, InterpreterMonad NoAnnot m)
  => AccountAddress -- ^the origin account of the top-level transaction
  -> ContractAddress -- ^the target contract of the transaction
  -> Expr NoAnnot -- ^the receive function of the contract
  -> Address -- ^the invoker of this particular transaction, in general different from the origin
  -> Amount -- ^amount of funds on the sender's account before the execution
  -> Amount -- ^amount that was sent to the contract in the transaction
  -> MessageFormat -- ^message wrapped in a Maybe, at top level it will be an expression, and in nested calls a value
  -> Value -- ^current local state of the target contract
  -> Amount -- ^current amount of the target contract
  -> m [Event]
handleTransaction origin cref receivefun txsender senderamount transferamount maybeMsg model contractamount = do
  -- a transaction is rejected in case we try to transfer amounts we don't have.
  -- This rejection is different from rejection by a contract, but the effect is the same.
  -- FIXME: Possibly this will need to be changed.
  unless (senderamount >= transferamount) $ rejectTransaction (AmountTooLarge txsender transferamount)

  cm <- getChainMetadata
  let receiveCtx = ReceiveContext { invoker = origin, selfAddress = cref }
  result <- case maybeMsg of
              ValueMessage m -> runInterpreter (I.applyReceiveFunVal cm receiveCtx receivefun model txsender transferamount m)
              ExprMessage m ->  runInterpreter (I.applyReceiveFun cm receiveCtx receivefun model txsender transferamount m)
  case result of
    Nothing -> -- transaction rejected, no other changes were recorder in the global state (in particular the amount was not transferred)
      rejectTransaction Rejected -- transaction rejected due to contract logic
    Just (newmodel, txout) ->
      let (contractamount', senderamount') =
            case txsender of
              AddressContract addr | addr == cref ->
                  -- if sender and receiver are the same then we need to be a bit careful
                  -- the amounts do not in fact change in this case
                  (contractamount, senderamount)
                  -- otherwise we increase the receiver's, and decrease the sender's amounts 
              _ -> (contractamount + transferamount, senderamount - transferamount)
      in
      -- decrease the funds on the senders account or contract
      withAmount txsender senderamount' $ do
        -- and update the amount and local state of the receiving contract
        withInstance cref contractamount' newmodel $ do
          -- and then process the generated messages in the new context in sequence from left to right, depth first.
          foldM (\res tx -> combineTx res $ do
                    tickEnergy Cost.interContractMessage -- Charge a small amount just for the fact that a message was generated.
                    -- we need to get the fresh amount each time since it might have changed for each execution
                    -- NB: fromJust is justified since at this point we know the sender contract exists
                    senderamount'' <- (Ins.instanceAmount . fromJust) <$> getCurrentContractInstance cref
                    case tx of
                      TSend cref' transferamount' message' -> do
                        -- the only way to send is to first check existence, so this must succeed
                        cinstance <- fromJust <$> getCurrentContractInstance cref' 
                        let receivefun' = Ins.ireceiveFun cinstance
                        let model' = Ins.instanceModel cinstance
                        handleTransaction origin
                                          cref'
                                          receivefun'
                                          (AddressContract cref)
                                          senderamount''
                                          transferamount'
                                          (ValueMessage (I.aJust message'))
                                          model'
                                          (Ins.instanceAmount cinstance)
                      -- simple transfer to a contract is the same as a call to update with Nothing
                      TSimpleTransfer (AddressContract cref') transferamount' -> do
                        cinstance <- fromJust <$> getCurrentContractInstance cref' -- the only way to send is to first check existence, so this must succeed
                        let receivefun' = Ins.ireceiveFun cinstance
                        let model' = Ins.instanceModel cinstance
                        handleTransaction origin
                                          cref'
                                          receivefun'
                                          (AddressContract cref)
                                          senderamount''
                                          transferamount'
                                          (ValueMessage I.aNothing)
                                          model'
                                          (Ins.instanceAmount cinstance)
                      TSimpleTransfer (AddressAccount acc) transferamount' -> do -- FIXME: This is temporary until accounts have their own functions
                        handleTransferAccount origin acc (AddressContract cref) senderamount'' transferamount'
                        )
              [Updated cref transferamount maybeMsg] txout

combineTx :: Monad m => [Event] -> m [Event] -> m [Event]
combineTx x ma = (x ++) <$> ma

handleTransfer
  :: (TransactionMonad m, InterpreterMonad NoAnnot m)
     => TransactionHeader
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> Address -- either account or contract
     -> m [Event]
handleTransfer meta accountamount amount addr =
  case addr of
    AddressContract cref -> do
      i <- getCurrentContractInstance cref `rejectingWith` InvalidContractAddress cref
      let rf = Ins.ireceiveFun i
          model = Ins.instanceModel i
          contractamount = Ins.instanceAmount i
          -- we assume that gasAmount was available on the account due to the previous check (checkHeader)

          qmsgExpLinked = I.mkNothingE -- give Nothing as argument to the receive method
      handleTransaction (thSender meta)
                        cref
                        rf
                        (AddressAccount (thSender meta))
                        accountamount
                        amount
                        (ExprMessage (I.mkJustE qmsgExpLinked)) model contractamount

    AddressAccount acc ->
      handleTransferAccount (thSender meta) acc (AddressAccount (thSender meta)) accountamount amount

-- |TODO: Figure out whether we need the origin information in here (i.e., whether an account can observe it).
handleTransferAccount ::
  TransactionMonad m
  => AccountAddress -- the origin account of the top-level transaction
  -> AccountAddress -- the target account
  -> Address -- the invoker of this particular transaction, in general different from the origin
  -> Amount -- amount on the sender's account or contract instance
  -> Amount -- amount that was sent in the transaction
  -> m [Event]
handleTransferAccount _origin acc txsender senderamount amount = do
  -- the sender must have the amount available. Otherwise we reject the transaction immediately.
  unless (senderamount >= amount) $! rejectTransaction (AmountTooLarge txsender amount)

  -- check if target account exists and get its public balance, 
  targetAmount <- getCurrentAmount acc `rejectingWith` InvalidAccountReference acc
  -- FIXME: Should pay for execution here as well.

  -- and if we have the funds after subtracting the deposit for energy costs we can proceed with the transaction
  case txsender of
    AddressAccount acc' | acc == acc' -> do -- if sender and receiver are the same then we do nothing
      return [Transferred txsender amount (AddressAccount acc)]
    _ ->  -- and otherwise
      withAmount txsender (senderamount - amount) $ -- decrease sender's amount
          withAmount (AddressAccount acc) (targetAmount + amount) $ do -- NB: Consider whether an overflow can happen
            return [Transferred txsender amount (AddressAccount acc)]

-- |Run the interpreter with the remaining amount of energy. If the interpreter
-- runs out of gas set the remaining gas to 0 and reject the transaction,
-- otherwise decrease the consumed amount of gas and return the result.
{-# INLINE runInterpreter #-}
runInterpreter :: TransactionMonad m => (Energy -> m (Maybe (a, Energy))) -> m a
runInterpreter f = do
  getEnergy >>= f >>= \case Just (x, energy') -> x <$ putEnergy energy'
                            Nothing -> putEnergy 0 >> rejectTransaction OutOfEnergy

-- |TODO: Figure out what context information will the check that
-- a credential is valid need. Presumably public keys of id providers.
handleDeployCredential ::
  SchedulerMonad m
    =>
    -- |Header of the transaction.
    TransactionHeader ->
    -- |Credentials to deploy in serialized form. We pass these to the verify function.
    AH.CredentialDeploymentInformationBytes ->
    -- |Credentials to deploy.
    ID.CredentialDeploymentInformation ->
    -- |Amount of energy allowed for execution of this transaction.
    Energy ->
    m TxResult
handleDeployCredential meta cdiBytes cdi energy = do
  if Cost.deployCredential > energy then do
     payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas
     chargeExecutionCost (thSender meta) payment
     return $! TxValid (TxReject OutOfEnergy)
   else do
     let cdv = ID.cdiValues cdi
     payment <- energyToGtu (Cost.deployCredential + Cost.checkHeader) -- charge for checking header and deploying credential
     chargeExecutionCost (thSender meta) payment
     -- check that a registration id does not yet exist
     regIdEx <- accountRegIdExists (ID.cdvRegId cdv)
     if regIdEx then
       return $! TxValid $ TxReject $ DuplicateAccountRegistrationID (ID.cdvRegId cdv)
     else
       -- We now look up the identity provider this credential is derived from.
       -- Of course if it does not exist we reject the transaction.
       getIPInfo (ID.cdvIpId cdv) >>= \case
         Nothing -> return $! TxValid $ TxReject $ NonExistentIdentityProvider (ID.cdvIpId cdv)
         Just IdentityProviderData{ipArInfo=AnonymityRevokerData{..},..} -> do
            CryptographicParameters{..} <- getCrypoParams
            -- first check whether an account with the address exists in the global store
            let aaddr = AH.accountAddress (ID.cdvVerifyKey cdv) (ID.cdvSigScheme cdv)
            getAccount aaddr >>= \case
              Nothing ->  -- account does not yet exist, so create it, but we need to be careful
                let account = newAccount (ID.cdvVerifyKey cdv) (ID.cdvSigScheme cdv)
                in if AH.verifyCredential
                      elgamalGenerator
                      attributeCommitmentKey
                      ipVerifyKey
                      arElgamalGenerator
                      arPublicKey
                      cdiBytes then do
                     _ <- putNewAccount account -- first create new account, but only if credential was valid.
                                                -- We know the address does not yet exist.
                     addAccountCredential aaddr cdv  -- and then add the credentials
                     return $! TxValid (TxSuccess [AccountCreated aaddr, CredentialDeployed cdi])
                   else return $! TxValid $ TxReject AccountCredentialInvalid
     
              Just _ -> -- otherwise we just try to add a credential to the account
                        if AH.verifyCredential
                           elgamalGenerator
                           attributeCommitmentKey
                           ipVerifyKey
                           arElgamalGenerator
                           arPublicKey
                           cdiBytes then do
                          addAccountCredential aaddr cdv
                          return $! TxValid $ TxSuccess [CredentialDeployed cdi]
                        else
                          return $! TxValid $ TxReject AccountCredentialInvalid

handleDeployEncryptionKey ::
  SchedulerMonad m
    => TransactionHeader -- ^Header of the transaction.
    -> Account -- ^Account onto which the encryption key should be deployed.
    -> ID.AccountEncryptionKey -- ^The encryption key.
    -> Energy -- ^Amount of energy allowed for the execution of this transaction.
    -> m TxResult
handleDeployEncryptionKey meta senderAccount encKey energy = do
  if Cost.deployEncryptionKey > energy then do
    payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas amount
    chargeExecutionCost (thSender meta) payment
    return $! TxValid (TxReject OutOfEnergy)
  else do
    payment <- energyToGtu (Cost.deployEncryptionKey + Cost.checkHeader) -- charge for checking header and executing this particular transaction type
    chargeExecutionCost (thSender meta) payment
    case senderAccount ^. accountEncryptionKey of
      Nothing -> do
        let aaddr = senderAccount ^. accountAddress
        addAccountEncryptionKey aaddr encKey
        return . TxValid . TxSuccess $ [AccountEncryptionKeyDeployed aaddr encKey]
      Just encKey' -> return . TxValid . TxReject $ AccountEncryptionKeyAlreadyExists (senderAccount ^. accountAddress) encKey'


-- FIXME: The baker handling is purely proof-of-concept.
-- In particular there is no checking of proofs that the baker holds relevant
-- private keys, etc, and the precise logic for when a baker can be added and removed 
-- should be analyzed from a security perspective.


-- |The following functions are placeholders until we have sigma protocols and
-- can check these proofs.
checkElectionKeyProof :: BakerElectionVerifyKey -> Proof -> Bool             
checkElectionKeyProof _ _ = True

checkSignatureVerifyKeyProof :: BakerSignVerifyKey -> Proof -> Bool             
checkSignatureVerifyKeyProof _ _ = True

checkAccountOwnership :: SigScheme.SchemeId -> ID.AccountVerificationKey -> Proof -> Bool             
checkAccountOwnership _ _ _ = True


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
    => TransactionHeader
    -> Account
    -> BakerElectionVerifyKey
    -> BakerSignVerifyKey
    -> AccountAddress
    -> Proof
    -> Energy
    -> m TxResult
handleAddBaker meta _senderAccount abElectionVerifyKey abSignatureVerifyKey abAccount abProof energy = do
  if Cost.addBaker > energy then do
     payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas
     chargeExecutionCost (thSender meta) payment
     return $! TxValid (TxReject OutOfEnergy)
   else do
     payment <- energyToGtu (Cost.addBaker + Cost.checkHeader) -- charge for checking header and deploying credential
     chargeExecutionCost (thSender meta) payment
     
     getAccount abAccount >>=
       \case Nothing -> return $! TxValid (TxReject (NonExistentRewardAccount abAccount))
             Just Account{..} ->
               let electionP = checkElectionKeyProof abElectionVerifyKey abProof
                   signP = checkSignatureVerifyKeyProof abSignatureVerifyKey abProof
                   accountP = checkAccountOwnership _accountSignatureScheme _accountVerificationKey abProof
               in if electionP && signP && accountP then do
                    -- the proof validates that the baker owns all the private keys.
                    -- Moreover at this point we know the reward account exists and belongs
                    -- to the baker.
                    -- Thus we can create the baker, starting it off with 0 lottery power.
                    bid <- addBaker (BakerCreationInfo abElectionVerifyKey abSignatureVerifyKey abAccount)
                    return $! TxValid (TxSuccess [BakerAdded bid])
                  else return $! TxValid (TxReject InvalidProof)

-- |Remove a baker from the baker pool.
-- The current logic is that if the proof validates that the sender of the
-- transaction knows the baker's private key corresponding to its signature key.
handleRemoveBaker ::
  SchedulerMonad m
    => TransactionHeader
    -> Account
    -> BakerId
    -> Proof
    -> Energy
    -> m TxResult
handleRemoveBaker meta senderAccount rbId rbProof energy = 
  if Cost.removeBaker > energy then do
     payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas
     chargeExecutionCost (thSender meta) payment
     return $! TxValid (TxReject OutOfEnergy)
   else do
     payment <- energyToGtu (Cost.removeBaker + Cost.checkHeader) -- charge for checking header and deploying credential
     chargeExecutionCost (thSender meta) payment
     getBakerInfo rbId >>=
       \case Nothing ->
               return $ TxValid (TxReject (RemovingNonExistentBaker rbId))
             Just binfo ->
               if checkSignatureVerifyKeyProof (binfo ^. bakerSignatureVerifyKey) rbProof then do
                 -- only the baker itself can remove themselves from the pool
                 removeBaker rbId
                 return $ TxValid (TxSuccess [BakerRemoved rbId])
               else
                 return $ TxValid (TxReject (InvalidBakerRemoveSource (senderAccount ^. accountAddress)))

-- |Update the baker's reward account. The transaction is considered valid if
--
--  * The transaction is coming from the baker. This is established by
--    the sender of the transaction proving that they own the secret key
--    corresponding to the baker's signature verification key.
--  * The account they wish to set as their reward account exists.
--  * They own the account (meaning they know the private key corresponding to
--    the public key of the account)
-- TODO: Figure out (same as previous transaction) whether we need the sender account here
-- to validate the transaction.
handleUpdateBakerAccount ::
  SchedulerMonad m
    => TransactionHeader
    -> Account
    -> BakerId
    -> AccountAddress
    -> Proof
    -> Energy
    -> m TxResult
handleUpdateBakerAccount meta _senderAccount ubaId ubaAddress ubaProof energy = do
  if Cost.updateBakerAccount > energy then do
    payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas
    chargeExecutionCost (thSender meta) payment
    return $! TxValid (TxReject OutOfEnergy)
  else do
    payment <- energyToGtu (Cost.updateBakerAccount + Cost.checkHeader) -- charge for checking header and deploying credential
    chargeExecutionCost (thSender meta) payment
    getBakerInfo ubaId >>=
      \case Nothing ->
              return $ TxValid (TxReject (UpdatingNonExistentBaker ubaId))
            Just binfo ->
              if checkSignatureVerifyKeyProof (binfo ^. bakerSignatureVerifyKey) ubaProof then do
                -- only the baker itself can update its account
                -- now check the account exists and the baker owns it
                getAccount ubaAddress >>=
                  \case Nothing -> return $! TxValid (TxReject (NonExistentRewardAccount ubaAddress))
                        Just Account{..} ->
                          let accountP = checkAccountOwnership _accountSignatureScheme _accountVerificationKey ubaProof
                          in if accountP then do
                               updateBakerAccount ubaId ubaAddress
                               return $ TxValid (TxSuccess [BakerAccountUpdated ubaId ubaAddress])
                             else return $ TxValid (TxReject InvalidProof)
              else
                return $ TxValid (TxReject InvalidProof)

-- |Update the baker's public signature key. The transaction is considered valid if
--
--  * The transaction is coming from the baker. This is established by
--    the sender of the transaction proving that they own the secret key
--    corresponding to the baker's signature verification key.
--  * The transaction proves that they own the private key corresponding to the __NEW__
--    signature verification key.
-- Same as above, figure out whether we need the sender account.
handleUpdateBakerSignKey ::
  SchedulerMonad m
    => TransactionHeader
    -> Account
    -> BakerId
    -> BakerSignVerifyKey
    -> Proof
    -> Energy
    -> m TxResult
handleUpdateBakerSignKey meta _senderAccount ubsId ubsKey ubsProof energy = 
  if Cost.updateBakerKey > energy then do
    payment <- energyToGtu (thGasAmount meta) -- use up all the deposited gas
    chargeExecutionCost (thSender meta) payment
    return $! TxValid (TxReject OutOfEnergy)
  else do
    payment <- energyToGtu (Cost.updateBakerKey + Cost.checkHeader) -- charge for checking header and deploying credential
    chargeExecutionCost (thSender meta) payment
    getBakerInfo ubsId >>=
      \case Nothing ->
              return $ TxValid (TxReject (UpdatingNonExistentBaker ubsId))
            Just binfo ->
              if checkSignatureVerifyKeyProof (binfo ^. bakerSignatureVerifyKey) ubsProof then
                -- only the baker itself can update its own key
                -- now also check that they own the private key for the new signature key
                let signP = checkSignatureVerifyKeyProof ubsKey ubsProof -- FIXME: We will need a separate proof object here.
                in if signP then do
                     updateBakerSignKey ubsId ubsKey
                     return $ TxValid (TxSuccess [BakerKeyUpdated ubsId ubsKey])
                   else return $ TxValid (TxReject InvalidProof)
              else
                return $ TxValid (TxReject InvalidProof)

-- |Update an account's stake delegate.
handleDelegateStake ::
  SchedulerMonad m
    => TransactionHeader
    -> Account
    -> Maybe BakerId
    -> Energy
    -> m TxResult
handleDelegateStake meta senderAccount targetBaker energy =
  if delegateCost > energy then do
    payment <- energyToGtu (thGasAmount meta) -- use up all deposited gas
    chargeExecutionCost (thSender meta) payment
    return $! TxValid (TxReject OutOfEnergy)
  else do
    payment <- energyToGtu (delegateCost + Cost.checkHeader)
    chargeExecutionCost (thSender meta) payment
    res <- delegateStake (thSender meta) targetBaker
    return $! if res then
                TxValid (TxSuccess [maybe StakeUndelegated StakeDelegated targetBaker])
            else
                TxValid (TxReject (InvalidStakeDelegationTarget $ fromJust targetBaker))
  where
    delegateCost = Cost.updateStakeDelegate (Set.size $ senderAccount ^. accountInstances)

-- *Exposed methods.
-- |Make a valid block out of a list of transactions. The list is traversed from
-- left to right and any invalid transactions are not included in the block. The
-- return value is a pair of lists of transactions @(valid, invalid)@ where
--    * @valid@ transactions is the list of transactions that should appear
--      on the block in the order they should appear
--    * @invalid@ is a list of invalid transactions.
--    The order these transactions appear is arbitrary
--    (i.e., they do not necessarily appear in the same order as in the input).
filterTransactions :: (TransactionData msg, SchedulerMonad m)
                      => [msg] -> m ([(msg, ValidResult)], [(msg, FailureKind)])
filterTransactions = go [] []
  where go valid invalid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) invalid ts
            TxInvalid reason -> go valid ((t, reason):invalid) ts
        go valid invalid [] = return (reverse valid, invalid)

-- |Execute transactions in sequence. Return 'Nothing' if one of the transactions
-- fails, and otherwise return a list of transactions with their outcomes.
runTransactions :: (TransactionData msg, SchedulerMonad m)
                   => [msg] -> m (Either FailureKind [(msg, ValidResult)])
runTransactions = go []
  where go valid (t:ts) =
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) ts
            TxInvalid reason -> return (Left reason)
        go valid [] = return (Right (reverse valid))

-- |Execute transactions in sequence only for sideffects on global state.
-- Returns 'Right' '()' if block executed successfully, and 'Left' 'FailureKind' at
-- first failed transaction. This is more efficient than 'runTransactions' since it
-- does not have to build a list of results.
execTransactions :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Either FailureKind ())
execTransactions = go
  where go (t:ts) =
          dispatch t >>= \case
            TxValid _ -> go ts
            TxInvalid reason -> return (Left reason)
        go [] = return (Right ())
