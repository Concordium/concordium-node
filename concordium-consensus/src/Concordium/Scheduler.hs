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

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as ID

import qualified Concordium.GlobalState.Instances as Ins
import qualified Concordium.Scheduler.Cost as Cost

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)

import Control.Exception(assert)

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

getCurrentAmount :: TransactionMonad m => AccountAddress -> m (Maybe Amount)
getCurrentAmount addr = ((^. accountAmount) <$>) <$> getCurrentAccount addr

-- |Check that the transaction has a valid sender, and that the amount they have
-- deposited is on their account.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> m (Either FailureKind ())
checkHeader meta =
  if transactionGasAmount meta <= Cost.minimumDeposit then return (Left DepositInsufficient)
  else do
    macc <- getAccount (transactionSender meta)
    case macc of
      Nothing -> return (Left (UnknownAccount (transactionSender meta)))
      Just acc ->
        let amnt = acc ^. accountAmount
            nextNonce = acc ^. accountNonce
            txnonce = transactionNonce meta
        -- NB: checking txnonce = nextNonce should also make sure that the nonce >= minNonce
        in return (runExcept $! do
                       -- check they have enough funds to cover the deposit
                       unless (transactionGasAmount meta <= amnt) (throwError InsufficientFunds)
                       unless (txnonce == nextNonce) (throwError (NonSequentialNonce nextNonce))
                       let sigCheck = verifyTransactionSignature' (acc ^. accountCreationInformation . to ID.aci_verifKey) -- the signature is correct.
                                                                  meta
                                                                  (transactionSignature meta)
                       assert sigCheck (return ())) -- only use assert because we rely on the signature being valid in the transaction table
                       -- unless sigCheck (throwError IncorrectSignature))
        -- TODO: If we are going to check that the signature is correct before adding the transaction to the table then this check can be removed,
        -- but only for transactions for which this was done.
        -- One issue is that if we don't include the public key with the transaction then we cannot do this, which is especially problematic for transactions
        -- which come as part of blocks.


dispatch :: (TransactionData msg, SchedulerMonad m) => msg -> m TxResult
dispatch msg = do
  let meta = transactionHeader msg
  validMeta <- checkHeader msg
  case validMeta of
    Left fk -> return $ TxInvalid fk
    Right _ -> do
      -- at this point the transaction is going to be commited to the block. Hence we can increase the
      -- account nonce of the sender account.
      increaseAccountNonce (thSender meta)
      -- and decrease the amount on the account to pay for the header check, as well as reduce the amount of energy left
      -- from this point on this amount will never be recouped.
      _ <- payForExecution (thSender meta) Cost.checkHeader
      let energy = gtuToEnergy (thGasAmount meta) - Cost.checkHeader  -- the remaining gas available for execution
      let psize = payloadSize (transactionPayload msg)
      case decodePayload (transactionPayload msg) of -- FIXME: Before doing this need to charge some amount.
        Left err -> return $ TxValid $ TxReject (SerializationFailure err)
        Right payload -> 
          case payload of
            DeployModule mod -> do
              -- deposit the remaining amount
              _ <- payForExecution (thSender meta) energy -- ignore remaining amount, we don't need it for this particular transaction type
              (res, (energy', _)) <- runLocalT (handleModule meta psize mod) energy
              case res of
                Left reason -> refundEnergy (thSender meta) energy' >> return (TxValid (TxReject reason))
                Right (mhash, iface, viface) -> do
                  b <- commitModule mhash iface viface
                  if b then do
                    refundEnergy (thSender meta) energy'
                    return $ (TxValid $ TxSuccess [ModuleDeployed mhash])
                  else do
                    -- FIXME:
                    -- we should reject the transaction immediately if we figure out that the module with the hash already exists.
                    -- otherwise we can waste some effort in checking before reaching this point.
                    -- This could be chedked immediately even before we reach the dispatch since module hash is the hash of module serialization.
                    refundEnergy (thSender meta) energy
                    return $ TxValid (TxReject (ModuleHashAlreadyExists mhash))

            InitContract amount modref cname param paramSize -> do
              remainingAmount <- payForExecution (thSender meta) energy
              (result, (energy', _)) <- runLocalT (handleInit meta remainingAmount amount modref cname param paramSize) energy
              case result of
                Left reason -> return $ TxValid (TxReject reason)
                Right (contract, iface, viface, msgty, model, initamount) -> do
                    refundEnergy (thSender meta) energy'
                    let ins = makeInstance modref cname contract msgty iface viface model initamount
                    addr <- putNewInstance ins
                    return (TxValid $ TxSuccess [ContractInitialized modref cname addr])
      
            -- FIXME: This is only temporary for now.
            -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
            Transfer toaddr amount -> do
              remainingAmount <- payForExecution (thSender meta) energy
              (res, (energy', changeSet)) <- runLocalT (handleTransfer meta remainingAmount amount toaddr) energy
              case res of
                Right events -> do
                  commitStateAndAccountChanges changeSet
                  refundEnergy (thSender meta) energy'
                  return $ TxValid $ TxSuccess events
                Left reason -> do
                  refundEnergy (thSender meta) energy'
                  return $ TxValid (TxReject reason)

            Update amount cref maybeMsg msgSize -> do
              accountamount <- payForExecution (thSender meta) energy
              (result, (energy', changeSet)) <- runLocalT (handleUpdate meta accountamount amount cref maybeMsg msgSize) energy
              case result of
                Right events -> do
                   commitStateAndAccountChanges changeSet
                   refundEnergy (thSender meta) energy'
                   return $ TxValid $ TxSuccess events
                Left reason -> do
                    refundEnergy (thSender meta) energy'
                    return $ TxValid (TxReject reason)
  
            CreateAccount aci -> do
              -- before checking anything we check that the remaining amount of energy is sufficient to deploy the account.
              -- If not we simply reject the transaction, after we have withdrawn the deposit
              if Cost.deployAccount > energy then do
                _ <- payForExecution (thSender meta) energy
                return $! TxValid (TxReject OutOfEnergy)
              else do
                _ <- payForExecution (thSender meta) Cost.deployAccount
                -- first check if account with given registration does not already exist.
                ridExists <- accountRegIdExists (ID.aci_regId aci)
                if ridExists then return $ TxValid (TxReject (DuplicateAccountRegistrationID (ID.aci_regId aci)))
                else if AH.verifyAccount aci
                then do -- if account information is correct then we create the account with initial nonce 'minNonce', and 0 balance.
                  let aaddr = AH.accountAddress aci
                  let account = Account { _accountAddress = aaddr
                                        , _accountNonce = minNonce
                                        , _accountAmount = 0
                                        , _accountCreationInformation = aci }
                  r <- putNewAccount account
                  if r then
                    return $ TxValid (TxSuccess [AccountCreated aaddr])
                  else
                    return $ TxValid (TxReject (AccountAlreadyExists aaddr))
                else 
                  return $ TxValid (TxReject AccountCredentialsFailure)

handleModule :: TransactionMonad m => TransactionHeader -> Int -> Core.Module -> m (Core.ModuleRef, Interface, ValueInterface)
handleModule meta msize mod = do
  -- Consume the gas amount required for processing.
  -- This is done even if the transaction is rejected in the end.
  -- NB: The next line will reject the transaction in case there are not enough funds.
  tickEnergy (Cost.deployModule msize)
  imod <- pure (runExcept (Core.makeInternal mod)) `rejectingWith'` MissingImports
  iface <- runExceptT (TC.typeModule imod) `rejectingWith'` ModuleNotWF
  let mhash = Core.moduleHash mod
  viface <- runMaybeT (I.evalModule imod mhash) `rejectingWith` EvaluationError
  return (mhash, iface, viface)

handleInit
  :: (TransactionMonad m, InterpreterMonad m)
     => TransactionHeader
     -> Amount
     -> Amount
     -> Core.ModuleRef
     -> Core.TyName
     -> Core.Expr Core.ModuleName
     -> Int
     -> m (ContractValue, Interface, ValueInterface, Core.Type Core.ModuleRef, Value, Amount)
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

handleUpdate
  :: (TransactionMonad m, InterpreterMonad m)
     => TransactionHeader
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> ContractAddress
     -> Core.Expr Core.ModuleName
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
  (TransactionMonad m, InterpreterMonad m)
  => AccountAddress -- ^the origin account of the top-level transaction
  -> ContractAddress -- ^the target contract of the transaction
  -> Expr -- ^the receive function of the contract
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
                                          contractamount'
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
                                          contractamount'
                      TSimpleTransfer (AddressAccount acc) transferamount' -> do -- FIXME: This is temporary until accounts have their own functions
                        handleTransferAccount origin acc (AddressContract cref) senderamount'' transferamount'
                        )
              [Updated cref transferamount maybeMsg] txout

combineTx :: Monad m => [Event] -> m [Event] -> m [Event]
combineTx x ma = (x ++) <$> ma

handleTransfer
  :: (TransactionMonad m, InterpreterMonad m)
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

handleTransferAccount ::
  TransactionMonad m
  => AccountAddress -- the origin account of the top-level transaction
  -> AccountAddress -- the target account
  -> Address -- the invoker of this particular transaction, in general different from the origin
  -> Amount -- amount on the sender's account or contract instance
  -> Amount -- amount that was sent in the transaction
  -> m [Event]
handleTransferAccount origin acc txsender senderamount amount = do
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

-- *Exposed methods.
-- |Make a valid block out of a list of transactions. The list is traversed from
-- left to right and any invalid transactions are not included in the block. The
-- return value is a pair of lists of transactions @(valid, invalid)@ where
--    * @valid@ transactions is the list of transactions that should appear on the block in the order they should appear
--    * @invalid@ is a list of invalid transactions.
--    The order these transactions appear is arbitrary (i.e., they do not necessarily appear in the same order as in the input).
filterTransactions :: (TransactionData msg, SchedulerMonad m) => [msg] -> m ([(msg, ValidResult)], [(msg, FailureKind)])
filterTransactions = go [] []
  where go valid invalid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) invalid ts
            TxInvalid reason -> go valid ((t, reason):invalid) ts
        go valid invalid [] = return (reverse valid, invalid)

-- |Execute transactions in sequence. Return 'Nothing' if one of the transactions
-- fails, and otherwise return a list of transactions with their outcomes.
runTransactions :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Maybe [(msg, ValidResult)])
runTransactions = go []
  where go valid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) ts
            TxInvalid _ -> return Nothing
        go valid [] = return (Just (reverse valid))

-- |Execute transactions in sequence only for sideffects on global state.
-- Returns @Right ()@ if block executed successfully, and @Left@ @FailureKind@ at
-- first failed transaction. This is more efficient than 'runTransactions' since it
-- does not have to build a list of results.
execTransactions :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Either FailureKind ())
execTransactions = go
  where go (t:ts) = do
          dispatch t >>= \case
            TxValid _ -> go ts
            TxInvalid reason -> return (Left reason)
        go [] = return (Right ())
