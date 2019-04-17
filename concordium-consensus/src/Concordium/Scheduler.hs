{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler
  (makeValidBlock
  ,runBlock
  ,execBlock
  ) where

import qualified Acorn.TypeCheck as TC
import qualified Acorn.Interpreter as I
import qualified Acorn.Core as Core
import Concordium.Scheduler.Types
import Concordium.Scheduler.Environment

import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as ID

import qualified Concordium.GlobalState.Instances as Ins

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

getCurrentAmount :: TransactionMonad m => AccountAddress -> m (Maybe Amount)
getCurrentAmount addr = ((^. accountAmount) <$>) <$> getCurrentAccount addr

-- |Check that the transaction has a valid sender, and that the amount they have
-- deposited is on their account.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> m (Either FailureKind ())
checkHeader meta = do
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
                     let sigCheck = verifyTransactionSignature (acc ^. accountCreationInformation & ID.aci_verifKey) -- the signature is correct.
                                                               (transactionSerialized meta)
                                                               (transactionSignature meta)
                     unless sigCheck (throwError IncorrectSignature))
      -- TODO: If we are going to check that the signature is correct before adding the transaction to the table then this check can be removed,
      -- but only for transactions for which this was done.
      -- One issue is that if we don't include the public key with the transaction then we cannot do this, which is especially problematic for transactions
      -- which come as part of blocks.


dispatch :: (TransactionData msg, SchedulerMonad m) => msg -> m TxResult
dispatch msg = do
  let meta = transactionHeader msg
  let energy = gtuToEnergy (thGasAmount meta)
  validMeta <- checkHeader msg
  case validMeta of
    Left fk -> return $ TxInvalid fk
    Right _ -> do
      -- at this point the transaction is going to be commited to the block. Hence we can increase the
      -- account nonce of the sender account.
      increaseAccountNonce (thSender meta)
      case decodePayload (transactionPayload msg) of -- FIXME: Before doing this need to charge some amount.
        Left err -> return $ TxValid $ TxReject (SerializationFailure err)
        Right payload -> 
          case payload of
            DeployModule mod -> do
              _ <- payForExecution (thSender meta) energy -- ignore remaining amount, we don't need it for this particular transaction type
              res <- evalLocalT (handleModule meta mod energy)
              case res of
                (Left reason, energy') -> refundEnergy (thSender meta) energy' >> return (TxValid (TxReject reason))
                (Right (mhash, iface, viface), energy') -> do
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

            InitContract amount modref cname param -> do
              remainingAmount <- payForExecution (thSender meta) energy
              result <- evalLocalT (handleInit meta remainingAmount amount modref cname param energy)
              case result of
                (Left reason, _) -> return $ TxValid (TxReject reason)
                (Right (rmethod, iface, viface, msgty, model, initamount, impls), energy') -> do
                    refundEnergy (thSender meta) energy'
                    addr <- firstFreeAddress -- NB: It is important that there is no other thread adding contracts
                    let ins = let iaddress = addr
                                  ireceiveFun = rmethod
                                  iModuleIface = (iface, viface)
                                  imsgTy = msgty
                                  imodel = model
                                  iamount = initamount
                                  instanceImplements = impls
                              in Ins.Instance{..}
                    r <- putNewInstance ins
                    if r then
                      return (TxValid $ TxSuccess [ContractInitialized modref cname addr])
                    else error "dispatch: internal error: firstFreeAddress invariant broken."
      
            -- FIXME: This is only temporary for now.
            -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
            Transfer toaddr amount -> do
              remainingAmount <- payForExecution (thSender meta) energy
              res <- evalLocalT (handleTransfer meta remainingAmount amount toaddr energy)
              case res of
                (Right (events, changeSet), energy') -> do
                  commitStateAndAccountChanges changeSet
                  refundEnergy (thSender meta) energy'
                  return $ TxValid $ TxSuccess events
                (Left reason, energy') -> do
                  refundEnergy (thSender meta) energy'
                  return $ TxValid (TxReject reason)
  
            Update amount cref maybeMsg -> do
              accountamount <- payForExecution (thSender meta) energy
              result <- evalLocalT (handleUpdate meta accountamount amount cref maybeMsg energy)
              case result of
                (Right (events, changeSet), energy') -> do
                   commitStateAndAccountChanges changeSet
                   refundEnergy (thSender meta) energy'
                   return $ TxValid $ TxSuccess events
                (Left reason, energy') -> do
                    refundEnergy (thSender meta) energy'
                    return $ TxValid (TxReject reason)
  
            CreateAccount aci -> 
              if AH.verifyAccount aci
              then do -- if account information is correct then we create the account with initial nonce 'minNonce'
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

{-# INLINE rejectWith #-}
rejectWith :: Monad m => InvalidKind -> Energy -> m (Either InvalidKind b, Energy)
rejectWith fk energy = return ((Left fk), energy)

{-# INLINE succeedWith #-}
succeedWith :: Monad m => c -> Energy -> m (Either b c, Energy)
succeedWith b energy = return (Right b, energy)

handleModule :: TransactionMonad m => TransactionHeader -> Core.Module -> Energy -> m (Either InvalidKind (Core.ModuleRef, Interface, ValueInterface), Energy)
handleModule meta mod energy = do
  case runExcept (Core.makeInternal mod) of
    Left err -> rejectWith (ModuleNotWF err) energy
    Right imod -> do
      miface <- runExceptT (TC.typeModule imod)
      case miface of
        Left tyerror -> rejectWith (ModuleNotWF tyerror) energy
        Right iface ->
          let mhash = Core.moduleHash mod
          in do modVals <- runExceptT (I.evalModule imod mhash)
                case modVals of
                  Left eerr -> rejectWith (EvaluationError eerr) energy
                  Right viface -> succeedWith (mhash, iface, viface) energy

handleInit
  :: (TransactionMonad m, InterpreterMonad m)
     => TransactionHeader
     -> Amount
     -> Amount
     -> Core.ModuleRef
     -> Core.TyName
     -> Core.Expr Core.ModuleName
     -> Energy
     -> m (Either InvalidKind
            (UpdateType, Interface, ValueInterface, Core.Type Core.ModuleRef, Value, Amount, Map.HashMap (Core.ModuleRef, Core.TyName) ImplementsValue),
           Energy)
handleInit meta senderAmount amount modref cname param energy = do
  if senderAmount >= amount then do
    miface <- getModuleInterfaces modref
    case miface of
      Nothing -> rejectWith (InvalidModuleReference modref) energy
      Just (iface, viface) -> do
        case Map.lookup cname (exportedContracts iface) of
          Nothing -> rejectWith (InvalidContractReference modref cname) energy
          Just ciface -> do
            qparamExpE <- runExceptT (TC.checkTyInCtx iface param (paramTy ciface))
            case qparamExpE of
              Left err -> rejectWith (ParamsTypeError err) energy
              Right qparamExp ->
                let contract = (exportedDefsConts viface) Map.! cname
                    initFun = initMethod contract
                in do params' <- fromJust <$> runMaybeT (link (exportedDefsVals viface) qparamExp)
                      cm <- getChainMetadata
                      res <- I.applyInitFun cm (InitContext (thSender meta)) initFun params' (thSender meta) amount energy
                      case res of
                        Nothing -> do -- we ran out of energy
                          rejectWith OutOfEnergy 0
                        Just (v, energy') -> -- make new instance
                          succeedWith ((updateMethod contract), iface, viface, (msgTy ciface), v, amount, (implements contract)) energy'
  else rejectWith (AmountTooLarge (AddressAccount (thSender meta)) amount) energy

handleUpdate
  :: (TransactionMonad m, InterpreterMonad m)
     => TransactionHeader
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> ContractAddress
     -> Core.Expr Core.ModuleName
     -> Energy
     -> m (Either InvalidKind ([Event], ChangeSet), Energy)
handleUpdate meta accountamount amount cref msg energy = do
  cinstance <- getCurrentContractInstance cref
  case cinstance of
    Nothing -> rejectWith (InvalidContractAddress cref) energy
    Just i -> let rf = Ins.ireceiveFun i
                  msgType = Ins.imsgTy i
                  (iface, viface) = Ins.iModuleIface i
                  model = Ins.imodel i
                  contractamount = Ins.iamount i
                  -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
              in do qmsgExpE <- runExceptT (TC.checkTyInCtx iface msg msgType)
                    case qmsgExpE of
                      Left err -> rejectWith (MessageTypeError err) energy
                      Right qmsgExp -> do
                        qmsgExpLinked <- fromJust <$> runMaybeT (link (exportedDefsVals viface) qmsgExp)
                        (result, energy') <- handleTransaction (thSender meta)
                                                               cref
                                                               rf
                                                               (AddressAccount (thSender meta))
                                                               accountamount
                                                               amount
                                                               (ExprMessage (I.mkJustE qmsgExpLinked))
                                                               model
                                                               contractamount
                                                               energy
                        case result of
                          TxSuccess evs -> do
                            cset <- getChanges
                            succeedWith (evs, cset) energy'
                          TxReject reason -> rejectWith reason energy'

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
  -> Energy -- ^the amount of energy remaining for the execution
  -> m (ValidResult, Energy)
handleTransaction origin cref receivefun txsender senderamount transferamount maybeMsg model contractamount energy = do
  if senderamount >= transferamount then do
    cm <- getChainMetadata
    let receiveCtx = ReceiveContext { invoker = origin, selfAddress = cref }
    result <- case maybeMsg of
                ValueMessage m -> I.applyReceiveFunVal cm receiveCtx receivefun model txsender transferamount m energy
                ExprMessage m ->  I.applyReceiveFun cm receiveCtx receivefun model txsender transferamount m energy
    case result of
      Nothing -> -- ran out of energy, so must have consumed it all, hence there is no remaining
                 -- there were no other changes to the global state. In particular the amount was not transferred from sender.
        return (TxReject OutOfEnergy, 0)
      Just (res', energy') -> do
        case res' of
          Nothing -> -- transaction rejected, no other changes were recorder in the global state (in particular the amount was not transferred)
            return (TxReject Rejected, energy')
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
                          -- we need to get the fresh amount each time since it might have changed for each execution
                          -- NB: fromJust is justified since at this point we know the sender contract exists
                          senderamount'' <- (Ins.iamount . fromJust) <$> getCurrentContractInstance cref
                          case tx of
                            TSend cref' transferamount' message' -> do
                              -- the only way to send is to first check existence, so this must succeed
                              cinstance <- fromJust <$> getCurrentContractInstance cref' 
                              let receivefun' = Ins.ireceiveFun cinstance
                              let model' = Ins.imodel cinstance
                              return $ handleTransaction origin
                                                         cref'
                                                         receivefun'
                                                         (AddressContract cref)
                                                         senderamount''
                                                         transferamount'
                                                         (ValueMessage (I.mkJust message'))
                                                         model'
                                                         contractamount'
                            -- simple transfer to a contract is the same as a call to update with Nothing
                            TSimpleTransfer (AddressContract cref') transferamount' -> do
                              cinstance <- fromJust <$> getCurrentContractInstance cref' -- the only way to send is to first check existence, so this must succeed
                              let receivefun' = Ins.ireceiveFun cinstance
                              let model' = Ins.imodel cinstance
                              return $ handleTransaction origin
                                                         cref'
                                                         receivefun'
                                                         (AddressContract cref)
                                                         senderamount''
                                                         transferamount'
                                                         (ValueMessage I.mkNothing)
                                                         model'
                                                         contractamount'
                            TSimpleTransfer (AddressAccount acc) transferamount' -> do -- FIXME: This is temporary until accounts have their own functions
                              return $ handleTransferAccount origin acc (AddressContract cref) senderamount'' transferamount'
                              )
                    (TxSuccess [Updated cref transferamount maybeMsg], energy') txout
      -- a transaction is rejected in case we try to transfer amounts we don't have.
      -- This rejection is different from rejection by a contract, but the effect is the same.
      -- FIXME: Possibly this will need to be changed.
      else return (TxReject (AmountTooLarge txsender transferamount), energy)

combineTx :: Monad m => (ValidResult, Energy) -> m (Energy -> m (ValidResult, Energy)) -> m (ValidResult, Energy)
combineTx x ma =
  case x of
    (TxSuccess evs, energy') ->
      ma >>= \f -> f energy' >>= \case (TxSuccess evs', energy'') -> return $ (TxSuccess (evs ++ evs'), energy'')
                                       other -> return other
    other -> return other


handleTransfer
  :: (TransactionMonad m, InterpreterMonad m)
     => TransactionHeader
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> Address -- either account or contract
     -> Energy
     -> m (Either InvalidKind ([Event], ChangeSet), Energy)
handleTransfer meta accountamount amount addr energy =
  case addr of
    AddressContract cref -> do
      cinstance <- getCurrentContractInstance cref
      case cinstance of
        Nothing -> rejectWith (InvalidContractAddress cref) energy
        Just i -> let rf = Ins.ireceiveFun i
                      model = Ins.imodel i
                      contractamount = Ins.iamount i
                  -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
                  in do 
                        let qmsgExpLinked = I.mkNothingE -- give Nothing as argument to the receive method
                        (result, energy') <- handleTransaction (thSender meta)
                                                               cref
                                                               rf
                                                               (AddressAccount (thSender meta))
                                                               accountamount
                                                               amount
                                                               (ExprMessage (I.mkJustE qmsgExpLinked)) model contractamount energy
                        case result of
                          TxSuccess evs -> do
                            cset <- getChanges
                            succeedWith (evs, cset) energy'
                          TxReject reason -> rejectWith reason energy'
    AddressAccount acc -> do
      (result, energy') <- handleTransferAccount (thSender meta) acc (AddressAccount (thSender meta)) accountamount amount energy
      case result of
        TxSuccess evs -> do
          cset <- getChanges
          succeedWith (evs, cset) energy'
        TxReject reason -> rejectWith reason energy'

handleTransferAccount ::
  TransactionMonad m
  => AccountAddress -- the origin account of the top-level transaction
  -> AccountAddress -- the target account
  -> Address -- the invoker of this particular transaction, in general different from the origin
  -> Amount -- amount on the sender's account or contract instance
  -> Amount -- amount that was sent in the transaction
  -> Energy -- the amount of energy remaining for the execution
  -> m (ValidResult, Energy)
handleTransferAccount origin acc txsender senderamount amount energy = do
  mtargetAmount <- getCurrentAmount acc -- check if target account exists and get its public balance
                                        -- FIXME: Should pay for execution here as well.
  case mtargetAmount of 
    Nothing -> return (TxReject (InvalidAccountReference acc), energy)
    Just targetAmount -> 
        if senderamount >= amount then  -- if we have the funds after subtracting the deposit for energy costs
          case txsender of
            AddressAccount acc' | acc == acc' -> do -- if sender and receiver are the same then we do nothing
              return (TxSuccess [Transferred txsender amount (AddressAccount acc)], energy)
            _ ->  -- and otherwise
              withAmount txsender (senderamount - amount) $ -- decrease sender's amount
                  withAmount (AddressAccount acc) (targetAmount + amount) $ do -- NB: Consider whether an overflow can happen
                    return (TxSuccess [Transferred txsender amount (AddressAccount acc)], energy)
          else return (TxReject (AmountTooLarge txsender amount), energy) -- amount insufficient

-- *Exposed methods.
-- |Make a valid block out of a list of transactions. The list is traversed from
-- left to right and any invalid transactions are not included in the block. The
-- return value is a pair of lists of transactions @(valid, invalid)@ where
--    * @valid@ transactions is the list of transactions that should appear on the block in the order they should appear
--    * @invalid@ is a list of invalid transactions.
--    The order these transactions appear is arbitrary (i.e., they do not necessarily appear in the same order as in the input).
makeValidBlock :: (TransactionData msg, SchedulerMonad m) => [msg] -> m ([(msg, ValidResult)], [(msg, FailureKind)])
makeValidBlock = go [] []
  where go valid invalid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) invalid ts
            TxInvalid reason -> go valid ((t, reason):invalid) ts
        go valid invalid [] = return (reverse valid, invalid)

-- |Execute transactions in sequence. Return 'Nothing' if one of the transactions
-- fails, and otherwise return a list of transactions with their outcomes.
runBlock :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Maybe [(msg, ValidResult)])
runBlock = go []
  where go valid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) ts
            TxInvalid _ -> return Nothing
        go valid [] = return (Just (reverse valid))

-- |Execute transactions in sequence only for sideffects on global state.
-- Returns @Right ()@ if block executed successfully, and @Left@ @FailureKind@ at
-- first failed transaction. This is more efficient than 'runBlock' since it
-- does not have to build a list of results.
execBlock :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Either FailureKind ())
execBlock = go
  where go (t:ts) = do
          dispatch t >>= \case
            TxValid _ -> go ts
            TxInvalid reason -> return (Left reason)
        go [] = return (Right ())
