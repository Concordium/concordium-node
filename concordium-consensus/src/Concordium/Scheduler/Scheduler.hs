{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Scheduler
  (makeValidBlock
  ,runBlock
  ,execBlock
  ) where

import qualified Acorn.TypeCheck as TC
import qualified Acorn.Interpreter as I
import qualified Acorn.Core as Core
import Acorn.Types
import Concordium.Scheduler.Environment

import qualified Concordium.ID.AccountHolder as AH

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)


import Prelude hiding (exp, mod)

getAmount :: SchedulerMonad m => AccountAddress -> m (Maybe Amount)
getAmount addr = (accountAmount <$>) <$> getAccount addr

getCurrentAmount :: TransactionMonad m => AccountAddress -> m (Maybe Amount)
getCurrentAmount addr = (accountAmount <$>) <$> getCurrentAccount addr


-- |Check that the transaction has a valid sender, and that the amount they have
-- deposited is on their account.
-- FIXME: Mostly unimplemented at the moment since we do not have a good notion of Accounts.
checkHeader :: SchedulerMonad m => Header -> m Bool
checkHeader meta = do
  m <- getAmount (sender meta)
  case m of
    Nothing -> return False
    Just amnt -> return (gasAmount meta <= amnt)

dispatch :: (Message msg, SchedulerMonad m) => msg -> m TxResult
dispatch msg = do
  validMeta <- checkHeader (getHeader msg)
  let meta = getHeader msg
  let energy = gtuToEnergy (gasAmount meta)
  if validMeta then
    case getPayload msg of -- FIXME: Before doing this need to charge some amount.
      Left err -> return $ TxValid $ TxReject (SerializationFailure err)
      Right payload -> 
        case payload of
          DeployModule mod -> do
            _ <- payForExecution (sender meta) energy -- ignore remaining amount, we don't need it for this particular transaction type
            res <- evalLocalT (handleModule (getHeader msg) mod energy)
            case res of
              (Left fk, _) -> return (TxInvalid fk) -- one does not pay for completely failed transactions
              (Right (Left reason), energy') -> refundEnergy (sender meta) energy' >> return (TxValid (TxReject reason))
              (Right (Right (mhash, iface, viface)), energy') -> do
                b <- commitModule mhash iface viface
                if b then do
                  refundEnergy (sender meta) energy'
                  return $ (TxValid $ TxSuccess [ModuleDeployed mhash])
                else do
                  -- FIXME:
                  -- we should reject the transaction immediately if we figure out that the module with the hash already exists.
                  -- otherwise we can waste some effort in checking before reaching this point.
                  -- This could be chedked immediately even before we reach the dispatch since module hash is the hash of module serialization.
                  refundEnergy (sender meta) energy
                  return $ TxValid (TxReject (ModuleHashAlreadyExists mhash))
                  
          InitContract amount modref cname param -> do
            remainingAmount <- payForExecution (sender meta) energy
            result <- evalLocalT (handleInit (getHeader msg) remainingAmount amount modref cname param energy)
            case result of
              (Left err, _) -> refundEnergy (sender meta) energy >> return (TxInvalid err) -- in case of error refund everything
              (Right (Left reason), _) -> return $ TxValid (TxReject reason)
              (Right (Right (rmethod, iface, viface, msgty, model, initamount, impls)), energy') -> do
                  refundEnergy (sender meta) energy'
                  addr <- putNewInstance rmethod iface viface msgty model initamount impls
                  return (TxValid $ TxSuccess [ContractInitialized modref cname addr])
    
          -- FIXME: This is only temporary for now.
          -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
          Transfer to amount -> do
            remainingAmount <- payForExecution (sender meta) energy
            res <- evalLocalT (handleTransfer (getHeader msg) remainingAmount amount to energy)
            case res of
              (Left err, _) -> refundEnergy (sender meta) energy >> return (TxInvalid err) -- in case of error refund everything
              (Right (Right (events, changeSet)), energy') -> do
                commitStateAndAccountChanges changeSet
                refundEnergy (sender meta) energy'
                return $ TxValid $ TxSuccess events
              (Right (Left reason), energy') -> do
                refundEnergy (sender meta) energy'
                return $ TxValid (TxReject reason)
    
          Update amount cref maybeMsg -> do
            accountamount <- payForExecution (sender meta) energy
            result <- evalLocalT (handleUpdate (getHeader msg) accountamount amount cref maybeMsg energy)
            case result of
              (Left err, _) -> refundEnergy (sender meta) energy >> return (TxInvalid err) -- in case of error refund everything
              (Right (Right (events, changeSet)), energy') -> do
                 commitStateAndAccountChanges changeSet
                 refundEnergy (sender meta) energy'
                 return $ TxValid $ TxSuccess events
              (Right (Left reason), energy') -> do
                  refundEnergy (sender meta) energy'
                  return $ TxValid (TxReject reason)

          CreateAccount aci -> 
            if AH.verifyAccount aci
            then do -- if account information is correct then we create the account with initial nonce 1
              let aaddr = AH.accountAddress aci
              let account = Account { accountAddress = aaddr, accountNonce = 1, accountAmount = 0, accountCreationInformation = aci }
              r <- putNewAccount account
              if r then
                return $ TxValid (TxReject (AccountAlreadyExists aaddr))
              else
                return $ TxValid (TxSuccess [AccountCreated aaddr])
            else 
              return $ TxValid (TxReject AccountCredentialsFailure)
  else return $ TxInvalid InvalidHeader

{-# INLINE rejectWith #-}
rejectWith :: Monad m => InvalidKind -> Energy -> m (Either a (Either InvalidKind b), Energy)
rejectWith fk energy = return (Right (Left fk), energy)

{-# INLINE succeedWith #-}
succeedWith :: Monad m => c -> Energy -> m (Either a (Either b c), Energy)
succeedWith b energy = return (Right (Right b), energy)

handleModule :: TransactionMonad m => Header -> Core.Module -> Energy -> m (Either FailureKind (Either InvalidKind (Core.ModuleRef, Interface, ValueInterface)), Energy)
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
     => Header
     -> Amount
     -> Amount
     -> Core.ModuleRef
     -> Core.TyName
     -> Core.Expr Core.ModuleName
     -> Energy
     -> m (Either
             FailureKind
             (Either InvalidKind
                (UpdateType, Interface, ValueInterface, Core.Type Core.ModuleRef, Value, Amount, Map.HashMap (Core.ModuleRef, Core.TyName) ImplementsValue)),
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
                      res <- I.applyInitFun cm (InitContext (sender meta)) initFun params' (sender meta) amount energy
                      case res of
                        Nothing -> do -- we ran out of energy
                          rejectWith OutOfEnergy 0
                        Just (v, energy') -> -- make new instance
                          succeedWith ((updateMethod contract), iface, viface, (msgTy ciface), v, amount, (implements contract)) energy'
  else rejectWith (AmountTooLarge (AddressAccount (sender meta)) amount) energy

handleUpdate
  :: (TransactionMonad m, InterpreterMonad m)
     => Header
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> ContractAddress
     -> Core.Expr Core.ModuleName
     -> Energy
     -> m (Either FailureKind (Either InvalidKind ([Event], ChangeSet)), Energy)
handleUpdate meta accountamount amount cref msg energy = do
  cinstance <- getCurrentContractInstance cref
  case cinstance of
    Nothing -> rejectWith (InvalidContractAddress cref) energy
    Just i -> let rf = ireceiveFun i
                  msgType = imsgTy i
                  (iface, viface) = iModuleIface i
                  model = lState i
                  contractamount = iamount i
                  -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
              in do qmsgExpE <- runExceptT (TC.checkTyInCtx iface msg msgType)
                    case qmsgExpE of
                      Left err -> rejectWith (MessageTypeError err) energy
                      Right qmsgExp -> do
                        qmsgExpLinked <- fromJust <$> runMaybeT (link (exportedDefsVals viface) qmsgExp)
                        (result, energy') <- handleTransaction (sender meta)
                                                               cref
                                                               rf
                                                               (AddressAccount (sender meta))
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
                          senderamount'' <- (iamount . fromJust) <$> getCurrentContractInstance cref
                          case tx of
                            TSend cref' transferamount' message' -> do
                              -- the only way to send is to first check existence, so this must succeed
                              cinstance <- fromJust <$> getCurrentContractInstance cref' 
                              let receivefun' = ireceiveFun cinstance
                              let model' = lState cinstance
                              return $ handleTransaction origin
                                                         cref'
                                                         receivefun'
                                                         (AddressContract cref)
                                                         senderamount''
                                                         transferamount'
                                                         (ValueMessage (I.mkJust message'))
                                                         model'
                                                         contractamount'
                            -- simple transfer to a contract is the same as a call to update with nothing
                            TSimpleTransfer (AddressContract cref') transferamount' -> do
                              cinstance <- fromJust <$> getCurrentContractInstance cref' -- the only way to send is to first check existence, so this must succeed
                              let receivefun' = ireceiveFun cinstance
                              let model' = lState cinstance
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
     => Header
     -> Amount -- amount on the sender account before the transaction
     -> Amount -- amount to send as part of the transaction
     -> Address -- either account or contract
     -> Energy
     -> m (Either FailureKind (Either InvalidKind ([Event], ChangeSet)), Energy)
handleTransfer meta accountamount amount addr energy =
  case addr of
    AddressContract cref -> do
      cinstance <- getCurrentContractInstance cref
      case cinstance of
        Nothing -> rejectWith (InvalidContractAddress cref) energy
        Just i -> let rf = ireceiveFun i
                      model = lState i
                      contractamount = iamount i
                  -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
                  in do 
                        let qmsgExpLinked = I.mkNothingE -- give Nothing as argument to the receive method
                        (result, energy') <- handleTransaction (sender meta)
                                                               cref
                                                               rf
                                                               (AddressAccount (sender meta))
                                                               accountamount
                                                               amount
                                                               (ExprMessage (I.mkJustE qmsgExpLinked)) model contractamount energy
                        case result of
                          TxSuccess evs -> do
                            cset <- getChanges
                            succeedWith (evs, cset) energy'
                          TxReject reason -> rejectWith reason energy'
    AddressAccount acc -> do
      (result, energy') <- handleTransferAccount (sender meta) acc (AddressAccount (sender meta)) accountamount amount energy
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
-- |Make a valid block out of a list of messages/transactions. The list is
-- traversed from left to right and any invalid transactions are dropped. The
-- invalid transactions are not returned in order.
makeValidBlock :: (Message msg, SchedulerMonad m) => [msg] -> m ([(msg, ValidResult)], [(msg, FailureKind)])
makeValidBlock = go [] []
  where go valid invalid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) invalid ts
            TxInvalid reason -> go valid ((t, reason):invalid) ts
        go valid invalid [] = return (reverse valid, invalid)

-- |Execute transactions in sequence. 
runBlock :: (Message msg, SchedulerMonad m) => [msg] -> m (Maybe [(msg, ValidResult)])
runBlock = go []
  where go valid (t:ts) = do
          dispatch t >>= \case
            TxValid reason -> go ((t, reason):valid) ts
            TxInvalid _ -> return Nothing
        go valid [] = return (Just (reverse valid))

execBlock :: (Message msg, SchedulerMonad m) => [msg] -> m (Maybe FailureKind)
execBlock = go
  where go (t:ts) = do
          dispatch t >>= \case
            TxValid _ -> go ts
            TxInvalid reason -> return (Just reason)
        go [] = return Nothing
