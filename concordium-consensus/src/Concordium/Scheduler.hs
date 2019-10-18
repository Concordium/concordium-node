{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.ID.Account as AH
import qualified Concordium.ID.Types as ID

import Concordium.GlobalState.Bakers(bakerAccount)
import qualified Concordium.GlobalState.Instances as Ins
import qualified Concordium.Scheduler.Cost as Cost

import Control.Applicative
import Control.Monad.Except
import qualified Data.HashMap.Strict as Map
import Data.Maybe(fromJust)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import qualified Concordium.Crypto.Proofs as Proofs

import Control.Exception(assert)

import Lens.Micro.Platform

import Prelude hiding (exp, mod)

-- |Check that the transaction has a valid sender, and that the amount they have
-- deposited is on their account.
-- Return the sender account as well as the deposited amount converted from the dedicated gas amount.
checkHeader :: (TransactionData msg, SchedulerMonad m) => msg -> m (Either FailureKind Account)
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
                       let sigCheck = verifyTransactionSignature meta
                       assert sigCheck (return acc)) -- only use assert because we rely on the signature being valid in the transaction table
                       -- unless sigCheck (throwError IncorrectSignature))
        -- TODO: If we are going to check that the signature is correct before adding the transaction to the table then this check can be removed,
        -- but only for transactions for which this was done.
        -- One issue is that if we don't include the public key with the transaction then we cannot do this, which is especially problematic for transactions
        -- which come as part of blocks.


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
    Right senderAccount -> do
      -- at this point the transaction is going to be commited to the block. Hence we can increase the
      -- account nonce of the sender account.
      increaseAccountNonce senderAccount

      -- then we notify the block state that all the identity issuers on the sender's account should be rewarded
      -- TODO: Check for existence of valid identity provider.
      -- TODO: Alternative design would be to only reward them if the transaction is successful/committed, or
      -- to add additional parameters (such as deposited amount)
      mapM_ (notifyIdentityProviderCredential . ID.cdvIpId) (senderAccount ^. accountCredentials)

      -- available for execution remaining amount available on the sender's
      -- account. This is deducted prior to execution and refunded at the end,
      -- if there is any left.
      let psize = payloadSize (transactionPayload msg)
      -- TODO: Charge a small amount based just on transaction size.
      case decodePayload (transactionPayload msg) of
        Left err -> do
          -- in case of serialization failure we charge the sender for checking
          -- the header and reject the transaction
          -- FIXME: Add charge based on transaction size.
          let cost = Cost.checkHeader
          payment <- energyToGtu cost
          chargeExecutionCost senderAccount payment
          return $ TxValid $ TxReject (SerializationFailure err) payment cost
        Right payload -> 
          case payload of
            DeployModule mod ->
              handleDeployModule senderAccount meta psize mod

            InitContract amount modref cname param ->
              -- the payload size includes amount + address of module + name of
              -- contract + parameters, but since the first three fields are
              -- fixed size this is OK.
              let paramSize = fromIntegral (thPayloadSize meta)
              in handleInitContract senderAccount meta amount modref cname param paramSize
            -- FIXME: This is only temporary for now.
            -- Later on accounts will have policies, and also will be able to execute non-trivial code themselves.
            Transfer toaddr amount ->
              handleSimpleTransfer senderAccount meta toaddr amount
 
            Update amount cref maybeMsg ->
              -- the payload size includes amount + address + message, but since the first two fields are
              -- fixed size this is OK.
              let msgSize = fromIntegral (thPayloadSize meta)
              in handleUpdateContract senderAccount meta cref amount maybeMsg msgSize
           
            DeployCredential cdi ->
              handleDeployCredential senderAccount meta (payloadBodyBytes (transactionPayload msg)) cdi
            
            DeployEncryptionKey encKey ->
              handleDeployEncryptionKey senderAccount meta encKey

            AddBaker{..} ->
              handleAddBaker senderAccount meta abElectionVerifyKey abSignatureVerifyKey abAccount abProofSig abProofElection abProofAccount

            RemoveBaker{..} ->
              handleRemoveBaker senderAccount meta rbId rbProof

            UpdateBakerAccount{..} ->
              handleUpdateBakerAccount senderAccount meta ubaId ubaAddress ubaProof

            UpdateBakerSignKey{..} ->
              handleUpdateBakerSignKey senderAccount meta ubsId ubsKey ubsProof
            
            DelegateStake{..} ->
              handleDelegateStake senderAccount meta (Just dsID)
            
            UndelegateStake ->
              handleDelegateStake senderAccount meta Nothing

-- |Process the deploy module transaction.
handleDeployModule ::
  SchedulerMonad m
  => Account
  -> TransactionHeader -- ^Header of the transaction.
  -> PayloadSize -- ^Serialized size of the module. Used for charging execution cost.
  -> Module -- ^The module to deploy
  -> m TxResult
handleDeployModule senderAccount meta psize mod =
  withDeposit senderAccount meta (handleModule meta psize mod) $ \ls (mhash, iface, viface) -> do
    (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
    chargeExecutionCost senderAccount energyCost
    b <- commitModule mhash iface viface mod
    if b then
      return $! TxSuccess [ModuleDeployed mhash] energyCost usedEnergy
    else
      -- FIXME:
      -- we should reject the transaction immediately if we figure out that the module with the hash already exists.
      -- otherwise we can waste some effort in checking before reaching this point.
      -- This could be chedked immediately even before we reach the dispatch since module hash is the hash of module serialization.
      return $! TxReject (ModuleHashAlreadyExists mhash) energyCost usedEnergy


-- |TODO: Figure out whether we need the metadata or not here.
handleModule :: TransactionMonad m => TransactionHeader -> PayloadSize -> Module -> m (Core.ModuleRef, Interface, ValueInterface)
handleModule _meta msize mod = do
  -- Consume the gas amount required for processing.
  -- This is done even if the transaction is rejected in the end.
  -- NB: The next line will reject the transaction in case there are not enough funds.
  tickEnergy (Cost.deployModule (fromIntegral msize))
  let mhash = Core.moduleHash mod
  imod <- pure (runExcept (Core.makeInternal mhash (fromIntegral msize) mod)) `rejectingWith'` MissingImports
  iface <- runExceptT (TC.typeModule imod) `rejectingWith'` ModuleNotWF
  let viface = I.evalModule imod
  return (mhash, iface, viface)

-- |Handle the top-level initialize contract.
handleInitContract ::
  SchedulerMonad m
    => Account -- ^Account which is initializing the contract.
    -> TransactionHeader -- ^Header of the transaction.
    -> Amount   -- ^The amount to initialize the contract with.
    -> ModuleRef  -- ^Module reference of the contract to initialize.
    -> Core.TyName  -- ^Name of the contract in a module.
    -> Core.Expr Core.UA Core.ModuleName  -- ^Parameters of the contract.
    -> Int -- ^Serialized size of the parameters. Used for computing typechecking cost.
    -> m TxResult
handleInitContract senderAccount meta amount modref cname param paramSize =
  withDeposit senderAccount meta c k
    where c = do
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
            qparamExp <- runExceptT (TC.checkTyInCtx' iface param (paramTy ciface)) `rejectingWith'` ParamsTypeError
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
            chargeExecutionCost senderAccount energyCost

            -- we make a new changeset that also withdraws the amount from the sender's account
            -- this way of doing it means that if the contract observes current balance it will observe
            -- the value before the initialization of the contract
            commitChanges (addAmountToCS senderAccount (amountDiff 0 amount) (ls ^. changeSet))
            let ins = makeInstance modref cname contract msgty iface viface model initamount (thSender meta)
            addr <- putNewInstance ins
            return $ TxSuccess [ContractInitialized modref cname addr] energyCost usedEnergy

handleSimpleTransfer ::
  SchedulerMonad m
    => Account -- ^Sender account of the transaction
    -> TransactionHeader -- ^Header of the transaction.
    -> Address -- ^Address to send the amount to, either account or contract.
    -> Amount -- ^The amount to transfer.
    -> m TxResult
handleSimpleTransfer senderAccount meta toaddr amount =
  withDeposit senderAccount meta c (defaultSuccess meta senderAccount)
    where c = case toaddr of
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
    => Account -- ^Sender account of the transaction.
    -> TransactionHeader -- ^Header of the transaction.
    -> ContractAddress -- ^Address of the contract to invoke.
    -> Amount -- ^Amount to invoke the contract's receive method with.
    -> Core.Expr Core.UA Core.ModuleName -- ^Message to send to the receive method.
    -> Int  -- ^Serialized size of the message.
    -> m TxResult
handleUpdateContract senderAccount meta cref amount maybeMsg msgSize =
  withDeposit senderAccount meta c (defaultSuccess meta senderAccount)
  where c = do
          tickEnergy Cost.updatePreprocess
          i <- getCurrentContractInstance cref `rejectingWith` InvalidContractAddress cref
          let rf = Ins.ireceiveFun i
              msgType = Ins.imsgTy i
              (iface, _) = Ins.iModuleIface i
              model = Ins.instanceModel i
              -- we assume that gasAmount was available on the account due to the previous check (checkHeader)
          tickEnergy (Cost.updateMessageTypecheck msgSize)
          qmsgExp <- runExceptT (TC.checkTyInCtx' iface maybeMsg msgType) `rejectingWith'` MessageTypeError
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

  cm <- getChainMetadata
  let cref = instanceAddress (instanceParameters istance)
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
                  [Updated txsenderAddr cref transferamount maybeMsg] txout

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
  -- the sender must have the amount available.
  -- Otherwise we reject the transaction immediately.
  senderamount <- getCurrentAmount txsender

  unless (senderamount >= transferamount) $! rejectTransaction (AmountTooLarge (mkSenderAddr txsender) transferamount)

  -- check if target account exists and get it
  targetAccount <- getCurrentAccount accAddr `rejectingWith` InvalidAccountReference accAddr
  
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

handleDeployCredential ::
  SchedulerMonad m
    =>
    -- |Sender account of the transaction.
    Account ->
    -- |Header of the transaction.
    TransactionHeader ->
    -- |Credentials to deploy in serialized form. We pass these to the verify function.
    AH.CredentialDeploymentInformationBytes ->
    -- |Credentials to deploy.
    ID.CredentialDeploymentInformation ->
    m TxResult
handleDeployCredential senderAccount meta cdiBytes cdi =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.deployCredential
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          let cdv = ID.cdiValues cdi
          -- check that a registration id does not yet exist
          regIdEx <- accountRegIdExists (ID.cdvRegId cdv)
          if regIdEx then
            return $! TxReject (DuplicateAccountRegistrationID (ID.cdvRegId cdv)) energyCost usedEnergy
          else
            -- We now look up the identity provider this credential is derived from.
            -- Of course if it does not exist we reject the transaction.
            getIPInfo (ID.cdvIpId cdv) >>= \case
              Nothing -> return $! TxReject (NonExistentIdentityProvider (ID.cdvIpId cdv)) energyCost usedEnergy
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
                             addAccountCredential account cdv  -- and then add the credentials
                             return $! TxSuccess [AccountCreated aaddr, CredentialDeployed cdv] energyCost usedEnergy
                       else return $! TxReject AccountCredentialInvalid energyCost usedEnergy
     
                  Just account -> -- otherwise we just try to add a credential to the account
                            if AH.verifyCredential
                               elgamalGenerator
                               attributeCommitmentKey
                               ipVerifyKey
                               arElgamalGenerator
                               arPublicKey
                               cdiBytes then do
                              addAccountCredential account cdv
                              return $! TxSuccess [CredentialDeployed cdv] energyCost usedEnergy
                            else
                              return $! TxReject AccountCredentialInvalid energyCost usedEnergy

handleDeployEncryptionKey ::
  SchedulerMonad m
    => Account -- ^Account onto which the encryption key should be deployed.
    -> TransactionHeader -- ^Header of the transaction.
    -> ID.AccountEncryptionKey -- ^The encryption key.
    -> m TxResult
handleDeployEncryptionKey senderAccount meta encKey =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.deployEncryptionKey
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          case senderAccount ^. accountEncryptionKey of
            Nothing -> do
              let aaddr = senderAccount ^. accountAddress
              addAccountEncryptionKey senderAccount encKey
              return $ TxSuccess [AccountEncryptionKeyDeployed aaddr encKey] energyCost usedEnergy
            Just encKey' -> return $ TxReject (AccountEncryptionKeyAlreadyExists (senderAccount ^. accountAddress) encKey') energyCost usedEnergy


-- FIXME: The baker handling is purely proof-of-concept.
-- In particular there is no checking of proofs that the baker holds relevant
-- private keys, etc, and the precise logic for when a baker can be added and removed 
-- should be analyzed from a security perspective.


-- |The following functions are placeholders until we have sigma protocols and
-- can check these proofs.
checkElectionKeyProof :: BS.ByteString -> BakerElectionVerifyKey -> Proofs.Dlog25519Proof -> Bool             
checkElectionKeyProof = Proofs.checkDlog25519ProofVRF

checkSignatureVerifyKeyProof :: BS.ByteString -> BakerSignVerifyKey -> Proofs.Dlog25519Proof -> Bool             
checkSignatureVerifyKeyProof = Proofs.checkDlog25519ProofSig

checkAccountOwnership :: BS.ByteString -> SigScheme.SchemeId -> ID.AccountVerificationKey -> Proofs.Dlog25519Proof -> Bool             
checkAccountOwnership challenge schId vfkey proof =
  schId == SigScheme.Ed25519 && Proofs.checkDlog25519ProofSig challenge vfkey proof

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
    => Account
    -> TransactionHeader
    -> BakerElectionVerifyKey
    -> BakerSignVerifyKey
    -> AccountAddress
    -> Proofs.Dlog25519Proof
    -> Proofs.Dlog25519Proof
    -> Proofs.Dlog25519Proof
    -> m TxResult
handleAddBaker senderAccount meta abElectionVerifyKey abSignatureVerifyKey abAccount abProofSig abProofElection abProofAccount =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.addBaker
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          -- TODO:NB: during beta we have special accounts which can add and remove bakers.
          -- in the future the logic will be different. Thus this branching here is temporary.
          specialBetaAccounts <- getSpecialBetaAccounts
          if not (HashSet.member (senderAccount ^. accountAddress) specialBetaAccounts) then
            return $! TxReject (NotAllowedToManipulateBakers (senderAccount ^. accountAddress)) energyCost usedEnergy
          else  do
            getAccount abAccount >>=
                \case Nothing -> return $! TxReject (NonExistentRewardAccount abAccount) energyCost usedEnergy
                      Just Account{..} ->
                        let challenge = S.runPut (S.put abElectionVerifyKey <> S.put abSignatureVerifyKey <> S.put abAccount)
                            electionP = checkElectionKeyProof challenge abElectionVerifyKey abProofElection
                            signP = checkSignatureVerifyKeyProof challenge abSignatureVerifyKey abProofSig
                            accountP = checkAccountOwnership challenge _accountSignatureScheme _accountVerificationKey abProofAccount
                        in if electionP && signP && accountP then do
                          -- the proof validates that the baker owns all the private keys.
                          -- Moreover at this point we know the reward account exists and belongs
                          -- to the baker.
                          -- Thus we can create the baker, starting it off with 0 lottery power.
                          mbid <- addBaker (BakerCreationInfo abElectionVerifyKey abSignatureVerifyKey abAccount)
                          case mbid of
                            Nothing -> return $! TxReject (DuplicateSignKey abSignatureVerifyKey) energyCost usedEnergy
                            Just bid -> return $! TxSuccess [BakerAdded bid] energyCost usedEnergy
                        else return $ TxReject InvalidProof energyCost usedEnergy

-- |Remove a baker from the baker pool.
-- The current logic is that if the proof validates that the sender of the
-- transaction is the reward account of the baker.
-- TODO: Need to make sure that this proof is not duplicable (via the challenge prefix I suppose).
handleRemoveBaker ::
  SchedulerMonad m
    => Account
    -> TransactionHeader
    -> BakerId
    -> Proof
    -> m TxResult
handleRemoveBaker senderAccount meta rbId _rbProof =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.removeBaker
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          getBakerInfo rbId >>=
              \case Nothing ->
                      return $ TxReject (RemovingNonExistentBaker rbId) energyCost usedEnergy
                    Just binfo ->
                      if senderAccount ^. accountAddress == binfo ^. bakerAccount then do
                        -- only the baker itself can remove themselves from the pool
                        removeBaker rbId
                        return $ TxSuccess [BakerRemoved rbId] energyCost usedEnergy
                      else
                        return $ TxReject (InvalidBakerRemoveSource (senderAccount ^. accountAddress)) energyCost usedEnergy

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
    => Account
    -> TransactionHeader
    -> BakerId
    -> AccountAddress
    -> Proofs.Dlog25519Proof
    -> m TxResult
handleUpdateBakerAccount senderAccount meta ubaId ubaAddress ubaProof =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.updateBakerAccount
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost

          getBakerInfo ubaId >>= \case
            Nothing ->
                return $ TxReject (UpdatingNonExistentBaker ubaId) energyCost usedEnergy
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                  -- the transaction is coming from the current baker's account. 
                  -- now check the account exists and the baker owns it
                  getAccount ubaAddress >>= \case
                    Nothing -> return $! TxReject (NonExistentRewardAccount ubaAddress) energyCost usedEnergy
                    Just Account{..} ->
                      let challenge = S.runPut (S.put ubaId <> S.put ubaAddress)
                          accountP = checkAccountOwnership challenge _accountSignatureScheme _accountVerificationKey ubaProof
                      in if accountP then do
                        _ <- updateBakerAccount ubaId ubaAddress
                        return $ TxSuccess [BakerAccountUpdated ubaId ubaAddress] energyCost usedEnergy
                      else return $ TxReject InvalidProof energyCost usedEnergy
                else
                  return $ TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)) energyCost usedEnergy

-- |Update the baker's public signature key. The transaction is considered valid if
--
--  * The transaction is coming from the baker's current reward account.
--  * The transaction proves that they own the private key corresponding to the __NEW__
--    signature verification key.
handleUpdateBakerSignKey ::
  SchedulerMonad m
    => Account
    -> TransactionHeader
    -> BakerId
    -> BakerSignVerifyKey
    -> Proofs.Dlog25519Proof
    -> m TxResult
handleUpdateBakerSignKey senderAccount meta ubsId ubsKey ubsProof =
  withDeposit senderAccount meta c k
  where c = tickEnergy Cost.updateBakerKey
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          getBakerInfo ubsId >>= \case 
            Nothing ->
              return $ TxReject (UpdatingNonExistentBaker ubsId) energyCost usedEnergy
            Just binfo ->
              if binfo ^. bakerAccount == senderAccount ^. accountAddress then
                -- only the baker itself can update its own keys
                -- now also check that they own the private key for the new signature key
                let challenge = S.runPut (S.put ubsId <> S.put ubsKey)
                    signP = checkSignatureVerifyKeyProof challenge ubsKey ubsProof
                in if signP then do
                     success <- updateBakerSignKey ubsId ubsKey
                     if success then 
                       return $ TxSuccess [BakerKeyUpdated ubsId ubsKey] energyCost usedEnergy
                     else return $! TxReject (DuplicateSignKey ubsKey) energyCost usedEnergy
                   else return $ TxReject InvalidProof energyCost usedEnergy
              else
                return $ TxReject (NotFromBakerAccount (senderAccount ^. accountAddress) (binfo ^. bakerAccount)) energyCost usedEnergy

-- |Update an account's stake delegate.
handleDelegateStake ::
  SchedulerMonad m
    => Account
    -> TransactionHeader
    -> Maybe BakerId
    -> m TxResult
handleDelegateStake senderAccount meta targetBaker =
  withDeposit senderAccount meta c k
  where c = tickEnergy delegateCost
        k ls _ = do
          (usedEnergy, energyCost) <- computeExecutionCharge meta (ls ^. energyLeft)
          chargeExecutionCost senderAccount energyCost
          res <- delegateStake (thSender meta) targetBaker
          if res then
            let addr = senderAccount ^. accountAddress
            in return $! TxSuccess [maybe (StakeUndelegated addr) (StakeDelegated addr) targetBaker] energyCost usedEnergy
          else 
            return $! TxReject (InvalidStakeDelegationTarget $! fromJust targetBaker) energyCost usedEnergy
        delegateCost = Cost.updateStakeDelegate (Set.size $! senderAccount ^. accountInstances)

-- *Exposed methods.
-- |Make a valid block out of a list of transactions, respecting the given
-- maximum block size. The list of input transactions is traversed from left to
-- right and any invalid transactions are not included in the block. The return
-- value is a FilteredTransactions object where
--
--   * @ftAdded@ is the list of transactions that should appear on the block in
--     the order they should appear
--   * @ftFailed@ is a list of invalid transactions. The order these transactions
--     appear is arbitrary (i.e., they do not necessarily appear in the same order
--     as in the input).
--   * @ftUnprocessed@ is a list of transactions which were not
--     processed due to size restrictions.
filterTransactions :: (TransactionData msg, SchedulerMonad m)
                      => Integer -> [msg] -> m (FilteredTransactions msg, Energy)
filterTransactions maxSize = go 0 0 [] [] []
  where go !totalEnergyUsed size valid invalid unprocessed (t:ts) = do
          let csize = size + fromIntegral (transactionSize t)
          if csize <= maxSize then -- if the next transaction can fit into a block then add it.
             dispatch t >>= \case
               TxValid reason -> go (totalEnergyUsed + vrEnergyCost reason) csize ((t, reason):valid) invalid unprocessed ts
               TxInvalid reason -> go totalEnergyUsed csize valid ((t, reason):invalid) unprocessed ts
          else -- otherwise still try the remaining transactions to avoid deadlocks from
               -- one single too-big transaction.
             go totalEnergyUsed size valid invalid (t:unprocessed) ts
        go !totalEnergyUsed _ valid invalid unprocessed [] = 
          let txs = FilteredTransactions{
                      ftAdded = reverse valid,
                      ftFailed = invalid,
                      ftUnprocessed = unprocessed
                    }
          in return (txs, totalEnergyUsed)

-- |Execute transactions in sequence. Return 'Nothing' if one of the transactions
-- fails, and otherwise return a list of transactions with their outcomes.
runTransactions :: (TransactionData msg, SchedulerMonad m)
                   => [msg] -> m (Either FailureKind ([(msg, ValidResult)], Energy))
runTransactions = go 0 []
  where go !totalEnergyUsed valid (t:ts) =
          dispatch t >>= \case
            TxValid reason -> go (totalEnergyUsed + vrEnergyCost reason) ((t, reason):valid) ts
            TxInvalid reason -> return (Left reason)
        go !totalEnergyUsed valid [] = return (Right (reverse valid, totalEnergyUsed))

-- |Execute transactions in sequence only for sideffects on global state.
-- Returns 'Right' '()' if block executed successfully, and 'Left' 'FailureKind' at
-- first failed transaction. This is more efficient than 'runTransactions' since it
-- does not have to build a list of results.
execTransactions :: (TransactionData msg, SchedulerMonad m) => [msg] -> m (Either FailureKind Energy)
execTransactions = go 0
  where go !totalEnergyUsed (t:ts) =
          dispatch t >>= \case
            TxValid reason -> go (totalEnergyUsed + vrEnergyCost reason) ts
            TxInvalid reason -> return (Left reason)
        go !totalEnergyUsed [] = return (Right totalEnergyUsed)
