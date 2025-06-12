{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains the implementation for running the scheduler computations.
module Concordium.Scheduler.EnvironmentImplementation where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.HashMap.Strict as Map
import qualified Data.Kind as DK
import Lens.Micro.Platform

import Concordium.Types.Tokens

import Concordium.GlobalState.Account
import qualified Concordium.GlobalState.BakerInfo as BI
import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens (PLTConfiguration (..), TokenIndex)
import Concordium.GlobalState.TreeState
import Concordium.Logger
import Concordium.Scheduler.Environment
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.Types
import Concordium.TimeMonad
import qualified Concordium.TransactionVerification as TVer

-- | Context for executing a scheduler computation.
data ContextState = ContextState
    { -- | Chain metadata
      _chainMetadata :: !ChainMetadata,
      -- | Maximum allowed block energy.
      _maxBlockEnergy :: !Energy,
      -- | Maximum number of accounts to be created in the same block.
      _accountCreationLimit :: !CredentialsPerBlockLimit
    }

makeLenses ''ContextState

-- | State accumulated during execution of a scheduler computation.
data SchedulerState (m :: DK.Type -> DK.Type) = SchedulerState
    { -- | Current block state.
      _ssBlockState :: !(UpdatableBlockState m),
      -- | Energy used so far.
      _ssEnergyUsed :: !Energy,
      -- | The total execution costs so far.
      _ssExecutionCosts :: !Amount,
      -- | The next available transaction index.
      _ssNextIndex :: !TransactionIndex
    }

makeLenses ''SchedulerState

-- | Create an initial state for running a scheduler computation.
makeInitialSchedulerState :: UpdatableBlockState m -> SchedulerState m
makeInitialSchedulerState _ssBlockState =
    SchedulerState
        { _ssEnergyUsed = 0,
          _ssExecutionCosts = 0,
          _ssNextIndex = 0,
          ..
        }

data PLTExecutionState m = PLTExecutionState
    { -- | The current block state.
      _plteBlockState :: !(UpdatableBlockState m),
      -- | The events that have been emitted during the execution in reverse order.
      _plteEvents :: ![Event],
      -- | The energy used for the execution of the PLT module.
      _plteEnergyUsed :: !Energy
    }
makeLenses ''PLTExecutionState

-- | Execution context for PLT computations.
data PLTExecutionContext = PLTExecutionContext
    { -- | The token index.
      _pltecTokenIndex :: !TokenIndex,
      -- | The PLT configuration.
      _pltecConfiguration :: !PLTConfiguration,
      -- | The available energy
      _pltecEnergy :: !(Maybe Energy)
    }

-- | Alias for the internal type used in @SchedulerT@.
type InternalSchedulerT m = RWST ContextState () (SchedulerState m)

-- | Scheduler monad transformer. Extends a monad with the ability to execute scheduler computations.
--  Use @runSchedulerT@ to run the computation.
newtype SchedulerT (m :: DK.Type -> DK.Type) (a :: DK.Type) = SchedulerT
    { _runSchedulerT :: InternalSchedulerT m m a
    }
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (SchedulerState m),
          MonadReader ContextState,
          MonadLogger,
          TimeMonad
        )

instance MonadTrans SchedulerT where
    {-# INLINE lift #-}
    lift = SchedulerT . lift

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        BlockStateTypes (SchedulerT m)

instance (BS.BlockStateOperations m) => StaticInformation (SchedulerT m) where
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = view maxBlockEnergy

    {-# INLINE getChainMetadata #-}
    getChainMetadata = view chainMetadata

    {-# INLINE getModuleInterfaces #-}
    getModuleInterfaces mref = do
        s <- use ssBlockState
        lift (BS.bsoGetModule s mref)

    {-# INLINE getAccountCreationLimit #-}
    getAccountCreationLimit = view accountCreationLimit

    {-# INLINE getContractInstance #-}
    getContractInstance addr = lift . flip BS.bsoGetInstance addr =<< use ssBlockState

    {-# INLINE getStateAccount #-}
    getStateAccount !addr = lift . flip BS.bsoGetAccount addr =<< use ssBlockState

    {-# INLINE getExchangeRates #-}
    getExchangeRates = lift . BS.bsoGetExchangeRates =<< use ssBlockState

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (MonadProtocolVersion m) => MonadProtocolVersion (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.AccountOperations m) => BS.AccountOperations (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.ContractStateOperations m) => BS.ContractStateOperations (SchedulerT m)

deriving via
    (MGSTrans (InternalSchedulerT m) m)
    instance
        (BS.ModuleQuery m) => BS.ModuleQuery (SchedulerT m)

instance
    (BS.BlockStateOperations m, MonadProtocolVersion m) =>
    TVer.TransactionVerifier (SchedulerT m)
    where
    {-# INLINE registrationIdExists #-}
    registrationIdExists !regid =
        lift . flip BS.bsoRegIdExists regid =<< use ssBlockState
    {-# INLINE getIdentityProvider #-}
    getIdentityProvider !ipId = do
        s <- use ssBlockState
        lift (BS.bsoGetIdentityProvider s ipId)
    {-# INLINE getAnonymityRevokers #-}
    getAnonymityRevokers !arIds = do
        s <- use ssBlockState
        lift (BS.bsoGetAnonymityRevokers s arIds)
    {-# INLINE getCryptographicParameters #-}
    getCryptographicParameters = lift . BS.bsoGetCryptoParams =<< use ssBlockState
    {-# INLINE getAccount #-}
    getAccount !aaddr = do
        s <- use ssBlockState
        lift (fmap snd <$> BS.bsoGetAccount s aaddr)
    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uType = lift . flip BS.bsoGetNextUpdateSequenceNumber uType =<< use ssBlockState
    {-# INLINE getUpdateKeysCollection #-}
    getUpdateKeysCollection = lift . BS.bsoGetUpdateKeyCollection =<< use ssBlockState
    {-# INLINE getAccountAvailableAmount #-}
    getAccountAvailableAmount = lift . BS.getAccountAvailableAmount
    {-# INLINE getNextAccountNonce #-}
    getNextAccountNonce = lift . BS.getAccountNonce
    {-# INLINE getAccountVerificationKeys #-}
    getAccountVerificationKeys = lift . BS.getAccountVerificationKeys
    {-# INLINE energyToCcd #-}
    energyToCcd v = do
        s <- use ssBlockState
        rate <- lift $ _erEnergyRate <$> BS.bsoGetExchangeRates s
        return (computeCost rate v)
    {-# INLINE getMaxBlockEnergy #-}
    getMaxBlockEnergy = do
        ctx <- ask
        let maxEnergy = ctx ^. maxBlockEnergy
        return maxEnergy
    {-# INLINE checkExactNonce #-}
    checkExactNonce = pure True

instance
    ( BS.BlockStateOperations m,
      MonadLogger m,
      MonadProtocolVersion m
    ) =>
    SchedulerMonad (SchedulerT m)
    where
    {-# INLINE markEnergyUsed #-}
    markEnergyUsed energy = ssEnergyUsed += energy

    {-# INLINE getUsedEnergy #-}
    getUsedEnergy = use ssEnergyUsed

    {-# INLINE bumpTransactionIndex #-}
    bumpTransactionIndex = ssNextIndex <<%= (+ 1)

    {-# INLINE getAccountIndex #-}
    getAccountIndex addr = lift . flip BS.bsoGetAccountIndex addr =<< use ssBlockState

    {-# INLINE putNewInstance #-}
    putNewInstance !mkInstance = do
        (caddr, s') <- lift . flip BS.bsoPutNewInstance mkInstance =<< use ssBlockState
        ssBlockState .= s'
        return caddr

    {-# INLINE createAccount #-}
    createAccount cparams addr credential = do
        s <- use ssBlockState
        (res, s') <- lift (BS.bsoCreateAccount s cparams addr credential)
        ssBlockState .= s'
        return res

    {-# INLINE addressWouldClash #-}
    addressWouldClash !addr =
        lift . flip BS.bsoAddressWouldClash addr =<< use ssBlockState

    {-# INLINE commitModule #-}
    commitModule !iface = do
        (res, s') <- lift . (\s -> BS.bsoPutNewModule s iface) =<< use ssBlockState
        ssBlockState .= s'
        return res

    {-# INLINE increaseAccountNonce #-}
    increaseAccountNonce (ai, acc) = do
        s <- use ssBlockState
        nonce <- BS.getAccountNonce acc
        s' <- lift (BS.bsoModifyAccount s (emptyAccountUpdate ai & auNonce ?~ (nonce + 1)))
        ssBlockState .= s'

    {-# INLINE updateAccountCredentials #-}
    updateAccountCredentials !ai !idcs !creds !threshold = do
        s <- use ssBlockState
        s' <- lift (BS.bsoUpdateAccountCredentials s ai idcs creds threshold)
        ssBlockState .= s'

    {-# INLINE commitChanges #-}
    commitChanges !cs = do
        s <- use ssBlockState
        -- ASSUMPTION: the property which should hold at this point is that any
        -- changed instance must exist in the global state and moreover all instances
        -- are distinct by the virtue of a HashMap being a function
        s1 <-
            lift
                ( foldM
                    ( \s' (addr, (modIdx, amnt, val)) ->
                        -- If the modification index is 0, this means that we have only recorded the
                        -- state in the changeset because we needed to due to calls to other contracts,
                        -- but the state of the instance did not change. So we don't have to modify the
                        -- instance.
                        if modIdx /= 0 then BS.bsoModifyInstance s' addr amnt val Nothing else return s'
                    )
                    s
                    (Map.toList (cs ^. instanceV0Updates))
                )
        -- since V0 and V1 instances are disjoint, the order in which we do updates does not matter.
        s2 <-
            lift
                ( foldM
                    ( \s' (addr, InstanceV1Update{..}) ->
                        BS.bsoModifyInstance s' addr amountChange newState newInterface
                    )
                    s1
                    (Map.toList (cs ^. instanceV1Updates))
                )
        -- Notify account transfers.
        -- This also updates the release schedule.
        s3 <-
            lift
                ( foldM
                    BS.bsoModifyAccount
                    s2
                    (cs ^. accountUpdates)
                )
        ssBlockState .= s3

    {-# INLINE energyToGtu #-}
    energyToGtu v = do
        s <- use ssBlockState
        rate <- lift $ _erEnergyRate <$> BS.bsoGetExchangeRates s
        return $! computeCost rate v

    {-# INLINE notifyExecutionCost #-}
    notifyExecutionCost !amnt = ssExecutionCosts += amnt

    {-# INLINE notifyEncryptedBalanceChange #-}
    notifyEncryptedBalanceChange !amntDiff = do
        s <- use ssBlockState
        s' <- lift (BS.bsoNotifyEncryptedBalanceChange s amntDiff)
        ssBlockState .= s'

    {-# INLINE addBaker #-}
    addBaker ai badd = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoAddBaker s ai badd)
        ssBlockState .= s'
        return ret

    {-# INLINE addValidator #-}
    addValidator ai removeDelegator vadd = do
        s <- use ssBlockState
        (s', res) <- lift (doAdd s)
        ssBlockState .= s'
        return res
      where
        doAdd s0 | RemoveExistingStake ts <- removeDelegator = do
            -- We need to remove the delegator first.
            -- We take a snapshot of the state so we can rollback if the add fails.
            snapshot <- BS.bsoSnapshotState s0
            rdRes <- BS.bsoUpdateDelegator s0 ts ai BI.delegatorRemove
            case rdRes of
                Left e ->
                    -- Removing the delegator cannot fail, since the account must have a delegator.
                    error $ "addValidator: Failed to remove delegator: " ++ show e
                Right (_, s1) -> do
                    res <- BS.bsoAddValidator s1 ai vadd
                    case res of
                        Left e -> do
                            -- Rollback the state to the snapshot.
                            s' <- BS.bsoRollback s1 snapshot
                            return (s', Left e)
                        Right s' -> return (s', Right ())
        doAdd s = do
            res <- BS.bsoAddValidator s ai vadd
            return $! case res of
                Left e -> (s, Left e)
                Right s' -> (s', Right ())

    {-# INLINE updateValidator #-}
    updateValidator ts ai vadd = do
        s <- use ssBlockState
        lift (BS.bsoUpdateValidator s ts ai vadd) >>= \case
            Left e -> return (Left e)
            Right (events, s') -> do
                ssBlockState .= s'
                return (Right events)

    {-# INLINE addDelegator #-}
    addDelegator ai removeValidator dadd = do
        s <- use ssBlockState
        (s', res) <- lift (doAdd s)
        ssBlockState .= s'
        return res
      where
        doAdd s0 | RemoveExistingStake ts <- removeValidator = do
            -- We need to remove the validator first.
            -- We take a snapshot of the state so we can rollback if the add fails.
            snapshot <- BS.bsoSnapshotState s0
            rvRes <- BS.bsoUpdateValidator s0 ts ai BI.validatorRemove
            case rvRes of
                Left e ->
                    -- Removing the validator cannot fail, since the account must have a validator.
                    error $ "addDelegator: Failed to remove validator: " ++ show e
                Right (_, s1) -> do
                    res <- BS.bsoAddDelegator s1 ai dadd
                    case res of
                        Left e -> do
                            -- Rollback the state to the snapshot.
                            s' <- BS.bsoRollback s1 snapshot
                            return (s', Left e)
                        Right s' -> return (s', Right ())
        doAdd s = do
            res <- BS.bsoAddDelegator s ai dadd
            return $! case res of
                Left e -> (s, Left e)
                Right s' -> (s', Right ())

    {-# INLINE updateDelegator #-}
    updateDelegator ts ai dadd = do
        s <- use ssBlockState
        lift (BS.bsoUpdateDelegator s ts ai dadd) >>= \case
            Left e -> return (Left e)
            Right (events, s') -> do
                ssBlockState .= s'
                return (Right events)

    {-# INLINE removeBaker #-}
    removeBaker ai = do
        s <- use ssBlockState
        (ret, s') <- lift (BS.bsoRemoveBaker s ai)
        ssBlockState .= s'
        return ret

    {-# INLINE updateBakerKeys #-}
    updateBakerKeys ai keyUpd = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerKeys s ai keyUpd)
        ssBlockState .= s'
        return r

    {-# INLINE updateBakerStake #-}
    updateBakerStake bi bsu = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerStake s bi bsu)
        ssBlockState .= s'
        return r

    {-# INLINE updateBakerRestakeEarnings #-}
    updateBakerRestakeEarnings bi bre = do
        s <- use ssBlockState
        (r, s') <- lift (BS.bsoUpdateBakerRestakeEarnings s bi bre)
        ssBlockState .= s'
        return r

    {-# INLINE updateCredentialKeys #-}
    updateCredentialKeys accIndex credIndex newKeys = do
        s <- use ssBlockState
        s' <- lift (BS.bsoSetAccountCredentialKeys s accIndex credIndex newKeys)
        ssBlockState .= s'

    {-# INLINE getUpdateKeyCollection #-}
    getUpdateKeyCollection = lift . BS.bsoGetUpdateKeyCollection =<< use ssBlockState

    {-# INLINE getNextUpdateSequenceNumber #-}
    getNextUpdateSequenceNumber uty = do
        s <- use ssBlockState
        lift (BS.bsoGetNextUpdateSequenceNumber s uty)

    {-# INLINE enqueueUpdate #-}
    enqueueUpdate tt p = do
        s <- use ssBlockState
        s' <- lift (BS.bsoEnqueueUpdate s tt p)
        ssBlockState .= s'

    {-# INLINE incrementPLTUpdateSequenceNumber #-}
    incrementPLTUpdateSequenceNumber = do
        s <- use ssBlockState
        s' <- lift (BS.bsoIncrementPLTUpdateSequenceNumber s)
        ssBlockState .= s'

    {-# INLINE getTokenIndex #-}
    getTokenIndex tokenId = do
        blockState <- use ssBlockState
        lift (BS.getTokenIndex blockState tokenId)

    {-# INLINE getTokenConfiguration #-}
    getTokenConfiguration tokenIndex = do
        blockState <- use ssBlockState
        lift (BS.getTokenConfiguration blockState tokenIndex)

    withBlockStateRollback op = do
        s0 <- use ssBlockState
        snapshot <- lift $ BS.bsoSnapshotState s0
        (res, doRollback) <- op
        when doRollback $ do
            s1 <- use ssBlockState
            s2 <- lift $ BS.bsoRollback s1 snapshot
            ssBlockState .= s2
        return res

    runPLT tokenIx op = do
        s <- use ssBlockState
        let initialExecutionState =
                PLTExecutionState
                    { _plteBlockState = s,
                      _plteEvents = [],
                      _plteEnergyUsed = 0
                    }

        (res, finalExecutionState) <- lift $ do
            config <- BS.getTokenConfiguration s tokenIx
            let context =
                    PLTExecutionContext
                        { _pltecTokenIndex = tokenIx,
                          _pltecConfiguration = config,
                          _pltecEnergy = Nothing
                        }
            runKernelT op context initialExecutionState
        return (res, reverse $ finalExecutionState ^. plteEvents)

    runPLTWithEnergy tokenIx energy op = do
        s <- use ssBlockState
        let initialExecutionState =
                PLTExecutionState
                    { _plteBlockState = s,
                      _plteEvents = [],
                      _plteEnergyUsed = 0
                    }

        (res, finalExecutionState) <- lift $ do
            config <- BS.getTokenConfiguration s tokenIx
            let context =
                    PLTExecutionContext
                        { _pltecTokenIndex = tokenIx,
                          _pltecConfiguration = config,
                          _pltecEnergy = Just energy
                        }
            runKernelT op context initialExecutionState
        return (res, reverse $ finalExecutionState ^. plteEvents, finalExecutionState ^. plteEnergyUsed)

    createToken pltConfig = do
        s <- use ssBlockState
        (tokenIx, s') <- lift $ BS.bsoCreateToken s pltConfig
        ssBlockState .= s'
        return tokenIx

-- | Execute the computation using the provided context and scheduler state.
-- The return value is the value produced by the computation and the updated state of the scheduler.
runSchedulerT ::
    (Monad m) =>
    SchedulerT m a ->
    ContextState ->
    SchedulerState m ->
    m (a, SchedulerState m)
runSchedulerT computation contextState initialState = do
    (value, resultingState, ()) <- runRWST (_runSchedulerT computation) contextState initialState
    return (value, resultingState)

newtype KernelT fail ret m a = KernelT {runKernelT' :: ReaderT PLTExecutionContext (ContT (Either fail ret) (StateT (PLTExecutionState m) m)) a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (PLTExecutionState m),
          MonadReader PLTExecutionContext
        )

runKernelT :: (Monad m) => KernelT fail a m a -> PLTExecutionContext -> PLTExecutionState m -> m (Either fail a, PLTExecutionState m)
runKernelT a tokenIx = runStateT (runContT (runReaderT (runKernelT' a) tokenIx) (return . Right))

instance MonadTrans (KernelT fail ret) where
    lift = KernelT . lift . lift . lift

-- | The block state types for `KernelT fail ret m` are derived from the base monad `m`.
deriving via (MGSTrans (KernelT fail ret) m) instance BlockStateTypes (KernelT fail ret m)

instance (BS.BlockStateOperations m, PVSupportsPLT (MPV m)) => PLTKernelQuery (KernelT fail ret m) where
    type PLTAccount (KernelT fail ret m) = (AccountIndex, AccountAddress)
    getTokenState key = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        lift $ BS.getTokenState bs tokenIx key
    getAccount addr = do
        bs <- use plteBlockState
        lift $ fmap ((,addr) . fst) <$> BS.bsoGetAccount bs addr
    getAccountBalance (acctIndex, _) = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        lift $
            BS.bsoGetAccountByIndex bs acctIndex >>= \case
                Nothing -> error "getAccountBalance: Account does not exist"
                Just acct -> BS.getAccountTokenBalance acct tokenIx
    getAccountState (acctIndex, _) key = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        lift $
            BS.bsoGetAccountByIndex bs acctIndex >>= \case
                Nothing -> error "getAccountState: Account does not exist"
                Just acct -> BS.getAccountTokenState acct tokenIx key
    getAccountCanonicalAddress (acctIndex, _) = do
        bs <- use plteBlockState
        lift $
            BS.bsoGetAccountByIndex bs acctIndex >>= \case
                Nothing -> error "getAccountCanonicalAddress: Account does not exist"
                Just acct -> BS.getAccountCanonicalAddress acct
    getCirculatingSupply = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        lift $ BS.getTokenCirculatingSupply bs tokenIx
    getDecimals = asks (_pltDecimals . _pltecConfiguration)

instance (BS.BlockStateOperations m, PVSupportsPLT (MPV m)) => PLTKernelUpdate (KernelT fail ret m) where
    setTokenState key mValue = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        newBS <- lift $ BS.bsoSetTokenState bs tokenIx key mValue
        plteBlockState .= newBS
    setAccountState (account, _) key mValue = do
        let updates = case mValue of
                Nothing -> [(key, TASVDelete)]
                Just value -> [(key, TASVUpdate value)]
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        newBS <- lift $ BS.bsoUpdateTokenAccountModuleState bs tokenIx account updates
        plteBlockState .= newBS
        return ()
    transfer (accIxFrom, accAddrFrom) (accIxTo, accAddrTo) amount mbMemo = do
        context <- ask
        let tokenIx = _pltecTokenIndex context
        bs0 <- use plteBlockState
        mbs2 <- lift $ do
            mbs1 <-
                BS.bsoUpdateTokenAccountBalance bs0 tokenIx accIxFrom $
                    negativeTokenAmountDelta amount
            case mbs1 of
                Nothing -> return Nothing -- Sender has insufficient funds.
                Just bs1 -> do
                    mbs2 <-
                        BS.bsoUpdateTokenAccountBalance bs1 tokenIx accIxTo $
                            toTokenAmountDelta amount
                    return $ case mbs2 of
                        Nothing ->
                            -- This case cannot occur if the total supply is accurately
                            -- recorded, since it would imply that the total supply
                            -- exceeds the maximum representable amount.
                            error "Token kernel: transfer would overflow receiver balance"
                        Just bs2 ->
                            Just bs2
        case mbs2 of
            Nothing -> return False
            Just bs2 -> do
                -- Log the transfer event.
                plteEvents
                    %= ( TokenTransfer
                            { ettTokenId = _pltTokenId (_pltecConfiguration context),
                              ettFrom = accAddrFrom,
                              ettTo = accAddrTo,
                              ettAmount = TokenAmount amount (_pltDecimals (_pltecConfiguration context)),
                              ettMemo = mbMemo
                            }
                            :
                       )
                plteBlockState .= bs2
                return True

    logTokenEvent eventType eventDetails = do
        tokenId <- asks (_pltTokenId . _pltecConfiguration)
        plteEvents %= (TokenModuleEvent tokenId eventType eventDetails :)

instance (BS.BlockStateOperations m, PVSupportsPLT (MPV m)) => PLTKernelPrivilegedUpdate (KernelT fail ret m) where
    mint (accIx, accAddr) amount = do
        context <- ask
        let tokenIx = _pltecTokenIndex context
        bs <- use plteBlockState
        currentSupply <- lift $ BS.getTokenCirculatingSupply bs tokenIx
        if maxBound - amount < currentSupply
            then return False -- Minting would overflow the circulating supply.
            else do
                bs' <- lift $ BS.bsoSetTokenCirculatingSupply bs tokenIx (currentSupply + amount)
                mbNewBs <-
                    lift $
                        BS.bsoUpdateTokenAccountBalance
                            bs'
                            tokenIx
                            accIx
                            ( TokenAmountDelta (fromIntegral (theTokenRawAmount amount))
                            )
                case mbNewBs of
                    Nothing ->
                        -- This case cannot occur if the total supply is accurately
                        -- recorded, since it would imply that the total supply
                        -- exceeds the maximum representable amount.
                        error "Token kernel: mint would overflow receiver balance"
                    Just newBs -> do
                        -- Log the mint event.
                        plteEvents
                            %= ( TokenMint
                                    { etmTokenId = _pltTokenId (_pltecConfiguration context),
                                      etmTarget = accAddr,
                                      etmAmount = TokenAmount amount (_pltDecimals (_pltecConfiguration context))
                                    }
                                    :
                               )
                        plteBlockState .= newBs
                        return True
    burn (accIx, accAddr) amount = do
        context <- ask
        let tokenIx = _pltecTokenIndex context
        bs0 <- use plteBlockState
        mbs1 <- lift $ BS.bsoUpdateTokenAccountBalance bs0 tokenIx accIx (negativeTokenAmountDelta amount)
        case mbs1 of
            Nothing -> return False
            Just bs1 -> do
                bs2 <- lift $ do
                    currentSupply <- BS.getTokenCirculatingSupply bs1 tokenIx
                    when (currentSupply < amount) $
                        -- This case cannot occur if the total supply is accurately
                        -- recorded, since it would imply that the initial balance
                        -- on the target account exceeded to the total supply.
                        error "Token kernel: burn would underflow total supply"
                    BS.bsoSetTokenCirculatingSupply bs1 tokenIx (currentSupply - amount)
                -- Log the burn event.
                plteEvents
                    %= ( TokenBurn
                            { etbTokenId = _pltTokenId (_pltecConfiguration context),
                              etbTarget = accAddr,
                              etbAmount = TokenAmount amount (_pltDecimals (_pltecConfiguration context))
                            }
                            :
                       )
                plteBlockState .= bs2
                return True

instance
    ( BS.BlockStateOperations m,
      PVSupportsPLT (MPV m)
    ) =>
    PLTKernelChargeEnergy (KernelT (PLTExecutionError fail) ret m)
    where
    pltTickEnergy nrg = do
        plteEnergyUsed += nrg
        mbAvailableEnergy <- asks _pltecEnergy
        case mbAvailableEnergy of
            Nothing -> return ()
            Just availableEnergy -> do
                energyUsed <- use plteEnergyUsed
                unless (availableEnergy >= energyUsed) $ KernelT $ ReaderT $ \_ -> ContT $ \_ -> return (Left PLTEOutOfEnergy)

instance {-# OVERLAPPABLE #-} (Monad m, fail ~ fail') => (PLTKernelFail fail (KernelT fail' ret m)) where
    -- To abort, we simply drop the continuation and return the error.
    pltError err = KernelT $ ReaderT $ \_ -> ContT $ \_ -> return (Left err)

instance (Monad m) => (PLTKernelFail fail (KernelT (PLTExecutionError fail) ret m)) where
    -- To abort, we simply drop the continuation and return the error.
    pltError err = KernelT $ ReaderT $ \_ -> ContT $ \_ -> return (Left $ PLTEFail err)
