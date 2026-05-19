{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.Scheduler.ProtocolLevelTokens.KernelImplementation where

import Control.Monad
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..))
import Lens.Micro.Platform

import Concordium.Types.Tokens

import qualified Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Persistent.Account.ProtocolLevelTokens
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens (PLTConfiguration (..), TokenIndex)
import Concordium.GlobalState.TreeState
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.Types

data PLTExecutionState m = PLTExecutionState
    { -- | The current block state.
      _plteBlockState :: !(UpdatableBlockState m),
      -- | The events that have been emitted during the execution in reverse order.
      _plteEvents :: ![Event],
      -- | The energy used for the execution of the PLT module.
      _plteEnergyUsed :: !Energy,
      -- | Tracking the token mutable state has been updated during the execution.
      _plteStateIsDirty :: !Bool
    }
makeLenses ''PLTExecutionState

-- | Execution context for PLT computations.
data PLTExecutionContext m = PLTExecutionContext
    { -- | The token index.
      _pltecTokenIndex :: !TokenIndex,
      -- | The PLT configuration.
      _pltecConfiguration :: !PLTConfiguration,
      -- | The available energy.
      _pltecEnergy :: !(Maybe Energy),
      -- | The mutable token state.
      _pltecMutableState :: !(MutableTokenState m)
    }

-- | PLT module execution error.
data PLTExecutionError fail
    = -- | The PLT module run out of energy during execution.
      PLTEOutOfEnergy
    | -- | The PLT module encountered a runtime error during execution.
      PLTEFail fail

newtype KernelT fail ret m a = KernelT {runKernelT' :: ReaderT (PLTExecutionContext m) (ContT (Either fail ret) (StateT (PLTExecutionState m) m)) a}
    deriving
        ( Functor,
          Applicative,
          Monad,
          MonadState (PLTExecutionState m),
          MonadReader (PLTExecutionContext m)
        )

runKernelT :: (Monad m) => KernelT fail a m a -> PLTExecutionContext m -> PLTExecutionState m -> m (Either fail a, PLTExecutionState m)
runKernelT a tokenIx = runStateT (runContT (runReaderT (runKernelT' a) tokenIx) (return . Right))

instance MonadTrans (KernelT fail ret) where
    lift = KernelT . lift . lift . lift

-- | The block state types for `KernelT fail ret m` are derived from the base monad `m`.
deriving via (MGSTrans (KernelT fail ret) m) instance BlockStateTypes (KernelT fail ret m)

instance (BS.BlockStateOperations m, PVSupportsHaskellManagedPLT (MPV m)) => PLTKernelQuery (KernelT fail ret m) where
    type PLTAccount (KernelT fail ret m) = (AccountIndex, AccountAddress)
    getTokenState key = do
        mutableState <- asks _pltecMutableState
        lift $ BS.lookupTokenState key mutableState
    getAccount addr = do
        bs <- use plteBlockState
        lift $ fmap ((,addr) . fst) <$> BS.bsoGetAccount bs addr
    getAccountIndex (acctIndex, _acct) = return acctIndex
    getAccountByIndex accountIndex = do
        bs <- use plteBlockState
        lift $
            BS.bsoGetAccountByIndex bs accountIndex >>= \case
                Nothing -> return Nothing
                Just account -> do
                    address <- BS.getAccountCanonicalAddress account
                    return $ Just (accountIndex, address)
    getAccountBalance (acctIndex, _) = do
        tokenIx <- asks _pltecTokenIndex
        bs <- use plteBlockState
        lift $
            BS.bsoGetAccountByIndex bs acctIndex >>= \case
                Nothing -> error "getAccountBalance: Account does not exist"
                Just acct -> BS.getAccountTokenBalance acct tokenIx
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

instance (BS.BlockStateOperations m, PVSupportsHaskellManagedPLT (MPV m)) => PLTKernelUpdate (KernelT fail ret m) where
    setTokenState key mValue = do
        plteStateIsDirty .= True
        mutableState <- asks _pltecMutableState
        lift $ BS.updateTokenState key mValue mutableState

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
                              ettFrom = HolderAccount accAddrFrom,
                              ettTo = HolderAccount accAddrTo,
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

    touch (accIx, _) = do
        context <- ask
        let tokenIx = _pltecTokenIndex context
        bs0 <- use plteBlockState
        mbBs1 <- lift $ BS.bsoTouchTokenAccount bs0 tokenIx accIx
        case mbBs1 of
            Nothing -> return False
            Just bs1 -> do
                plteBlockState .= bs1
                return True

instance (BS.BlockStateOperations m, PVSupportsHaskellManagedPLT (MPV m)) => PLTKernelPrivilegedUpdate (KernelT fail ret m) where
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
                            (TokenAmountDelta (fromIntegral (theTokenRawAmount amount)))
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
                                      etmTarget = HolderAccount accAddr,
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
                              etbTarget = HolderAccount accAddr,
                              etbAmount = TokenAmount amount (_pltDecimals (_pltecConfiguration context))
                            }
                            :
                       )
                plteBlockState .= bs2
                return True

instance
    ( BS.BlockStateOperations m,
      PVSupportsHaskellManagedPLT (MPV m)
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
