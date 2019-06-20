{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import Data.HashMap.Strict as Map
import Data.Map.Strict as OrdMap

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.RWS.Strict

import Concordium.ID.Types(cdi_regId)
import Concordium.Scheduler.Types
import Concordium.GlobalState.TreeState(auAddress, updateAccount)
import Concordium.GlobalState.TreeState.Basic
import qualified Concordium.GlobalState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod
import qualified Concordium.GlobalState.Instances as Ins

newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWS ChainMetadata () BlockState a }
    deriving (Functor, Applicative, Monad, MonadReader ChainMetadata, MonadState BlockState)

runSI :: SchedulerImplementation a -> ChainMetadata -> BlockState -> (a, BlockState)
runSI sc cd gs = let (a, s, _) = runRWS (_runScheduler sc) cd gs in (a, s)

execSI :: SchedulerImplementation a -> ChainMetadata -> BlockState -> BlockState
execSI sc cd gs = fst (execRWS (_runScheduler sc) cd gs)

evalSI :: SchedulerImplementation a -> ChainMetadata -> BlockState -> a
evalSI sc cd gs = fst (evalRWS (_runScheduler sc) cd gs)

instance StaticEnvironmentMonad SchedulerImplementation where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = ask

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces addr = Mod.getInterfaces addr <$> use blockModules

instance SchedulerMonad SchedulerImplementation where

  {-# INLINE getContractInstance #-}
  getContractInstance addr = Ins.getInstance addr <$> use blockInstances

  {-# INLINE getAccount #-}
  getAccount addr = Acc.getAccount addr <$> use blockAccounts

  {-# INLINE accountRegIdExists #-}
  accountRegIdExists regid =
    Acc.regIdExists regid <$> use blockAccounts

  commitStateAndAccountChanges !cs = do
    s <- get
    -- INVARIANT: the invariant which should hold at this point is that any
    -- changed instance must exist in the global state moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    let instances' = Map.foldlWithKey' (\iis addr (amnt, val) -> Ins.updateInstanceAt addr amnt val iis)
                                                       (s ^. blockInstances)
                                                       (cs ^. instanceUpdates)
    let accounts' = Map.foldl' (\accs upd -> let acc = Acc.unsafeGetAccount (upd ^. auAddress) accs
                                             in Acc.putAccount (updateAccount upd acc) accs)
                               (s ^. blockAccounts)
                               (cs ^. accountUpdates)
    blockInstances .= instances'
    blockAccounts .= accounts'

  {-# INLINE commitModule #-}
  commitModule mhash iface viface source = do
    mods <- use blockModules
    let mod' = Mod.putInterfaces mhash iface viface source mods
    case mod' of
      Nothing -> return False
      Just modules -> True <$ (blockModules .= modules)

  {-# INLINE putNewInstance #-}
  putNewInstance !istance = do
    istances <- use blockInstances
    let (ca, istances') = Ins.createInstance istance istances
    blockInstances .= istances'
    return ca

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce !addr =
    blockAccounts . ix addr . accountNonce += 1


  {-# INLINE addAccountCredential #-}
  addAccountCredential !addr !cdi = do
    blockAccounts . ix addr . accountCredentials %= (cdi :)
    blockAccounts %= Acc.recordRegId (cdi_regId cdi)

  {-# INLINE addAccountEncryptionKey #-}
  addAccountEncryptionKey addr encKey = blockAccounts . ix addr . accountEncryptionKey .= (Just encKey)


  {-# INLINE putNewAccount #-}
  putNewAccount !acc = do
    accs <- use blockAccounts
    let addr = acc ^. accountAddress
    if addr `Acc.exists` accs then return False
    else True <$ (blockAccounts .= Acc.putAccount acc accs)

  {-# INLINE energyToGtu #-}
  -- |FIXME: Replace this by sourcing the exchange rate from somewhere else.
  energyToGtu = return . fromIntegral

  {-# INLINE notifyExecutionCost #-}
  notifyExecutionCost !amnt =
    blockBank . executionCost += amnt

  {-# INLINE notifyIdentityProviderCredential #-}
  notifyIdentityProviderCredential idk =
    blockBank . identityIssuersRewards . at idk . non 0 += 1

  {-# INLINE getBakerInfo #-}
  getBakerInfo bid =
    use (blockBirkParameters . to birkBakers . at bid)

  addBaker binfo = do
    bps <- use blockBirkParameters
    let bid = nextBakerId bps
    let bakers' = OrdMap.insert bid binfo (birkBakers bps)
    blockBirkParameters .= bps { birkBakers = bakers',
                                 nextBakerId = bid + 1
                               }
    return bid

  {-# INLINE removeBaker #-}
  removeBaker bid = do
    bps <- use blockBirkParameters
    blockBirkParameters .= bps { birkBakers = OrdMap.delete bid (birkBakers bps) }

  {-# INLINE updateBakerSignKey #-}
  updateBakerSignKey bid signKey =
    blockBirkParameters %= (\bps -> bps { birkBakers = OrdMap.adjust (\binfo -> binfo {bakerSignatureVerifyKey = signKey}) bid (birkBakers bps)})

  {-# INLINE updateBakerAccount #-}
  updateBakerAccount bid bacc = 
    blockBirkParameters %= (\bps -> bps { birkBakers = OrdMap.adjust (\binfo -> binfo {bakerAccount = bacc}) bid (birkBakers bps)})
