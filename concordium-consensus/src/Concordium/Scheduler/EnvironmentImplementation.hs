{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Concordium.Scheduler.EnvironmentImplementation where

import Concordium.Scheduler.Environment

import Data.HashMap.Strict as Map

import Control.Monad.Reader
import Control.Monad.RWS.Strict

import Concordium.Scheduler.Types
import Concordium.GlobalState.BlockState
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
  getModuleInterfaces addr = (Mod.getInterfaces addr . blockModules) <$> get

instance SchedulerMonad SchedulerImplementation where

  {-# INLINE getContractInstance #-}
  getContractInstance addr = (Ins.getInstance addr . blockInstances) <$> get

  {-# INLINE getAccount #-}
  getAccount addr = (Acc.getAccount addr . blockAccounts) <$> get

  commitStateAndAccountChanges (ChangeSet{..}) = do
    s <- get
    -- INVARIANT: the invariant which should hold at this point is that any
    -- changed instance must exist in the global state moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    let Ins.Instances is = blockInstances s
    let instances' = Ins.Instances $ Map.foldlWithKey' (\acc addr (amnt, val) -> Map.adjust (\i -> i { iamount = amnt, imodel = val }) addr acc)
                                                       is
                                                       newContractStates
    let accounts' = Map.foldl' (\r acc -> Acc.putAccount acc r) (blockAccounts s) newAccounts
    put (s { blockInstances = instances', blockAccounts = accounts'})

  {-# INLINE commitModule #-}
  commitModule mhash iface viface = do
    s <- get
    let mod' = Mod.putInterfaces mhash iface viface (blockModules s)
    case mod' of
      Nothing -> return False
      Just modules -> True <$ put (s { blockModules = modules })

  {-# INLINE putNewInstance #-}
  putNewInstance istance = do
    s <- get
    case Ins.newInstance istance (blockInstances s) of
      Nothing -> return False
      Just is -> True <$ put (s { blockInstances = is}) 

  {-# INLINE putNewAccount #-}
  putNewAccount acc = do
    s <- get
    let addr = accountAddress acc
    if addr `Acc.exists` blockAccounts s then return False
    else True <$ put (s { blockAccounts = Acc.putAccount acc (blockAccounts s) })

  {-# INLINE payForExecution #-}
  -- INVARIANT: should only be called when there are enough funds available, and thus it does not check the amounts.
  payForExecution addr amnt = do
    s <- get
    let acc = Acc.unsafeGetAccount addr (blockAccounts s) -- should be safe since accounts must exist before this is called (invariant)
    let camnt = accountAmount acc
    let newamount = camnt - (energyToGtu amnt)
    let accs = Acc.putAccount (acc { accountAmount = newamount}) (blockAccounts s)
    put (s { blockAccounts = accs })
    return newamount

  {-# INLINE refundEnergy #-}
  refundEnergy addr amnt = do
    s <- get
    let acc = Acc.unsafeGetAccount addr (blockAccounts s) -- should be safe since accounts must exist before this is called (invariant)
    let camnt = accountAmount acc
    let newamount = camnt + (energyToGtu amnt)
    let accs = Acc.putAccount (acc { accountAmount = newamount}) (blockAccounts s)
    put (s { blockAccounts = accs })

  {-# INLINE firstFreeAddress #-}
  firstFreeAddress =
    Ins.firstFreeContract . blockInstances <$> get
