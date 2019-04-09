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

newtype SchedulerImplementation a = SchedulerImplementation { _runScheduler :: RWS ChainMetadata () GlobalState a }
    deriving (Functor, Applicative, Monad, MonadReader ChainMetadata, MonadState GlobalState)

runSI :: SchedulerImplementation a -> ChainMetadata -> GlobalState -> (a, GlobalState)
runSI sc cd gs = let (a, s, _) = runRWS (_runScheduler sc) cd gs in (a, s)

execSI :: SchedulerImplementation a -> ChainMetadata -> GlobalState -> GlobalState
execSI sc cd gs = fst (execRWS (_runScheduler sc) cd gs)

evalSI :: SchedulerImplementation a -> ChainMetadata -> GlobalState -> a
evalSI sc cd gs = fst (evalRWS (_runScheduler sc) cd gs)

instance StaticEnvironmentMonad SchedulerImplementation where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = ask

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces addr = (Map.lookup addr . modules) <$> get

instance SchedulerMonad SchedulerImplementation where

  {-# INLINE getContractInstance #-}
  getContractInstance addr = (Map.lookup addr . instances) <$> get

  {-# INLINE getAccount #-}
  getAccount addr = (Map.lookup addr . accounts) <$> get

  commitStateAndAccountChanges (ChangeSet{..}) = do
    s <- get
    -- INVARIANT: the invariant which should hold at this point is that any
    -- changed instance must exist in the global state moreover all instances
    -- are distinct by the virtue of a HashMap being a function
    let instances' = Map.foldlWithKey' (\acc addr (amnt, val) -> Map.adjust (\i -> i { iamount = amnt, lState = val }) addr acc)
                                       (instances s)
                                       newContractStates
    let accounts' = Map.union newAccounts (accounts s) -- union is left-biased
    put (s { instances = instances', accounts = accounts'})

  {-# INLINE commitModule #-}
  commitModule mhash iface viface = do
    s <- get
    if mhash `Map.member` (modules s)
    then return False
    else True <$ put (s { modules = Map.insert mhash (iface, viface) (modules s) })

  {-# INLINE putNewInstance #-}
  putNewInstance rfun iface viface msgType model amnt impls = do
    s <- get
    let addr = firstFreeContract s
    put (s { instances = Map.insert addr (Instance addr rfun (iface, viface) msgType model amnt impls) (instances s)})
    return addr

  {-# INLINE putNewAccount #-}
  putNewAccount acc = do
    s <- get
    let addr = accountAddress acc
    if addr `Map.member` accounts s then return True
    else False <$ put (s { accounts = Map.insert addr acc (accounts s) })

  {-# INLINE payForExecution #-}
  -- INVARIANT: should only be called when there are enough funds available, and thus it does not check the amounts.
  payForExecution addr amnt = do
    s <- get
    let camnt = accountAmount $ (accounts s) Map.! addr -- should be safe since accounts must exist before this is called (invariant)
    let newamount = camnt - (energyToGtu amnt)
    put (s { accounts = Map.adjust (\acc -> acc { accountAmount = newamount }) addr (accounts s) })
    return newamount

  {-# INLINE refundEnergy #-}
  refundEnergy addr amnt = do
    s <- get
    let camnt = accountAmount $ (accounts s) Map.! addr -- should be safe since accounts must exist before this is called (invariant)
    let newamount = camnt + (energyToGtu amnt)
    put (s { accounts = Map.adjust (\acc -> acc { accountAmount = newamount }) addr (accounts s) })
