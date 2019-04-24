{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Concordium.Scheduler.FootprintImplementation where

import Concordium.Scheduler.Environment

import Data.HashMap.Strict as Map
import Data.HashSet as Set

import Lens.Micro.Platform

import Control.Monad.Reader
import Control.Monad.RWS.Strict

import Concordium.Scheduler.Types
import Concordium.GlobalState.BlockState
import qualified Concordium.GlobalState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod
import qualified Concordium.GlobalState.Instances as Ins

-- monoid instance derived as product 
type Footprint = (Set.HashSet ContractAddress, Set.HashSet AccountAddress)

tellContract :: MonadWriter Footprint m => ContractAddress -> m ()
tellContract c = tell (Set.singleton c, Set.empty)

tellAccount :: MonadWriter Footprint m => AccountAddress -> m ()
tellAccount c = tell (Set.empty, Set.singleton c)


newtype FootprintImplementation a = FootprintImplementation { _runScheduler :: RWS ChainMetadata Footprint BlockState a }
    deriving (Functor, Applicative, Monad, MonadReader ChainMetadata, MonadState BlockState, MonadWriter Footprint)

runFI :: FootprintImplementation a -> ChainMetadata -> BlockState -> (a, BlockState, Footprint)
runFI sc cd gs = runRWS (_runScheduler sc) cd gs

execFI :: FootprintImplementation a -> ChainMetadata -> BlockState -> (BlockState, Footprint)
execFI sc cd gs = execRWS (_runScheduler sc) cd gs

evalFI :: FootprintImplementation a -> ChainMetadata -> BlockState -> (a, Footprint)
evalFI sc cd gs = evalRWS (_runScheduler sc) cd gs

instance StaticEnvironmentMonad FootprintImplementation where
  {-# INLINE getChainMetadata #-}
  getChainMetadata = ask

  {-# INLINE getModuleInterfaces #-}
  getModuleInterfaces addr = Mod.getInterfaces addr <$> use blockModules

instance SchedulerMonad FootprintImplementation where

  {-# INLINE getContractInstance #-}
  getContractInstance addr = do
    -- NB: even if lookup fails we should record the address since this transction should then not be reordered
    -- with respect to creation of new contracts
    tellContract addr
    Ins.getInstance addr <$> use blockInstances

  {-# INLINE getAccount #-}
  getAccount addr = do
    -- NB: even if lookup fails we should record the address since this transction should then not be reordered
    -- with respect to creation of new accounts
    tellAccount addr
    Acc.getAccount addr <$> use blockAccounts

  commitStateAndAccountChanges cs = do
    s <- get
    -- first log all changed instances that need to be changed
    mapM_ tellContract (Map.keys (cs ^. newContractStates))
    -- and all changed accounts
    mapM_ tellAccount (Map.keys (cs ^. newAccounts))
    -- INVARIANT: the invariant which should hold at this point is that any
    -- changed instance must exist in the global state.
    -- Moreover all instances are distinct by the virtue of a HashMap being a function
    let instances' = Map.foldlWithKey' (\acc addr (amnt, val) -> Ins.updateInstanceAt addr amnt val acc)
                                                       (s ^. blockInstances)
                                                       (cs ^. newContractStates)
    let accounts' = Map.foldl' (flip Acc.putAccount) (s ^. blockAccounts) (cs ^. newAccounts)
    blockInstances .= instances'
    blockAccounts .= accounts'

  {-# INLINE commitModule #-}
  commitModule mhash iface viface = do
    mods <- use blockModules
    let mod' = Mod.putInterfaces mhash iface viface mods
    case mod' of
      Nothing -> return False
      Just modules -> True <$ (blockModules .= modules)

  {-# INLINE putNewInstance #-}
  putNewInstance istance = do
    istances <- use blockInstances
    let (ca, istances') = Ins.createInstance istance istances
    blockInstances .= istances'
    tellContract ca
    return ca

  {-# INLINE increaseAccountNonce #-}
  increaseAccountNonce addr = do
    tellAccount addr
    blockAccounts . ix addr . accountNonce += 1
    -- s <- get
    -- let acc = Acc.unsafeGetAccount addr (blockAccounts s) -- NB: Relies on precondition.
    -- put (s { blockAccounts = Acc.putAccount (acc & accountNonce +~ 1) (blockAccounts s) })

  {-# INLINE putNewAccount #-}
  putNewAccount acc = do
    accs <- use blockAccounts
    let addr = acc ^. accountAddress
    tellAccount addr

    if addr `Acc.exists` accs then return False
    else True <$ (blockAccounts .= Acc.putAccount acc accs)

  {-# INLINE payForExecution #-}
  -- INVARIANT: should only be called when there are enough funds available, and thus it does not check the amounts.
  payForExecution addr amnt = do
    tellAccount addr
    blockAccounts . singular (ix addr) . accountAmount <%= subtract (energyToGtu amnt)


  {-# INLINE refundEnergy #-}
  refundEnergy addr amnt = do
    -- NB: do not need to log here since this should only be called after
    -- payForExecution has been called, and thus the change has already been
    -- logged.
    blockAccounts . ix addr . accountAmount += (energyToGtu amnt)

