{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Concordium.Scheduler.Types (module Concordium.Scheduler.Types,
                                   module Concordium.GlobalState.Types,
                                   module Concordium.GlobalState.Execution,
                                   module Concordium.GlobalState.Acorn.Interfaces) where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as Map

import Prelude hiding(fail)

import qualified Acorn.Core as Core
import Concordium.GlobalState.Types
import Concordium.GlobalState.Acorn.Interfaces
import Concordium.GlobalState.Execution

-- | This is the part of the global store that is needed by the interpreter to look up, e.g., values of other contract
-- instances, and by the typechecker to know the types of definitions and contract instances.
data GlobalState = GlobalState 
    {instances :: !(HashMap ContractAddress Instance)  -- ^Contract instances.
    ,accounts :: !(HashMap AccountAddress Account)      -- ^FIXME: Unclear what the distinction between accounts/instances is.
    ,modules :: !(HashMap Core.ModuleRef (Interface, ValueInterface)) -- ^Interfaces of deployed modules.
    }

firstFreeContract :: GlobalState -> ContractAddress
firstFreeContract (GlobalState{..}) = 
  ContractAddress (fromIntegral (length instances)) 0 

emptyGlobalState :: GlobalState
emptyGlobalState = GlobalState {
  instances = Map.empty
  , accounts = Map.empty
  , modules = Map.empty
  }


dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotNumber = 0
                               , blockHeight = 0
                               , finalizedHeight = 0}
