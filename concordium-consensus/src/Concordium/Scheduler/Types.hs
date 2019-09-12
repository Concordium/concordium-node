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
                                   module Concordium.Types,
                                   module Concordium.Types.Execution,
                                   module Concordium.Types.Acorn.Interfaces,
                                   module Concordium.GlobalState.Transactions,
                                   module Concordium.GlobalState.Instances,
                                   module Concordium.GlobalState.Rewards,
                                   module Concordium.GlobalState.Parameters,
                                   module Concordium.GlobalState.IdentityProviders,
                                   IdentityProviderIdentity,
                                   ReceiveContext(..),
                                   InitContext(..),
                                   linkWithMaxSize) where

import Prelude hiding(fail)

import Concordium.Types
import Concordium.Types.Acorn.Interfaces hiding(Value, Interface, ValueInterface, ContractValue)
import qualified Concordium.Types.Acorn.Interfaces as Interfaces
import Concordium.Types.Execution
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders

import Concordium.ID.Types(IdentityProviderIdentity)

import Acorn.Types(ReceiveContext(..), InitContext(..), linkWithMaxSize)
import qualified Acorn.Core as Core

type Value = Interfaces.Value NoAnnot
type ContractValue = Interfaces.LinkedContractValue NoAnnot
type ValueInterface = Interfaces.UnlinkedValueInterface NoAnnot
type Interface = Interfaces.Interface Core.UA

type Module = Core.Module Core.UA

dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotNumber = 0
                               , blockHeight = 0
                               , finalizedHeight = 0}
