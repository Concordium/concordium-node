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
                                   ReceiveContext(..),
                                   InitContext(..),
                                   link) where

import Prelude hiding(fail)

import Concordium.Types
import Concordium.Types.Acorn.Interfaces
import Concordium.Types.Execution
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Rewards
import Concordium.GlobalState.Transactions

import Acorn.Types(ReceiveContext(..), InitContext(..), link)


dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotNumber = 0
                               , blockHeight = 0
                               , finalizedHeight = 0}
