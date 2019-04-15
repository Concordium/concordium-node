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
                                   module Concordium.GlobalState.Acorn.Interfaces,
                                   module Concordium.GlobalState.Transactions,
                                   module Concordium.GlobalState.Instances,
                                   module Concordium.GlobalState) where

import Prelude hiding(fail)

import Concordium.GlobalState.Types
import Concordium.GlobalState.Acorn.Interfaces
import Concordium.GlobalState.Execution
import Concordium.GlobalState
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Transactions

dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotNumber = 0
                               , blockHeight = 0
                               , finalizedHeight = 0}
