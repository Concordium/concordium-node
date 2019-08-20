{-# LANGUAGE TemplateHaskell, RecordWildCards, MultiParamTypeClasses, TypeFamilies, GeneralizedNewtypeDeriving, DerivingStrategies, DerivingVia #-}
module Concordium.GlobalState.Rust.BlockState where

import Concordium.GlobalState.BlockState as BS
import Concordium.GlobalState.Rust.Block as RBS
import Concordium.GlobalState.Basic.BlockState as BBS

newtype PureBlockStateMonad m a = PureBlockStateMonad {runPureBlockStateMonad :: m a}
    deriving (Functor, Applicative, Monad)
    deriving (BS.BlockStateQuery) via (BBS.PureBlockStateMonad m)
    deriving (BS.BlockStateOperations) via (BBS.PureBlockStateMonad m)

type instance BS.BlockPointer (Concordium.GlobalState.Rust.BlockState.PureBlockStateMonad m) = RBS.BlockPointer
type instance BS.UpdatableBlockState (Concordium.GlobalState.Rust.BlockState.PureBlockStateMonad m) = BBS.BlockState
