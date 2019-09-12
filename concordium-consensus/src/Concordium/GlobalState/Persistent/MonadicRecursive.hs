{-# LANGUAGE MultiParamTypeClasses #-}
module Concordium.GlobalState.Persistent.MonadicRecursive where

import Data.Functor.Foldable

class Monad m => MRecursive m t where
    mproject :: t -> m (Base t t)

class Monad m => MCorecursive m t where
    membed :: Base t t -> m t
