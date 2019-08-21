{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Concordium.GlobalState.Rust.Block (
  BlockFields
  , PendingBlock(..)
  , BlockPointer(..)
  ) where

import Concordium.GlobalState.Rust.FFI
