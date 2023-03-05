{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |This module implements the 'TransactionVerifier' for consensus protocol V1.
module Concordium.KonsensusV1.TransactionVerifier where

-- |Where a received transaction stems from.
-- A transaction is either received as part of a block or it
-- has been submitted individually to the consensus.
data TransactionOrigin = Block | Individual
    deriving (Eq, Show)
