{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Utils.Init.Example (initialState, update, mateuszAccount, mateuszACI) where

import qualified Data.HashMap.Strict as Map
import System.Random

import qualified Concordium.Crypto.Signature as S
import Concordium.GlobalState.Types
import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types

import Data.Maybe(fromJust)

import qualified Data.Text as Text

import Prelude hiding(mod)

import qualified Acorn.Core as Core
import qualified Acorn.Parser as Parser
import Concordium.Scheduler.Scheduler(execBlock)

import Acorn.Utils.Init
import Acorn.Utils.Init.TH

-- * The rest of this module is an example global state with the simple account and simple counter modules loaded.

-- |Global state with the core modules and the example counter modules loaded.
baseStateWithCounter :: (Parser.Env, Core.ModuleName, ProcessedModules)
baseStateWithCounter = foldl handleFile
                             baseState
                             $(embedFiles ([Left  "test/contracts/SimpleAccount.acorn"
                                           ,Left  "test/contracts/SimpleCounter.acorn"]
                                          ))

first :: (a, b, c) -> a
first (x, _, _) = x

simpleCounterHash :: Core.ModuleRef
simpleCounterHash = let (_, mref, _, _) = fromJust (Map.lookup "SimpleCounter" (Parser.modNames (first baseStateWithCounter))) in mref

simpleCounterCtx :: Map.HashMap Text.Text Core.Name
simpleCounterCtx = let (_, _, tms, _) = fromJust (Map.lookup "SimpleCounter" (Parser.modNames (first baseStateWithCounter))) in tms

simpleCounterTyCtx :: Map.HashMap Text.Text Core.TyName
simpleCounterTyCtx = let (_, _, _, tys) = fromJust (Map.lookup "SimpleCounter" (Parser.modNames (first baseStateWithCounter))) in tys

inCtx :: Text.Text -> Core.Name
inCtx txt = fromJust (Map.lookup txt simpleCounterCtx)

inCtxTm :: Text.Text -> Core.Expr origin
inCtxTm = Core.Atom . Core.LocalDef . inCtx

initialTrans :: Int -> [Types.MessageTy]
initialTrans n = map initSimpleCounter $ enumFromTo 0 (n-1)
 
mateuszAccount :: AccountAddress
mateuszAccount = AH.accountAddress mateuszACI

mateuszACI :: AH.AccountCreationInformation
mateuszACI = AH.createAccount (S.verifyKey (fst (S.randomKeyPair (mkStdGen 0))))


initSimpleCounter :: Int -> Types.MessageTy
initSimpleCounter n = (Types.Header { sender = mateuszAccount
                                    , gasAmount = 10000
                                    , nonce = fromIntegral n},
                       Types.encodePayload (Types.InitContract 1000 simpleCounterHash (fromJust (Map.lookup "Counter" simpleCounterTyCtx)) (Core.Literal (Core.Int64 0))))

-- | Generate a given number of transactions, all of which are updates to a contract instance given as second argument.
update ::
  (Integral a, Integral b) =>
  a -- ^Whether to increment or decrement. If n == 0 (mod 9) then decrement, else increment.
  -> b -- ^The update is going to affect the contract on address Tid-x where x is this argument.
  -> Types.MessageTy
update b n = (Types.Header { sender = mateuszAccount
                           , gasAmount = 100000
                           , nonce = fromIntegral n + 1000
                           },
              Types.encodePayload (Types.Update 0 (ContractAddress (fromIntegral n) 0) (Core.App (if b `rem` 9 == 0 then (inCtxTm "Dec") else (inCtxTm "Inc")) (Core.Literal (Core.Int64 10)))))

-- |State with the given number of contract instances of the counter contract specified.
initialState :: Int -> Types.GlobalState
initialState n = 
    let (_, _, mods) = foldl handleFile
                           baseState
                           $(embedFiles [Left "test/contracts/SimpleAccount.acorn"
                                        ,Left "test/contracts/SimpleCounter.acorn"]
                            )
        gs = Types.GlobalState Map.empty (Map.fromList [(mateuszAccount, Types.Account mateuszAccount 1 (2 ^ (62 :: Int)) mateuszACI)]) mods
        gs' = Types.execSI (execBlock (initialTrans n)) Types.dummyChainMeta gs
    in gs'
