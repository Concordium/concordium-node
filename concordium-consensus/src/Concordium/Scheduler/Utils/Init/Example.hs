{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Utils.Init.Example (initialState, makeTransaction, mateuszAccount, mateuszACI) where

import qualified Data.HashMap.Strict as Map
import System.Random

import Concordium.Crypto.SignatureScheme(KeyPair, SchemeId(Ed25519))
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.Crypto.Ed25519Signature(randomKeyPair)


import Concordium.Types
import qualified Concordium.ID.AccountHolder as AH
import qualified Concordium.ID.Types as AH
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types

import qualified Concordium.GlobalState.BlockState as BlockState
import qualified Concordium.GlobalState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod

import Lens.Micro.Platform

import Data.Maybe(fromJust)

import qualified Data.Text as Text

import Prelude hiding(mod)

import qualified Acorn.Core as Core
import qualified Acorn.Parser as Parser
import Concordium.Scheduler(execTransactions)

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

initialTrans :: Int -> [Types.Transaction]
initialTrans n = map initSimpleCounter $ enumFromTo 1 n
 
mateuszAccount :: AccountAddress
mateuszAccount = AH.accountAddress mateuszACI

mateuszKP :: KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

mateuszACI :: AH.AccountCreationInformation
mateuszACI = AH.createAccount (Sig.verifyKey mateuszKP)


initSimpleCounter :: Int -> Types.Transaction
initSimpleCounter n = Types.signTransaction Ed25519
                                            mateuszKP
                                            (Types.TransactionHeader { thSender = mateuszAccount
                                                                     , thGasAmount = 10000
                                                                     , thNonce = fromIntegral n})
                       (Types.encodePayload (Types.InitContract 1000 simpleCounterHash (fromJust (Map.lookup "Counter" simpleCounterTyCtx)) (Core.Literal (Core.Int64 0))))

makeTransaction :: Bool -> ContractAddress -> Nonce -> Types.Transaction
makeTransaction inc ca n = Types.signTransaction Ed25519 mateuszKP hdr payload
    where
        hdr = Types.TransactionHeader {thSender = mateuszAccount, thGasAmount = 100000, thNonce = n}
        payload = Types.encodePayload (Types.Update 0 ca (Core.App (if inc then (inCtxTm "Inc") else (inCtxTm "Dec")) (Core.Literal (Core.Int64 10))))


-- |State with the given number of contract instances of the counter contract specified.
initialState :: Int -> BlockState.BlockState
initialState n = 
    let (_, _, mods) = foldl handleFile
                           baseState
                           $(embedFiles [Left "test/contracts/SimpleAccount.acorn"
                                        ,Left "test/contracts/SimpleCounter.acorn"]
                            )
        initAccount = Acc.putAccount (Types.Account mateuszAccount 1 (2 ^ (62 :: Int)) mateuszACI) Acc.emptyAccounts
        gs = BlockState.emptyBlockState &
               (BlockState.blockAccounts .~ initAccount) .
               (BlockState.blockModules .~ Mod.Modules mods)
        gs' = Types.execSI (execTransactions (initialTrans n)) Types.dummyChainMeta gs
    in gs' & BlockState.blockAccounts .~ initAccount
