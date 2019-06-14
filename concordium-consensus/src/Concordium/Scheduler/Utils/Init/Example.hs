{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Concordium.Scheduler.Utils.Init.Example (initialState, makeTransaction, mateuszAccount) where

import qualified Data.HashMap.Strict as Map
import System.Random

import Concordium.Crypto.SignatureScheme(KeyPair, SchemeId(Ed25519))
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.Crypto.Ed25519Signature(randomKeyPair)
import Concordium.Crypto.SHA256(Hash(..))
import qualified Data.FixedByteString as FBS

import Concordium.Types
import qualified Concordium.ID.Account as AH
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types

import qualified Concordium.GlobalState.TreeState.Basic as BlockState
import qualified Concordium.GlobalState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Parameters(BirkParameters)
import qualified Concordium.Scheduler.Runner as Runner

import qualified Acorn.Core as Core
import qualified Acorn.Parser as Parser
import Concordium.Scheduler(execTransactions)

import Acorn.Utils.Init
import Acorn.Utils.Init.TH

import Lens.Micro.Platform

import Data.Maybe(fromJust)

import qualified Data.Text as Text

import Prelude hiding(mod)

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
simpleCounterHash = let (_, mref, _, _) = Parser.modNames (first baseStateWithCounter) Map.! "SimpleCounter"  in mref

simpleCounterCtx :: Map.HashMap Text.Text Core.Name
simpleCounterCtx = let (_, _, tms, _) = Parser.modNames (first baseStateWithCounter) Map.! "SimpleCounter" in tms

simpleCounterTyCtx :: Map.HashMap Text.Text Core.TyName
simpleCounterTyCtx = let (_, _, _, tys) = Parser.modNames (first baseStateWithCounter) Map.! "SimpleCounter" in tys

inCtx :: Text.Text -> Core.Name
inCtx txt = fromJust (Map.lookup txt simpleCounterCtx)

inCtxTm :: Text.Text -> Core.Expr origin
inCtxTm = Core.Atom . Core.LocalDef . inCtx

initialTrans :: Int -> [Types.Transaction]
initialTrans n = map initSimpleCounter $ enumFromTo 1 n
 
mateuszAccount :: AccountAddress
mateuszAccount = AH.accountAddress (Sig.verifyKey mateuszKP) Ed25519

mateuszKP :: KeyPair
mateuszKP = fst (randomKeyPair (mkStdGen 0))

blockPointer :: BlockHash
blockPointer = Hash (FBS.pack (replicate 32 (fromIntegral (0 :: Word))))

makeHeader :: KeyPair -> Nonce -> Energy -> Types.TransactionHeader
makeHeader kp nonce amount = Types.makeTransactionHeader Ed25519 (Sig.verifyKey kp) nonce amount blockPointer 

initSimpleCounter :: Int -> Types.Transaction
initSimpleCounter n = Runner.signTx
                             mateuszKP
                             (makeHeader mateuszKP (fromIntegral n) 10000)
                             (Types.encodePayload (Types.InitContract 0 simpleCounterHash (fromJust (Map.lookup "Counter" simpleCounterTyCtx)) (Core.Literal (Core.Int64 0)) (-1))) -- -1 as the size is ignore by encodePayload

makeTransaction :: Bool -> ContractAddress -> Nonce -> Types.Transaction
makeTransaction inc ca n = Runner.signTx mateuszKP hdr payload
    where
        hdr = makeHeader mateuszKP n 100000
        payload = Types.encodePayload (Types.Update 0 ca (Core.App (if inc then (inCtxTm "Inc") else (inCtxTm "Dec")) (Core.Literal (Core.Int64 10))) (-1))


-- |State with the given number of contract instances of the counter contract specified.
initialState :: BirkParameters -> [Account] -> Int -> BlockState.BlockState
initialState birkParams bakerAccounts n = 
    let (_, _, mods) = foldl handleFile
                           baseState
                           $(embedFiles [Left "test/contracts/SimpleAccount.acorn"
                                        ,Left "test/contracts/SimpleCounter.acorn"]
                            )
        initialAmount = 2 ^ (62 :: Int)
        initAccount = foldl (flip Acc.putAccount)
                            (Acc.putAccount (Types.Account mateuszAccount 1 initialAmount [] Nothing (Sig.verifyKey mateuszKP) Ed25519 []) Acc.emptyAccounts)
                            bakerAccounts
        gs = BlockState.emptyBlockState birkParams &
               (BlockState.blockAccounts .~ initAccount) .
               (BlockState.blockModules .~ Mod.fromModuleList (moduleList mods)) .
               (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- 10 GTU minted per slot.
        gs' = Types.execSI (execTransactions (initialTrans n)) Types.dummyChainMeta gs
    in gs' & (BlockState.blockAccounts .~ initAccount) .
             (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- also reset the bank after execution to maintain invariants.

