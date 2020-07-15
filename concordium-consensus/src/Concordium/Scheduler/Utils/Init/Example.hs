{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -Wno-deprecations #-}
module Concordium.Scheduler.Utils.Init.Example
    (initialState, initialStateWithMateuszAccount, makeTransaction, makeTransferTransaction, createCustomAccount,
     mateuszAccount, dummyCredential) where

import qualified Data.HashMap.Strict as Map

import qualified Concordium.Crypto.SignatureScheme as Sig

import qualified Data.PQueue.Prio.Max as Queue

import qualified Concordium.ID.Types as ID

import Concordium.Types
import qualified Concordium.Scheduler.Cost as Cost
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler.Environment as Types
import qualified Concordium.GlobalState.IdentityProviders as IPS
import qualified Concordium.GlobalState.Basic.BlockState as BlockState
import qualified Concordium.GlobalState.Basic.BlockState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Parameters (CryptographicParameters)
import qualified Concordium.Scheduler.Runner as Runner

import qualified Acorn.Core as Core
import qualified Acorn.Parser as Parser
import Concordium.Scheduler(execTransactions)

import Acorn.Utils.Init
import Acorn.Utils.Init.TH

import Lens.Micro.Platform

import Data.Maybe(fromJust)

import qualified Data.Text as Text

import Concordium.ID.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import Prelude hiding(mod)

-- * The rest of this module is an example global state with the simple account and simple counter modules loaded.

-- |Global state with the core modules and the example counter modules loaded.
baseStateWithCounter :: (Parser.Env, Core.ModuleName, ProcessedModules)
baseStateWithCounter = foldl handleFile
                             baseState
                             $(embedFiles [Left  "test/contracts/SimpleAccount.acorn"
                                          ,Left  "test/contracts/SimpleCounter.acorn"]
                                          )

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

inCtxTm :: Text.Text -> Core.Atom origin
inCtxTm = Core.Var . Core.LocalDef . inCtx

initialTrans :: Int -> [Types.BareTransaction]
initialTrans n = map initSimpleCounter $ enumFromTo 1 n

initSimpleCounter :: Int -> Types.BareTransaction
initSimpleCounter n = Runner.signTx
                             mateuszKP
                             header
                             payload
    where payload = Types.encodePayload (Types.InitContract 0
                                          simpleCounterHash
                                          (fromJust (Map.lookup "Counter" simpleCounterTyCtx))
                                          (Core.Atom (Core.Literal (Core.Int64 0)))
                                        )
          header = Runner.TransactionHeader{
            thNonce = fromIntegral n,
            thSender = mateuszAccount,
            thEnergyAmount = 100000,
            thExpiry = dummyMaxTransactionExpiryTime
            }


{-# WARNING makeTransaction "Dummy transaction, only use for testing." #-}
-- All transactions have the same arrival time (0)
makeTransaction :: Bool -> ContractAddress -> Nonce -> Types.BlockItem
makeTransaction inc ca n = Types.normalTransaction . Types.fromBareTransaction 0 $ Runner.signTx mateuszKP header payload
    where
        header = Runner.TransactionHeader{
            thNonce = n,
            thSender = mateuszAccount,
            thEnergyAmount = 1000000,
            thExpiry = dummyMaxTransactionExpiryTime
            }
        payload = Types.encodePayload (Types.Update 0
                                                    ca
                                                    (Core.App (if inc then (inCtxTm "Inc") else (inCtxTm "Dec"))
                                                              [Core.Literal (Core.Int64 10)])
                                                    )
{-# WARNING makeTransferTransaction "Dummy transaction, only use for testing." #-}
makeTransferTransaction :: (Sig.KeyPair, AccountAddress) -> AccountAddress -> Amount -> Nonce -> Types.BlockItem
makeTransferTransaction (fromKP, fromAddress) toAddress amount n =
  Types.normalTransaction . Types.fromBareTransaction 0 $ Runner.signTx fromKP header payload
    where
        header = Runner.TransactionHeader{
            thNonce = n,
            thSender = fromAddress,
            thEnergyAmount = Cost.checkHeader 100 1 + Cost.transferAccount,
            thExpiry = dummyMaxTransactionExpiryTime
        }
        payload = Types.encodePayload (Types.Transfer (AddressAccount toAddress) amount)

initialStateWithMateuszAccount :: BlockState.BasicBirkParameters
                               -> CryptographicParameters
                               -> [Account]
                               -> IPS.IdentityProviders
                               -> Int
                               -> Amount
                               -> BlockState.BlockState
initialStateWithMateuszAccount birkParams cryptoParams bakerAccounts ips n amount =
    initialState birkParams cryptoParams bakerAccounts ips n [createCustomAccount amount mateuszKP mateuszAccount]

createCustomAccount :: Amount -> Sig.KeyPair -> AccountAddress -> Account
createCustomAccount amount kp address =
    newAccount (ID.makeSingletonAC (Sig.correspondingVerifyKey kp)) address (ID.cdvRegId credential)
        & (accountAmount .~ amount)
        . (accountCredentials .~ Queue.singleton dummyMaxValidTo credential)
  where credential = dummyCredential address dummyMaxValidTo dummyCreatedAt

-- |State with the given number of contract instances of the counter contract specified.
{-# WARNING initialState "Dummy initial state, only use for testing." #-}
initialState :: BlockState.BasicBirkParameters
             -> CryptographicParameters
             -> [Account]
             -> IPS.IdentityProviders
             -> Int
             -> [Account]
             -> BlockState.BlockState
initialState birkParams cryptoParams bakerAccounts ips n customAccounts =
    let (_, _, mods) = foldl handleFile
                           baseState
                           $(embedFiles [Left "test/contracts/SimpleAccount.acorn"
                                        ,Left "test/contracts/SimpleCounter.acorn"]
                            )
        allAccounts = customAccounts ++ bakerAccounts
        initAccount = foldl (flip Acc.putAccountWithRegIds)
                            Acc.emptyAccounts
                            allAccounts
        initialAmount = sum (_accountAmount <$> allAccounts)
        gs = BlockState.emptyBlockState birkParams cryptoParams &
               (BlockState.blockIdentityProviders .~ ips) .
               (BlockState.blockAccounts .~ initAccount) .
               (BlockState.blockModules .~ Mod.fromModuleList (moduleList mods)) .
               (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- 10 GTU minted per slot.
        finState = Types.execSI (execTransactions (map (Types.normalTransaction . Types.fromBareTransaction 0) (initialTrans n)))
                                Types.emptySpecialBetaAccounts
                                Types.dummyChainMeta
                                maxBound
                                gs
        gs' = finState ^. Types.ssBlockState
    in gs' & (BlockState.blockAccounts .~ initAccount) .
             (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- also reset the bank after execution to maintain invariants.
