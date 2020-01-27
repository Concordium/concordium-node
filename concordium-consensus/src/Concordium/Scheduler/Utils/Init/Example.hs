{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Wno-deprecations #-}
module Concordium.Scheduler.Utils.Init.Example {-# WARNING "This module should not be used in production code" #-}
    (initialState, makeTransaction, mateuszAccount, dummyCredential, dummyExpiryTime) where

import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as OrdMap
import System.Random

import Concordium.Crypto.SignatureScheme(KeyPair(..))
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.Crypto.Ed25519Signature(randomKeyPair)

import qualified Data.PQueue.Prio.Max as Queue
import qualified Data.Hashable as IntHash
import qualified Data.FixedByteString as FBS

import qualified Concordium.ID.Types as ID

import Concordium.Types
import qualified Concordium.Scheduler.Types as Types
import qualified Concordium.Scheduler.EnvironmentImplementation as Types
import qualified Concordium.Scheduler.Environment as Types

import qualified Concordium.GlobalState.Basic.BlockState as BlockState
import qualified Concordium.GlobalState.Basic.BlockState.Account as Acc
import qualified Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Parameters(BirkParameters, CryptographicParameters)
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

-- This credential value is invalid and does not satisfy the invariants normally expected of credentials.
-- Should only be used when only the existence of a credential is needed in testing, but the credential
-- will neither be serialized, nor inspected.
{-# WARNING dummyCredential "Invalid credential, only for testing." #-}
dummyCredential :: ID.AccountAddress -> ID.CredentialExpiryTime -> ID.CredentialDeploymentValues
dummyCredential address pExpiry  = ID.CredentialDeploymentValues
    {
      cdvAccount = ID.ExistingAccount address,
      cdvRegId = dummyRegId address,
      cdvIpId = ID.IP_ID 0,
      cdvThreshold = ID.Threshold 2,
      cdvArData = [],
      cdvPolicy = ID.Policy {
        pAttributeListVariant = 0,
        pItems = OrdMap.empty,
        ..
        },
      ..
    }

{-# WARNING dummyExpiryTime "Invalid expiry time, only for testing." #-}
dummyExpiryTime :: ID.CredentialExpiryTime
dummyExpiryTime = maxBound

-- Derive a dummy registration id from a verification key. This hashes the
-- account address, and uses it as a seed of a random number generator.
dummyRegId :: ID.AccountAddress -> ID.CredentialRegistrationID
dummyRegId addr = ID.RegIdCred . FBS.pack $ bytes
  where bytes = take (FBS.fixedLength (undefined :: ID.RegIdSize)) . randoms . mkStdGen $ IntHash.hash addr



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

mateuszAccount :: AccountAddress
mateuszAccount = AccountAddress (FBS.pack (take ID.accountAddressSize (randoms (mkStdGen 0))))

mateuszKP :: KeyPair
mateuszKP = uncurry Sig.KeyPairEd25519 . fst $ randomKeyPair (mkStdGen 0)

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
            thEnergyAmount = 100000
            }


makeTransaction :: Bool -> ContractAddress -> Nonce -> Types.BareTransaction
makeTransaction inc ca n = Runner.signTx mateuszKP header payload
    where
        header = Runner.TransactionHeader{
            thNonce = n,
            thSender = mateuszAccount,
            thEnergyAmount = 1000000
            }
        payload = Types.encodePayload (Types.Update 0
                                                    ca
                                                    (Core.App (if inc then (inCtxTm "Inc") else (inCtxTm "Dec"))
                                                              [Core.Literal (Core.Int64 10)])
                                                    )

-- |State with the given number of contract instances of the counter contract specified.
initialState :: BirkParameters
             -> CryptographicParameters
             -> [Account]
             -> [Types.IpInfo]
             -> Int
             -> BlockState.BlockState
initialState birkParams cryptoParams bakerAccounts ips n =
    let (_, _, mods) = foldl handleFile
                           baseState
                           $(embedFiles [Left "test/contracts/SimpleAccount.acorn"
                                        ,Left "test/contracts/SimpleCounter.acorn"]
                            )
        initialAmount = 2 ^ (62 :: Int)
        customAccounts = [newAccount (ID.makeSingletonAC (Sig.correspondingVerifyKey mateuszKP)) mateuszAccount
                          & (accountAmount .~ initialAmount)
                          . (accountCredentials .~ Queue.singleton dummyExpiryTime (dummyCredential mateuszAccount dummyExpiryTime))]
        initAccount = foldl (flip Acc.putAccountWithRegIds)
                            Acc.emptyAccounts
                            (customAccounts ++ bakerAccounts)
        gs = BlockState.emptyBlockState birkParams cryptoParams &
               (BlockState.blockIdentityProviders .~ Types.IdentityProviders (Map.fromList (map (\r -> (Types.ipIdentity r, r)) ips))) .
               (BlockState.blockAccounts .~ initAccount) .
               (BlockState.blockModules .~ Mod.fromModuleList (moduleList mods)) .
               (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- 10 GTU minted per slot.
        gs' = Types.execSI (execTransactions (initialTrans n)) Types.emptySpecialBetaAccounts Types.dummyChainMeta gs
    in gs' & (BlockState.blockAccounts .~ initAccount) .
             (BlockState.blockBank .~ Types.makeGenesisBankStatus initialAmount 10) -- also reset the bank after execution to maintain invariants.
