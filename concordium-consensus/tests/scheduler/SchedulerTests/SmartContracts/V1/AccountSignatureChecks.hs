{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module tests that tests account signature checks
-- and retrieving account keys.
module SchedulerTests.SmartContracts.V1.AccountSignatureChecks (tests) where

import Control.Monad (when)
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Test.Hspec
import qualified Data.Serialize as S
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString as BS
import Data.Word

import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Wasm

import qualified SchedulerTests.Helpers as Helpers
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers
import Concordium.Crypto.SignatureScheme (SchemeId(Ed25519), KeyPair (verifyKey), sign, Signature(..))
import Control.Monad.IO.Class

seed :: Int
seed = 17

-- empty state, no accounts, no modules, no instances
initialBlockState :: Types.IsProtocolVersion pv => Helpers.PersistentBSM pv (HashedPersistentBlockState pv)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [Helpers.makeTestAccountFromSeed 1_000 seed]

sourceFile :: FilePath
sourceFile = "../concordium-base/smart-contracts/testdata/contracts/v1/account-signature-checks.wasm"

deployModule1 ::
    forall pv.
    Types.IsProtocolVersion pv =>
    PersistentBlockState pv ->
    Helpers.PersistentBSM
        pv
        ( (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1),
          PersistentBlockState pv
        )
deployModule1 = InvokeHelpers.deployModuleV1 (Types.protocolVersion @pv) sourceFile

-- Initialize contract a.
initContract ::
    Types.IsProtocolVersion pv =>
    PersistentBlockState pv ->
    (InvokeHelpers.PersistentModuleInterfaceV GSWasm.V1, WasmModuleV GSWasm.V1) ->
    Helpers.PersistentBSM pv (Types.ContractAddress, PersistentBlockState pv)
initContract =
    InvokeHelpers.initContractV1
        (Helpers.accountAddressFromSeed seed)
        (InitName "init_contract")
        emptyParameter
        0

-- |Invoke the entrypoint @@ on contract @a@.
getKeys ::
    Types.IsProtocolVersion pv =>
    Types.ContractAddress ->
    Parameter ->
    HashedPersistentBlockState pv ->
    Helpers.PersistentBSM pv InvokeContract.InvokeContractResult
getKeys ccContract ccParameter bs = do
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "contract.get_keys",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx (Types.ChainMetadata 123) bs

-- |Invoke the entrypoint @@ on contract @a@.
checkSignature ::
    Types.IsProtocolVersion pv =>
    Types.ContractAddress ->
    Parameter ->
    HashedPersistentBlockState pv ->
    Helpers.PersistentBSM pv InvokeContract.InvokeContractResult
checkSignature ccContract ccParameter bs = do
    let ctx =
            InvokeContract.ContractContext
                { ccInvoker = Nothing,
                  ccAmount = 0,
                  ccMethod = ReceiveName "contract.check_signature",
                  ccEnergy = 1_000_000_000,
                  ..
                }
    InvokeContract.invokeContract ctx (Types.ChainMetadata 123) bs

runGetKeysTests :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runGetKeysTests spv pvString = when (Types.demoteProtocolVersion spv >= Types.P4) $ do
    Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule1 initState
        (addr1, mutState) <- initContract bsWithMod mod1
        bs <- freezeBlockState mutState
        let param = Parameter (BSS.toShort (S.encode (Helpers.accountAddressFromSeed seed)))
        result <- getKeys addr1 param bs
        case result of
          InvokeContract.Failure{..} -> do
            if Types.supportsAccountSignatureChecks (Types.protocolVersion @pv) then 
              liftIO $ assertFailure $ pvString ++ ": Call failed with reason: " ++ show rcrReason
            else
              liftIO $ assertEqual (pvString ++ ": Execution should cause runtime error: ") Types.RuntimeFailure rcrReason
          InvokeContract.Success{rcrReturnValue = Nothing} -> do
            liftIO $ assertFailure "Call succeded with no return value."
          InvokeContract.Success{rcrReturnValue = Just rv} -> do
            let expected = S.runPut $ do
                  S.putWord8 1 -- length of the outer map
                  S.putWord8 0 -- credential index
                  S.putWord8 1 -- length of the inner map
                  S.putWord8 0 -- key index
                  S.put Ed25519 -- scheme id
                  S.put (verifyKey (Helpers.keyPairFromSeed seed)) -- the public key
                  S.putWord8 1 -- threshold for the credential
                  S.putWord8 1 -- threshold for the account
            liftIO (assertEqual (pvString ++ ": Retrieved key is correct: ") expected rv)
            
runCheckSignatureTests ::
  forall pv. Types.IsProtocolVersion pv =>
  Types.SProtocolVersion pv ->
  String ->
  -- |Parameter for the call.
  BS.ByteString ->
  -- |Expected response code.
  Word64 ->
  Assertion
runCheckSignatureTests spv pvString paramBS expectedCode = when (Types.demoteProtocolVersion spv >= Types.P4) $ do
    Helpers.runTestBlockState @pv $ do
        initState <- thawBlockState =<< initialBlockState
        (mod1, bsWithMod) <- deployModule1 initState
        (addr1, mutState) <- initContract bsWithMod mod1
        bs <- freezeBlockState mutState
        let param = Parameter (BSS.toShort paramBS)
        result <- checkSignature addr1 param bs
        case result of
          InvokeContract.Failure{..} -> do
            if Types.supportsAccountSignatureChecks (Types.protocolVersion @pv) then 
              liftIO $ assertFailure $ pvString ++ ": Call failed with reason: " ++ show rcrReason
            else
              liftIO $ assertEqual (pvString ++ ": Execution should cause runtime error: ") Types.RuntimeFailure rcrReason
          InvokeContract.Success{rcrReturnValue = Nothing} -> do
            liftIO $ assertFailure "Call succeded with no return value."
          InvokeContract.Success{rcrReturnValue = Just rv} -> do
            let expected = S.runPut $ S.putWord64le expectedCode
            liftIO (assertEqual (pvString ++ ": Correct response code: ") (BS.unpack expected) (BS.unpack rv))

-- |Test for success
runCheckSignatureTest1 :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runCheckSignatureTest1 spv pvString = do
        let message = BS.replicate 123 17
        let kp = Helpers.keyPairFromSeed seed
        let Signature sig = sign kp message
        let paramBS = S.runPut $ do
              S.put $ Helpers.accountAddressFromSeed seed
              S.putWord32le (fromIntegral (BS.length message))
              S.putByteString message
              S.putWord8 1 -- number of outer signatures
              S.putWord8 0 -- credential index
              S.putWord8 1 -- number of inner signatures
              S.putWord8 0 -- key index
              S.put Ed25519 -- signature scheme ID
              S.putShortByteString sig
        runCheckSignatureTests spv pvString paramBS 0

-- |Test missing account.
runCheckSignatureTest2 :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runCheckSignatureTest2 spv pvString = do
        let message = BS.replicate 123 17
        let kp = Helpers.keyPairFromSeed seed
        let Signature sig = sign kp message
        let paramBS = S.runPut $ do
              S.put $ Helpers.accountAddressFromSeed (seed + 1) -- use incorrect seed, account does not exist.
              S.putWord32le (fromIntegral (BS.length message))
              S.putByteString message
              S.putWord8 1 -- number of outer signatures
              S.putWord8 0 -- credential index
              S.putWord8 1 -- number of inner signatures
              S.putWord8 0 -- key index
              S.put Ed25519 -- signature scheme ID
              S.putShortByteString sig
        runCheckSignatureTests spv pvString paramBS 0x02_0000_0000

-- |Test incorrect signature.
runCheckSignatureTest3 :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runCheckSignatureTest3 spv pvString = do
        let message = BS.replicate 123 17
        let kp = Helpers.keyPairFromSeed (seed + 1) -- use incorrect seed for keys, leading to incorrect signature
        let Signature sig = sign kp message
        let paramBS = S.runPut $ do
              S.put $ Helpers.accountAddressFromSeed seed -- use incorrect seed, account does not exist.
              S.putWord32le (fromIntegral (BS.length message))
              S.putByteString message
              S.putWord8 1 -- number of outer signatures
              S.putWord8 0 -- credential index
              S.putWord8 1 -- number of inner signatures
              S.putWord8 0 -- key index
              S.put Ed25519 -- signature scheme ID
              S.putShortByteString sig
        runCheckSignatureTests spv pvString paramBS 0x0b_0000_0000

-- |Test malformed data.
runCheckSignatureTest4 :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runCheckSignatureTest4 spv pvString = do
        let message = BS.replicate 123 17
        let kp = Helpers.keyPairFromSeed (seed + 1) -- use incorrect seed for keys, leading to incorrect signature
        let Signature sig = sign kp message
        let paramBS = S.runPut $ do
              S.put $ Helpers.accountAddressFromSeed seed -- use incorrect seed, account does not exist.
              S.putWord32le maxBound -- incorrect length data
              S.putByteString message
              S.putWord8 1 -- number of outer signatures
              S.putWord8 0 -- credential index
              S.putWord8 1 -- number of inner signatures
              S.putWord8 0 -- key index
              S.put Ed25519 -- signature scheme ID
              S.putShortByteString sig
        runCheckSignatureTests spv pvString paramBS 0x0a_0000_0000

tests :: Spec
tests = describe "V1: Get account keys and check signatures" $ do
    specify "Get account keys" $
        sequence_ $
            Helpers.forEveryProtocolVersion runGetKeysTests
    specify "Check account signature keys 1" $
        sequence_ $
            Helpers.forEveryProtocolVersion runCheckSignatureTest1
    specify "Check account signature keys 2" $
        sequence_ $
            Helpers.forEveryProtocolVersion runCheckSignatureTest2
    specify "Check account signature keys 3" $
        sequence_ $
            Helpers.forEveryProtocolVersion runCheckSignatureTest3
    specify "Check account signature keys 4" $
        sequence_ $
            Helpers.forEveryProtocolVersion runCheckSignatureTest4
