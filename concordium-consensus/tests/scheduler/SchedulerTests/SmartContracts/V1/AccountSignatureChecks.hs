{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module tests account signature checks and retrieving account keys in P6.
module SchedulerTests.SmartContracts.V1.AccountSignatureChecks (tests) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as S
import Data.Word
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)
import Test.Hspec

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Wasm as GSWasm
import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Types as Types
import qualified Concordium.Types.Execution as Types
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Wasm

import Concordium.Crypto.SignatureScheme (KeyPair (verifyKey), SchemeId (Ed25519), Signature (..), sign)
import Control.Monad.IO.Class
import qualified SchedulerTests.Helpers as Helpers
import qualified SchedulerTests.SmartContracts.V1.InvokeHelpers as InvokeHelpers

seed :: Int
seed = 17

-- Set up a state with a single account with 4 credentials, and account threshold 3.
initialBlockState :: Types.IsProtocolVersion pv => Helpers.PersistentBSM pv (HashedPersistentBlockState pv)
initialBlockState = do
    let cred1 = Helpers.makeTestCredentialFromSeed (seed + 100)
    let cred2 = Helpers.makeTestCredentialFromSeed (seed + 200)
    let cred4 = Helpers.makeTestCredentialFromSeed (seed + 300)
    let acc = updateAccountCredentials [] (Map.fromList [(1, cred1), (2, cred2), (4, cred4)]) 3 =<< Helpers.makeTestAccountFromSeed 1_000 seed
    Helpers.createTestBlockStateWithAccountsM [acc]

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

-- |Invoke the entrypoint @get_keys@ on contract @contract@.
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

-- |Invoke the entrypoint @check_signature@ on contract @contract@.
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
                if Types.supportsAccountSignatureChecks (Types.protocolVersion @pv)
                    then liftIO $ assertFailure $ pvString ++ ": Call failed with reason: " ++ show rcrReason
                    else liftIO $ assertEqual (pvString ++ ": Execution should cause runtime error: ") Types.RuntimeFailure rcrReason
            InvokeContract.Success{rcrReturnValue = Nothing} -> do
                liftIO $ assertFailure "Call succeded with no return value."
            InvokeContract.Success{rcrReturnValue = Just rv} -> do
                let expected = S.runPut $ do
                        S.putWord8 4 -- length of the outer map
                        S.putWord8 0 -- credential index
                        S.putWord8 1 -- length of the inner map
                        S.putWord8 0 -- key index
                        S.put Ed25519 -- scheme id
                        S.put (verifyKey (Helpers.keyPairFromSeed seed)) -- the public key
                        S.putWord8 1 -- threshold for the credential
                        S.putWord8 1 -- credential index
                        S.putWord8 1 -- length of the inner map
                        S.putWord8 0 -- key index
                        S.put Ed25519 -- scheme id
                        S.put (verifyKey (Helpers.keyPairFromSeed (seed + 100))) -- the public key
                        S.putWord8 1 -- threshold for the credential
                        S.putWord8 2 -- credential index
                        S.putWord8 1 -- length of the inner map
                        S.putWord8 0 -- key index
                        S.put Ed25519 -- scheme id
                        S.put (verifyKey (Helpers.keyPairFromSeed (seed + 200))) -- the public key
                        S.putWord8 1 -- threshold for the credential
                        S.putWord8 4 -- credential index
                        S.putWord8 1 -- length of the inner map
                        S.putWord8 0 -- key index
                        S.put Ed25519 -- scheme id
                        S.put (verifyKey (Helpers.keyPairFromSeed (seed + 300))) -- the public key
                        S.putWord8 1 -- threshold for the credential
                        S.putWord8 3 -- threshold for the account
                liftIO (assertEqual (pvString ++ ": Retrieved key is correct: ") expected rv)

runCheckSignatureTests ::
    forall pv.
    Types.IsProtocolVersion pv =>
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
                if Types.supportsAccountSignatureChecks (Types.protocolVersion @pv)
                    then liftIO $ assertFailure $ pvString ++ ": Call failed with reason: " ++ show rcrReason
                    else liftIO $ assertEqual (pvString ++ ": Execution should cause runtime error: ") Types.RuntimeFailure rcrReason
            InvokeContract.Success{rcrReturnValue = Nothing} -> do
                liftIO $ assertFailure "Call succeded with no return value."
            InvokeContract.Success{rcrReturnValue = Just rv} -> do
                liftIO (assertBool "Success means the protocol version must support signature checks." (Types.supportsAccountSignatureChecks (Types.protocolVersion @pv)))
                let expected = S.runPut $ S.putWord64le expectedCode
                liftIO (assertEqual (pvString ++ ": Correct response code: ") (BS.unpack expected) (BS.unpack rv))

-- |Test for success
runCheckSignatureTest1 :: forall pv. Types.IsProtocolVersion pv => Types.SProtocolVersion pv -> String -> Assertion
runCheckSignatureTest1 spv pvString = do
    let message = BS.replicate 123 17
    let kp0 = Helpers.keyPairFromSeed seed
    let kp1 = Helpers.keyPairFromSeed (seed + 100)
    let kp4 = Helpers.keyPairFromSeed (seed + 300)
    let Signature sig0 = sign kp0 message
    let Signature sig1 = sign kp1 message
    let Signature sig4 = sign kp4 message
    let paramBS = S.runPut $ do
            S.put $ Helpers.accountAddressFromSeed seed
            S.putWord32le (fromIntegral (BS.length message))
            S.putByteString message
            S.putWord8 3 -- number of outer signatures
            S.putWord8 0 -- credential index
            S.putWord8 1 -- number of inner signatures
            S.putWord8 0 -- key index
            S.put Ed25519 -- signature scheme ID
            S.putShortByteString sig0
            S.putWord8 1 -- credential index
            S.putWord8 1 -- number of inner signatures
            S.putWord8 0 -- key index
            S.put Ed25519 -- signature scheme ID
            S.putShortByteString sig1
            S.putWord8 4 -- credential index
            S.putWord8 1 -- number of inner signatures
            S.putWord8 0 -- key index
            S.put Ed25519 -- signature scheme ID
            S.putShortByteString sig4
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
