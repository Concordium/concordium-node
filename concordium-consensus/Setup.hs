import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System

import System.Directory
import System.Environment

import Data.Maybe

smartContractRoot = "../concordium-base/smart-contracts"

makeRust :: Args -> ConfigFlags -> IO HookedBuildInfo
makeRust args flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "mkdir" ["-p", smartContractRoot ++ "/lib"]

    -- This way of determining the platform is not ideal.
    notice verbosity "Calling 'cargo build'"
    rawSystemExit
        verbosity
        "cargo"
        ["build", "--release", "--manifest-path", smartContractRoot ++ "/wasm-chain-integration/Cargo.toml", "--features=enable-ffi"]
    case buildOS of
        Windows -> do
            notice verbosity "Copying concordium_smart_contract_engine library"
            rawSystemExit verbosity "cp" ["-u", smartContractRoot ++ "/wasm-chain-integration/target/release/concordium_smart_contract_engine.dll", smartContractRoot ++ "/lib/"]
            rawSystemExit verbosity "cp" ["-u", smartContractRoot ++ "/wasm-chain-integration/target/release/libconcordium_smart_contract_engine.a", smartContractRoot ++ "/lib/"]
        _ -> do
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.a", smartContractRoot ++ "/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.dylib", smartContractRoot ++ "/lib/libconcordium_smart_contract_engine.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.so", smartContractRoot ++ "/lib/libconcordium_smart_contract_engine.so"]
    return emptyHookedBuildInfo

main =
    defaultMainWithHooks $
        simpleUserHooks
            { preConf = makeRust
            }
