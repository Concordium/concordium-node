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
        ["build", "--release", "--manifest-path", smartContractRoot ++ "/wasm-chain-integration/Cargo.toml"]
    case buildOS of
        Windows -> do
            notice verbosity "Copying wasm_chain_integration library"
            rawSystemExit verbosity "cp" ["-u", smartContractRoot ++ "/wasm-chain-integration/target/release/wasm_chain_integration.dll", smartContractRoot ++ "/lib/"]
            rawSystemExit verbosity "cp" ["-u", smartContractRoot ++ "/wasm-chain-integration/target/release/libwasm_chain_integration.a", smartContractRoot ++ "/lib/"]
        _ -> do
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.a", smartContractRoot ++ "/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.dylib", smartContractRoot ++ "/lib/libwasm_chain_integration.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.so", smartContractRoot ++ "/lib/libwasm_chain_integration.so"]
    return emptyHookedBuildInfo

main =
    defaultMainWithHooks $
        simpleUserHooks
            { preConf = makeRust
            }
