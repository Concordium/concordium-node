import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System

import System.Directory
import System.Environment

import Data.Maybe


makeRust :: Args -> ConfigFlags -> IO HookedBuildInfo
makeRust args flags = do
    let verbosity = fromFlag $ configVerbosity flags
    rawSystemExit verbosity "mkdir" ["-p", "./smart-contracts/lib"]

    -- This way of determining the platform is not ideal.
    notice verbosity "Calling 'cargo build'"
    rawSystemExit verbosity "cargo"
        ["build", "--release", "--manifest-path", "./smart-contracts/wasm-chain-integration/Cargo.toml"]
    case buildOS of
       Windows -> do
            notice verbosity "Copying wasm_chain_integration library"
            rawSystemExit verbosity "cp" ["-u", "./smart-contracts/wasm-chain-integration/target/release/wasm_chain_integration.dll", "./smart-contracts/lib/"]
            rawSystemExit verbosity "cp" ["-u", "./smart-contracts/wasm-chain-integration/target/release/libwasm_chain_integration.a", "./smart-contracts/lib/"]
       _ -> do
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.a", "./smart-contracts/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.dylib", "./smart-contracts/lib/libwasm_chain_integration.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.so", "./smart-contracts/lib/libwasm_chain_integration.so"]
    return emptyHookedBuildInfo

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeRust
  }

