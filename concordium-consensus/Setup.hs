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
    env <- getEnvironment
    rawSystemExit verbosity "mkdir" ["-p", "../smart-contracts/lib"]

    -- This way of determining the platform is not ideal.
    notice verbosity "Calling 'cargo build'"
    rawSystemExitWithEnv verbosity "cargo"
        ["build", "--release", "--manifest-path", "../smart-contracts/wasm-chain-integration/Cargo.toml"]
        (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
    case buildOS of
       Windows -> do
            -- Copy just the dynamic library, since it doesn't link with the static one.
            notice verbosity "Copying wasmer_interp.dll"
            rawSystemExit verbosity "cp" ["-u", "../smart-contracts/wasm-chain-integration/target/release/wasm_chain_integration.dll", "../smart-contracts/lib/"]
       _ -> do
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.a", "../smart-contracts/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.dylib", "../smart-contracts/lib/libwasm_chain_integration.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libwasm_chain_integration.so", "../smart-contracts/lib/libwasm_chain_integration.so"]
    return emptyHookedBuildInfo

-- This is a quick and dirty hook to copy the wasm_chain_integration DLL on Windows.
copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags _ lbi = case hostPlatform lbi of
    Platform _ Windows -> do
        let verbosity = fromFlag $ copyVerbosity flags
        let dest = fromPathTemplate $ bindir $ installDirTemplates lbi
        notice verbosity $ "Copying wasm_chain_integration.dll to: " ++ dest
        rawSystemExit verbosity "cp" ["../smart-contracts/wasm-chain-integration/target/release/wasm_chain_integration.dll", dest]
    _ -> return ()


main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeRust
  , postCopy = copyExtLib
  }

