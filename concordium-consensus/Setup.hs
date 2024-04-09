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
            -- We remove the static library if it exists. Previously, it would have been copied
            -- over, but now we want to just link with the dynamic library, so we ensure it is
            -- removed.
            rawSystemExit verbosity "rm" ["-f", smartContractRoot ++ "/lib/libconcordium_smart_contract_engine.a"]
        _ -> do
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.a", smartContractRoot ++ "/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.dylib", smartContractRoot ++ "/lib/libconcordium_smart_contract_engine.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasm-chain-integration/target/release/libconcordium_smart_contract_engine.so", smartContractRoot ++ "/lib/libconcordium_smart_contract_engine.so"]
    return emptyHookedBuildInfo

-- | On Windows, copy the DLL files to the binary install directory. This is to ensure that they
-- are accessible when running the binaries, tests and benchmarks.
copyDlls :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyDlls _ flags pkgDescr lbi = case buildOS of
    Windows -> do
        let installDirs = absoluteComponentInstallDirs pkgDescr lbi (localUnitId lbi) copydest
        let copyLib lib = do
                rawSystemExit verbosity "cp" ["-u", smartContractRoot ++ "/lib/" ++ lib ++ ".dll", bindir installDirs]
                notice verbosity $ "Copy " ++ lib ++ " to " ++ bindir installDirs
        copyLib "concordium_smart_contract_engine"
    _ -> return ()
  where
    distPref = fromFlag (copyDistPref flags)
    verbosity = fromFlag (copyVerbosity flags)
    copydest = fromFlag (copyDest flags)

main =
    defaultMainWithHooks $
        simpleUserHooks
            { preConf = makeRust,
              postCopy = copyDlls
            }
