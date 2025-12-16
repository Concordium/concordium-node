import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Verbosity

import System.Directory
import System.Environment

import Data.Maybe

-- | Notify and execute a command, if fails exit with the same exit code.
runCmd :: Verbosity -> String -> IO ()
runCmd verbosity cmd = do
  notice verbosity $ "Running '" ++ cmd ++ "'"
  let command:args = words cmd
  rawSystemExit verbosity command $ args

-- | Path to Concordium Smart Contract Engine rust crate relative to this file.
smartContractEngineCrateRelative = "../concordium-base/smart-contracts/wasm-chain-integration"
-- | Path to plt-scheduler rust crate relative to this file.
pltSchedulerCrateRelative = "../plt-scheduler"

preConfHook :: Args -> ConfigFlags -> IO HookedBuildInfo
preConfHook args flags = do
    let verbosity = fromFlag $ configVerbosity flags

    -- Convert relative paths into absolute paths.
    libraryDestination <- canonicalizePath "./lib"
    -- Ensure destination directory exists.
    runCmd verbosity $ "mkdir -p " ++ libraryDestination

    -- Build and copy/symlink Concordium Smart contract Engine library.
    smartContractEngineCrate <- canonicalizePath smartContractEngineCrateRelative
    runCmd verbosity $ "cargo build --release --locked --features=enable-ffi --manifest-path=" ++ smartContractEngineCrate ++"/Cargo.toml"
    case buildOS of
      Windows -> do
        runCmd verbosity $ "cp -u " ++ smartContractEngineCrate ++ "/target/release/concordium_smart_contract_engine.dll " ++ libraryDestination
      OSX -> do
        runCmd verbosity $ "ln -s -f " ++ smartContractEngineCrate ++ "/target/release/libconcordium_smart_contract_engine.a " ++ libraryDestination
        runCmd verbosity $ "ln -s -f " ++ smartContractEngineCrate ++ "/target/release/libconcordium_smart_contract_engine.dylib " ++ libraryDestination
      _ -> do
        runCmd verbosity $ "ln -s -f " ++ smartContractEngineCrate ++ "/target/release/libconcordium_smart_contract_engine.a " ++ libraryDestination
        runCmd verbosity $ "ln -s -f " ++ smartContractEngineCrate ++ "/target/release/libconcordium_smart_contract_engine.so " ++ libraryDestination

    -- Build and copy/symlink PLT scheduler project
    pltSchedulerCrate <- canonicalizePath pltSchedulerCrateRelative
    runCmd verbosity $ "cargo build --release --locked --manifest-path=" ++ pltSchedulerCrate ++"/Cargo.toml"
    case buildOS of
      Windows -> do
        runCmd verbosity $ "cp -u " ++ pltSchedulerCrate ++ "/target/release/plt_scheduler.dll " ++ libraryDestination
      OSX -> do
        runCmd verbosity $ "ln -s -f " ++ pltSchedulerCrate ++ "/target/release/libplt_scheduler.a " ++ libraryDestination
        runCmd verbosity $ "ln -s -f " ++ pltSchedulerCrate ++ "/target/release/libplt_scheduler.dylib " ++ libraryDestination
      _ -> do
        runCmd verbosity $ "ln -s -f " ++ pltSchedulerCrate ++ "/target/release/libplt_scheduler.a " ++ libraryDestination
        runCmd verbosity $ "ln -s -f " ++ pltSchedulerCrate ++ "/target/release/libplt_scheduler.so " ++ libraryDestination
    return emptyHookedBuildInfo

-- | On Windows, copy the DLL files to the binary install directory. This is to ensure that they
-- are accessible when running the binaries, tests and benchmarks.
postCopyHook :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyHook _ flags pkgDescr lbi = case buildOS of
    Windows -> do
        let installDirs = absoluteComponentInstallDirs pkgDescr lbi (localUnitId lbi) copydest
        -- Copy DLL for Concordium Smart Contract Engine
        smartContractEngineCrate <- canonicalizePath smartContractEngineCrateRelative
        runCmd verbosity $ "cp -u " ++ smartContractEngineCrate ++ "/target/release/concordium_smart_contract_engine.dll " ++ bindir installDirs
        -- Copy DLL for PLT scheduler
        pltSchedulerCrate <- canonicalizePath pltSchedulerCrateRelative
        runCmd verbosity $ "cp -u " ++ pltSchedulerCrate ++ "/target/release/plt_scheduler.dll " ++ bindir installDirs
    _ -> return ()
  where
    distPref = fromFlag (copyDistPref flags)
    verbosity = fromFlag (copyVerbosity flags)
    copydest = fromFlag (copyDest flags)

main =
    defaultMainWithHooks $
        simpleUserHooks
            { preConf = preConfHook,
              postCopy = postCopyHook
            }
