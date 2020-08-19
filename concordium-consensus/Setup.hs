import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils
import Distribution.System

import System.Directory
import System.Environment

import Data.Maybe

updateExtraLibDirs :: LocalBuildInfo -> IO LocalBuildInfo
updateExtraLibDirs localBuildInfo = do
    let packageDescription = localPkgDescr localBuildInfo
        lib = fromJust $ library packageDescription
        libBuild = libBuildInfo lib
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            library = Just $ lib {
                libBuildInfo = libBuild {
                    extraLibDirs = (dir ++ "/../smart-contracts/wasmer-interp/target/release") :
                        extraLibDirs libBuild
                }
            }
        }
    }

makeRust :: Args -> ConfigFlags -> IO HookedBuildInfo
makeRust args flags = do
    let verbosity = fromFlag $ configVerbosity flags
    env <- getEnvironment
    -- This way of determining the platform is not ideal.
    case buildOS of
        -- On Windows, we work around to build wasmer using the msvc toolchain
        -- because it doesn't currently work with gnu.
        Windows -> do
            rawSystemExitWithEnv verbosity "rustup"
                ["run", "stable-x86_64-pc-windows-msvc", "cargo", "build", "--release", "--manifest-path", "../smart-contracts/wasmer-interp/Cargo.toml"]
                (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
            {-
            -- We also copy the generated DLL
            installOrdinaryFile normal "../smart-contracts/wasmer-interp/target/release/wasmer_interp.dll" "."
            installOrdinaryFile normal "../smart-contracts/wasmer-interp/target/release/wasmer_interp.dll" ".."
            -}
        _ ->
            rawSystemExitWithEnv verbosity "cargo"
                ["build", "--release", "--manifest-path", "../smart-contracts/wasmer-interp/Cargo.toml"]
                (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
    return emptyHookedBuildInfo

-- This is a quick and dirty hook to copy the wasmer_interp DLL on Windows.
copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyExtLib _ flags _ lbi = case hostPlatform lbi of
    Platform _ Windows -> do
        let verbosity = fromFlag $ copyVerbosity flags
        let dest = fromPathTemplate $ bindir $ installDirTemplates lbi
        putStrLn $ "Copying DLL to: " ++ dest
        rawSystemExit verbosity "cp" ["../smart-contracts/wasmer-interp/target/release/wasmer_interp.dll", dest]
    _ -> return ()


main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeRust
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  , postCopy = copyExtLib
  }

