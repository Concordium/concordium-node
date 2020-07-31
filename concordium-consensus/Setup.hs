import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils

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
                    extraLibDirs = (dir ++ "../smart-contracts/wasmer-interp/target/release") :
                        extraLibDirs libBuild
                }
            }
        }
    }

-- copyExtLib :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
-- copyExtLib args flags pkg_descr lbi = do
--     let libPref = dynlibdir (configInstallDirs . configFlags $ lbi)
--     let verbosity = fromFlag $ copyVerbosity flags
    
--     -- rawSystemExit verbosity "cp" ["rust-src/target/release/libec_vrf_ed25519.so",
-- --                                  "rust-src/target/release/libeddsa_ed25519.so",
-- --                                  "rust-src/target/release/libsha_2.so",
-- --                                  libPref]

makeRust :: Args -> ConfigFlags -> IO HookedBuildInfo
makeRust args flags = do
    let verbosity = fromFlag $ configVerbosity flags
    env <- getEnvironment
    rawSystemExitWithEnv verbosity "cargo"
        ["build", "--release", "--manifest-path", "../smart-contracts/wasmer-interp/Cargo.toml"]
        (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
    return emptyHookedBuildInfo

main = defaultMainWithHooks simpleUserHooks
  {
    preConf = makeRust
  , confHook = \a f -> confHook simpleUserHooks a f >>= updateExtraLibDirs
  }

