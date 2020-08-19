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
    -- This way of determining the platform is not ideal.
    case buildOS of
        -- On Windows, we work around to build wasmer using the msvc toolchain
        -- because it doesn't currently work with gnu.
        Windows -> do
            rawSystemExitWithEnv verbosity "rustup"
                ["run", "stable-x86_64-pc-windows-msvc", "cargo", "build", "--release", "--manifest-path", "../smart-contracts/wasmer-interp/Cargo.toml"]
                (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
            _ <- rawSystemExitCode verbosity "mkdir" ["../smart-contracts/lib"]
            -- Copy just the dynamic library, since it doesn't link with the static one.
            rawSystemExit verbosity "cp" ["../smart-contracts/wasmer-interp/target/release/wasmer_interp.dll", "../smart-contracts/lib/"]
        _ -> do
            rawSystemExitWithEnv verbosity "cargo"
                ["build", "--release", "--manifest-path", "../smart-contracts/wasmer-interp/Cargo.toml"]
                (("CARGO_NET_GIT_FETCH_WITH_CLI", "true") : env)
            _ <- rawSystemExitCode verbosity "mkdir" ["../smart-contracts/lib"]
            rawSystemExit verbosity "ln" ["-s", "-f", "../wasmer-interp/target/release/libwasmer_interp.a", "../smart-contracts/lib/"]
            case buildOS of
                OSX ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasmer-interp/target/release/libwasmer_interp.dylib", "../smart-contracts/lib/libwasmer_interp.dylib"]
                _ ->
                    rawSystemExit verbosity "ln" ["-s", "-f", "../wasmer-interp/target/release/libwasmer_interp.so", "../smart-contracts/lib/libwasmer_interp.so"]
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
  , postCopy = copyExtLib
  }

