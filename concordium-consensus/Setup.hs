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
    let command : args = words cmd
    rawSystemExit verbosity command $ args


-- | Path to the Rust node library workspace relative to this file.
nodeRustLibraryWorkspaceRelative = "../plt"

postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postConfHook args flags _ _ = do
    let verbosity = fromFlag $ configVerbosity flags

    -- Convert relative paths into absolute paths.
    libraryDestination <- canonicalizePath "./lib"
    -- Ensure destination directory exists.
    runCmd verbosity $ "mkdir -p " ++ libraryDestination

    -- Build and copy/symlink PLT scheduler project
    nodeRustLibraryWorkspace <- canonicalizePath nodeRustLibraryWorkspaceRelative
    withCurrentDirectory nodeRustLibraryWorkspace $ runCmd verbosity $ "cargo build --release --locked -p node-rust-library"
    case buildOS of
        Windows -> do
            runCmd verbosity $ "cp -u " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.a.dll " ++ libraryDestination
        OSX -> do
            runCmd verbosity $ "ln -s -f " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.a.a " ++ libraryDestination
            runCmd verbosity $ "ln -s -f " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.a.dylib " ++ libraryDestination
        _ -> do
            runCmd verbosity $ "ln -s -f " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.a.a " ++ libraryDestination
            runCmd verbosity $ "ln -s -f " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.a.so " ++ libraryDestination
    return ()

-- | On Windows, copy the DLL files to the binary install directory. This is to ensure that they
-- are accessible when running the binaries, tests and benchmarks.
postCopyHook :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postCopyHook _ flags pkgDescr lbi = case buildOS of
    Windows -> do
        let installDirs = absoluteComponentInstallDirs pkgDescr lbi (localUnitId lbi) copydest        
        -- Copy DLL for PLT scheduler
        nodeRustLibraryWorkspace <- canonicalizePath nodeRustLibraryWorkspaceRelative
        runCmd verbosity $ "cp -u " ++ nodeRustLibraryWorkspace ++ "/target/release/libnode_rust_library.dll " ++ bindir installDirs
    _ -> return ()
  where
    distPref = fromFlag (copyDistPref flags)
    verbosity = fromFlag (copyVerbosity flags)
    copydest = fromFlag (copyDest flags)

main =
    defaultMainWithHooks $
        simpleUserHooks
            { postConf = postConfHook,
              postCopy = postCopyHook
            }
