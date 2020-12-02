# The build process for the rust component

The [build.rs](./build.rs) script links with a number of Haskell libraries and
supports a number of configuration options that can be configured in two
different ways, either via environment variables or Cargo features.

## Features

The package supports the following features

- `static`: This feature enables linking with static Haskell libraries.
   They are expected to be available in `./deps/static-libs/linux/vanilla`, i.e. in that subdirectory of this directory.
   The `download-static-libraries.sh` script from the p2p-client repository will download them into the correct location.

- `profiling`: This will link with Haskell libraries built with profiling support. This option implies `static`, with the difference
  that the libraries must be available in `./deps/static-libs/linux/profiling`.

By default no features are enabled.

## Environment variables

Environment variables only apply to the default build. This links with shared Haskell libraries.

- `CONCORDIUM_HASKELL_ROOT` should, if present, be a directory containing
   - libHSConcordium-0.1.0.0
   - libHSconcordium-crypto-0.1
   - libHSglobalstate-types-0.1.0.0
   - libHSglobalstate-0.1.0.0
   - libHSlmdb-0.2.5
   - libHSscheduler-0.1.0.0

   This only applies to non-windows platforms. It is not used on other platforms.
   On Windows the Concordium haskell package is built with a `standalone` option which embeds all dependent libraries into one single DLL.
   This option does not work on linux, hence the more elaborate linking process.

   If this flag is not present the build script will try to automatically discover all the Haskell libraries by using `stack path`.

- `HASKELL_RTS_VARIANT` applies to all platforms and is the prefix of the GHC runtime library that should be linked.
   This defaults to `libHSrts_thr-` for the threaded version.

- `HASKELL_GHC_LIBDIR` applies to all platforms and should be the location of the GHC libraries directory.
   If not given the script will try to discover the value by running `stack ghc
   -- --print-libdir`.


Note that [build.rs](./build.rs) will not automatically build the Haskell
dependencies. This should be done before by running `stack build`.
