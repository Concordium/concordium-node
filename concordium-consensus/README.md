# concordium-consensus

Implementation of consensus, finalization, scheduler, and a storage backend
needed by the former.

# Build requirements

### [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) and [Rust](https://www.rust-lang.org/tools/install)

### lmdb
The global state depends on the lmdb library, which you may need to install using your system's package manager.
- On Windows, `stack` installs msys2.
  To install lmdb there, use the following command:
  ```
  stack exec -- pacman -S mingw-w64-x86_64-lmdb
  ```
- On Linux, install the `liblmdb-dev` package (or a similarly named in your
  package manager).
- On OS X, use `brew install lmdb`.

### Troubleshooting on Windows

### `error: linking with link.exe failed`
This might happen if Rust is installed with the MSVC [ABI](https://en.wikipedia.org/wiki/Application_binary_interface).
You can check this by running `rustup show`.
If Rust is using the MSVC toolchain you can switch to GNU instead by running
```
rustup default stable-x86_64-pc-windows-gnu
```

### `user specified .o/.so/.DLL could not be loaded (addDLL: pthread or dependencies not loaded. (Win32 error 5)) whilst trying to load:  (dynamic) pthread`
Copy `%APPDATA%\Local\Programs\stack\x86_64-windows\ghc-9.2.5\mingw\bin\libwinpthread-1.dll` to `%APPDATA%\Local\Programs\stack\x86_64-windows\ghc-9.2.5\mingw\bin\pthread.dll`.

# The library and dependencies

The core of this repository is the `concordium-consensus` Haskell library. The
sources are in [src](./src). It depends on

- [haskell-lmdb](./haskell-lmdb/) which is our fork of the bindings for the lmdb
  library, brought in as a submodule. The fork fixes a number of subtle issues
  and exposes more functions that are needed by globalstate.

- [smart-contracts](./smart-contracts/) contains a number of Rust packages that
  implement validation, processing, and execution of Wasm smart contracts.
  The main dependency is the package `concordium-smart-contract-engine` which is
  automatically built by the `Setup.hs` script for the `concordium-consensus`
  package.

The `concordium-consensus` library itself can conceptually be split into three
parts

- globalstate: modules prefixed with `Concordium.GlobalState` and located mostly
  in [src/Concordium/GlobalState](./src/Concordium/GlobalState), contains
  functionality related to the block and tree storage, used by the scheduler and
  consensus, respectively.
- scheduler: modules prefixed with `Concordium.Scheduler` and located mostly in
  [src/Concordium/Scheduler](./src/Concordium/Scheduler) contains functionality
  for executing blocks, filtering transactions, etc.
- consensus: finalization is mostly implemented in
  [src/Concordium/Afgjort/](./src/Concordium/Afgjort/) set of modules, the
  tree layer with block processing and execution is spread between
  [src/Concordium/Skov](./src/Concordium/Skov) and
  [src/Concordium/Birk/](./src/Concordium/Birk/). The rest are auxiliary modules.
- [src/Concordium/External.hs](./src/Concordium/External.hs) is the external
  interface of the library in the form of a number of FFI exports. This is how
  the library is integrated into the [concordium-node](../concordium-node/).

# Tests

## Automated tests

Tests are grouped roughly along the library components listed above. They all
reside in subdirectories of [tests](./tests/)

- [tests/scheduler/](./tests/scheduler/) contains scheduler tests, i.e., tests
  of block execution independent of the consensus
- [tests/globalstate/](./tests/globalstate/) contains tests that just test
  the storage implementation in isolation
- [tests/consensus/](./tests/consensus/) contains both tests of consensus and
  finalization in isolation, as well as integration tests that exercise
  globalstate, scheduler, and consensus at the same time.

## Auxiliary testdata

The directory [testdata/](./testdata/) contains a number of pre-generated
files to help with tests. The contents in here might have to be regenerated
periodically if the protocols, or dependencies, change.

## Tools for manual testing

In addition to the automated tests there are a number of [test-runners/](./test-runners/), which
are programs that start a small number of bakers inside a single process and run
consensus in specific configurations. They can be configured to run with or
without transactions, and with various configurations of state, finalization
and consensus parameters.

- [Concordium-exe](./test-runners/app/) is the basic runner, it starts a number
  of bakers, every baker connected to every other baker, and outputs blocks in a
  format that can be used to display the block tree using
  [graphviz](./https://graphviz.org/).
- [catchup-runner](./test-runners/catchup/) is designed for testing the catchup
  mechanism. This is achieved by making it drop normal messages (how many and
  how often depends on the testing configuration), and only reliably relay
  catchup messages.
- [deterministic-runner](./test-runners/deterministic/) is designed for
  performance and resource usage (e.g., size of database storage) testing. It
  runs consensus deterministically, without relying on system randomness or
  timing information.
- [execute-chain](./test-runners/execute-chain/) is designed to execute a chain
  based on the block catch-up list. This is intended for testing for regressions
  against an existing chain, testing different global state implementation against
  each other on real data, and testing state serialization on real data.

# Tools

In addition to the library and tests this package contains a number of supporting tools.

## [database-exporter](./tools/database-exporter/)

This is a tool that can export blocks from the node's database into a file that can later be used to
import them quickly if a new node is, or to analyze the blocks. See its
[README](./tools/database-exporter/README.md) for additional information.
