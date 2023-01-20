# Concordium node implementation

## Dependencies to build the project

- Rust (stable 1.62 for using static libraries)
- binutils >= 2.22
  - For macOS one should use the binutils provided by Xcode.
- cmake >= 3.8.0
- [flatc](http://google.github.io/flatbuffers/flatbuffers_guide_building.html)
  v22.12.06 is what we currently use. Either build from the v22.12.06 tag of the repository using CMake and copy the `flatc` binary somewhere in your PATH, or download a released binary from <https://github.com/google/flatbuffers/releases/tag/v22.12.06> and place it somewhere in your PATH.
- protobuf >= 3.15
- LLVM and Clang >= 3.9
- As noted in the [caveats](#caveats) section below, you need to build the [Concordium Consensus](../concordium-consensus/) project before building this project. 
  So you also need the [dependencies listed in the README for Concordium Consensus](https://github.com/Concordium/concordium-node/tree/main/concordium-consensus#build-requirements).

### Optional dependencies

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/) if **not** building using static libraries

## Supported features

- network_dump - makes the network dumping capabilites available.
- static - build against static haskell libraries (Linux only)
- profiling - build against haskell libraries with profiling support enabled (Linux only)
- collector - enables the build of the node-collector and backend
- dedup_benchmarks - enable support in the benchmarks for deduplication queues

## Building the node

The [build.rs](./build.rs) script links with a number of Haskell libraries and
supports a number of configuration options that can be configured in two
different ways, either via environment variables or Cargo features.

### Features

The package supports the following features related to linking with the Haskell component.

- `static`: This feature enables linking with static Haskell libraries on linux
   They are expected to be available in `./deps/static-libs/linux/vanilla`.

   These static libraries can be built using the docker image built from
   [../scripts/static-libraries/static-libraries.Dockerfile] by doing the following from the **root of the repository**.

  ```console
  docker build -t concordium/static-libraries -f scripts/static-libraries/static-libraries.Dockerfile .
  mkdir out
  docker run -v $(pwd)/out:/out concordium/static-libraries
  mkdir -p concordium-node/deps/static-libs/linux
  tar -xf out/static-consensus-9.2.5.tar.gz --strip-components=1 -C concordium-node/deps/static-libs/linux
  ```

  (this is assuming a GNU version of tar)

- `profiling`: This will link with Haskell libraries built with profiling support. This option implies `static`, with the difference
  that the libraries must be available in `./deps/static-libs/linux/profiling`.
  The same process and Dockerfile can be used to build them. In fact both versions of static libraries are always built.

By default none of these features are enabled.

Building a node with any of these features, e.g., `cargo build --release
--features=static` produces a mostly statically linked binary `concordium-node`,
apart from system libraries.

### Environment variables

Environment variables only apply to the default build. This links with shared Haskell libraries.

- `CONCORDIUM_HASKELL_ROOT` should, if present, be a directory containing
  - libHSconcordium-consensus-0.1.0.0.so
  - libHSconcordium-base-0.1.0.0.so
  - libHSlmdb-0.2.5.so

   This only applies to non-windows platforms. It is not used on other platforms.
   On Windows the Concordium haskell package is built with a `standalone` option which embeds all dependent libraries into one single DLL.
   This option does not work on linux, hence the more elaborate linking process.

   If this flag is not present the build script will try to automatically discover all the Haskell libraries by using `stack path`.

- `HASKELL_RTS_VARIANT` applies to all platforms and is the prefix of the GHC runtime library that should be linked.
   This defaults to `libHSrts_thr-` for the threaded version.

- `HASKELL_GHC_LIBDIR` applies to all platforms and should be the location of
   the GHC libraries directory (by GHC libraries we mean the base runtime system
   libraries, base haskell libraries, and dependenencies).
   If not given the script will try to discover the value by running `stack ghc -- --print-libdir`.

### CAVEATS

- Note that [build.rs](./build.rs) **will not** automatically build the Haskell
  dependencies, it will only try to discover them. Building should be done before
  by running `stack build` inside
  [../concordium-consensus/](../concordium-consensus/) or running

  ```console
  stack --stack-yaml ../concordium-consensus/stack.yaml build
  ```

  Once the node is built it can be run as

  ```console
  cargo run --
  ```

  or

  ```console
  cargo run --release --
  ```

  to be run in release mode for improved performance. Note that the
  [concordium-consensus](../concordium-consensus/) dependency is the same regardless of how the
  node itself is built, the `--release` only applies to the optimization of the Rust node xcomponents.

- The node built with Haskell library auto-discovery is not suitable for distribution to other
  machines. It is a dynamically linked binary with a large number of shared library dependencies.

### MacOS Specific

You may need to add the following entries to your `~/.stack/config.yaml` for the libraries installed via `brew`:

```yaml
extra-lib-dirs:
- /opt/homebrew/lib

extra-include-dirs:
- /opt/homebrew/include/
```

### M1 MacOS Specific

You may need to add the following entry to your `~/.stack/config.yaml` for the `libffi` include:

```yaml
extra-include-dirs:
- /Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include/ffi/
```

To determine the exact path to the `libffi` include directory, run the following command:

```sh
pkg-config --cflags libffi
```

## Building on Windows

### Dependencies

Before building the node, you should install the following dependencies:

- Haskell [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
- [Rust](https://www.rust-lang.org/tools/install)
  - For building the node, the toolchain `1.62.1-x86_64-pc-windows-gnu` is required, which can be installed with the command: `rustup toolchain install 1.62.1-x86_64-pc-windows-gnu`.
  - For building the node runner service (optional), the toolchain `1.62.1-x86_64-pc-windows-msvc`  is required, which can be installed with the command: `rustup toolchain install 1.62.1-x86_64-pc-windows-msvc`.
- [flatc](https://github.com/google/flatbuffers/releases/tag/v22.12.06) 22.12.06 (should be in the path)
- [protoc](https://github.com/protocolbuffers/protobuf/releases) >= 3.15
- LMDB should be installed under `stack`'s `msys2` installation, which can be done with the following commands:

```console
stack exec -- pacman -Syuq --noconfirm
stack exec -- pacman -Syq mingw-w64-x86_64-lmdb --noconfirm
```

- If building the installer, the [Wix Toolset](https://wixtoolset.org/releases/) is required, and should be in the path.

### Building and Running

The simplest way to build the complete node, as well as the service runner and installer is with the [`build-all.ps1`](../scripts/distribution/windows/build-all.ps1`) powershell script.

The node binary will be built at `.\target\release\concordium-node.exe`.
However, running the node requires the consensus DLL, which is compiled to `..\concordium-consensus\HSdll.dll`.
If you wish to run the node directly, you should copy `HSdll.dll` to the location of `concordium-node.exe`.
The node also depends on a number of DLLs that are part of `stack`'s `msys2` installation.
To run the node with these DLLs in the path, you can use `stack exec`, as in:

```console
stack exec -- concordium-node.exe --help
```

### Build issue: long paths

If the root directory of the repository is too long, then the build may fail with `No such file or directory` errors, such as:

```console
concordium-consensus   >   = note: x86_64-w64-mingw32-gcc.exe: error: C:\msys64\jenkins-agent\workspace\concordium-node-windows\concordium-consensus\smart-contracts\wasm-chain-integration\target\release\build\winapi-x86_64-pc-windows-gnu-0838443214203ebf\build_script_build-0838443214203ebf.build_script_build.8py00u3m-cgu.0.rcgu.o: No such file or directory
```

This is caused by the path exceeding the Windows maximum path length.
The simplest solution for this is to clone the repository into a shorter root path.

## Running a bootstrapper node

The bootstrapper node uses a configuration similar to the one of a
normal node. Using `--help` will show all the available flags.

There is one mandatory parameter that must be set:
`--regenesis-block-hashes`. This parameter accepts a list of block hashes that
will be shared when performing a handshake with a node to only accept nodes
whose list of regenesis block hashes is a prefix of the one from the
bootstrapper, or viceversa, indicating that both nodes belong to the same network.

As the bootstrapper node doesn't have a running consensus inside, it will not
update the regenesis block hashes list, so it should be updated whenever the
bootstrapper node is restarted (and there had been a protocol update).

The format would be the following:

```console
p2p_boostrapper-cli ... --regenesis-block-hashes 0e8a30009f9cf7c7ab76929cf6bad057a20b7002fee6fe0be48682d32b331b91 c8ebf79db99dec96e5f32a09dbcdfd31744a88526e70bb3305837dcb8147241a ...
```

## Running all tests

```console
$> cargo test --all
```

## Obtaining documentation

The output is placed in [./target/doc](./target/doc) by default.

```console
$> cargo doc
```

To automatically open the browser with the documentation once it is generated use the `--open` flag.

```console
$> cargo doc --open
```

## Collector

To allow the network dashboard to display nodes in the network and their current status, a node must run a collector, which is a process that uses the GRPC api of the node to collect information and sends it to a centralized collector backend.

See [./collector-backend/](./collector-backend/) for details of the collector backend and how to run it.

Assuming you have a node running locally with GRPC available at `127.0.0.1:10000` and a collector backend at `127.0.0.1:8080`, a collector can be run using:

```console
$> cargo run --bin node-collector --features=collector -- --collector-url http://127.0.0.1:8080/post/nodes --grpc-host http://127.0.0.1:10000  --node-name "my-node-name"
```
