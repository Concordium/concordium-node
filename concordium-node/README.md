# Concordium node implementation

`master` [![master pipeline status](https://gitlab.com/Concordium/concordium-node/badges/master/pipeline.svg)](https://gitlab.com/Concordium/concordium-node/commits/master).

## General usage information
This repository relies on git submodules for some internal and external component dependencies.
Do remember to clone recursively or use `git submodule update --init --recursive` after having cloned it.

## Dependencies to build the project
* Rust (stable 1.45.2 for using static libraries)
* binutils >= 2.22
* cmake >= 3.8.0
* [flatc](http://google.github.io/flatbuffers/flatbuffers_guide_building.html)
  commit fec58aa129818ed0c0613a7ec36b55135bf81278, but others around it are
  likely to work as well (build using CMake and copy to `~/.local/bin`)
* protobuf >= 3.7.1
* LLVM and Clang >= 3.9
* [Unbound](https://www.nlnetlabs.nl/documentation/unbound/howto-setup/) >= 1.9.2 (the dependency `openssl-devel` is named `libssl-dev` on Ubuntu 19.10)
* PostGreSQL >= 10

### Optional dependencies
* [Haskell stack](https://docs.haskellstack.org/en/stable/README/) if **not** building using static libraries
* capnp (for running `s11n_capnp` enabled benches only)

## Supported features
* instrumentation - switches the default internal counter implementation out with prometheus
* s11n_serde_cbor - enables serialization using [serde_cbor](https://crates.io/crates/serde_cbor) (only used in benches)
* s11n_serde_msgpack - enables serialization using [rmp-serde](https://crates.io/crates/rmp-serde) (only used in benches)
* s11n_capnp - enables serialization using [Cap'n'Proto](https://crates.io/crates/capnp) (only used in benches)
* instrumentation - enables stats data exporting to [prometheus](https://crates.io/crates/prometheus)
* network_dump - makes the network dumping capabilites available.
* static - build against static haskell libraries in GIT LFS (Linux only)
* profiling - build against haskell libraries in GIT LFS with profiling support enabled (Linux only)
* collector - enables the build of the node-collector and backend
* staging_net - enables special staging network only features like client username/password validation
* database_emitter - enables building the database emitter binary to inject a database exported to a set of nodes
* genesis_tester - a tool used by a CI to validate the genesis data
* dedup_benchmarks - enable support in the benchmarks for deduplication queues
* malicious_testing - enables cli options for malicious testing (breakage and network partitioning)

## Building the node

The [build.rs](./build.rs) script links with a number of Haskell libraries and
supports a number of configuration options that can be configured in two
different ways, either via environment variables or Cargo features.

### Features

The package supports the following features related to linking with the Haskell component.

- `static`: This feature enables linking with static Haskell libraries.
   They are expected to be available in `./deps/static-libs/linux/vanilla`.
   The [../scripts/download-static-libs.sh](../scripts/download-static-libs.sh)`
   script will download them into the correct location.
   **The script is intended to be run from the root of the repository.**

- `profiling`: This will link with Haskell libraries built with profiling support. This option implies `static`, with the difference
  that the libraries must be available in `./deps/static-libs/linux/profiling`.

By default none of these features are enabled.

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
  machines. It is a dynamically linked binary with a large number of shared library dependencies
  that have

## Installing genesis data
Unpack the relevant set of genesis data and private baker data from [genesis-data/](/genesis-data) to the correct OS folder (e.g. on Linux this would be `$HOME/.local/share/concordium`). This determines how many bakers you need to run for the network to be able to work properly.

# Running all tests
```console
$> cargo test --all
```

# Obtaining documentation
The output is placed in [./target/doc](./target/doc) by default.
```console
$> cargo doc
```

To automatically open the browser with the documentation once it is generated use the `--open` flag.
```console
$> cargo doc --open
```

# Collector-backend and collectors

To allow the network dashboard to display nodes in the network and their current status, a node must run a collector, which is a process that uses the GRPC api of the node to collect information and sends it to a centralized collector backend.

## Collectors
Assuming you have a node running locally with GRPC available at `127.0.0.1:10000` and a collector backend at `127.0.0.1:8080`, a collector can be run using:

```console
$> cargo run --bin node-collector --features=collector -- --collector-url http://127.0.0.1:8080/post/nodes --grpc-host http://127.0.0.1:10000  --node-name "my-node-name"
```

## Collector-backend

To run a collector-backend:
```console
$> cargo run --bin node-collector-backend --features=collector
```

### Adjusting validation
The collector backend tries to do some validation of the data received from the collectors. These checks can be set using either command line arguments or environment variables.
For a description of all the arguments run:

```console
$> cargo run --bin node-collector-backend --features=collector -- --help
```

But some of the settings require a bit more explaination:

#### Limiting total data size
Setting `--valid-content-length` is a limit on the total byte size of the data received from a node, meaning it should also accommodate for everything in the data set. If we change something like the allowed node name length or the valid node peers count, this should probably be adjusted as well.

#### Comparing block heights against the average
Data where the best block height or finalized block height is too far from the current average will be rejected.

Adjusting what is considered 'too far' can be done for each check using `--valid-additional-best-block-height` and`--valid-additional-finalized-block-height`.

Comparing with averages only makes sense when the collector-backend have enough data points, the minimum number of required data points can be adjusted using `--validate-against-average-at`.

For the average to better withstand outliers, it is calculated from a percentage of the nodes, where the leftout nodes are the highest and lowest data values.
The percentage can be adjusted using `--percentage-used-for-averages` and must be an integer between 1 and 100.

Example: Say, we set the percentage to 60, with 20 nodes running, then new data would be compared to the average of 12 nodes, leaving out the nodes with the 3 lowest values and the 3 highest values.




