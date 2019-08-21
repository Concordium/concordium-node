# P2P Layer implementation in Rust
## General usage information
This repository uses git lfs for storing binary dependencies, and relies on git submodules for internal component dependencies. Therefore the git lfs extension needs to be installed, and to update the code remember to pull submodules too recursively.

## Dependencies to build the project
* Rust (stable 1.35+, and stable 1.37.0 (eae3437df 2019-08-13) for using static libraries)
* openssl >= 1.0.1
* cmake
* go >=1.10
* protobuf >= 3.5.1
* [Unbound](https://www.nlnetlabs.nl/projects/unbound/about/)
* libclang >= 6.0
* Stack (GHC-8.6.5, if not building using static libraries)
* capnp (for running `s11n_capnp` enabled benches only)

## Supported features
* instrumentation - switches the default internal counter implementation out with prometheus
* s11n_nom - enables serialization using [nom](https://crates.io/crates/nom) (only used in benches)
* s11n_serde_cbor - enables serialization using [serde_cbor](https://crates.io/crates/serde_cbor) (only used in benches)
* s11n_capnp - enables serialization using [capnp](https://crates.io/crates/capnp) (only used in benches)
* instrumentation - enables stats data exporting to [prometheus](https://crates.io/crates/prometheus)
* benchmark - enables the TPS testing
* network_dump - makes the network dumping capabilites available.
* static - build against static haskell libraries in GIT LFS (Linux only)
* profiling - build against haskell libraries in GIT LFS with profiling support enabled (Linux only)

## Setting up basic local build environment
Install the needed dependencies from the list above (Windows build is special, for that see cross-compilation build environment setup script in [scripts/init.win.build.env.sh](/scripts/init.win.build.env.sh) for further details), and run the script (requires that the user executing is has sudo privileges) `scripts/local-setup-unix-deps.sh` and pay special attention to setting the right version of GHC (see [build scripts](/scripts/local-setup-unix-deps.sh#L25) for details).

Alternatively use `--features=static` to build statically against the haskell dependencies (only available on Linux, and requries that you download them using [scripts/download-static-libs.sh](/scripts/download-static-libs.sh) before first compilation, and whenever the pointer to any internal dependencies are updated).

## Installing genesis data
Unpack the relevant set of genesis data and private baker data from [scripts/genesis-data/](/scripts/genesis-data) to the correct OS folder (e.g. on Linux this would be `$HOME/.local/share/ConcordiumP2P`). This determines how many bakers you need to run for the network to be able to work properly.

## Running the library as a binary (usable via gRPC)
```bash
$> cargo run --bin p2p_client-cli -- --debug
```

## Running all tests
```
$> cargo test --all
```

## Docker-Compose
For a local docker compose setup, a docker-compose.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the develop branch.

For the most simple and common setup, simply run
```
NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose up --scale baker=5
```
in the repository root

For more complicated setups the EXTRA_ARGS environment variable can be set.

## Nix
Currently this project only sports support for Nix on Linux platforms.
### Development
All `zsh` wrapper functions wraps around `nix-shell`, and if dropping into a `nix-shell` directly remember to use the cargo flag `--features=static` to build against the static libraries in LFS.
### Install binaries as a package
```
$> scripts/download-static-libs.sh
$> nix-env -f . -i
```
