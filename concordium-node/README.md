# P2P Layer implementation in Rust
`master` [![master pipeline status](https://gitlab.com/Concordium/p2p-client/badges/master/pipeline.svg)](https://gitlab.com/Concordium/p2p-client/commits/master) `develop` [![develop pipeline status](https://gitlab.com/Concordium/p2p-client/badges/develop/pipeline.svg)](https://gitlab.com/Concordium/p2p-client/commits/develop)

## General usage information
This repository relies on git submodules for internal component dependencies, so do remember to clone recursively or use `git submodule update --init --recursive` after having cloned it.

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
* Stack (and GHC-8.8.4, if not building using static libraries)
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

See instructions in
[consensus](./deps/internal/consensus/consensus-rust/README.md) for how to link
the node.

## Setting up basic local build environment
Install the needed dependencies from the list above, and run the script (requires that the user executing is has sudo privileges) `scripts/local-setup-unix-deps.sh` and pay special attention to setting the right version of GHC (see [build scripts](/scripts/local-setup-unix-deps.sh#L28) for details).

Alternatively use `--features=static` to build statically against the haskell dependencies (only available on Linux, and requries that you download them using [scripts/download-static-libs.sh](/scripts/download-static-libs.sh) before first compilation, and whenever the pointer to any internal dependencies are updated).

## Installing genesis data
Unpack the relevant set of genesis data and private baker data from [genesis-data/](/genesis-data) to the correct OS folder (e.g. on Linux this would be `$HOME/.local/share/concordium`). This determines how many bakers you need to run for the network to be able to work properly.

## Running the library as a binary (usable via gRPC)
```bash
$> cargo run -- --debug
```

## Running all tests
```bash
$> cargo test --all
```

## Obtaining documentation
The output is placed in `target/doc` by default.
```bash
$> cargo doc
```

## Nix
Currently this project only sports support for Nix on Linux platforms.
### Development
All `zsh` wrapper functions wrap around `nix-shell`, and if dropping into a `nix-shell` directly remember to use the cargo flag `--features=static` to build against the static libraries (`nix-shell` will automatically pull these down from S3).
### Install binaries as a package
```bash
$> scripts/download-static-libs.sh
$> nix-env -f . -i
```

## Docker-Compose
### Building docker images
To build the stable image built in a Jenkins pipeline (it gets tagged `latest`, if not changed in the line shown below, so it matches the image hosted on docker-hub - and as the layers will have a newer version, it won't download from docker-hub unless the locally built image is removed via e.g. `docker image rmi ..`). It passes the local `ssh-agent` into the docker build environment for the needed stages to download internal crates with git directly. This image builds on `192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base` so make sure to have either built this locally (check [devops:base-images/build-base.sh](https://gitlab.com/Concordium/devops/blob/master/base-images/build-base-docker.sh) for the syntax and current version), or have access to AWS ECR to pull it.
```bash
$> git clone -b master --single-branch git@gitlab.com:Concordium/tools/baker_id_gen.git baker_id_gen # Only needed once, as it's a vital component to scaling the bakers inside docker-compose
$> scripts/download-genesis-data.sh
$> scripts/download-genesis-complementary-bundle.sh
$> echo $(cd deps/internal/consensus && git rev-parse HEAD) > CONSENSUS_VERSION
$> DOCKER_BUILDKIT=1 docker build -f scripts/dev-client.Dockerfile -t concordium/dev-client:latest --ssh default . --no-cache
```
### Latest stable from master branch
For a local docker compose setup, a docker-compose.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the master branch.

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose up --scale baker=5 --force-recreate
```


For more complicated setups the EXTRA_ARGS environment variable can be set.

### Latest unstable from develop branch
For a local docker compose setup, a docker-compose.develop.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built automatically upon push to the develop branch.

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.yml up --scale baker=5 --force-recreate
```

### Latest debug from custom branch
For a local docker compose setup, a docker-compose.debug.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built manually from a specific branch. These builds must be considered extremely volatile!

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.debug.yml up --scale baker=5 --force-recreate
```

### Latest debug from custom branch (with smart contract support)
For a local docker compose setup, a docker-compose.debug-sc.yml file has been provided in the root of this repository. It uses a image hosted in Docker hub built manually from a specific branch. These builds must be considered extremely volatile!

For the most simple and common setup, simply run the below command in the root of the checked out repository
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.debug-sc.yml up --scale baker=5 --force-recreate
```



For more complicated setups the EXTRA_ARGS environment variable can be set.

## Middleware local development mode
The PostGreSQL instance is exposed on port 5432/tcp and the username is `concordium`, password: `concordium`, and database name is `concordium`.

### Running the local development version from the stable master branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.middleware.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.middleware.yml down
```

### Running the local development version from the unstable develop branch (middleware)
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml down
```

### Delay baker startup if PostGreSQL starts too slowly
If PostGreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP`


## Wallet local development mode
The PostGreSQL instance is exposed on port 5432/tcp and the username is `concordium`, password: `concordium`, and database name is `concordium`.
The wallet-proxy is mapped on port 14000/tcp.

### Running the local development version from the stable master branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.wallet-dev.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.wallet-dev.yml down
```

### Running the local development version from the unstable develop branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.wallet-dev.yml up --scale baker=5 --force-recreate
```

Remember to clean out PostGreSQL data between runs using
```bash
$> NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.wallet-dev.yml down
```

### Delay baker startup if PostGreSQL starts too slowly
If PostGreSQL starts too slowly the baker enabled for logging to it can be delayed by using the variable `DB_SLEEP` (the wallet-proxy has a default value of 30 set to delay start until PostGreSQL is up).
