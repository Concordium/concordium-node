# P2P Layer implementation in Rust
## General usage information
This repository uses git lfs for storing binary dependencies, and relies on git submodules for internal component dependencies. Therefore the git lfs extension needs to be installed, and to update the code remember to pull submodules too recursively.

## Dependencies to build the project
* Rust (stable 1.37+, and stable 1.37.0 (eae3437df 2019-08-13) for using static libraries)
* binutils >= 2.22
* cmake >= 3.8.0
* flatc >= 1.11.0
* protobuf >= 3.7.1
* LLVM and Clang >= 3.9
* [Unbound](https://www.nlnetlabs.nl/projects/unbound/about/) >= 1.9.2

### Optional dependencies
* Stack (and GHC-8.6.5, if not building using static libraries)
* capnp (for running `s11n_capnp` enabled benches only)

## Supported features
* instrumentation - switches the default internal counter implementation out with prometheus
* s11n_serde_cbor - enables serialization using [serde_cbor](https://crates.io/crates/serde_cbor) (only used in benches)
* s11n_serde_msgpack - enables serialization using [rmp-serde](https://crates.io/crates/rmp-serde) (only used in benches)
* s11n_capnp - enables serialization using [Cap'n'Proto](https://crates.io/crates/capnp) (only used in benches)
* instrumentation - enables stats data exporting to [prometheus](https://crates.io/crates/prometheus)
* benchmark - enables the TPS testing
* network_dump - makes the network dumping capabilites available.
* static - build against static haskell libraries in GIT LFS (Linux only)
* profiling - build against haskell libraries in GIT LFS with profiling support enabled (Linux only)
* elastic_logging - enable ability to log transaction events to elastic search
* collector - enables the build of the node-collector and backend
* beta - enables special beta only features like client username/password validation
* no_rgs - use consensus with haskell global state implementation (this must match the proper static libraries if compiled against them)

## Setting up basic local build environment
Install the needed dependencies from the list above (Windows build is special, for that see cross-compilation build environment setup script in [scripts/init.win.build.env.sh](/scripts/init.win.build.env.sh) for further details), and run the script (requires that the user executing is has sudo privileges) `scripts/local-setup-unix-deps.sh` and pay special attention to setting the right version of GHC (see [build scripts](/scripts/local-setup-unix-deps.sh#L25) for details).

Alternatively use `--features=static` to build statically against the haskell dependencies (only available on Linux, and requries that you download them using [scripts/download-static-libs.sh](/scripts/download-static-libs.sh) before first compilation, and whenever the pointer to any internal dependencies are updated).

## Installing genesis data
Unpack the relevant set of genesis data and private baker data from [scripts/genesis-data/](/scripts/genesis-data) to the correct OS folder (e.g. on Linux this would be `$HOME/.local/share/ConcordiumP2P`). This determines how many bakers you need to run for the network to be able to work properly.

## Running the library as a binary (usable via gRPC)
```bash
$> cargo run -- --debug
```

## Running all tests
```bash
$> cargo test --all
```

## Nix
Currently this project only sports support for Nix on Linux platforms.
### Development
All `zsh` wrapper functions wraps around `nix-shell`, and if dropping into a `nix-shell` directly remember to use the cargo flag `--features=static` to build against the static libraries in LFS.
### Install binaries as a package
```bash
$> scripts/download-static-libs.sh
$> nix-env -f . -i
```

## Docker-Compose
### Building docker images
To build the stable image built in a Jenkins pipeline (it gets tagged `latest`, if not changed in the line shown below, so it matches the image hosted on docker-hub - and as the layers will have a newer version, it won't download from docker-hub unless the locally built image is removed via e.g. `docker image rmi ..`). It passes the local `ssh-agent` into the docker build environment for the needed stages to download internal crates with git directly.
```bash
$> git clone -b master --single-branch git@gitlab.com:Concordium/tools/baker_id_gen.git baker_id_gen # Only needed once, as it's a vital component to scaling the bakers inside docker-compose
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


For more complicated setups the EXTRA_ARGS environment variable can be set.

## Elastic search in local development mode
### Running the local development version from the stable master branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> ELASTIC_SEARCH_LOGGING=1 NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.middleware.yml up --scale baker=5 --force-recreate
```

### Running the local development version from the unstable develop branch
Use docker-compose if you only need a middle-ware enabled set of nodes to test on
```bash
$> ELASTIC_SEARCH_LOGGING=1 NUM_BAKERS=5 DESIRED_PEERS=4 docker-compose -f docker-compose.develop.middleware.yml up --scale baker=5 --force-recreate
```

### Delay baker startup if Elastic Search starts too slowly
If Elastic Search starts too slowly the baker enabled for logging to it can be delayed by using the variable `ES_SLEEP`

### Using persistent local Elastic Search setup with Kibana
To run a pair of elastic search with kibana for local development do the following
```bash
$> docker network create elasticsearch
$> docker run -d --name elasticsearch --net elasticsearch -p 9200:9200 -p 9300:9300 -e "discovery.type=single-node" elasticsearch:7.3.2
$> docker run -d --name kibana --net elasticsearch -p 5601:5601 kibana:7.3.2
```


To delete the docker setup run
```bash
$> docker stop kibana
$> docker rm kibana
$> docker stop elasticsearch
$> docker rm elasticsearch
$> docker network rm elasticsearch
```
