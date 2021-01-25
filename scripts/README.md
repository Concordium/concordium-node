# Scripts for the concordium-node

This is a collection of scripts that create different docker 
containers to run the project.

## [`build-binaries.sh`](./build-binaries.sh)

This script builds the project in debug or release mode and with `static` or 
`profiling` flags. It is used by the Dockerfiles in here.

## [`start.sh`](./start.sh)

This script runs each component in every docker image. It has different behaviors
depending on the environment variables that are defined and it is used as entrypoint
for the Docker images and for entrypoint for `supervisord` on the [opentestnet/staging_net clients](./distribution).

## Testnet deployments

The folder [`testnet-deployments`](./testnet-deployments) contains a set of scripts that create docker
images with the different components of the project that can be used to create
ad-hoc deployments and are used in testnet deployments.

## Distribution

The [`distribution`](./distribution) folder contains the scripts to build and push the opentestnet and staging_net clients.

## Static libraries

The [`static-libraries`](./static-libraries) folder contains scripts to build the haskell static version of
the consensus that can be used to compile the node with the flags `static/profiling` enabled, allowing for 
profiling and making the user not need to install the Haskell ecosystem to run the node.

### [`download-static-libraries.sh`](./download-static-libraries)

Using the [`static-libraries/LATEST_STATIC_LIBRARIES`](static-libraries/LATEST_STATIC_LIBRARIES) file
this script downloads a set of static libraries [built by Jenkins](../jenkinsfiles/static-libraries.Jenkinsfile).


## Downloader scripts

Some scripts are meant to be used for downloading different complementary data
that might be needed to run the node properly.

### [`download-genesis-data.sh`](./download-genesis-data.sh)

Using the [`GENESIS_DATA_VERSION`](./GENESIS_DATA_VERSION) file this script downloads tar archives with
configurations for running the network with a different set of bakers. These
archives come from building the [genesis-data](https://gitlab.com/Concordium/genesis-data) 
repository.

### [`download-genesis-complementary-bundle.sh`](./download-genesis-complementary-bundle.sh)

Using the [`GENESIS_DATA_VERSION`](./GENESIS_DATA_VERSION) file this script downloads some configuration 
files that are used together with the genesis data, namely anonymity revokers 
and identity providers private keys, additional accounts, global cryptographic
parameters.
