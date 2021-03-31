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

## Genesis

Contains scripts and auxiliary files for creating genesis setups for testing.

The [./genesis/generate-test-genesis.py](./genesis/generate-test-genesis.py)
should be run from the [./genesis](./genesis/) directory. It supports two modes,
either running with host binaries directly (see the script for how to specify
paths) or using a docker image with all the tools. The docker image can be
specified via an environment variable (see the script for details). The mode can
be controlled by setting or unsetting the `USE_DOCKER` environment variable.

Of note is the environment variable `PURGE` which, if set, will make the script
delete the `genesis_data` directory before generating fresh genesis data.

An example invocation using the docker image is

```console
USE_DOCKER= ./generate-test-genesis.py
```

The script requires python3, and if USE_DOCKER is enabled, the `docker` package, installed. The latter can
be achieved by, for example,

```console
pip3 install docker
```
