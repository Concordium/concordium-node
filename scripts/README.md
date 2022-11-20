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

## Docker

The folder [`docker`](docker) contains a set of scripts for creating docker
images for the different components of the project.
They can be used to create ad-hoc deployments.
Possible usages range from local bakers over special-purpose nodes running in Kubernetes to full test environments.

## Distribution

The [`distribution`](./distribution) folder contains the scripts to build and push the opentestnet and staging_net clients.

## Static libraries

The [`static-libraries`](./static-libraries) folder contains scripts to build the Haskell static version of
the consensus that can be used to compile the node with the flags `static/profiling` enabled.
