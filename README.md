# concordium-node

This repository contains the implementation of the concordium p2p node with its
dependencies. It is split into two parts

- [concordium-consensus](./concordium-consensus/)
  is a Haskell package that contains the implementation of the consensus with
  its dependencies. This includes basic consensus, finalization, scheduler,
  implementations of block and tree storage, and auxiliaries.
- [concordium-node](./concordium-node/)
  is a Rust package containing a number of executables, the chief among them
  being [concordium-node.rs](./concordium-node/src/bin/cli.rs) which is the
  program that participates in the Concordium network and runs consensus,
  finalization, and other components. It uses
  [concordium-consensus](./concordium-consensus/) as a package, either linked
  dynamically or statically, depending on the build configuration. The main
  feature added by the [concordium-node](./concordium-node/) is the network layer.

## Submodules

The [concordium-base](./concordium-base/) is a a direct dependency of both
[concordium-consensus/](./concordium-consensus/) and
[concordium-node](./concordium-node/). Because
[concordium-base](./concordium-base/) is also used by other components it is a
separate repository brought in as a submodule.

The [concordium-grpc-api](./concordium-grpc-api/) is a simple repository that
defines the external GRPC API of the node. This is in term of the `.proto` file.
Because this is used by other components it is also a small separate repository
brought in as a submodule.


## Configurations and scripts

- The [jenkinsfiles](./jenkinsfiles/) directory contains Jenkins configurations
  for deployment and testing.
- The [scripts](./scripts/) directory contains a variety of bash scripts,
  Dockerfiles, and similar, to build different configurations of the node for
  testing and deployment.
- The [docker-compose](./docker-compose/) directory contains a number of
  different [docker-compose](https://docs.docker.com/compose/) configurations
  that can be used for setting up small local network for testing.

## Contributing

## Building the node
