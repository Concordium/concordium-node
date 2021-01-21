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

## Building the node

See [concordium-node/README.md](./concordium-node/README.md).

# Contributing

To contribute start a new branch starting from master, make changes, and make a
merge request. A person familiar with the codebase should be asked to review the
changes before they are merged.

## Haskell workflow

We typically use [stack](https://docs.haskellstack.org/en/stable/README/) to
build, run, and test the code. In order to build the haskell libraries the rust
dependencies must be pre-build, which is done automatically by the cabal setup
script.

We do not use any code formatting or linting tool on the CI. Running hlint might
uncover common issues, and with regards to formatting, the general rule is that
lines should not be too long, and follow the naming scheme and code style that
already exists.

## Rust workflow

We use **stable version** of rust, 1.45.2, to compile the code.

The CI is configured to check two things
- the [clippy](https://github.com/rust-lang/rust-clippy) tool is run to check
  for common mistakes and issues. We try to have no clippy warnings. Sometimes
  what clippy thinks is not reasonable is necessary, in which case you should
  explicitly disable the warning on that site (a function or module), such as
  `#[allow(clippy::too_many_arguments)]`, but that is a method of last resort.
  Try to resolve the issue in a different way first.

- the [rust fmt](https://github.com/rust-lang/rustfmt) tool is run to check the
  formatting. Unfortunately the stable version of the tool is quite outdated, so
  we use a nightly version, which is updated a few times a year. Thus in order
  for the CI to pass you will need to install the relevant nightly version (for
  which see the [.gitlab-ci.yml](.gitlab-ci.yml) file, the `"lint:fmt"`
  section).
