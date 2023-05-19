# concordium-node

- [![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

- ![Build and test](https://github.com/Concordium/concordium-node/actions/workflows/build-test.yaml/badge.svg)

This repository contains the implementation of the concordium p2p node with its
dependencies and auxiliaries. The node is split into two parts

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

The auxiliary packages are the
- [collector](./collector) The collector is a service that queries the node for
  some information and publishes data to the `collector-backend`. The collector
  runs alongside the node.
- [collector-backend](./collector-backend) The collector backend listens for
  data from the collectors and serves a summary of it. This component is used by
  the
  [concordium-network-dashboard](https://github.com/Concordium/concordium-network-dashboard)
  to display the network overview.
- [macos_logger_wrapper](./macos_logger_wrapper/) provides an interface to the
  macOS logging interface using `os_log_create`. This is used by both the node
  and the collector so that the mac distribution package logs to the system
  logging service.

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

Do remember to clone recursively or use `git submodule update --init --recursive` after
cloning this repository, or after changing branches.

## Configurations and scripts

- The [jenkinsfiles](./jenkinsfiles/) directory contains Jenkins configurations
  for deployment and testing.
- The [scripts](./scripts/) directory contains a variety of bash scripts,
  Dockerfiles, and similar, to build different configurations of the node for
  testing and deployment.

## Building the node

See [concordium-node/README.md](./concordium-node/README.md).

# Contributing

To contribute start a new branch starting from `main`, make changes, and make a
merge request. A person familiar with the codebase should be asked to review the
changes before they are merged.

## Haskell workflow

We typically use [stack](https://docs.haskellstack.org/en/stable/README/) to
build, run, and test the code. In order to build the haskell libraries the rust
dependencies must be pre-build, which is done automatically by the cabal setup
script.

Code should be formatted using [`fourmolu`](https://github.com/fourmolu/fourmolu)
version `0.9.0.0` and using the config `fourmolu.yaml` found in the project root.
The CI is setup to ensure the code follows this style.

To check the formatting locally run the following command from the project root:

**On unix-like systems**:

```
$ fourmolu --mode check $(git ls-files '*.hs')
```

To format run the following command from the project root:

**On unix-like systems**:

```
$ fourmolu --mode inplace $(git ls-files '*.hs')
```

Lines should strive to be at most 100 characters, naming and code style should
follow the scheme that already exists.

We do not use any linting tool on the CI. Running hlint might uncover common
issues.

## Rust workflow

We use **stable version** of rust, 1.68, to compile the code.

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
  for the CI to pass you will need to install the relevant nightly version, see
  the `rustfmt` job in the file [.github/workflows/build-test.yaml](.github/workflows/build-test.yaml),
  look for `nightly-...`).
