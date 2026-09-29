# Maintaining the Concordium node image

This directory contains the combined Concordium node image. User-facing configuration and run instructions are in [`hub-description.md`](./hub-description.md).

## Files

- `builder.Dockerfile` builds Consensus, `concordium-node`, and `node-collector`, then creates the runtime image.
- `entrypoint.sh` starts the node and optionally manages the collector.
- `genesis/` contains the genesis files included in the image.
- `hub-description.md` is the user-facing Docker Hub description.

## Build design

The Dockerfile uses two stages:

1. The builder installs Stack, lets the resolver in `stack.yaml` select GHC, builds the shared Consensus library, and builds both Rust executables.
2. The runtime stage contains the executables, their shared-library dependencies, genesis files, CA certificates, and `tini`.

The runtime supports Linux AMD64 only. The builder and runtime use the same Ubuntu release to keep their native library ABIs compatible.

## Prerequisites

Initialize all repository submodules before building:

```shell
git submodule update --init --recursive
```

Use Docker BuildKit or Docker Buildx.

## Build

Run the build from the repository root:

```shell
docker build \
  --platform linux/amd64 \
  --file scripts/distribution/docker-image/builder.Dockerfile \
  --tag concordium/node:latest \
  .
```

Stack and Cargo compilation take most of the initial build time. The build caches the compiler and external Haskell dependencies in a layer that source-only changes do not invalidate. Cargo compilation still follows the complete source copy.

## Validate

Confirm that both executables start and that the expected genesis files exist:

```shell
docker run --rm \
  --entrypoint /usr/local/bin/concordium-node \
  concordium/node:latest \
  --version

docker run --rm \
  --entrypoint /usr/local/bin/node-collector \
  concordium/node:latest \
  --version

docker run --rm \
  --entrypoint /bin/sh \
  concordium/node:latest \
  -c 'ls -l /genesis; ! ldd /usr/local/bin/concordium-node | grep -q "not found"'
```

Run the external end-to-end suite against the image when it is available in the workspace:

```shell
CONCORDIUM_NODE_IMAGE=concordium/node:latest \
  cargo run --release --manifest-path ../e2e-testing/Cargo.toml
```

## Update build tools

The repository files select Rust and Haskell dependencies:

- `rust-toolchain.toml` selects Rust.
- `stack.yaml` selects the Stack resolver and GHC.
- Cargo lock files select Rust dependencies.

The Dockerfile copies the Stack manifests and builds Haskell dependencies before it copies the complete source tree. Source-only changes reuse the compiler and Haskell dependency layer. Changes to `stack.yaml`, `stack.yaml.lock`, either local package manifest, or the local LMDB package invalidate that layer.

The Dockerfile separately pins Stack, protoc, and FlatBuffers. Keep these arguments aligned with the release workflow when that workflow changes:

```dockerfile
ARG STACK_VERSION=...
ARG PROTOC_VERSION=...
ARG FLATBUFFERS_VERSION=...
```

A tool-version change invalidates the related Docker layers. Rust dependencies are still built after the complete source copy, so source changes can invalidate the Cargo layer. Perform a clean build and run the end-to-end suite after a tool-version change.

## Add a network genesis file

Add each genesis file to `genesis/` with a descriptive network name:

```text
genesis/<network>-genesis.dat
```

The Dockerfile copies the complete directory to `/genesis`. Update `hub-description.md` with the new path and the required network settings. Do not select a default network in the image.

## Change runtime behavior

Keep these contracts synchronized when runtime behavior changes:

- Dockerfile environment defaults
- `entrypoint.sh` validation and process management
- `hub-description.md` migration and user instructions
- The e2e node fixture when executable paths, users, or startup behavior change

The image runs as UID and GID `10001:10001`. The entrypoint requires an explicit genesis path, starts the collector only when enabled, keeps the node running if the collector exits, and stops the collector when the node exits.

## Publish documentation

Use `hub-description.md` as the Docker Hub repository description. Keep operational instructions there rather than duplicating them in this maintainer document.
