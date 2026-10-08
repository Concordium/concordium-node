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

## Release guide

Use these instructions to release the `concordium/node` image.

### 1. Publish the image

1. Check the version in `concordium-node/Cargo.toml`.
2. Update `hub-description.md` if the configuration or operating instructions changed.
3. Create a Git tag with the same version and a new build number. Use the format `<version>-<build>-rc` or `<version>-<build>-alpha`.
4. Push the tag. For example:

   ```shell
   git tag 9.0.1-0-rc
   git push origin 9.0.1-0-rc
   ```

5. Check that [Docker node image release](../../../.github/workflows/docker-release.yaml) completes successfully in GitHub Actions.

### 2. Check the description

[Docker node description release](../../../.github/workflows/docker-description-release.yaml) starts automatically after the image release succeeds.

1. Check that the description workflow completes successfully.
2. Check the description on Docker Hub.

For a documentation-only release, commit the changes to `hub-description.md`. Run the description workflow manually from the branch with those changes.

### 3. Test the image

1. Pull `concordium/node:<version>`.
2. Do the checks in [Validate](#validate). Replace `concordium/node:latest` with the versioned image in each command.
3. Run the end-to-end tests with the same image.

### 4. Update latest

1. Open [Promote Docker node image to latest](../../../.github/workflows/docker-promote-latest.yaml) in GitHub Actions.
2. Select **Run workflow** and select the repository's default branch.
3. Enter the image version tag, for example `9.0.1-0`. Do not enter `9.0.1-0-rc`, `latest`, or a full image reference.
4. Start the workflow.
5. Check that the workflow completes successfully.
