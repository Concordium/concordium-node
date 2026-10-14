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

### 1. Build the staging image

1. Check the version in `concordium-node/Cargo.toml`.
2. Update `hub-description.md` if the configuration or operating instructions changed.
3. Create a Git tag with the same version and a new build number. Use the format `<version>-<build>-rc` or `<version>-<build>-alpha`.
4. Push the tag. For example:

   ```shell
   git tag 9.0.1-0-rc
   git push origin 9.0.1-0-rc
   ```

5. Check that [Docker node image build](../../../.github/workflows/docker-build.yaml) completes successfully in GitHub Actions. It pushes directly to ECR and signs the digest.

Manual builds use `<version>-<short SHA>` staging tags. Alpha and manual builds cannot be released to Docker Hub. A build rerun can reuse an existing image only when its revision and version labels match. A first attempt with an existing version fails.

### 2. Publish to Docker Hub

1. Open [Docker node image release](../../../.github/workflows/docker-release.yaml).
2. Select **Run workflow** on the default branch.
3. Enter an existing ECR RC tag, for example `9.0.1-0-rc`.
4. Review the validation summary. It records the source revision and pinned ECR digest before approval.
5. Approve publication through the `release` environment.
6. Check that publication and signing complete. The public tag is `concordium/node:9.0.1-0`.

The release verifies the exact RC-tag signature from `docker-build.yaml` and checks image metadata against the Git tag. It verifies the signature again after approval, copies the pinned digest without rebuilding, checks the public digest, and signs it under the release workflow identity. Builds signed by the former build workflow are not eligible; create a new RC build.

A matching public digest permits a retry, including completion of signing. A different public digest fails and is never overwritten. Publication does not update `latest`.

### 3. Check the description

[Docker node description release](../../../.github/workflows/docker-description-release.yaml) starts automatically after the image release succeeds.

1. Check that the description workflow completes successfully.
2. Check the description on Docker Hub.

For a documentation-only release, commit the changes to `hub-description.md`. Run the description workflow manually from the branch with those changes.

### 4. Test the image

1. Pull `concordium/node:<version>`.
2. Do the checks in [Validate](#validate). Replace `concordium/node:latest` with the versioned image in each command.
3. Run the end-to-end tests with the same image.

### 5. Update latest

1. Open [Promote Docker node image to latest](../../../.github/workflows/docker-promote-latest.yaml) in GitHub Actions.
2. Select **Run workflow** and select the repository's default branch.
3. Enter the image version tag, for example `9.0.1-0`. Do not enter `9.0.1-0-rc`, `latest`, or a full image reference.
4. Start the workflow.
5. Check that the workflow completes successfully.

## AWS and GitHub setup

How to set the repository-level GitHub Actions variables. Do not override them in the `release` environment:

| Variable | Value |
| --- | --- |
| `AWS_REGION` | ECR region, for example `eu-west-1` |
| `ECR_REPOSITORY` | Full URI, for example `192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node` |
| `AWS_ECR_PUSH_ROLE_ARN` | OIDC role for staging builds |
| `AWS_ECR_PULL_ROLE_ARN` | Read-only OIDC role for release validation and publication |

Configure ECR tag immutability as `IMMUTABLE_WITH_EXCLUSION`, with a wildcard exclusion of `sha256-*.sig`. Version tags must be immutable. Cosign v2 signature tags must remain writable so signing can complete on retries.

Both roles need `ecr:GetAuthorizationToken` on `*`. Scope the remaining permissions to the staging repository:

- Pull role: `ecr:DescribeImages`, `ecr:BatchGetImage`, and `ecr:GetDownloadUrlForLayer`.
- Push role: all pull permissions, plus `ecr:BatchCheckLayerAvailability`, `ecr:InitiateLayerUpload`, `ecr:UploadLayerPart`, `ecr:CompleteLayerUpload`, and `ecr:PutImage`.

Use GitHub's OIDC provider with audience `sts.amazonaws.com`. Restrict role trust to this repository. The push role must allow the RC/alpha tag subjects and approved manual-build branch subjects. The pull role must allow the default-branch subject for validation and `repo:Concordium/concordium-node:environment:release` for publication. Configure the `release` environment with required reviewers and restrict it to the default branch.

Set `DOCKER_USERNAME` and `DOCKER_TOKEN` in the `release` environment. The token must permit image, signature, and description publication to `concordium/node`. These workflows do not provision AWS resources.
