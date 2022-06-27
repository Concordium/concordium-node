# syntax=docker/dockerfile:experimental

# This dockerfile builds a distribution image that contains
# - the node binary at /concordium-node
# - the node collector binary at /node-collector
# - environment specific genesis data at /${environment}-genesis.dat
#
# No environment variables are set for the node or the collector. Those are
# intended to be set by a docker-compose or similar configuration for the
# specific container runtime environment.

# This builder image expects the following environment variables to be set
# - base_image_tag (image with the relevant rust toolchain and other build dependencies installed)
# - static_libraries_image_tag (image with dependencies for building Haskell libraries that are linked statically)
# - ghc_version (version of the GHC compiler used to build static binaries)
# - genesis_path (root of the genesis directory from which the genesis block is taken)
# - genesis_ref (branch or commit of the genesis_data repository where the genesis data is located)
# - consensus_profiling (whether to enable a node with profiling, defaults to `false`)
# - environment (testnet/mainnet/stagenet, affects the naming of the genesis block)
#
# The build of the image will clone the genesis data repository.

# Build node.
ARG base_image_tag
ARG static_libraries_image_tag

# Fetch genesis-data.
FROM alpine/git:latest as genesis-data
ARG genesis_ref
ARG genesis_path
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git
RUN mv "genesis-data/${genesis_path}" /data

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} as static-builder
COPY . /build
ARG ghc_version
WORKDIR /build
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build node.
FROM concordium/base:${base_image_tag} as build

ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build
WORKDIR /build

COPY ./scripts/build-binaries.sh ./build-binaries.sh

# Copy static libraries that were built by the static-builder into the correct place
# (/build/concordium-node/deps/static-libs/linux).
ARG ghc_version
COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz

RUN ./build-binaries.sh "collector" release && \
    strip /build/concordium-node/target/release/concordium-node && \
    strip /build/concordium-node/target/release/node-collector

# Collect artifacts from build image.
FROM ubuntu:20.04

# Which environment we are building the image for.
# Currently it should be either
#   - stagenet
#   - testnet
#   - mainnet
ARG environment

RUN apt-get update && \
    apt-get install -y libgmp10 libssl1.1 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

COPY --from=build /build/concordium-node/target/release/concordium-node /concordium-node
COPY --from=build /build/concordium-node/target/release/node-collector /node-collector
COPY --from=genesis-data /data/genesis.dat /${environment}-genesis.dat
