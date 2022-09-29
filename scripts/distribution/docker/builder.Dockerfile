# syntax=docker/dockerfile:1

# This dockerfile builds a distribution image that contains
# - the node binary at /concordium-node
# - the node collector binary at /node-collector
# - environment specific genesis data at /${environment}-genesis.dat
#
# No environment variables are set for the node or the collector. Those are
# intended to be set by a docker-compose or similar configuration for the
# specific container runtime environment.

# This builder image expects the following environment variables to be set
# - genesis_path (root of the genesis directory from which the genesis block is taken)
# - genesis_ref (branch or commit of the genesis_data repository where the genesis data is located)
# - environment (testnet/mainnet/stagenet, affects the naming of the genesis block)
#
# The build of the image will clone the genesis data repository so needs
# credentials to access it.

ARG static_binaries_image_tag
FROM static-node-binaries:${static_binaries_image_tag} as binaries

# Fetch genesis-data.
FROM alpine/git:latest as genesis-data
ARG genesis_ref
ARG genesis_path
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git
RUN mv "genesis-data/${genesis_path}" /data

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

COPY --from=binaries /build/bin/concordium-node /concordium-node
COPY --from=binaries /build/bin/node-collector /node-collector
COPY --from=genesis-data /data/genesis.dat /${environment}-genesis.dat
