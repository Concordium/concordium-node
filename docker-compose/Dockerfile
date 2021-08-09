# This is a multi-stage build process
#
# Stage one builds a number of genesis configurations inside the genesis-tools image
#
# Stage two buils the node, the collector, the collector backend, and the wallet-proxy
#
# The final image collects all the binaries in a minimal package without build dependencies.
# Inside the image there are private keys for all the bakers and accounts, as well as genesis.dat
# files for the configurations that are built.
# The genesis configurations are in the /genesis-data directory, in subdirectories
# named genesis-$NUM_BAKERS-bakers.

FROM concordium/genesis-tools:latest as genesis-builder

RUN apt-get update && \
    apt-get -y install python3 && \
    rm -rf /var/lib/apt/lists/*

# Use globally installed tools.
ARG GENESIS_DAT_TOOL=genesis
ARG GENERATE_UPDATE_KEYS=generate-update-keys
ARG CLIENT_TOOL=client
ARG GENESIS_ACCOUNTS_TOOL=genesis_tool

# Build images for different configurations. Currently for 1, 5, 10, and 25
# bakers using the default genesis.json configuration.
# Build all the relevant genesis files and embed them in the image itself.
COPY scripts/genesis/ /genesis
WORKDIR /genesis
RUN GENESIS_DIR=./out/genesis-1-bakers NUM_BAKERS=1 python3 generate-test-genesis.py && \
    GENESIS_DIR=./out/genesis-5-bakers NUM_BAKERS=5 python3 generate-test-genesis.py && \
    GENESIS_DIR=./out/genesis-10-bakers NUM_BAKERS=10 python3 generate-test-genesis.py && \
    GENESIS_DIR=./out/genesis-25-bakers NUM_BAKERS=25 python3 generate-test-genesis.py

# Build static consensus libraries.
FROM concordium/static-libraries:latest as static-builder
# Copy source files (only copy what's needed to utilize caching).
COPY ./concordium-base /build/concordium-base
COPY ./concordium-consensus /build/concordium-consensus
COPY ./concordium-node /build/concordium-node
COPY ./concordium-grpc-api /build/concordium-grpc-api
COPY ./deps /build/deps
COPY ./scripts/static-libraries/build-static-libraries.sh /build/build-static-libraries.sh
COPY ./LICENSE /build/LICENSE
WORKDIR /build
ARG ghc_version
RUN GHC_VERSION="${ghc_version}" ./build-static-libraries.sh

# And build the node
FROM concordium/base:latest as build

ARG ghc_version
ARG consensus_profiling
ENV CONSENSUS_PROFILING=$consensus_profiling

# Copy source files (only copy what's needed to utilize caching).
COPY ./concordium-base /build/concordium-base
COPY ./concordium-consensus /build/concordium-consensus
COPY ./concordium-node /build/concordium-node
COPY ./concordium-grpc-api /build/concordium-grpc-api
COPY ./deps /build/deps
COPY ./scripts/build-binaries.sh /build/build-binaries.sh
WORKDIR /build

# Node
# Copy static libraries that were built in the 'static-builder' step into the correct place ('concordium-node/deps/static/linux').
COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz .
RUN mkdir -p concordium-node/deps/static-libs/linux && \
    tar -xf static-consensus-${ghc_version}.tar.gz && \
    cp -r target/* concordium-node/deps/static-libs/linux/

# Build the Rust parts of the node.
RUN ./build-binaries.sh "collector"

# Build baker id gen.
RUN git clone --recurse-submodules --depth 1 --branch main https://github.com/Concordium/concordium-tools-baker-id-gen.git /build/baker_id_gen && \
    (cd /build/baker_id_gen && cargo build --release)

# Build Wallet Proxy.
RUN git clone --recurse-submodules --depth 1 --branch main https://github.com/Concordium/concordium-wallet-proxy.git /build/wallet-proxy && \
    (cd /build/wallet-proxy && stack build --copy-bins --ghc-options -j4 --local-bin-path target)

# Copy artifacts into fresh image.
FROM ubuntu:20.04

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt-get update && \
    apt-get install -y less curl unbound postgresql-server-dev-12 libnuma1 && \
    rm -rf /var/lib/apt/lists/*

# Genesis data files for all configurations.
COPY --from=genesis-builder /genesis/out/ /genesis-data

# Node files.
COPY --from=build /build/concordium-node/target/debug/concordium-node /concordium-node
COPY --from=build /build/concordium-node/target/debug/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build/concordium-node/target/debug/node-collector /node-collector
COPY --from=build /build/concordium-node/target/debug/node-collector-backend /node-collector-backend

# Baker ID generator.
COPY --from=build /build/baker_id_gen/target/release/baker_id_gen /baker_id_generator

# Wallet proxy.
COPY --from=build /build/wallet-proxy/target/wallet-proxy /wallet-proxy
COPY --from=build /build/wallet-proxy/deps/concordium-client/deps/concordium-base/rust-src/target/release/*.so /usr/lib/

# Set up dirs expected by node and bootstrapper.
ENV CONCORDIUM_NODE_DATA_DIR=/var/lib/concordium/data
ENV CONCORDIUM_NODE_CONFIG_DIR=/var/lib/concordium/config
RUN mkdir -p "${CONCORDIUM_NODE_CONFIG_DIR}" "${CONCORDIUM_NODE_DATA_DIR}"
    
# Entrypoints for the various components.
# No default entrypoint is defined because the image serves multiple purposes.
COPY ./docker-compose/node-entrypoint.sh /node-entrypoint.sh
COPY ./docker-compose/collector-entrypoint.sh /collector-entrypoint.sh
# TODO Add wallet-proxy endpoint.
