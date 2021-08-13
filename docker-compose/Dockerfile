# This is a multi-stage build process
#
# Stage one builds a number of genesis configurations inside the genesis-tools image.
#
# Stage two buils the node, the collector, the collector backend, and the wallet-proxy.
#
# The final image collects all the binaries in a minimal image without intermediate build files.
# The genesis configuration ('genesis.dat' and keys of all bakers and accounts)
# for a cluster of N bakers is stored in the directory '/genesis-data/genesis-N-bakers'.

# ARGs that are used in multiple stages are defined globally in order to set a shared default value.
# They need to be redeclared in the steps that they are used but retain the default value defined here.
ARG ghc_version=8.10.4

# Build genesis packages.
FROM concordium/genesis-tools:latest as genesis-builder
RUN apt-get update && \
    apt-get -y install python3 && \
    rm -rf /var/lib/apt/lists/*

# Use globally installed tools.
ENV GENESIS_DAT_TOOL=genesis
ENV GENERATE_UPDATE_KEYS=generate-update-keys
ENV CLIENT_TOOL=client
ENV GENESIS_ACCOUNTS_TOOL=genesis_tool

# Build configurations for 1, 5, 10, and 25 bakers using the default configuration in 'scripts/genesis/genesis.json'.
# All the generated genesis files are baked into the image.
COPY ./scripts/genesis/ /genesis
WORKDIR /genesis
RUN for n in 1 5 10 25; do \
        GENESIS_DIR="./out/genesis-${n}-bakers" NUM_BAKERS="${n}" python3 generate-test-genesis.py; \
    done

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

# Build concordium-node and wallet-proxy.
FROM concordium/base:latest as build
ARG ghc_version

# Copy source files (only copy what's needed to utilize caching).
COPY ./concordium-base /build/concordium-base
COPY ./concordium-consensus /build/concordium-consensus
COPY ./concordium-node /build/concordium-node
COPY ./concordium-grpc-api /build/concordium-grpc-api
COPY ./deps /build/deps
COPY ./scripts/build-binaries.sh /build/build-binaries.sh
WORKDIR /build

# Copy static libraries that were built in the 'static-builder' step into the correct place ('concordium-node/deps/static/linux').
COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz .
RUN mkdir -p concordium-node/deps/static-libs/linux && \
    tar -xf static-consensus-${ghc_version}.tar.gz && \
    cp -r target/* concordium-node/deps/static-libs/linux/

# Build the Rust parts of the node.
RUN ./build-binaries.sh "collector"

# Build Wallet Proxy.
ARG wallet_proxy_branch=main
RUN git clone --recurse-submodules --depth=1 --branch="${wallet_proxy_branch}" https://github.com/Concordium/concordium-wallet-proxy.git /build/wallet-proxy && \
    (cd /build/wallet-proxy && stack build --copy-bins --ghc-options="-j4" --local-bin-path=target)

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
