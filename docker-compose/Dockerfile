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

# Build all the relevant genesis files and embed them in the image itself.
COPY scripts/genesis/ /genesis
WORKDIR /genesis
RUN apt-get update && \
    apt-get -y install python3 && \
    rm -rf /var/lib/apt/lists/*

# Use globally installed tools
ARG GENESIS_DAT_TOOL=genesis
ARG GENERATE_UPDATE_KEYS=generate-update-keys
ARG CLIENT_TOOL=client
ARG GENESIS_ACCOUNTS_TOOL=genesis_tool
# And build images for different configurations. Currently for 1, 5, 10, and 25
# bakers using the default genesis.json configuration.
RUN GENESIS_DIR=./out/genesis-1-bakers NUM_BAKERS=1 python3 generate-test-genesis.py
RUN GENESIS_DIR=./out/genesis-5-bakers NUM_BAKERS=5 python3 generate-test-genesis.py
RUN GENESIS_DIR=./out/genesis-10-bakers NUM_BAKERS=10 python3 generate-test-genesis.py
RUN GENESIS_DIR=./out/genesis-25-bakers NUM_BAKERS=25 python3 generate-test-genesis.py


# Build static consensus libraries
FROM concordium/static-libraries:0.19 as static-builder
COPY scripts/static-libraries/build-static-libraries.sh /build-static-libraries.sh
COPY . /build
ARG ghc_version
RUN GHC_VERSION="${ghc_version}" ./build-static-libraries.sh

# And build the node
FROM concordium/base:0.19 as build

ARG ghc_version
ARG consensus_profiling
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build/
COPY scripts/start.sh /build/start.sh
COPY scripts/build-binaries.sh /build/build-binaries.sh

# Node
WORKDIR /build
# Copy static libraries that were built by the static-builder into the correct place, which is concordium-node/deps/static/linux
COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz .
RUN mkdir -p concordium-node/deps/static-libs/linux
RUN tar -xf static-consensus-${ghc_version}.tar.gz && cd target && cp -r * ../concordium-node/deps/static-libs/linux/
# And then start the build of the rust parts of the node.
RUN ./build-binaries.sh "collector"

# Baker id gen
RUN git clone --recurse-submodules --depth 1 --branch main https://github.com/Concordium/concordium-tools-baker-id-gen.git /baker_id_gen
WORKDIR /baker_id_gen
RUN cargo build --release

# Wallet-proxy
RUN git clone --recurse-submodules --depth 1 --branch main https://github.com/Concordium/concordium-wallet-proxy.git /wallet-proxy
WORKDIR /wallet-proxy
RUN stack build --copy-bins --ghc-options -j4 --local-bin-path target

# Construct the final image. This contains only the necessary binaries without all the build artifacts to make it reasonably small.
FROM ubuntu:20.04

EXPOSE 8950
EXPOSE 8888
EXPOSE 9090
EXPOSE 8900
EXPOSE 10000

RUN apt update && apt -y install unbound curl postgresql-server-dev-12 libnuma1

# Genesis data files for all configurations.
COPY --from=genesis-builder /genesis/out/ /genesis-data

# Node files.
COPY --from=build /build/concordium-node/target/debug/concordium-node /concordium-node
COPY --from=build /build/concordium-node/target/debug/p2p_bootstrapper-cli /p2p_bootstrapper-cli
COPY --from=build /build/concordium-node/target/debug/node-collector /node-collector
COPY --from=build /build/concordium-node/target/debug/node-collector-backend /node-collector-backend 
COPY --from=build /build/start.sh /start.sh

# Baker ID generator.
COPY --from=build /baker_id_gen/target/release/baker_id_gen /baker_id_generator

# Wallet proxy.
COPY --from=build /wallet-proxy/target/wallet-proxy /wallet-proxy
COPY --from=build /wallet-proxy/deps/concordium-client/deps/concordium-base/rust-src/target/release/*.so /usr/lib/

ENTRYPOINT ["/start.sh"]
