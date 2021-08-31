# syntax=docker/dockerfile:experimental

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

COPY ./scripts/start.sh ./start.sh
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

FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node-dashboard:0.1.2 as node-dashboard

# Collect artifacts from build image.
FROM ubuntu:20.04

# Which environment we are building the image for.
# This affects URLs. Currently it should be either
#   - eu.staging.concordium.com
#   - testnet.concordium.com
#   - mainnet.concordium.software
ARG environment

EXPOSE 8888
# Node dashboard
EXPOSE 8099
# GRPC
EXPOSE 10000
    
# Concordium Node configuration    
ENV CONCORDIUM_NODE_RPC_SERVER_ADDR=0.0.0.0
ENV CONCORDIUM_NODE_DATA_DIR=/var/lib/concordium/data
ENV CONCORDIUM_NODE_CONFIG_DIR=/var/lib/concordium/config
ENV CONCORDIUM_NODE_CONNECTION_BOOTSTRAP_NODES=bootstrap.${environment}:8888
ENV ENABLE_TERM_HANDLER=true
ENV DISTRIBUTION_CLIENT=true
    
# Concordium Node Collector configuration
ENV CONCORDIUM_NODE_COLLECTOR_URL=https://dashboard.${environment}/nodes/post
ENV CONCORDIUM_NODE_COLLECTOR_GRPC_HOST=http://localhost:10000

RUN apt-get update && apt-get install -y curl netbase ca-certificates supervisor nginx libnuma1 libtinfo6 libpq-dev liblmdb-dev jq
RUN curl -L https://getenvoy.io/install.sh | bash -s -- -b /usr/local/bin
RUN getenvoy use 1.18.3

COPY --from=node-dashboard /static /node-dashboard/static
COPY --from=node-dashboard /envoy.yaml /node-dashboard/envoy.yaml
COPY --from=node-dashboard /nginx.conf /etc/nginx/sites-enabled/node-dashboard

COPY --from=build /build/concordium-node/target/release/concordium-node /concordium-node
COPY --from=build /build/concordium-node/target/release/node-collector /node-collector
COPY --from=build /build/start.sh /start.sh
COPY --from=genesis-data /data/genesis.dat /genesis.dat

COPY ./scripts/distribution/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/distribution/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/distribution/docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]
