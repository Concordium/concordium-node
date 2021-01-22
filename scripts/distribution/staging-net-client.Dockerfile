# syntax=docker/dockerfile:experimental

# Build node.
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.17 as build

ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build-project
WORKDIR /build-project

COPY ./scripts/start.sh ./start.sh
COPY ./genesis-data ./genesis-data
COPY ./scripts/build-binaries.sh ./build-binaries.sh

RUN --mount=type=ssh ./scripts/download-static-libs.sh
RUN --mount=type=ssh ./build-binaries.sh "collector,staging_net" release && \
    strip /build-project/concordium-node/target/release/concordium-node && \
    strip /build-project/concordium-node/target/release/node-collector && \
    cd /build-project/genesis-data && \
    tar -xf 20-bakers.tar.gz && \
    cd genesis_data && \
    sha256sum genesis.dat && \
    cp genesis.dat /build-project/

# Collect artifacts from build image.
FROM ubuntu:20.04

EXPOSE 8888
EXPOSE 10000
ENV RPC_SERVER_ADDR=0.0.0.0
ENV MODE=basic
ENV BOOTSTRAP_FIRST_NODE=bootstrap.eu.staging.concordium.com:8888
ENV DATA_DIR=/var/lib/concordium/data
ENV CONFIG_DIR=/var/lib/concordium/config
ENV EXTRA_ARGS="--no-dnssec"
ENV NODE_URL=localhost:10000
ENV COLLECTORD_URL=https://dashboard.eu.staging.concordium.com/nodes/post
ENV GRPC_HOST=http://localhost:10000
ENV DISTRIBUTION_CLIENT=true
ENV ENABLE_TERM_HANDLER=true

RUN apt-get update && apt-get install -y unbound curl netbase ca-certificates supervisor nginx libnuma1 libtinfo6 libpq-dev liblmdb-dev jq

COPY --from=build /build-project/concordium-node/target/release/concordium-node /concordium-node
COPY --from=build /build-project/concordium-node/target/release/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis.dat /genesis.dat
RUN sha256sum /genesis.dat

COPY ./scripts/distribution/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/distribution/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/distribution/staging-net-client.sh /staging-net-client.sh
ENTRYPOINT [ "/staging-net-client.sh" ]
