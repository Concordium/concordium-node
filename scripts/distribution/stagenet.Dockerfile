# syntax=docker/dockerfile:experimental

# Build node.
ARG base_image_tag

# Clone genesis-data.
FROM alpine/git:latest as genesis-data
ARG genesis_ref
ARG genesis_path
WORKDIR /tmp
RUN mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
RUN --mount=type=ssh git clone --depth 1 --branch "${genesis_ref}" git@gitlab.com:Concordium/genesis-data.git
RUN ls -R && \
    mv "genesis-data/${genesis_path}/generated-data" /genesis-data

# Build node.
FROM concordium/base:${base_image_tag} as build

ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build-project
WORKDIR /build-project

COPY ./scripts/start.sh ./start.sh
COPY ./scripts/build-binaries.sh ./build-binaries.sh

RUN ./scripts/download-static-libs.sh
RUN ./build-binaries.sh "collector,staging_net" release && \
    strip /build-project/concordium-node/target/release/concordium-node && \
    strip /build-project/concordium-node/target/release/node-collector

FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/node-dashboard:0.1.0-alpha as node-dashboard

# Collect artifacts from build image.
FROM ubuntu:20.04

EXPOSE 8888
# Node dashboard
EXPOSE 8099
# GRPC-web proxy
EXPOSE 9999
# GRPC
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
RUN curl -L https://getenvoy.io/cli | bash -s -- -b /usr/local/bin
RUN getenvoy fetch standard:1.17.0

COPY --from=node-dashboard /static /node-dashboard/static
COPY --from=node-dashboard /envoy.yaml /node-dashboard/envoy.yaml
COPY --from=node-dashboard /nginx.conf /etc/nginx/sites-enabled/node-dashboard

COPY --from=build /build-project/concordium-node/target/release/concordium-node /concordium-node
COPY --from=build /build-project/concordium-node/target/release/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=genesis-data /genesis-data/genesis.dat /genesis.dat
RUN sha256sum /genesis.dat

COPY ./scripts/distribution/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/distribution/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/distribution/docker-entrypoint.sh /docker-entrypoint.sh
ENTRYPOINT [ "/docker-entrypoint.sh" ]
