# syntax=docker/dockerfile:experimental

# Build node.
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.15 as build
ARG consensus_type
ENV CONSENSUS_TYPE=$consensus_type
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling
COPY . /build-project
WORKDIR /build-project
COPY ./scripts/init.build.env.sh ./init.build.env.sh
COPY ./scripts/start.sh ./start.sh
COPY ./genesis-data ./genesis-data
COPY ./scripts/build-binaries.sh ./build-binaries.sh
ENV LD_LIBRARY_PATH=/usr/local/lib
RUN --mount=type=ssh ./init.build.env.sh
RUN --mount=type=ssh ./build-binaries.sh "collector,staging_net" release && \
    strip /build-project/target/release/p2p_client-cli && \
    strip /build-project/target/release/node-collector && \
    cp /build-project/target/release/p2p_client-cli /build-project/ && \
    cp /build-project/target/release/node-collector /build-project/ && \
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
COPY --from=build /build-project/p2p_client-cli /p2p_client-cli
COPY --from=build /build-project/node-collector /node-collector
COPY --from=build /build-project/start.sh /start.sh
COPY --from=build /build-project/genesis.dat /genesis.dat
RUN sha256sum /genesis.dat
RUN ln -s /usr/lib/x86_64-linux-gnu/libtinfo.so.6.1 /usr/lib/x86_64-linux-gnu/libtinfo.so.5
COPY ./scripts/supervisord.conf /etc/supervisor/supervisord.conf
COPY ./scripts/concordium.conf /etc/supervisor/conf.d/concordium.conf
COPY ./scripts/staging-net-client.sh /staging-net-client.sh
ENTRYPOINT [ "/staging-net-client.sh" ]
