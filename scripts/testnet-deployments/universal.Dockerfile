# syntax=docker/dockerfile:experimental

FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.17 as build

ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling

COPY . /build-project
WORKDIR /build-project

RUN mkdir -p /out/release && mkdir -p /out/debug

RUN ./scripts/download-static-libs.sh

# Build
RUN ./scripts/build-binaries.sh "instrumentation,collector" "release"
RUN ./scripts/build-binaries.sh "instrumentation,collector"

# Copy
RUN cp concordium-node/target/release/concordium-node \
       concordium-node/target/release/p2p_bootstrapper-cli \
       concordium-node/target/release/node-collector \
       concordium-node/target/release/node-collector-backend /out/release/
RUN cp concordium-node/target/release/concordium-node \
       concordium-node/target/release/p2p_bootstrapper-cli \
       concordium-node/target/release/node-collector \
       concordium-node/target/release/node-collector-backend /out/debug/
RUN cp -r genesis-data /out/genesis-data

RUN cp scripts/start.sh /out/start.sh
RUN chmod +x /out/start.sh

FROM ubuntu:20.04

COPY --from=build /out /out
