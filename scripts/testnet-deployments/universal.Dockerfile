ARG base_image_tag
FROM concordium/base:${base_image_tag} as build
COPY . /build-project
WORKDIR /build-project

RUN ./scripts/download-static-libs.sh # TODO build in place

# Build in both release and debug mode.
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING=$consensus_profiling
RUN ./scripts/build-binaries.sh "instrumentation,collector" "release" && \
    ./scripts/build-binaries.sh "instrumentation,collector" && \
    mkdir -p /out/release && \
    mkdir -p /out/debug && \
    cp concordium-node/target/release/concordium-node \
       concordium-node/target/release/p2p_bootstrapper-cli \
       concordium-node/target/release/node-collector \
       concordium-node/target/release/node-collector-backend /out/release/ && \
    cp concordium-node/target/debug/concordium-node \
       concordium-node/target/debug/p2p_bootstrapper-cli \
       concordium-node/target/debug/node-collector \
       concordium-node/target/debug/node-collector-backend /out/debug/ && \
    cp scripts/start.sh /out/start.sh

FROM ubuntu:20.04
COPY --from=build /out /out
