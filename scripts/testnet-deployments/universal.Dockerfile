ARG base_image_tag
ARG static_libraries_image_tag

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} as static-builder
COPY . /build
ARG ghc_version
WORKDIR /build
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

FROM concordium/base:${base_image_tag} as build
COPY . /build
WORKDIR /build

# Copy static libraries that were built by the static-builder into the correct place
# (/build/concordium-node/deps/static-libs/linux).
ARG ghc_version
COPY --from=static-builder "/build/static-consensus-${ghc_version}.tar.gz" /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz

# Build in both release and debug mode.
ARG consensus_profiling=false
ENV CONSENSUS_PROFILING="${consensus_profiling}"
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
