ARG static_libraries_image_tag
ARG base_image_tag

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} as build-static
WORKDIR /build
COPY . .
ARG ghc_version
RUN GHC_VERSION="${ghc_version}" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build binaries.
FROM concordium/base:${base_image_tag} as build
WORKDIR /build
COPY . .
# Copy static libraries into the correct place ('/build/concordium-node/deps/static-libs/linux').
ARG ghc_version
COPY --from=build-static "/build/static-consensus-${ghc_version}.tar.gz" /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz
# Build in both release and debug mode.
ARG consensus_profiling=false
RUN CONSENSUS_PROFILING="${consensus_profiling}" /build/scripts/build-binaries.sh "instrumentation,collector" "release" && \
    CONSENSUS_PROFILING="${consensus_profiling}" /build/scripts/build-binaries.sh "instrumentation,collector"
# Evaluate the following RUN command in Bash to allow brace expansion.
SHELL ["/bin/bash", "-c"]
WORKDIR /target
RUN for build_profile in release debug; do \
        mkdir -p "./${build_profile}" && \
        cp "/build/concordium-node/target/${build_profile}"/{concordium-node,p2p_bootstrapper-cli,node-collector} "./${build_profile}/"; \
    done
