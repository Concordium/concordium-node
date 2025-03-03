ARG ubuntu_version=latest
ARG static_libraries_image_tag=latest

# Build static consensus libraries.
FROM concordium/static-libraries:${static_libraries_image_tag} AS static-builder
COPY . /build
ARG ghc_version
WORKDIR /build
RUN GHC_VERSION="$ghc_version" \
      /build/scripts/static-libraries/build-static-libraries.sh

# Build static node binaries.
FROM ubuntu:${ubuntu_version}

COPY scripts/static-binaries/build-on-ubuntu.sh /build-on-ubuntu.sh
COPY . /build

# Copy static libraries that were built by the static-builder into the correct place
# (/build/concordium-node/deps/static-libs/linux).
ARG ghc_version
COPY --from=static-builder /build/static-consensus-${ghc_version}.tar.gz /tmp/static-consensus.tar.gz
RUN tar -C /tmp -xf /tmp/static-consensus.tar.gz && \
    mkdir -p /build/concordium-node/deps/static-libs && \
    mv /tmp/target /build/concordium-node/deps/static-libs/linux && \
    rm /tmp/static-consensus.tar.gz

# Build (mostly) static node and some auxiliary binaries
ARG build_version
ARG extra_features
ARG protoc_version
ARG flatbuffers_version
ARG rust_toolchain_version

WORKDIR /build
RUN BRANCH="${branch}" \
    EXTRA_FEATURES="${extra_features}" \
    PROTOC_VERSION="${protoc_version}" \
    FLATBUFFERS_VERSION="${flatbuffers_version}" \
    RUST_TOOLCHAIN_VERSION="${rust_toolchain_version}" \
      /build-on-ubuntu.sh

# The binaries are available in the
# /build/bin
# directory of the image.
