# syntax=docker/dockerfile:1

# Use the same Ubuntu release for the build and runtime stages. This prevents
# incompatibilities between the build-time and runtime system libraries.
ARG UBUNTU_VERSION=22.04

# Compile the Haskell and Rust components in a disposable build stage.
FROM ubuntu:${UBUNTU_VERSION} AS builder

# Pin tools that are not selected by the repository toolchain files.
ARG DEBIAN_FRONTEND=noninteractive
ARG STACK_VERSION=3.7.1
ARG PROTOC_VERSION=28.3
ARG FLATBUFFERS_VERSION=23.5.26

# Make pipelines fail when any command in a pipeline fails.
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install the native compilers, headers, and utilities required by Stack and
# Cargo. Remove the package index after installation to keep the layer smaller.
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        git \
        libffi-dev \
        libgmp-dev \
        liblmdb-dev \
        libnuma-dev \
        libssl-dev \
        libtinfo-dev \
        pkg-config \
        unzip \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Stack directly because Ubuntu does not provide the required version.
# Stack will download the GHC version selected by the repository resolver.
RUN curl --fail --location --show-error --silent \
        "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64.tar.gz" \
        --output /tmp/stack.tar.gz \
    && tar --extract --gzip --file /tmp/stack.tar.gz --directory /tmp \
    && install "/tmp/stack-${STACK_VERSION}-linux-x86_64/stack" /usr/local/bin/stack \
    && rm --recursive --force /tmp/stack.tar.gz "/tmp/stack-${STACK_VERSION}-linux-x86_64"

# Install the code generators used by the node and collector build scripts.
# These archives contain Linux AMD64 executables, so the image currently
# supports only that architecture.
RUN curl --fail --location --show-error --silent \
        "https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-x86_64.zip" \
        --output /tmp/protoc.zip \
    && unzip -q /tmp/protoc.zip -d /usr/local bin/protoc 'include/*' \
    && rm /tmp/protoc.zip \
    && curl --fail --location --show-error --silent \
        "https://github.com/google/flatbuffers/releases/download/v${FLATBUFFERS_VERSION}/Linux.flatc.binary.g++-10.zip" \
        --output /tmp/flatc.zip \
    && unzip -q /tmp/flatc.zip -d /tmp/flatc \
    && install /tmp/flatc/flatc /usr/local/bin/flatc \
    && rm --recursive --force /tmp/flatc /tmp/flatc.zip

# Install rustup with a minimal profile. Cargo reads rust-toolchain.toml after
# the source tree is copied and installs the repository's Rust toolchain.
ENV PATH=/root/.cargo/bin:${PATH}
RUN curl --proto '=https' --tlsv1.2 --fail --location --show-error --silent \
        https://sh.rustup.rs \
        | sh -s -- -y --profile minimal

# Copy only the Stack project definition and package manifests first. This
# keeps the compiler and Haskell dependency layer cached when source files
# change. The local LMDB package is an extra dependency, so include its source.
WORKDIR /build
COPY stack.yaml stack.yaml.lock ./
COPY concordium-base/package.yaml concordium-base/package.yaml
COPY concordium-consensus/package.yaml concordium-consensus/package.yaml
COPY concordium-consensus/haskell-lmdb/ concordium-consensus/haskell-lmdb/

# Download the selected GHC and build external Haskell dependencies before the
# application source can invalidate the Docker layer.
RUN stack setup \
    && stack build --only-dependencies --flag concordium-consensus:dynamic

# Put the complete repository, including initialized submodules, in the build
# directory. The repository .dockerignore excludes local build products.
COPY . .

# Build the project packages. Dependencies remain cached from the prior layer.
# The dynamic flag builds the shared Consensus foreign library used by the node.
RUN stack build --flag concordium-consensus:dynamic

# Build release versions of both Rust executables. The node omits the static
# feature, so its build script links it to the shared Haskell libraries.
RUN cargo build --locked --release --manifest-path concordium-node/Cargo.toml \
    && cargo build --locked --release --manifest-path collector/Cargo.toml

# Assemble the files that the runtime stage needs under /runtime. Copy all
# project, snapshot, and GHC shared libraries, including Concordium's symlinked
# Rust libraries. Use Ubuntu's libffi at runtime instead of GHC's bundled copy.
# Remove debugging information from staged ELF libraries, validate that both
# executables can resolve their libraries, and then strip the executables.
RUN set -eux; \
    install -d /runtime/usr/local/bin /runtime/usr/local/lib; \
    install concordium-node/target/release/concordium-node /runtime/usr/local/bin/concordium-node; \
    install collector/target/release/node-collector /runtime/usr/local/bin/node-collector; \
    local_install_root="$(stack path --local-install-root)"; \
    snapshot_install_root="$(stack path --snapshot-install-root)"; \
    ghc_lib_dir="$(stack ghc -- --print-libdir)"; \
    project_library_dirs=(/build/concordium-base/lib); \
    for directory in \
        /build/concordium-base/smart-contracts/lib \
        /build/concordium-consensus/lib; do \
        if [[ -d "${directory}" ]]; then \
            project_library_dirs+=("${directory}"); \
        fi; \
    done; \
    find \
        "${local_install_root}/lib" \
        "${snapshot_install_root}/lib" \
        "${ghc_lib_dir}" \
        "${project_library_dirs[@]}" \
        \( -type f -o -type l \) \
        \( -name '*.so' -o -name '*.so.*' \) \
        ! -name 'libffi.so*' \
        -exec cp --dereference --no-clobber --target-directory=/runtime/usr/local/lib {} +; \
    find /runtime/usr/local/lib -type f -name '*.so*' -print0 \
        | while IFS= read -r -d '' library; do \
            if readelf --file-header "$library" >/dev/null 2>&1; then \
                strip --strip-debug "$library"; \
            fi; \
        done; \
    LD_LIBRARY_PATH=/runtime/usr/local/lib ldd /runtime/usr/local/bin/concordium-node \
        | tee /tmp/concordium-node.ldd; \
    ! grep --quiet 'not found' /tmp/concordium-node.ldd; \
    LD_LIBRARY_PATH=/runtime/usr/local/lib ldd /runtime/usr/local/bin/node-collector \
        | tee /tmp/node-collector.ldd; \
    ! grep --quiet 'not found' /tmp/node-collector.ldd; \
    strip /runtime/usr/local/bin/concordium-node /runtime/usr/local/bin/node-collector

# Start the production image without compilers, source files, or build caches.
FROM ubuntu:${UBUNTU_VERSION} AS runtime

ARG DEBIAN_FRONTEND=noninteractive

# Install only native runtime libraries and tini. Create a fixed unprivileged
# identity and give it ownership of the node's default state directory.
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        ca-certificates \
        libffi8 \
        libgmp10 \
        liblmdb0 \
        libnuma1 \
        libssl3 \
        libtinfo6 \
        tini \
        zlib1g \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 concordium \
    && useradd --uid 10001 --gid concordium --no-create-home --shell /usr/sbin/nologin concordium \
    && install --directory --owner concordium --group concordium /mnt/data

# Copy the compiled executables and shared Haskell libraries from the builder.
COPY --from=builder /runtime/ /

# Install the process manager script that starts the node and optional collector.
COPY scripts/distribution/docker-image/entrypoint.sh /usr/local/bin/concordium-entrypoint

# Include every tracked genesis file. Add future networks to this source directory.
COPY scripts/distribution/docker-image/genesis/ /genesis/

# Register the copied libraries and fail the build if either executable has an
# unresolved dependency or cannot start far enough to print its version.
RUN chmod 0755 /usr/local/bin/concordium-entrypoint \
    && ldconfig \
    && ldd /usr/local/bin/concordium-node | tee /tmp/concordium-node.ldd \
    && ! grep --quiet 'not found' /tmp/concordium-node.ldd \
    && ldd /usr/local/bin/node-collector | tee /tmp/node-collector.ldd \
    && ! grep --quiet 'not found' /tmp/node-collector.ldd \
    && /usr/local/bin/concordium-node --version \
    && /usr/local/bin/node-collector --version \
    && rm /tmp/concordium-node.ldd /tmp/node-collector.ldd

# Store mutable node state outside the application directories. Enable gRPC on
# the documented container port, but keep the collector disabled until the
# operator supplies its identity and backend URL.
ENV CONCORDIUM_NODE_CONFIG_DIR=/mnt/data \
    CONCORDIUM_NODE_DATA_DIR=/mnt/data \
    CONCORDIUM_NODE_GRPC2_LISTEN_ADDRESS=0.0.0.0 \
    CONCORDIUM_NODE_GRPC2_LISTEN_PORT=20000 \
    CONCORDIUM_NODE_COLLECTOR_ENABLED=false \
    CONCORDIUM_NODE_COLLECTOR_GRPC_HOST=http://127.0.0.1:20000

# Document the mainnet P2P and gRPC ports. Operators can configure other ports.
EXPOSE 8888/tcp 20000/tcp

# Run the node without root privileges from its writable state directory.
USER concordium:concordium
WORKDIR /mnt/data

# Use tini for signal forwarding and zombie reaping. The entrypoint manages the
# optional collector, and the default command starts the node.
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/concordium-entrypoint"]
CMD ["/usr/local/bin/concordium-node"]
