# syntax=docker/dockerfile:1

# Use the same Ubuntu release for the build and runtime stages. This prevents
# incompatibilities between the build-time and runtime system libraries.
ARG UBUNTU_VERSION=26.04

# Compile the Haskell and Rust components in a disposable build stage.
FROM ubuntu:${UBUNTU_VERSION} AS builder

# Pin tools that are not selected by the repository toolchain files. BuildKit
# sets TARGETARCH to the platform being built (amd64 or arm64).
ARG TARGETARCH
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
        cmake \
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
# Stack and protoc name the ARM64 archives differently, so map both names.
RUN case "${TARGETARCH}" in \
        amd64) stack_arch=x86_64; protoc_arch=x86_64 ;; \
        arm64) stack_arch=aarch64; protoc_arch=aarch_64 ;; \
        *) echo "Unsupported TARGETARCH: ${TARGETARCH}" >&2; exit 1 ;; \
    esac \
    && curl --fail --location --show-error --silent \
        "https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-${stack_arch}.tar.gz" \
        --output /tmp/stack.tar.gz \
    && tar --extract --gzip --file /tmp/stack.tar.gz --directory /tmp \
    && install "/tmp/stack-${STACK_VERSION}-linux-${stack_arch}/stack" /usr/local/bin/stack \
    && rm --recursive --force /tmp/stack.tar.gz "/tmp/stack-${STACK_VERSION}-linux-${stack_arch}" \
    && curl --fail --location --show-error --silent \
        "https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-${protoc_arch}.zip" \
        --output /tmp/protoc.zip \
    && unzip -q /tmp/protoc.zip -d /usr/local bin/protoc 'include/*' \
    && rm /tmp/protoc.zip

# Build flatc from the pinned source release. Upstream publishes no Linux ARM64
# binary, so build it on every architecture to keep the generated code identical.
RUN curl --fail --location --show-error --silent \
        "https://github.com/google/flatbuffers/archive/refs/tags/v${FLATBUFFERS_VERSION}.tar.gz" \
        --output /tmp/flatbuffers.tar.gz \
    && tar --extract --gzip --file /tmp/flatbuffers.tar.gz --directory /tmp \
    && cmake -S "/tmp/flatbuffers-${FLATBUFFERS_VERSION}" -B /tmp/flatbuffers-build \
        -DCMAKE_BUILD_TYPE=Release \
        -DFLATBUFFERS_BUILD_TESTS=OFF \
        -DFLATBUFFERS_BUILD_FLATLIB=OFF \
        -DFLATBUFFERS_BUILD_FLATHASH=OFF \
    && cmake --build /tmp/flatbuffers-build --target flatc --parallel "$(nproc)" \
    && install /tmp/flatbuffers-build/flatc /usr/local/bin/flatc \
    && rm --recursive --force /tmp/flatbuffers.tar.gz "/tmp/flatbuffers-${FLATBUFFERS_VERSION}" /tmp/flatbuffers-build

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
# The dynamic flag builds the shared Consensus library used by the node.
# It also links the database exporter to the shared Haskell libraries.
RUN stack build --flag concordium-consensus:dynamic

# Build release versions of the Rust executables.
# The node package also builds the bootstrapper.
# Without the static feature, its build script links both to the shared Haskell libraries.
RUN cargo build --locked --release --manifest-path concordium-node/Cargo.toml \
    && cargo build --locked --release --manifest-path collector/Cargo.toml

# Assemble the files that the later stages need.
# /runtime holds the node image executables and the shared libraries.
# /exporter holds the database exporter executable.
# Copy all project, snapshot, and GHC shared libraries, including Concordium's symlinked Rust libraries.
# Use Ubuntu's libffi at runtime instead of GHC's bundled copy.
# Remove debugging information from the staged ELF libraries.
# Check that every executable can resolve its libraries, then strip the executables.
RUN set -eux; \
    install -d /runtime/usr/local/bin /runtime/usr/local/lib /exporter; \
    local_install_root="$(stack path --local-install-root)"; \
    install concordium-node/target/release/concordium-node /runtime/usr/local/bin/concordium-node; \
    install concordium-node/target/release/p2p_bootstrapper-cli /runtime/usr/local/bin/p2p-bootstrapper; \
    install collector/target/release/node-collector /runtime/usr/local/bin/node-collector; \
    install "${local_install_root}/bin/database-exporter" /exporter/database-exporter; \
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
    executables=( \
        /runtime/usr/local/bin/concordium-node \
        /runtime/usr/local/bin/node-collector \
        /runtime/usr/local/bin/p2p-bootstrapper \
        /exporter/database-exporter \
    ); \
    for executable in "${executables[@]}"; do \
        LD_LIBRARY_PATH=/runtime/usr/local/lib ldd "${executable}" | tee /tmp/executable.ldd; \
        if grep --quiet 'not found' /tmp/executable.ldd; then \
            echo "Unresolved shared library in ${executable}" >&2; \
            exit 1; \
        fi; \
    done; \
    rm /tmp/executable.ldd; \
    strip "${executables[@]}"

# Hold only the compiled executables and shared libraries.
# CI builds this stage once and passes its output to the image builds as the artifacts build context.
# That context replaces this stage, so the image builds do not compile again.
FROM scratch AS artifacts

COPY --from=builder /runtime/ /runtime/
COPY --from=builder /exporter/ /exporter/

# Install the AWS CLI for the exporter image.
# Change the version and the archive checksum together.
FROM ubuntu:${UBUNTU_VERSION} AS aws-cli

ARG DEBIAN_FRONTEND=noninteractive
ARG AWS_CLI_VERSION=2.37.5
ARG AWS_CLI_SHA256=850ba65f1342a1f725f4868de3c4621ea729af31dac96e54b574cbb0ef309029

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN apt-get update \
    && apt-get install --yes --no-install-recommends ca-certificates curl unzip \
    && rm -rf /var/lib/apt/lists/* \
    && curl --fail --location --show-error --silent \
        "https://awscli.amazonaws.com/awscli-exe-linux-x86_64-${AWS_CLI_VERSION}.zip" \
        --output /tmp/awscli.zip \
    && echo "${AWS_CLI_SHA256}  /tmp/awscli.zip" | sha256sum --check --strict \
    && unzip -q /tmp/awscli.zip -d /tmp \
    && /tmp/aws/install --install-dir /usr/local/aws-cli --bin-dir /usr/local/bin \
    && rm --recursive --force /tmp/aws /tmp/awscli.zip

# Build the internal database exporter image with --target exporter.
# The image is amd64 only, like the reporter hosts.
FROM ubuntu:${UBUNTU_VERSION} AS exporter

ARG DEBIAN_FRONTEND=noninteractive

# Install the same native libraries as the runtime stage, plus jq.
# Change this package list and the runtime stage list together.
# Use the same unprivileged identity as the node image.
RUN apt-get update \
    && apt-get install --yes --no-install-recommends \
        ca-certificates \
        jq \
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
    && useradd --uid 10001 --gid concordium --no-create-home --shell /usr/sbin/nologin concordium

COPY --from=aws-cli /usr/local/aws-cli/ /usr/local/aws-cli/
COPY --from=artifacts /runtime/usr/local/lib/ /usr/local/lib/
COPY --from=artifacts /exporter/database-exporter /usr/local/bin/database-exporter
COPY scripts/db-exporter/database-exporter-publish.sh /usr/local/bin/database-exporter-publish.sh

# Use the reporter host layout, with the data disk mounted at /var/lib/concordium.
# The export directory is on that disk.
# Register the copied libraries and fail the build if a tool cannot resolve its libraries or start.
RUN chmod 0755 /usr/local/bin/database-exporter-publish.sh \
    && ln --symbolic /usr/local/aws-cli/v2/current/bin/aws /usr/local/bin/aws \
    && ln --symbolic /var/lib/concordium/data/database_exporter /var/lib/database_exporter \
    && ldconfig \
    && ldd /usr/local/bin/database-exporter | tee /tmp/database-exporter.ldd \
    && ! grep --quiet 'not found' /tmp/database-exporter.ldd \
    && /usr/local/bin/database-exporter --help \
    && aws --version \
    && jq --version \
    && rm /tmp/database-exporter.ldd

USER concordium:concordium

# Run the publish script once, then exit.
ENTRYPOINT ["/usr/bin/tini", "--", "/usr/local/bin/database-exporter-publish.sh"]

# Start the node image without compilers, source files, or build caches.
# Keep this stage last so that it stays the default target.
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

# Copy the compiled executables and shared Haskell libraries.
COPY --from=artifacts /runtime/ /

# Install the process manager script that starts the node and optional collector.
COPY scripts/distribution/docker-image/entrypoint.sh /usr/local/bin/concordium-entrypoint

# Include every tracked genesis file. Add future networks to this source directory.
COPY scripts/distribution/docker-image/genesis/ /genesis/

# Register the copied libraries and fail the build if an executable has an
# unresolved dependency or cannot start far enough to print its version.
RUN chmod 0755 /usr/local/bin/concordium-entrypoint \
    && ldconfig \
    && ldd /usr/local/bin/concordium-node | tee /tmp/concordium-node.ldd \
    && ! grep --quiet 'not found' /tmp/concordium-node.ldd \
    && ldd /usr/local/bin/node-collector | tee /tmp/node-collector.ldd \
    && ! grep --quiet 'not found' /tmp/node-collector.ldd \
    && ldd /usr/local/bin/p2p-bootstrapper | tee /tmp/p2p-bootstrapper.ldd \
    && ! grep --quiet 'not found' /tmp/p2p-bootstrapper.ldd \
    && /usr/local/bin/concordium-node --version \
    && /usr/local/bin/node-collector --version \
    && /usr/local/bin/p2p-bootstrapper --version \
    && rm /tmp/concordium-node.ldd /tmp/node-collector.ldd /tmp/p2p-bootstrapper.ldd

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
