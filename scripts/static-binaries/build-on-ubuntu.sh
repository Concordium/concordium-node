#!/usr/bin/env bash

# Build concordium-node and related binaries in a format suitable for distribution.
# The binaries are linked mostly statically, apart from a few common libraries that
# seem difficult to impossible to link statically. In particular libc.

set -euxo pipefail

# The script must be invoked with the concordium-node source directory as working directory.

# Optional parameters:
# - EXTRA_FEATURES, features to use in addition to `static` when building the node, e.g., collector. Comma separated list.

extra_features=${EXTRA_FEATURES:-""}

REQUIRED_PARAMETERS=("PROTOC_VERSION" "FLATBUFFERS_VERSION" "RUST_TOOLCHAIN_VERSION")

# Loop through the required variables and check if they are set
for VAR in "${REQUIRED_PARAMETERS[@]}"; do
  if [ -z "${!VAR}" ]; then
    echo "Error: $VAR is not set or empty."
    exit 1
  fi
done

# Install dependencies.

apt-get update && \
DEBIAN_FRONTEND=noninteractive apt-get -y install \
	curl \
	libprotobuf-dev \
	libssl-dev \
	pkg-config \
	libnuma-dev \
	libgmp-dev \
	liblmdb0 \
	locales \
	liblmdb-dev \
    unzip \
    build-essential

# Install protobuf

curl -L "https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-x86_64.zip" -o protoc.zip
unzip protoc.zip bin/protoc -d /usr/
rm protoc.zip

# Install Rust.

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"
rustup set profile minimal
rustup default "$RUST_TOOLCHAIN_VERSION"

# Install flatbuffers.

curl -L "https://github.com/google/flatbuffers/releases/download/v${FLATBUFFERS_VERSION}/Linux.flatc.binary.g++-10.zip" -O
unzip Linux.flatc.binary.g++-10.zip -d /usr/bin

# Build all the binaries and copy them to ./bin/
# This requires an up-to-date lockfile which should be committed to the repository.
cargo install --path "$(pwd)/concordium-node/" --locked --features=static,$extra_features --root "$(pwd)"
# Install the node-collector into /bin as well.
cargo install --path "$(pwd)/collector/" --locked --root "$(pwd)"
# Strip all the generated binaries to remove debugging and unused symbols
strip "$(pwd)"/bin/*
