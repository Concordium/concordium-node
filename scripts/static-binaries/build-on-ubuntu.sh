#!/usr/bin/env bash

# Build concordium-node and related binaries in a format suitable for distribution.
# The binaries are linked mostly statically, apart from a few common libraries that
# seem difficult to impossible to link statically. In particular libc.

set -euxo pipefail

# The script must be invoked with the concordium-node source directory as working directory.

# Optional parameters:
# - EXTRA_FEATURES, features to use in addition to `static` when building the node, e.g., collector. Comma separated list.

extra_features=${EXTRA_FEATURES:-""}

protoc_version=3.15.3
flatbuffers_version=v22.10.26
rust_toolchain_version=1.62

# Install dependencies.

apt-get update && \
DEBIAN_FRONTEND=noninteractive apt-get -y install \
	git \
	curl \
	libprotobuf-dev \
	libssl-dev \
	cmake \
	pkg-config \
	libnuma-dev \
	libgmp-dev \
	liblmdb0 \
	locales \
	liblmdb-dev \
    unzip \
    build-essential

# Install protobuf

curl -L https://github.com/protocolbuffers/protobuf/releases/download/v${protoc_version}/protoc-${protoc_version}-linux-x86_64.zip -o protoc.zip
unzip protoc.zip bin/protoc -d /usr/
rm protoc.zip

# Install Rust.

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"
rustup set profile minimal
rustup default "$rust_toolchain_version"

# Install flatbuffers.

git clone https://github.com/google/flatbuffers.git
( cd flatbuffers && git checkout "$flatbuffers_version" && cmake -G "Unix Makefiles" && make -j"$(nproc)" && make install )

# Build all the binaries and copy them to ./bin/
# This requires an up-to-date lockfile which should be committed to the repository.
cargo install --path "$(pwd)/concordium-node/" --locked --features=static,$extra_features --root "$(pwd)"
# Install the node-collector into /bin as well.
cargo install --path "$(pwd)/collector/" --locked --root "$(pwd)"
# Strip all the generated binaries to remove debugging and unused symbols
strip "$(pwd)"/bin/*
