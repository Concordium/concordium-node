#!/usr/bin/env bash

# Build concordium-node and related binaries in a format suitable for distribution.
# The binaries are linked mostly statically, apart from a few common libraries that
# seem difficult to impossible to link statically. These are libc, libpq.

set -euxo pipefail

# The script must be invoked with the concordium-node source directory as working directory.

# Optional parameters:
# - EXTRA_FEATURES, features to use in addition to `static` when building the node, e.g., collector. Comma separated list.

extra_features=${EXTRA_FEATURES:-""}

flatbuffers_version=v2.0.0
rust_toolchain_version=1.53

# Install dependencies.

apt-get update && \
DEBIAN_FRONTEND=noninteractive apt-get -y install \
	git \
	curl \
	libprotobuf-dev \
	libssl-dev \
	protobuf-compiler \
	cmake \
	pkg-config \
	libnuma-dev \
	libgmp-dev \
	liblmdb0 \
	postgresql-server-dev-all \
	locales \
	liblmdb-dev \
    build-essential

# Install Rust.

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
source "$HOME/.cargo/env"
rustup set profile minimal
rustup default "$rust_toolchain_version"

# Install flatbuffers.

# Must be kept in sync with 'base-images/base.Dockerfile'.
# See 'https://gitlab.com/Concordium/devops/-/commit/f41ac413c3583ec53d06a2c0fe5c8795e35f1a46'.
git clone https://github.com/google/flatbuffers.git
( cd flatbuffers && git checkout "$flatbuffers_version" && cmake -G "Unix Makefiles" && make -j"$(nproc)" && make install )

# Build all the binaries and copy them to ./bin/
# This requires an up-to-date lockfile which should be committed to the repository.
cargo install --path "$(pwd)/concordium-node/" --locked --features=static,$extra_features --root "$(pwd)"
# Strip all the generated binaries to remove debugging and unused symbols
strip "$(pwd)/bin/*"
