#!/usr/bin/env bash

set -euxo pipefail

# The script must be invoked with the concordium-node source directory as working directory.

# Expected parameters:
# - VERSION

version="${VERSION}"

# Install dependencies.

apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -y install pkg-config

# Build Debian packages.

build_dir=/build
p2p_bootstrapper_build_dir="$build_dir/p2p-bootstrapper_$version"
p2p_bootstrapper_regenesis_block_hashes_file="/var/lib/concordium/data/genesis_hash"

# - 'p2p-bootstrapper'

(

mkdir -p "$p2p_bootstrapper_build_dir"
cd "$p2p_bootstrapper_build_dir"
mkdir -p etc/default
mkdir -p usr/lib/systemd/system
mkdir -p var/lib/concordium/{data,config}
mkdir -p usr/bin
mkdir -p DEBIAN

cat <<EOF > etc/default/p2p-bootstrapper
CONCORDIUM_NODE_LISTEN_PORT=8888
CONCORDIUM_NODE_CONFIG_DIR=/var/lib/concordium/config
CONCORDIUM_NODE_DATA_DIR=/var/lib/concordium/data
CONCORDIUM_NODE_PROMETHEUS_LISTEN_ADDRESS=0.0.0.0
CONCORDIUM_NODE_PROMETHEUS_LISTEN_PORT=9090
CONCORDIUM_NODE_LOG_LEVEL_DEBUG=true
CONCORDIUM_NODE_NO_LOG_TIMESTAMP=true
CONCORDIUM_NODE_BOOTSTRAPPER_REGENESIS_BLOCK_HASHES_FILE=$p2p_bootstrapper_regenesis_block_hashes_file
EOF

cat <<EOF > DEBIAN/control
Package: p2p-bootstrapper
Version: $version
Section: extra
Priority: optional
Architecture: amd64
Depends: debhelper ( >= 10 ), libgmp10 ( >= 6.1.2 )
Maintainer: Concordium Foundation <developers@concordium.com>
Description: Concordium P2P Bootstrapper
EOF

cat <<EOF > usr/lib/systemd/system/p2p-bootstrapper.service
[Unit]
Description=Concordium P2P Bootstrapper
After=syslog.target network.target

[Service]
Type=simple
ExecStart=/usr/bin/p2p-bootstrapper
EnvironmentFile=/etc/default/p2p-bootstrapper
LimitNOFILE=500000
Restart=always
RestartSec=20

[Install]
WantedBy=multi-user.target
EOF

cp /tmp/p2p_bootstrapper-cli usr/bin/p2p-bootstrapper

)

dpkg-deb --build "$p2p_bootstrapper_build_dir"
