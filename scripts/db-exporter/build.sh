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
database_exporter_build_dir="$build_dir/database-exporter_$version"

# - 'database-exporter'

(

mkdir -p "$database_exporter_build_dir"
cd "$database_exporter_build_dir"
mkdir -p etc/default
mkdir -p usr/lib/systemd/system
mkdir -p usr/bin
mkdir -p DEBIAN

cat <<EOF > etc/default/database-exporter
BUCKET_NAME=
CF_DISTRIBUTION_ID=
CHUNK_SIZE=
EOF

cat <<EOF > DEBIAN/control
Package: database-exporter
Version: $version
Section: extra
Priority: optional
Architecture: amd64
Depends: debconf ( >= 1.5.73 ), debhelper ( >= 10 ), libgmp10 ( >= 6.1.2 ), libpq-dev ( >= 9.5.19)
Maintainer: Concordium Foundation <developers@concordium.com>
Description: Concordium Database Exporter
EOF

cat <<EOF > DEBIAN/templates
Template: database-exporter/s3-bucket-name
Type: string
Description: S3 bucket name
  Environment values:
  - Stagenet:    catchup-staging.concordium.com
  - Opentestnet: catchup-testnet.concordium.com

Template: database-exporter/cf-distribution-id
Type: string
Description: CloudFront distribution ID
  Environment values:
  - Stagenet:    E1QY95W0M4LA03
  - Opentestnet: E2XAZS8QKA0DXS

Template: database-exporter/chunk-size
Type: string
Description: Number of blocks per file
  Environment values:
  - Stagenet:    100000
  - Opentestnet: 100000

EOF

cat <<'EOF' > DEBIAN/config
#!/bin/sh
set -e
. /usr/share/debconf/confmodule
set -u
db_input high database-exporter/s3-bucket-name || true
db_input high database-exporter/cf-distribution-id || true
db_input high database-exporter/chunk-size || true
db_go # show interface
EOF

cat <<'EOF' > DEBIAN/postinst
#!/bin/sh
set -e
. /usr/share/debconf/confmodule
set -u
db_get database-exporter/s3-bucket-name
BUCKET_NAME="$RET"
db_get database-exporter/cf-distribution-id
CF_DISTRIBUTION_ID="$RET"
db_get database-exporter/chunk-size
CHUNK_SIZE="$RET"

echo "Writing S3 bucket name to '/etc/default/database-exporter'."
sed -i "/BUCKET_NAME/ c\\BUCKET_NAME=$BUCKET_NAME" /etc/default/database-exporter
echo "Writing CF distribution ID to '/etc/default/database-exporter'."
sed -i "/CF_DISTRIBUTION_ID/ c\\CF_DISTRIBUTION_ID=$CF_DISTRIBUTION_ID" /etc/default/database-exporter
echo "Writing chunk size to '/etc/default/database-exporter'."
sed -i "/CHUNK_SIZE/ c\\CHUNK_SIZE=$CHUNK_SIZE" /etc/default/database-exporter
EOF

chmod +x DEBIAN/config
chmod +x DEBIAN/postinst

cat <<EOF > usr/lib/systemd/system/database-exporter.service
[Unit]
Description=Concordium Database Exporter
After=syslog.target network.target

[Service]
Type=simple
ExecStart=/usr/bin/database-exporter-publish.sh
EnvironmentFile=/etc/default/database-exporter
LimitNOFILE=500000

[Install]
WantedBy=multi-user.target
EOF

cat <<EOF > usr/lib/systemd/system/database-exporter.timer
[Unit]
Description=Run service after boot and daily

[Timer]
OnBootSec=1m
OnCalendar=daily

[Install]
WantedBy=timers.target
EOF

cp /database-exporter usr/bin/database-exporter
cp /lib/*.so usr/lib/
# cp "$OLDPWD"/concordium-base/smart-contracts/wasm-chain-integration/target/release/*.so usr/lib
cp /database-exporter-publish.sh usr/bin/database-exporter-publish.sh

)

dpkg-deb --build "$database_exporter_build_dir"
