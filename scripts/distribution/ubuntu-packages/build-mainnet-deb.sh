#!/bin/bash

set -euxo pipefail

# Used environment variables.
# - UBUNTU_VERSION (mandatory)
# - STATIC_LIBRARIES_IMAGE_TAG (defaults to 'latest' if not given)
# - STATIC_BINARIES_IMAGE_TAG (defaults to 'latest' if not given)
# - GHC_VERSION (defaults to '9.2.5' if not given)

# Set STATIC_BINARIES_IMAGE_TAG to latest if not already set
export STATIC_BINARIES_IMAGE_TAG="${STATIC_BINARIES_IMAGE_TAG:-latest}"

if ! docker inspect --type=image static-node-binaries:$STATIC_BINARIES_IMAGE_TAG > /dev/null 2> /dev/null ; then
    # build static binaries
    export STATIC_LIBRARIES_IMAGE_TAG="${STATIC_LIBRARIES_IMAGE_TAG:-latest}"
    export GHC_VERSION="${GHC_VERSION:-9.2.5}"
    export EXTRA_FEATURES="collector"
    (cd ../../../; ./scripts/static-binaries/build-static-binaries.sh)
fi

docker build\
       --build-arg ubuntu_version=$UBUNTU_VERSION\
       --build-arg static_binaries_image_tag=$STATIC_BINARIES_IMAGE_TAG\
       --build-arg build_env_name=Mainnet\
       --build-arg build_env_name_lower=mainnet\
       --build-arg build_genesis_hash=9dd9ca4d19e9393877d2c44b70f89acbfc0883c2243e5eeaecc0d1cd0503f478\
       --build-arg build_collector_backend_url=https://dashboard.mainnet.concordium.software/nodes/post\
       --build-arg build_rpc_server_port=10000\
       --build-arg build_grpc2_listen_port=20000\
       --build-arg build_listen_port=8888\
       --build-arg build_bootstrap=bootstrap.mainnet.concordium.software:8888\
       -f deb.Dockerfile -t mainnet-deb . --no-cache

# Copy out the build artifacts. We create a temporary container and use docker
# cp. This makes the output artifacts have correct file permissions (they are
# owned by the user who ran the script).
id=$(docker create mainnet-deb)
docker cp $id:/out mainnet-build
docker rm $id
