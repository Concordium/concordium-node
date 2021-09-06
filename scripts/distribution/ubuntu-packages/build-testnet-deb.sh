#!/bin/bash

set -euxo pipefail

# Used environment variables.
# - UBUNTU_VERSION (mandatory)
# - STATIC_LIBRARIES_IMAGE_TAG (defaults to 'latest' if not given)
# - GHC_VERSION (defaults to '8.10.4' if not given)

# If the static-node-binaries image exists we use the binaries therein.
# Otherwise we build it first.
if ! docker inspect --type=image static-node-binaries > /dev/null 2> /dev/null ; then
    # build static binaries
    export STATIC_LIBRARIES_IMAGE_TAG="${STATIC_LIBRARIES_IMAGE_TAG:-latest}"
    export GHC_VERSION="${GHC_VERSION:-8.10.4}"
    export EXTRA_FEATURES="collector"
    (cd ../../../; ./scripts/static-binaries/build-static-binaries.sh)
fi

docker build\
       --build-arg ubuntu_version=$UBUNTU_VERSION\
       --build-arg build_env_name=Testnet\
       --build-arg build_env_name_lower=testnet\
       --build-arg build_genesis_hash=b6078154d6717e909ce0da4a45a25151b592824f31624b755900a74429e3073d\
       --build-arg build_collector_backend_url=https://dashboard.testnet.concordium.com/nodes/post\
       --build-arg build_rpc_server_port=10001\
       --build-arg build_listen_port=8889\
       --build-arg build_bootstrap=bootstrap.testnet.concordium.com:8888\
       -f deb.Dockerfile -t testnet-deb . --no-cache

# Copy out the build artifacts. We create a temporary container and use docker
# cp. This makes the output artifacts have correct file permissions (they are
# owned by the user who ran the script).
id=$(docker create testnet-deb)
docker cp $id:/out testnet-build
docker rm $id


