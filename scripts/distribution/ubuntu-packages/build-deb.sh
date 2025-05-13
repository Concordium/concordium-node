#!/bin/bash

set -euxo pipefail

# Used environment variables.
# - UBUNTU_VERSION (mandatory)
# - STATIC_LIBRARIES_IMAGE_TAG (defaults to 'latest' if not given)
# - STATIC_BINARIES_IMAGE_TAG (defaults to 'latest' if not given)
# - GHC_VERSION (defaults to '9.6.6' if not given)

# Set STATIC_BINARIES_IMAGE_TAG to latest if not already set
export STATIC_BINARIES_IMAGE_TAG="${STATIC_BINARIES_IMAGE_TAG:-latest}"

# If the static-node-binaries image exists we use the binaries therein.
# Otherwise we build it first.
if ! docker inspect --type=image static-node-binaries:$STATIC_BINARIES_IMAGE_TAG > /dev/null 2> /dev/null ; then
    # build static binaries
    export STATIC_LIBRARIES_IMAGE_TAG="${STATIC_LIBRARIES_IMAGE_TAG:-latest}"
    export GHC_VERSION="${GHC_VERSION:-9.6.6}"
    (cd ../../../; ./scripts/static-binaries/build-static-binaries.sh)
fi

docker build\
       --build-arg ubuntu_version=$UBUNTU_VERSION\
       --build-arg static_binaries_image_tag=$STATIC_BINARIES_IMAGE_TAG\
       --build-arg build_env_matrix= '[{"build_env_name"="Mainnet", "build_catchup_url"="https://catchup.mainnet.concordium.software/blocks.idx", "build_genesis_hash"="9dd9ca4d19e9393877d2c44b70f89acbfc0883c2243e5eeaecc0d1cd0503f478", "build_collector_backend_url"="https://dashboard.mainnet.concordium.software/nodes/post", "build_grpc2_listen_port"="20000", "build_listen_port"="8888", "build_bootstrap"="bootstrap.mainnet.concordium.software:8888"},{"build_env_name"="Testnet", "build_catchup_url"="https://catchup.testnet.concordium.software/blocks.idx", "build_genesis_hash"="4221332d34e1694168c2a0c0b3fd0f273809612cb13d000d5c2e00e85f50f796", "build_collector_backend_url"="https://dashboard.testnet.concordium.software/nodes/post", "build_grpc2_listen_port"="20001", "build_listen_port"="8889", "build_bootstrap"="bootstrap.testnet.concordium.software:8888"}]'\
       -f deb.Dockerfile -t deb . --no-cache

# Copy out the build artifacts. We create a temporary container and use docker
# cp. This makes the output artifacts have correct file permissions (they are
# owned by the user who ran the script).
id=$(docker create deb)
docker cp $id:/out build
docker rm $id
