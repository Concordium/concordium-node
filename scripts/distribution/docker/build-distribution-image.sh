#!/usr/bin/env bash

# NB: This script is intended to be run from the root of the repository.

# The script builds a distribution image that contains
# - the node binary at /concordium-node
# - the node collector binary at /node-collector
# - environment specific genesis data at /${environment}-genesis.dat
#
# No environment variables are set for the node or the collector. Those are
# intended to be set by a docker-compose or similar configuration for the
# specific container runtime environment.

# This builder script expects the following environment variables to be set
# - static_libraries_image_tag (image with dependencies for building Haskell libraries that are linked statically)
# - static_binaries_image_tag (image tag for the static libraries docker build, defaults to latest)
# - ghc_version (version of the GHC compiler used to build static binaries)
# - genesis_path (root of the genesis directory from which the genesis block is taken)
# - genesis_ref (branch or commit of the genesis_data repository where the genesis data is located)
# - environment (testnet/mainnet/stagenet, affects the naming of the genesis block)
#
# The build of the image will clone the genesis data repository.

set -euxo pipefail

# Build the image and tag it as static-node-binaries
GHC_VERSION="${ghc_version}" UBUNTU_VERSION=20.04 STATIC_LIBRARIES_IMAGE_TAG="${static_libraries_image_tag}" STATIC_BINARIES_IMAGE_TAG="${static_binaries_image_tag:-latest}" EXTRA_FEATURES="collector" ./scripts/static-binaries/build-static-binaries.sh

# Then pack it all together with genesis
DOCKER_BUILDKIT=1 docker build\
                  --build-arg environment="${environment}"\
                  --build-arg genesis_ref="${genesis_ref}"\
                  --build-arg genesis_path="${genesis_path}"\
                  --build-arg static_binaries_image_tag="${static_binaries_image_tag:-latest}"\
                  -t "concordium/${image_name}:${image_tag}"\
                  --ssh default\
                  -f scripts/distribution/docker/builder.Dockerfile\
                  --no-cache\
                  "."
