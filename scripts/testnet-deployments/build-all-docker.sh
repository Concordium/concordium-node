#!/usr/bin/env bash

set -exo pipefail

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release] [profiling:[true|false]]"
  exit 1
fi

VERSION_TAG="$1"
BUILD_TYPE="$2"
CONSENSUS_PROFILING="$3"

echo "Building docker images for $VERSION_TAG as $BUILD_TYPE with $CONSENSUS_PROFILING"
scripts/testnet-deployments/build-universal-docker.sh $VERSION_TAG $CONSENSUS_PROFILING
scripts/testnet-deployments/build-dockerized-component.sh bootstrapper $VERSION_TAG $BUILD_TYPE
scripts/testnet-deployments/build-dockerized-component.sh client $VERSION_TAG $BUILD_TYPE
scripts/testnet-deployments/build-dockerized-component.sh collector $VERSION_TAG $BUILD_TYPE
scripts/testnet-deployments/build-dockerized-component.sh collector-backend $VERSION_TAG $BUILD_TYPE
