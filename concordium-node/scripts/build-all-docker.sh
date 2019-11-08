#!/usr/bin/env bash

set -e 

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release] [default|no-rgs] [profiling:[true|false]]"
  exit 1
fi

CONSENSUS_VERSION=$( cd deps/internal/consensus && git rev-parse --verify HEAD )

echo "Consensus commit ID $CONSENSUS_VERSION with type $3"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

VERSION_TAG="$1"
BUILD_TYPE="$2"
CONSENSUS_TYPE="$3"
CONSENSUS_PROFILING="$4"

echo "Building docker images for $VERSION_TAG as $BUILD_TYPE with $CONSENSUS_TYPE/$CONSENSUS_PROFILING"

if [[ ! -z "$CONSENSUS_TYPE" && "$CONSENSUS_TYPE" != "default" ]]; then
  VERSION_TAG="$VERSION_TAG-$CONSENSUS_TYPE"
fi

scripts/build-universal-docker.sh $VERSION_TAG $CONSENSUS_TYPE $CONSENSUS_PROFILING
scripts/build-bootstrapper-docker.sh $VERSION_TAG $BUILD_TYPE
scripts/build-collector-docker.sh $VERSION_TAG $BUILD_TYPE
scripts/build-collector-backend-docker.sh $VERSION_TAG $BUILD_TYPE
scripts/build-client-docker.sh $VERSION_TAG $BUILD_TYPE
