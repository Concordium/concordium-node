#!/usr/bin/env bash

set -e 

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release] [default|no-rgs]"
  exit 1
fi

CONSENSUS_VERSION=$(cat scripts/CONSENSUS_VERSION)

echo "Consensus commit ID $CONSENSUS_VERSION with type $3"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

VERSION_TAG="$1"
CONSENSUS_TYPE="$3"
CONSENSUS_PROFILING="$4"

if [[ ! -z "$CONSENSUS_TYPE" && "$CONSENSUS_TYPE" != "default" ]]; then
  VERSION_TAG="$VERSION_TAG-$CONSENSUS_TYPE"
fi

echo "Building docker images for $VERSION_TAG with $CONSENSUS_TYPE/$CONSENSUS_PROFILING"

scripts/build-universal-docker.sh $VERSION_TAG $CONSENSUS_TYPE $CONSENSUS_PROFILING
scripts/build-bootstrapper-docker.sh $VERSION_TAG $2
scripts/build-collector-docker.sh $VERSION_TAG $2
scripts/build-collector-backend-docker.sh $VERSION_TAG $2
scripts/build-client-docker.sh $VERSION_TAG $2
