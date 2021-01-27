#!/usr/bin/env bash

set -e

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build-docker-compose-image.sh VERSION_TAG [consensus_profiling]"
  exit 1
fi

VERSION=$1
CONSENSUS_PROFILING=$2

echo "Going to build version $VERSION of dev-client for docker-hub"

#if [ ! -z "$JENKINS_HOME" ]; then
    export DOCKER_BUILDKIT=1

    docker build -f docker-compose/dev-client.Dockerfile --build-arg consensus_profiling=$CONSENSUS_PROFILING -t concordium/dev-client:$VERSION --ssh default .
#else
#    echo "This script should only be run from Jenkins."
#fi
