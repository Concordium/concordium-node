#!/usr/bin/env bash

set -euxo pipefail

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build.sh VERSION_TAG [consensus_profiling]"
  exit 1
fi

VERSION="$1"
CONSENSUS_PROFILING="$2"

DOCKER_BUILDKIT=1 docker build -f docker-compose/Dockerfile --build-arg "consensus_profiling=$CONSENSUS_PROFILING" -t "concordium/dev-node:$VERSION" --ssh default .
