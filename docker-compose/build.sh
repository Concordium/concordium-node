#!/usr/bin/env bash

set -euxo pipefail

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build.sh VERSION_TAG [consensus_profiling]"
  exit 1
fi

VERSION="$1"
CONSENSUS_PROFILING="$2"
GHC_VERSION=8.10.4

# Flag '--pull' ensures that images are re-pulled.
docker build \
	-f docker-compose/Dockerfile \
	--build-arg ghc_version="${GHC_VERSION}" \
	--build-arg "consensus_profiling=$CONSENSUS_PROFILING" \
	--label ghc_version="${GHC_VERSION}" \
	--label "consensus_profiling=$CONSENSUS_PROFILING" \
	-t "concordium/dev-node:$VERSION" \
	--pull \
	.
