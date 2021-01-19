#!/usr/bin/env bash
set -e

if [ "$#" -lt 1 ]
then
  echo "Usage: ./build-testnet-develop-release.sh [debug|release] [default] [profiling=[true|false]]"
  exit 1
fi

BUILD_TYPE=$1

CONSENSUS_TYPE=""
if [ ! -z "$2" ]; then
  CONSENSUS_TYPE="$2"
else
  CONSENSUS_TYPE="default"
fi

CONSENSUS_PROFILING="false"
if [[ ! -z "$3" && "$3" == "true" ]]; then
  CONSENSUS_PROFILING="true"
fi

if [ -z "$JENKINS_HOME" ]; then
  git pull
fi

VERSION=`git rev-parse --verify HEAD`
GENESIS_VERSION=$(cat scripts/GENESIS_DATA_VERSION)

./scripts/build-all-docker.sh $VERSION $BUILD_TYPE $CONSENSUS_TYPE $CONSENSUS_PROFILING

echo "Finished building and pushing develop release with tag $VERSION with consensus $CONSENSUS_TYPE with profiling $CONSENSUS_PROFILING and genesis $GENESIS_VERSION"
