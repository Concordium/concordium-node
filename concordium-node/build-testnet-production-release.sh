#!/usr/bin/env bash
set -e

if [ "$#" -lt 1 ]
then
  echo "Usage: ./build-testnet-production-release.sh [debug|release] [default|no-rgs] [profiling=[true|false]]"
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
  CURRENT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
  git checkout master
  git pull
fi

PATH="$PATH:/usr/local/bin" git lfs install
PATH="$PATH:/usr/local/bin" git lfs pull

VERSION=`cat Cargo.toml | grep "version = \"" | head -n1 | sed 's/version = \"//' | sed 's/\"//'`

./scripts/build-all-docker.sh $VERSION $BUILD_TYPE $CONSENSUS_TYPE $CONSENSUS_PROFILING

if [ -z "$JENKINS_HOME" ]; then
  git checkout $CURRENT_BRANCH
fi

echo "Finished building production release with tag $VERSION with consensus $CONSENSUS_TYPE, and profiling $CONSENSUS_PROFILING"
