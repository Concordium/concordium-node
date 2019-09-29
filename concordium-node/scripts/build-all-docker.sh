#!/usr/bin/env bash

set -e 

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

git submodule update --init --recursive

CONSENSUS_VERSION=`git submodule | grep consensus | head -n1 | awk '{print $1}'`

echo "Consensus commit ID $CONSENSUS_VERSION"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

scripts/build-universal-docker.sh $1

scripts/build-bootstrapper-docker.sh $1 $2
scripts/build-collector-docker.sh $1 $2
scripts/build-collector-backend-docker.sh $1 $2
scripts/build-client-docker.sh $1 $2