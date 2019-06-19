#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG"
  exit 1
fi

git submodule update --init --recursive

CONSENSUS_VERSION=`git submodule | grep consensus | head -n1 | awk '{print $1}'`

echo "Consensus commit ID $CONSENSUS_VERSION"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

scripts/build-universal-docker.sh $1

scripts/build-bootstrapper-docker.sh $1
scripts/build-client-docker.sh $1