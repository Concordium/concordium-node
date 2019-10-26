#!/usr/bin/env bash

set -e 

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release] [default|no-rgs]"
  exit 1
fi

CONSENSUS_VERSION=$(cat scripts/CONSENSUS_VERSION)

echo "Consensus commit ID $CONSENSUS_VERSION with type $3"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

scripts/build-universal-docker.sh $1 $3

scripts/build-bootstrapper-docker.sh $1 $2
scripts/build-collector-docker.sh $1 $2
scripts/build-collector-backend-docker.sh $1 $2
scripts/build-client-docker.sh $1 $2
