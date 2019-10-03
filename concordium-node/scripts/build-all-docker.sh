#!/usr/bin/env bash

set -e 

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

git submodule update --init deps/internal/consensus

CONSENSUS_VERSION=`git submodule | grep consensus | head -n1 | awk '{print $1}'`

CURRENT=$(pwd)
(cd deps/internal/consensus
 git submodule update --init acorn
 git submodule update --init crypto
 git submodule update --init globalstate-mockup

 cd globalstate-mockup
 git submodule update --init deps/concordium-global-state-sys
 cd deps/concordium-global-state-sys
 git submodule update --init deps/p2p-client
 cd deps/p2p-client
 git submodule update --init deps/internal/consensus
 cd deps/internal/consensus
 git submodule update --init crypto)

echo "Consensus commit ID $CONSENSUS_VERSION"

echo $CONSENSUS_VERSION > CONSENSUS_VERSION

scripts/build-universal-docker.sh $1

scripts/build-bootstrapper-docker.sh $1 $2
scripts/build-collector-docker.sh $1 $2
scripts/build-collector-backend-docker.sh $1 $2
scripts/build-client-docker.sh $1 $2
