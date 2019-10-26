#!/usr/bin/env bash

set -e

if [ "$#" -ne 3 ]
then
  echo "Usage: ./build-universal-docker.sh VERSION-TAG [default|no-rgs] [profiling:true|false]"
  exit 1
fi

export DOCKER_BUILDKIT=1

docker build -f scripts/universal.Dockerfile --build-arg consensus_type=$2 --build-arg consensus_profiling=$3 -t concordium/universal:$1 --ssh default .

docker tag concordium/universal:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/universal:$1

echo "DONE BUILDING universal!"
