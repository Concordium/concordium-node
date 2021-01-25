#!/usr/bin/env bash

set -e

if [ "$#" -lt 2 ]
then
  echo "Usage: ./build-universal-docker.sh VERSION-TAG [profiling:true|false]"
  exit 1
fi

export DOCKER_BUILDKIT=1
docker build -f scripts/testnet-deployments/universal.Dockerfile --build-arg consensus_profiling=$2 -t concordium/universal:$1 --ssh default .
docker tag concordium/universal:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/universal:$1
echo "DONE BUILDING universal!"
