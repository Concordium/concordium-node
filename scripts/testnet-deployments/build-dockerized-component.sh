#!/usr/bin/env bash

set -euxo pipefail

if [ "$#" -ne 3 ]
then
  echo "Usage: ./build-docker-component.sh COMPONENT VERSION-TAG [debug|release]"
  exit 1
fi

docker build --build-arg universal_version=$2 --build-arg build_type=$3 -f scripts/testnet-deployments/$1.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/$1:$2 .
docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/$1:$2
echo "DONE BUILDING $1!"
