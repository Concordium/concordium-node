#!/usr/bin/env bash

set -e

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-collector-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

sed -i "s/VERSION_TAG/$1/" scripts/collector.Dockerfile
sed -i "s/BUILD_TYPE/$2/" scripts/collector.Dockerfile

docker build -f scripts/collector.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/collector:$1 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/collector:$1
