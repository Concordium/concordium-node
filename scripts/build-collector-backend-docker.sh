#!/usr/bin/env bash

set -e

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-collector-backend-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

sed -i "s/VERSION_TAG/$1/" scripts/collector-backend.Dockerfile
sed -i "s/BUILD_TYPE/$2/" scripts/collector-backend.Dockerfile

docker build -f scripts/collector-backend.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/collector-backend:$1 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/collector-backend:$1
