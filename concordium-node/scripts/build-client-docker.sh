#!/usr/bin/env bash

set -e

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-client-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

sed -i "s/VERSION_TAG/$1/" scripts/client.Dockerfile
sed -i "s/BUILD_TYPE/$2/" scripts/client.Dockerfile

docker build -f scripts/client.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/client:$1 --build-arg CI_JOB_TOKEN=${2} .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/client:$1