#!/usr/bin/env bash

set -e

if [ "$#" -ne 2 ]
then
  echo "Usage: ./build-bootstrapper-docker.sh VERSION-TAG [debug|release]"
  exit 1
fi

sed -i "s/VERSION_TAG/$1/" scripts/bootstrapper.Dockerfile
sed -i "s/BUILD_TYPE/$2/" scripts/bootstrapper.Dockerfile

docker build -f scripts/bootstrapper.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:$1 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:$1