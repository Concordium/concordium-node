#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-build-docker.sh VERSION-TAG"
  exit 1
fi

docker build -f scripts/build.Dockerfile -t concordium/build:$1 .

docker tag concordium/build:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:$1

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:$1

echo "DONE BUILDING build!"
