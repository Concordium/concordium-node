#!/bin/bash

if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-bootstrapper-docker.sh VERSION-TAG"
  exit 1
fi

docker build -f scripts/bootstrapper.Dockerfile -t concordium/bootstrapper:$1 .

docker tag concordium/bootstrapper:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:$1

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:$1

echo "DONE BUILDING bootstrapper!"
