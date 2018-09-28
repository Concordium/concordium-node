#!/bin/bash

if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-testrunner-docker.sh VERSION-TAG"
  exit 1
fi

docker build -f scripts/basic.Dockerfile -t concordium/testrunner:$1 .

docker tag concordium/testrunner:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/testrunner:$1

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/testrunner:$1

echo "DONE BUILDING testrunner!"
