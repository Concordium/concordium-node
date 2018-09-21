#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-build-docker.sh VERSION-TAG"
  exit 1
fi

docker build -f scripts/build.Dockerfile -t concordium/build:latest .

docker tag concordium/build:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:latest

echo "DONE BUILDING build!"
