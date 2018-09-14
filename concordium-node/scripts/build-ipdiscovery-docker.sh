#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-ipdiscovery-docker.sh VERSION-TAG"
  exit 1
fi

docker build -f scripts/ipdiscovery.Dockerfile -t concordium/ipdiscovery:$1 .

docker tag concordium/ipdiscovery:$1 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/ipdiscovery:$1

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/ipdiscovery:$1

echo "DONE BUILDING ipdiscovery!"
