#!/bin/bash
docker build -f scripts/build.Dockerfile -t concordium/build .

docker tag concordium/build:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/build:latest

echo "DONE BUILDING build!"
