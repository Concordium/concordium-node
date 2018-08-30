#!/bin/bash
docker build -f scripts/ipdiscovery.Dockerfile -t concordium/ipdiscovery .

docker tag concordium/ipdiscovery:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/ipdiscovery:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/ipdiscovery:latest

echo "DONE BUILDING ipdiscovery!"
