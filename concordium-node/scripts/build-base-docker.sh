#!/bin/bash
docker build -f scripts/base.Dockerfile -t concordium/base .

docker tag concordium/base:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:latest

echo "DONE BUILDING base!"
