#!/bin/bash
docker build -f scripts/basic.Dockerfile -t concordium/basic .

docker tag concordium/basic:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/basic:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/basic:latest

echo "DONE BUILDING basic!"
