#!/bin/bash
docker build -f scripts/bootstrapper.Dockerfile -t concordium/bootstrapper .

docker tag concordium/bootstrapper:latest 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:latest

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/bootstrapper:latest

echo "DONE BUILDING bootstrapper!"
