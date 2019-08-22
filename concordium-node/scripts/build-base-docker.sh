#!/usr/bin/env bash

docker build -f scripts/base.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.1 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.1

echo "DONE BUILDING base!"
