#!/bin/bash

docker build -f scripts/base.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base

echo "DONE BUILDING base!"
