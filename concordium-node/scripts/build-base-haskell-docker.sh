#!/usr/bin/env bash

set -e

docker build -f scripts/base-haskell.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.1 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-haskell:0.1

echo "DONE BUILDING base-haskell!"