#!/usr/bin/env bash

set -e

docker build -f scripts/base-rust-fmt.Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-rust-fmt:2019-11-13 .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base-rust-fmt:2019-11-13

echo "DONE BUILDING base!"
