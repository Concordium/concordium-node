#!/bin/bash

# Foreign temporary dependency, potentially a smarter way?
rm -rf consensus
git clone git@gitlab.com:Concordium/consensus/prototype.git consensus
# Locked to oak-integration for PoC due to build issues on Windows with master currently.
( cd consensus && git checkout oak-integration && ./setup-env.sh )


docker build -f scripts/base.Dockerfile -t concordium/base:latest .
docker build -f scripts/build.Dockerfile -t concordium/build:latest .
docker build -f scripts/local.testbootstrapper.Dockerfile -t concordium/test/bootstrapper:latest .
docker build -f scripts/local.testnode.Dockerfile -t concordium/test/node:latest .
docker build -f scripts/local.testrunner.Dockerfile -t concordium/test/testrunner:latest .
