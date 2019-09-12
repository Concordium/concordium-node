#!/usr/bin/env bash

set -e

if [ ! -z "$JENKINS_HOME" ]; then
    git clone -b master --single-branch git@gitlab.com:Concordium/tools/baker_id_gen.git baker_id_gen

    CONSENSUS_VERSION=`git submodule | grep consensus | head -n1 | awk '{print $1}'`

    echo $CONSENSUS_VERSION > CONSENSUS_VERSION

    export DOCKER_BUILDKIT=1

    docker build -f scripts/dev-client.Dockerfile -t concordium/dev-client:latest --ssh default .

    rm -f CONSENSUS_VERSION
    rm -rf baker_id_gen
else
    echo "This script should only be run from Jenkins."
fi