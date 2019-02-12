#!/bin/bash

echo "Cloning repositories"

# Baker id Generator
git clone -b master --single-branch \
    https://gitlab.com/fmgr/baker_id_gen.git \
    assets/repos/baker_id_gen.git

# Consensus
git clone -b oak-integration --single-branch \
        git@gitlab.com:Concordium/consensus/prototype.git \
        assets/repos/consensus && \
    cd assets/repos/consensus && \
    git submodule update --init --recursive && \
    cd ../../../

# P2P Client
git clone \
        git@gitlab.com:Concordium/p2p-client.git \
        assets/repos/p2p-client

