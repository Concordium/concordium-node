#!/bin/bash

echo "Cloning or updating repositories"

# Baker id Generator
if [ -d  assets/repos/baker_id_gen.git ]; then
    cd assets/repos/baker_id_gen.git
    git pull
    cd ../../..
else
    git clone -b master --single-branch \
        https://gitlab.com/fmgr/baker_id_gen.git \
        assets/repos/baker_id_gen.git
fi

# Consensus
if [ -d assets/repos/consensus ]; then
    cd assets/repos/consensus
    git pull
    cd ../../..
else
    git clone -b oak-integration --single-branch \
            git@gitlab.com:Concordium/consensus/prototype.git \
            assets/repos/consensus && \
        cd assets/repos/consensus && \
        git submodule update --init --recursive && \
        cd ../../..
fi

# P2P Client
if [ -d assets/repos/p2p-client ]; then
    cd assets/repos/p2p-client
    git pull
    cd ../../..
else
    git clone \
            git@gitlab.com:Concordium/p2p-client.git \
            assets/repos/p2p-client
fi
