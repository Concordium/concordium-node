#!/usr/bin/env bash

echo "Cloning or updating repositories"

# Baker id Generator
if [ -d  assets/repos/baker_id_gen.git ]; then
    cd assets/repos/baker_id_gen.git
    git pull
    cd ../../..
else
    git clone -b master --single-branch \
        git@gitlab.com:Concordium/tools/baker_id_gen.git \
        assets/repos/baker_id_gen.git
fi

# P2P Client
rsync -ac --exclude=target/ --delete \
    --exclude=scripts/local/assets \
    ../.. assets/repos/p2p-client

