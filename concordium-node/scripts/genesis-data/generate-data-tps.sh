#!/usr/bin/env bash

set -e

cwd=$(dirname $0)
. $cwd/config-tps.sh
final_state=0
CONSENSUS_VERSION_TAG=$( cd $cwd/../../deps/internal/consensus && git rev-parse --verify HEAD )

( 
    cd $cwd 
    wget -qc "https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-binaries-$CONSENSUS_VERSION_TAG.tar.gz"
    tar -xf "static-consensus-binaries-$CONSENSUS_VERSION_TAG.tar.gz"

    mkdir -p genesis_data
    
    for baker_size in "${baker_sizes[@]}" 
    do
        echo "Generating genesis data with $baker_size baker(s)" &&
        rm -f genesis_data/genesis.dat &&
	    if [ ! -f genesis_data/bakers.json ];
	    then
	        rm -rf genesis_data/*
            LD_LIBRARY_PATH=$(pwd)/binaries/lib binaries/bin/genesis make-bakers $baker_size genesis_data
            fi &&
        LD_LIBRARY_PATH=$(pwd)/binaries/lib binaries/bin/genesis make-genesis --identity-providers=identity-providers.json --crypto-params=global.json --bakers=genesis_data/bakers.json genesis-tps.json genesis_data/genesis.dat --beta-accounts=beta-accounts.json &&
        tar czf $baker_size-bakers.tar.gz genesis_data
    done
    
    rm -r static-consensus-binaries-$CONSENSUS_VERSION_TAG.tar.gz binaries
)
