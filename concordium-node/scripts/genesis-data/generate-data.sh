#!/usr/bin/env bash

cwd=$(dirname $0)
. $cwd/config.sh
final_state=0

wget https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-binaries-9339bff59f29e6475fdab08c717ced3b7cf59ca6.tar.gz
tar -xf static-consensus-binaries-9339bff59f29e6475fdab08c717ced3b7cf59ca6.tar.gz

for baker_size in "${baker_sizes[@]}" 
do
    echo "Generating genesis data with $baker_size baker(s)" &&
    mkdir genesis_data &&
    LD_LIBRARY_PATH=$(pwd)/binaries/lib binaries/bin/genesis make-bakers $baker_size genesis_data &&
    LD_LIBRARY_PATH=$(pwd)/binaries/lib binaries/bin/genesis make-genesis --identity-providers=identity-providers.json --crypto-params=global.json --bakers=genesis_data/bakers.json genesis.json genesis_data/genesis.dat &&
    tar czf $baker_size-bakers.tar.gz genesis_data &&
    rm -r genesis_data
done

rm -r static-consensus-binaries-9339bff59f29e6475fdab08c717ced3b7cf59ca6.tar.gz binaries
