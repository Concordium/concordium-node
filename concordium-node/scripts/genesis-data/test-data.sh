#!/usr/bin/env bash

declare -a baker_sizes=("20" "30" "50" "75")
cwd=$(dirname $0)
final_state=0

for baker_size in "${baker_sizes[@]}" 
do
    echo "Testing genesis data with $baker_size baker(s)" &&
    mkdir -p $cwd/$baker_size &&
    tar xzf $cwd/$baker_size-bakers.tar.gz -C $cwd/$baker_size &&
    cargo run --features=static --bin=read_block_dump \
        $cwd/$baker_size/genesis_data/genesis.dat >/dev/null 
    if [[ $? != 0 ]]; then
        echo "- failed"
        final_state=-1
    fi
    rm -r $cwd/$baker_size
done

if [[ $final_state != 0 ]]; then
    exit $final_state
fi
