#!/usr/bin/env bash

cwd=$(dirname $0)
. $cwd/config.sh
final_state=0

for baker_size in "${baker_sizes[@]}" 
do
    echo "Generating genesis data with $baker_size baker(s)" &&
    cargo run --features=static --bin=make-bakers-data -- \
        --num-bakers=$baker_size --output-dir=$cwd/genesis_data &&
    (
        cd $cwd &&
        tar czf $baker_size-bakers.tar.gz genesis_data &&
        rm -r genesis_data
    )
    if [[ $? != 0 ]]; then
        echo "- failed"
        final_state=-1
    fi
done

if [[ $final_state != 0 ]]; then
    exit $final_state
fi

