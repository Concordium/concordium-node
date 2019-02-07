#!/bin/bash
# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts
RUST_BACKTRACE=1 ./target/debug/p2p_client-cli --desired-nodes $DESIRED_PEERS --external-port $EXTERNAL_PORT --private-node --bootstrap-node $BOOTSTRAP_NODE --baker-id $BAKER_ID --num-bakers $NUM_BAKERS $EXTRA_ARGS
