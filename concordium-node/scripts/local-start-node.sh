#!/bin/bash
# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts
/usr/bin/heaptrack ./target/release/p2p_client-cli --desired-nodes $DESIRED_PEERS --external-port $EXTERNAL_PORT --private-node --enable-baker --bootstrap-node $BOOTSTRAP_NODE $EXTRA_ARGS