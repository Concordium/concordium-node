#!/bin/bash
# Haskell binding needs proper library path to function
export LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.3/lib/ghc-8.4.3/rts
./target/debug/testrunner --private-node --desired-nodes $DESIRED_PEERS --external-port $EXTERNAL_PORT --bootstrap-node $BOOTSTRAP_NODE $EXTRA_ARGS 
