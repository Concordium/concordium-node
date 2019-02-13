#!/bin/bash

/build-project/p2p-client/target/debug/testrunner \
    --private-node \
    --debug \
    --no-dnssec \
    --desired-nodes $DESIRED_PEERS \
    --external-port $EXTERNAL_PORT \
    --bootstrap-node $BOOTSTRAP_NODE
