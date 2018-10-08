#!/bin/bash
/usr/bin/heaptrack ./target/release/p2p_client-cli --desired-nodes $DESIRED_PEERS --external-port $EXTERNAL_PORT --private-node --bootstrap-node $BOOTSTRAP_NODE $EXTRA_ARGS