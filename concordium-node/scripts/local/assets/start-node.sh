#!/bin/bash
export BAKER_ID=$((16#`awk 'END{ print $2}' /etc/hosts`))
echo "Using BAKER_ID $BAKER_ID"
echo "HOST"
cat /etc/hosts

export


/build-project/p2p-client/target/debug/p2p_client-cli \
    --desired-nodes $DESIRED_PEERS \
    --external-port $EXTERNAL_PORT \
    --private-node \
    --bootstrap-node $BOOTSTRAP_NODE \
    --baker-id $BAKER_ID \
    --num-bakers $NUM_BAKERS $EXTRA_ARGS
