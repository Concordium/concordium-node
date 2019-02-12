#!/bin/bash
export BAKER_ID=`curl http://baker_id_gen:8000/next_id`
echo "Using BAKER_ID $BAKER_ID"


/build-project/p2p-client/target/debug/p2p_client-cli \
    --private-node \
    --debug \
    --no-dnssec \
    --desired-nodes $DESIRED_PEERS \
    --external-port $EXTERNAL_PORT \
    --bootstrap-node $BOOTSTRAP_NODE \
    --baker-id $BAKER_ID \
    --num-bakers $NUM_BAKERS 
