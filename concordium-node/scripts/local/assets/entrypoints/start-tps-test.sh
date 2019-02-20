#!/bin/bash
export BAKER_ID=`curl http://baker_id_gen:8000/next_id`
echo "Using BAKER_ID $BAKER_ID"

if [ "$TPS_MODE" == "receiver" ]; then
    /build-project/p2p-client/target/debug/p2p_client-cli \
        --private-node \
        --debug \
        --no-dnssec \
        --id $ID \
        --desired-nodes $DESIRED_PEERS \
        --external-port $EXTERNAL_PORT \
        --baker-id $BAKER_ID \
        --num-bakers $NUM_BAKERS \
        --enable-tps-test-recv \
        --no-bootstrap
elif [ "$TPS_MODE" == "sender" ]; then
    mkdir -p $DATA_DIR
    mkdir -p $DATA_DIR/tps-test
    cd $DATA_DIR/tps-test
    for i in `seq 0 99`;
    do
	    echo $i
	    dd if=/dev/urandom of=test-$i bs=1 count=1024 > /dev/null 2>&1
    done
    /build-project/p2p-client/target/debug/p2p_client-cli \
        --private-node \
        --debug \
        --no-dnssec \
        --id $ID \
        --desired-nodes $DESIRED_PEERS \
        --external-port $EXTERNAL_PORT \
        --baker-id $BAKER_ID \
        --num-bakers $NUM_BAKERS \
        --tps-test-recv-id $TPS_RECV_ID \
        --tps-test-data-dir $DATA_DIR/tps_test \
        --no-bootstrap
fi