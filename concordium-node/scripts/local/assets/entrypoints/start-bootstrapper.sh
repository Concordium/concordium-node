#!/bin/bash

export NODE_ID=`awk 'END{ print $1}' /etc/hosts | sha256sum | awk '{print $1}'`
heaptrack /build-project/p2p-client/target/debug/p2p_bootstrapper-cli \
    --id $NODE_ID \
    --listen-port 8888 \
    --private-node \
    --debug
    --no-dnssec


