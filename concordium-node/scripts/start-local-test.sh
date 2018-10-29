#!/bin/bash
EXTERNAL_IP=$1
NODE_COUNT=$2
docker run -d --name=bootstrapper -p 8888:8888 concordium/test/bootstrapper:latest
sleep 5
for i in `seq 1 $NODE_COUNT`;
do
    PORT=$((8889+$i))
    docker run -d --name=nodetest$i -p $PORT:8888 -e "EXTERNAL_PORT=$PORT" -e "BOOTSTRAP_NODE=$EXTERNAL_IP:8888" concordium/test/node:latest 
done
TESTRUNNER_PORT=$((8889+$NODE_COUNT+1))
docker run -d --name=testrunner -p $TESTRUNNER_PORT:8888 -e "EXTERNAL_PORT=$TESTRUNNER_PORT" -e "BOOTSTRAP_NODE=$EXTERNAL_IP:8888" concordium/test/testrunner:latest
