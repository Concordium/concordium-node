#!/bin/bash
NODE_COUNT=$1
docker stop bootstrapper
docker rm bootstrapper
for i in `seq 1 $1`
do
    docker stop nodetest$i
    docker rm nodetest$i
done
docker stop testrunner
docker rm testrunner
