#!/bin/bash

./target/release/testrunner --private-node --desired-nodes $DESIRED_PEERS --external-port $EXTERNAL_PORT--bootstrap-node $BOOTSTRAP_NODE $EXTRA_ARGS 
