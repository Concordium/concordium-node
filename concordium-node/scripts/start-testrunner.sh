#!/bin/bash

./target/release/testrunner --listen-port $LISTEN_PORT --listen-http-port $LISTEN_HTTP_PORT --bootstrap-server $DNS_BOOSTRAP_NODE $EXTRA_ARGS 