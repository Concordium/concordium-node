#!/bin/bash

./target/release/ip_discovery --listen-port $LISTEN_PORT --prometheus-server $PROMETHEUS_METRICS_SERVER --prometheus-listen-port $PROMETHEUS_METRICS_PORT --prometheus-listen-addr $PROMETHEUS_METRICS_IP $EXTRA_ARGS
