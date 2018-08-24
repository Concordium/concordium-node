#!/bin/sh
docker build -f scripts/ipdiscovery.Dockerfile -t node-ipdiso:latest .
echo "BUILD_DONE"
