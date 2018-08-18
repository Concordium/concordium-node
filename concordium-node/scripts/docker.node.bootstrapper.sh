#!/bin/sh
docker build -f scripts/Dockerfile.bootstrapper -t node-bootstrapper:latest .
echo "BUILD_DONE"
