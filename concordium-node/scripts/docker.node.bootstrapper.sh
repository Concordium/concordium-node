#!/bin/sh
docker build -f scripts/bootstrapper.Dockerfile -t node-bootstrapper:latest .
echo "BUILD_DONE"
