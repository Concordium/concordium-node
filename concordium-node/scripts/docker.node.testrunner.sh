#!/bin/sh
docker build -f scripts/testrunner.Dockerfile -t node-basic:latest .
echo "BUILD DONE"
