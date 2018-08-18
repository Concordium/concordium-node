#!/bin/sh
docker build -f scripts/firstgen.node-test.Dockerfile -t node-test-firstgen:latest .
echo "BUILD DONE"
