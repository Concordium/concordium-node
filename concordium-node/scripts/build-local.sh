#!/bin/bash
docker build -f scripts/base.Dockerfile -t concordium/base:latest .
docker build -f scripts/build.Dockerfile -t concordium/build:latest .
docker build -f scripts/local.testbootstrapper.Dockerfile -t concordium/test/bootstrapper:latest .
docker build -f scripts/local.testnode.Dockerfile -t concordium/test/node:latest .