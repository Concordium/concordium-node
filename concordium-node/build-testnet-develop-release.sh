#!/bin/bash
set -e

if [ -z "$JENKINS_HOME" ]; then
  git pull
fi

if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-testnet-develop-release.sh [debug|release]"
  exit 1
fi

PATH="$PATH:/usr/local/bin" git lfs install
PATH="$PATH:/usr/local/bin" git lfs pull

VERSION=`git rev-parse --verify HEAD`

./scripts/build-all-docker.sh $VERSION $1

echo "Finished building and pushing develop release with tag $VERSION"
