#!/bin/bash
set -e

if [ -z "$JENKINS_HOME" ]; then
  git pull
fi

PATH="$PATH:/usr/local/bin" git lfs install
PATH="$PATH:/usr/local/bin" git lfs pull

VERSION=`git rev-parse --verify HEAD`

./scripts/build-all-docker.sh $VERSION

echo "Finished building and pushing develop release with tag $VERSION"
