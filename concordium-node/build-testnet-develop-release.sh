#!/bin/bash
set -e

if [ -z "$JENKINS_HOME" ]; then
  git pull
fi

VERSION=`git rev-parse --verify HEAD`

./scripts/build-all-docker.sh $VERSION

echo "Finished building and pushing develop release with tag $VERSION"
