#!/bin/bash
set -e

if [ -z "$JENKINS_HOME" ]; then
  CURRENT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)
  git checkout master
  git pull
fi

git lfs pull

VERSION=`cat Cargo.toml | grep "version = \"" | head -n1 | sed 's/version = \"//' | sed 's/\"//'`

./scripts/build-all-docker.sh $VERSION

if [ -z "$JENKINS_HOME" ]; then
  git checkout $CURRENT_BRANCH
fi

echo "Finished building production release with tag $VERSION"
