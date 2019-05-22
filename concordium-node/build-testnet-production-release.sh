#!/bin/bash
set -e

CURRENT_BRANCH=$(git branch | grep \* | cut -d ' ' -f2)

git checkout master
git pull

VERSION=`cat Cargo.toml | grep "version = \"" | head -n1 | sed 's/version = \"//' | sed 's/\"//'`

./scripts/build-all-docker.sh $VERSION

git checkout $CURRENT_BRANCH

echo "Finished building production release with tag $VERSION"
