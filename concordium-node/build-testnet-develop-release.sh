#!/bin/bash
set -e

git pull

VERSION=`git rev-parse --verify HEAD`

./scripts/build-all-docker.sh $VERSION

echo "Finished building and pushing develop release with tag $VERSION"
