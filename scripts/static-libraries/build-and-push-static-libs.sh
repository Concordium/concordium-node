#!/usr/bin/env bash

# Build the static libraries from the $GIT_BRANCH branch set by Jenkins
# ---------------------------------------------------------------------
#
# This will create a docker container that will build the tar archives to the
# `out` directory and will push the archives to aws s3.
#
# It will also modify the `scripts/LATEST_STATIC_LIBRARIES` file with the
# commit hash of the built libraries. This file is later consumed by
# `scripts/download-static-libraries.sh` to correctly place all the required
# libraries for the node to be started with the features `static` or `profiling`.
#
# Note this script cannot be used with branches that are push-protected such
# as `master` or `develop`.
#
# This script is consumed by `jenkinsfiles/static-libraries.Jenkinsfile`.

# checkout requested branch for later push
BRANCH=$(echo $GIT_BRANCH | cut -d'/' -f2-)
git checkout $BRANCH

set -e

# Build the libraries
docker build -t concordium/static-libraries -f scripts/static-libraries/static-libraries.Dockerfile --ssh default .
mkdir -p out
docker run -e GHC_VERSION -v $(pwd)/out:/out concordium/static-libraries

# Tag the build
VERSION_TAG=$(git rev-parse --verify HEAD)
mv out/static-consensus-$GHC_VERSION.tar.gz out/static-consensus-$VERSION_TAG.tar.gz
mv out/static-consensus-binaries-$GHC_VERSION.tar.gz out/static-consensus-binaries-$VERSION_TAG.tar.gz

# Push to AWS S3
aws s3 cp out/static-consensus-$VERSION_TAG.tar.gz s3://static-libraries.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
aws s3 cp out/static-consensus-binaries-$VERSION_TAG.tar.gz s3://static-libraries.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

# Commit and push changes
rm -rf out
echo "Going to push to branch $GIT_BRANCH"
echo $VERSION_TAG > scripts/static-libraries/LATEST_STATIC_LIBRARIES
git add scripts/static-libraries/LATEST_STATIC_LIBRARIES
# CI is skipped by default for these commits as the CI doesn't use the
# static libraries so the outcome would be the same as in the previous commit.
git commit -m "Jenkins: push $VERSION_TAG static libraries. [skip ci]"
git push origin $BRANCH
