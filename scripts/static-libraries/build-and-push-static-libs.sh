#!/usr/bin/env bash

# Build the static libraries from the $GIT_BRANCH branch set by Jenkins
# ---------------------------------------------------------------------
#
# This will create a docker container that will build the tar archives to the
# `out` directory and will push the archives to aws s3.

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

echo "The static libraries are pushed to S3/static-libraries bucket with version $VERSION_TAG."
echo "To record this in the concordium-node repository run"
echo "echo $VERSION_TAG > scripts/static-libraries/LATEST_STATIC_LIBRARIES"
