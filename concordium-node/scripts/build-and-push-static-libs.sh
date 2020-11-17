#!/usr/bin/env bash

set -e

[ -z $GHC_VERSION ] && (
    echo "GHC_VERSION not set"
    exit 1
)

docker build -t concordium/static-libraries -f scripts/static-libraries.Dockerfile --ssh default .

mkdir -p out

docker run -e GHC_VERSION -v $(pwd)/out:/out concordium/static-libraries

VERSION_TAG=$(cd deps/internal/consensus && git rev-parse --verify HEAD)

mv out/static-consensus-$GHC_VERSION.tar.gz out/static-consensus-$VERSION_TAG.tar.gz
mv out/static-consensus-binaries-$GHC_VERSION.tar.gz out/static-consensus-binaries-$VERSION_TAG.tar.gz

aws s3 cp out/static-consensus-$VERSION_TAG.tar.gz s3://static-libraries.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
aws s3 cp out/static-consensus-binaries-$VERSION_TAG.tar.gz s3://static-libraries.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
