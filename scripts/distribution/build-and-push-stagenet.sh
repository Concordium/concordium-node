#!/usr/bin/env bash

set -ex

version=$(awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml) # extract and unquote value of the first occurrence of a 'version' key in Cargo.toml

genesis_ref="${GENESIS_REF}"
genesis_path="${GENESIS_PATH}"
base_image_tag="${BASE_IMAGE_TAG}"

DOCKER_BUILDKIT=1 docker build \
  --build-arg genesis_ref="${genesis_ref}" \
  --build-arg genesis_path="${genesis_path}" \
  --build-arg base_image_tag="${base_image_tag}" \
  --label genesis_ref="${genesis_ref}" \
  --label genesis_path="${genesis_path}" \
  --label base_image_tag="${base_image_tag}" \
  -t "concordium/staging-client:${version}" \
  -f scripts/distribution/stagenet.Dockerfile \
  --ssh default \
  --no-cache \
  .

docker save "concordium/staging-client:${version}" | gzip > "staging-client-${version}.tar.gz"
echo "${version}" > VERSION

aws s3 cp "staging-client-${version}.tar.gz" s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
aws s3 cp VERSION s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
