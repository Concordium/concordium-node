#!/usr/bin/env bash

set -euxo pipefail

image_tag="${IMAGE_TAG}"
base_image_tag="${BASE_IMAGE_TAG}"
static_libraries_image_tag="${STATIC_LIBRARIES_IMAGE_TAG}"
genesis_ref="${GENESIS_REF}"
genesis_path="${GENESIS_PATH}"

# Using '--no-cache' because we're cloning genesis data
# and BuildKit (and '--ssh default') because the repo is on GitLab.
DOCKER_BUILDKIT=1 docker build \
  --build-arg base_image_tag="${base_image_tag}" \
  --build-arg static_libraries_image_tag="${static_libraries_image_tag}" \
  --build-arg ghc_version="${ghc_version}" \
  --build-arg genesis_ref="${genesis_ref}" \
  --build-arg genesis_path="${genesis_path}" \
  --label base_image_tag="${base_image_tag}" \
  --label static_libraries_image_tag="${static_libraries_image_tag}" \
  --label ghc_version="${ghc_version}" \
  --label genesis_ref="${genesis_ref}" \
  --label genesis_path="${genesis_path}" \
  -t "concordium/staging-client:${image_tag}" \
  -f scripts/distribution/stagenet.Dockerfile \
  --ssh default \
  --no-cache \
  .

docker save "concordium/staging-client:${image_tag}" | gzip > "staging-client-${image_tag}.tar.gz"
aws s3 cp "staging-client-${image_tag}.tar.gz" s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

# Make the image current if the tag is formatted as "<number>:<number>:<number>".
if [[ "${image_tag}" =~ ^[[:digit:]]\.[[:digit:]]\.[[:digit:]]$ ]]; then
	echo "${image_tag}" > VERSION
	aws s3 cp VERSION s3://distribution.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi
