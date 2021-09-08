#!/usr/bin/env bash

set -euxo pipefail

image_tag="${IMAGE_TAG}"
base_image_tag="${BASE_IMAGE_TAG}"
static_libraries_image_tag="${STATIC_LIBRARIES_IMAGE_TAG}"
ghc_version="${GHC_VERSION}"
genesis_ref="${GENESIS_REF}"
genesis_path="${GENESIS_PATH}"

# defined variables
image_name="stagenet-node"

# Using '--no-cache' because we're cloning genesis data
# and BuildKit (and '--ssh default') because the repo is on GitLab.
DOCKER_BUILDKIT=1 docker build \
  --build-arg environment="stagenet.concordium.com"\
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
  -t "concordium/${image_name}:${image_tag}" \
  -f scripts/distribution/builder.Dockerfile \
  --ssh default\
  --no-cache \
  .

# Name of the file that will be uploaded.
file="${image_name}-${image_tag}.tar.gz"

docker save concordium/"${image_name}:${image_tag}" | gzip > "${file}"
aws s3 cp "${file}" s3://distribution.stagenet.concordium.com/image/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

# Make the image current if the tag is formatted as "<number>:<number>:<number>".
# Other versions are for testing only.
if [[ "${image_tag}" =~ ^[[:digit:]]\.[[:digit:]]\.[[:digit:]]$ ]]; then
  echo "{\"image_tag\": \"${image_tag}\", \"file\": \"$file\", \"image_name\": \"$image_name\"}" > version.json
  aws s3 cp version.json s3://distribution.stagenet.concordium.com/image/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
fi
