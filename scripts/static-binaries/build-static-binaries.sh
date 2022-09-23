#!/usr/bin/env bash

set -euxo pipefail

ubuntu_version="${UBUNTU_VERSION}"
static_libraries_image_tag="${STATIC_LIBRARIES_IMAGE_TAG}"
static_binaries_image_tag="${STATIC_BINARIES_IMAGE_TAG}"
ghc_version="${GHC_VERSION}"
extra_features=${EXTRA_FEATURES:-""}

docker build \
    --build-arg ubuntu_version="$ubuntu_version" \
    --build-arg static_libraries_image_tag="$static_libraries_image_tag" \
    --build-arg ghc_version="$ghc_version" \
    --build-arg extra_features="$extra_features" \
    --label ubuntu_version="$ubuntu_version" \
    --label static_libraries_image_tag="$static_libraries_image_tag" \
    --label ghc_version="$ghc_version" \
    --label extra_features="$extra_features" \
    -f "scripts/static-binaries/static-binaries.Dockerfile" \
    -t static-node-binaries:"$static_binaries_image_tag" \
    "."

echo "The binaries are ready inside the 'static-node-binaries' image in the '/build/bin/' directory."
echo "They can be obtained by mounting the directory using 'docker run -v \$OUT_DIR:/out/ static-node-binaries' cp ..."
echo "and copying the files."
