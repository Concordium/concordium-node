#!/usr/bin/env bash
set -e
########################################################
#
# Download and build the ghc and libraries with support for static linked distribution on Linux x86_64.
#
# Takes ghc version as argument and optionally a path to use for building.
#
########################################################

# Ensure the ghc version is specified

if [ -z "$1" ]; then
    echo "Error: The ghc version is required."
    echo "Usage: $0 <ghc-version> <directory>"
    exit 1
fi
GHC_VERSION=$1
echo "Building ghc version ${GHC_VERSION}"

# Create temporary directory if nothing was provided
if [ -z "$2" ]; then
    BUILD_DIR=$(mktemp -d)
    echo "Build directory is not provided. Using ${BUILD_DIR}"
else
    BUILD_DIR=$2
fi
cd $BUILD_DIR

# Download ghc source

SOURCE="ghc-${GHC_VERSION}-src.tar.xz"
echo "Downloading source for ghc-${GHC_VERSION}"
curl "https://downloads.haskell.org/~ghc/${GHC_VERSION}/${SOURCE}" -o "ghc.tar.xz"
tar xf "ghc.tar.xz"
rm "ghc.tar.xz"
cd ghc-$GHC_VERSION

# Configure the build.

./configure --disable-numa
cabal update
mkdir -p _build
cat <<'EOF' >_build/hadrian.settings
stage1.*.ghc.*.opts += -fPIC
stage1.*.cc.*.opts += -fPIC
EOF

# Build ghc binaries.

./hadrian/build\
    --flavour="perf"\
    --docs=none\
    -j\
    binary-dist

echo "Archive of ghc binaries for distribution can be found at ${BUILD_DIR}/ghc-${GHC_VERSION}/_build/bindist/ghc-${GHC_VERSION}-x86_64-unknown-linux.tar.xz"
