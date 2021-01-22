#!/usr/bin/env bash

set -euo pipefail
# This script is intended to be run from the root of the repository, or with two arguments.
# The first argument must be the root of the concordium-node package, and the second should be
# the name of the file with a hash of the static libraries.

if [ "$#" -eq 2 ]; then
    NODE_DIR=$1
    VERSION_TAG=$(cat $2)
else
    # else assume we are run from the root of the repository.
    NODE_DIR=$(pwd)/concordium-node
    VERSION_TAG=$(cat scripts/static-libraries/LATEST_STATIC_LIBRARIES)
fi

if [ -d "$NODE_DIR" ]; then
  ARCHIVES_DIR=$NODE_DIR/deps/static-libs/linux/archives
  if [ ! -d $ARCHIVES_DIR ]; then
    mkdir -p $ARCHIVES_DIR
  fi
  cd $ARCHIVES_DIR
  (
    DISK_VERSION_FILE="$ARCHIVES_DIR/VERSIONTAG"
    if [ -f "$DISK_VERSION_FILE" ]; then
      DISK_VERSION_TAG=$(<"$DISK_VERSION_FILE")
    else
      DISK_VERSION_TAG="UNKNOWN"
    fi
    if [ "$DISK_VERSION_TAG" != "$VERSION_TAG" ]; then
      ARCHIVE="static-consensus-$VERSION_TAG.tar.gz"
      if [ ! -f "$ARCHIVE" ]; then
        printf "Downloading static consensus version $VERSION_TAG\n"
        curl -s https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$VERSION_TAG.tar.gz > $ARCHIVE
      fi
      printf "Expanding downloaded archive\n"
      rm -rf target
      tar -xf $ARCHIVE
      (
        cd target/
        printf "Replacing local version with upstream\n"
        rm -rf $NODE_DIR/deps/static-libs/linux/{profiling,rust,vanilla}
        cp -r * $NODE_DIR/deps/static-libs/linux/
      )
      rm -rf target/
      echo $VERSION_TAG > $ARCHIVES_DIR/VERSIONTAG
    else
        echo "Downloaded archive is already the latest version."
    fi
  )
else
    echo "The script should be run from the root of the concordium-node repository."
    exit 1
fi
