#!/usr/bin/env bash

set -e
BASEDIR="$( cd "$(dirname "$0")" ; pwd -P )"
(
  VERSION_TAG=$(cat $BASEDIR/CONSENSUS_VERSION)
  if [[ ! -z "$1" && "$1" != "default" ]]; then
    VERSION_TAG="$VERSION_TAG-$1"
  fi
  ARCHIVES_DIR=$BASEDIR/../deps/static-libs/linux/archives
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
        rm -rf $BASEDIR/../deps/static-libs/linux/{profiling,rust,vanilla}
        cp -r * $BASEDIR/../deps/static-libs/linux/
      )
      rm -rf target/
      echo $VERSION_TAG > $ARCHIVES_DIR/VERSIONTAG
    fi
  )
)