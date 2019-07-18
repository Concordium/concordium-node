#!/usr/bin/env bash

set -e
git submodule update --init --recursive
CURRENT=$(pwd)
OLD_TAG=$(cat deps/static-libs/linux/CURRENT_VERSION)
echo Old tag is $OLD_TAG
cd deps/internal/consensus

VERSION_TAG=$(git rev-parse --verify HEAD)
echo New tag is $VERSION_TAG

cd $CURRENT

if [ "$OLD_TAG" != "$VERSION_TAG" ]; then
  echo Tags are different. I\'m updating!
  mkdir tmp
  cd tmp
  wget https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$VERSION_TAG.tar.gz
  tar -xf static-consensus-$VERSION_TAG.tar.gz
  cd target/

  rm -r $CURRENT/deps/static-libs/linux/*
  cp -r * $CURRENT/deps/static-libs/linux/
  cd $CURRENT
  rm -r tmp

  echo "$VERSION_TAG" > deps/static-libs/linux/CURRENT_VERSION
fi
