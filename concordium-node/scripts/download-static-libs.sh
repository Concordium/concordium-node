#!/usr/bin/env bash

set -e
git submodule update --init --recursive
BASEDIR="$( cd "$(dirname "$0")" ; pwd -P )"
( 
  TMPDIR="$BASEDIR/../deps/internal/consensus/tmp"
  if [ -d $TMPDIR ]; then
    rm -r $TMPDIR
  fi
  cd $BASEDIR/../deps/internal/consensus
  VERSION_TAG=$(git rev-parse --verify HEAD)
  mkdir $TMPDIR
  (
    cd $TMPDIR
    wget https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/static-consensus-$VERSION_TAG.tar.gz
    tar -xf static-consensus-$VERSION_TAG.tar.gz
    cd target/
    rm -rf $BASEDIR/../deps/static-libs/linux/*
    cp -r * $BASEDIR/../deps/static-libs/linux/
  )
  rm -r $TMPDIR
)