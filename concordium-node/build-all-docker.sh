#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG"
  exit 1
fi

git submodule update --init --recursive

scripts/build-base-docker.sh
scripts/build-build-docker.sh $1
scripts/build-universal-docker.sh $1
