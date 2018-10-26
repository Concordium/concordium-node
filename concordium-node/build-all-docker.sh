#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG"
  exit 1
fi

# Foreign temporary dependency, potentially a smarter way?
git clone git@gitlab.com:Concordium/consensus/prototype.git consensus

scripts/build-base-docker.sh
scripts/build-build-docker.sh $1
scripts/build-bootstrapper-docker.sh $1
scripts/build-basic-docker.sh $1
scripts/build-ipdiscovery-docker.sh $1
scripts/build-testrunner-docker.sh $1