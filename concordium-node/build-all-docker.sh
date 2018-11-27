#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG"
  exit 1
fi

# Foreign temporary dependency, potentially a smarter way?
rm -rf consensus
git clone git@gitlab.com:Concordium/consensus/prototype.git consensus
# Lock to specific commit in git to avoid issues with linking
( cd consensus && git checkout 24a7053e1459be0bfec6a6fdcb13ebaaa5949b78 )

scripts/build-base-docker.sh
scripts/build-build-docker.sh $1
scripts/build-bootstrapper-docker.sh $1
scripts/build-basic-docker.sh $1
scripts/build-ipdiscovery-docker.sh $1
scripts/build-testrunner-docker.sh $1