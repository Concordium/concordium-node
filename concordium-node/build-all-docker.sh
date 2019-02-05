#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Usage: ./build-all-docker.sh VERSION-TAG"
  exit 1
fi

# Foreign temporary dependency, potentially a smarter way?
rm -rf consensus
git clone git@gitlab.com:Concordium/consensus/prototype.git consensus
# Locked to oak-integration for PoC due to build issues on Windows with master currently.
( cd consensus && git checkout 8399a2802fe7402663ce1a9a491b1b04e960c08c && ./setup-env.sh )

scripts/build-base-docker.sh
scripts/build-build-docker.sh $1
scripts/build-universal-docker.sh $1
