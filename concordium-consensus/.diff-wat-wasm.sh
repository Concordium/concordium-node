#!/usr/bin/env bash

# Check that committed .wasm files correspond to the given .wat files.
# The .wasm files are committed for convenience in testing, so that people
# who do not touch the scheduler or smart contracts do not need all the Wasm tooling installed.

# This script should be run from the directory that it resides in. The idea is
# to report all the files that failed, hence no early exit from the script.

pushd testdata/contracts

RET=0

for wat in $(find -name '*.wat'); do
   OUT=$(mktemp)
   wat2wasm $wat -o $OUT;
   if ! $(diff $OUT "${wat%.wat}.wasm")
   then
     RET=1
     echo "The $wat contract's .wasm output does not match the expected one. Regenerate the .wasm file."
   fi
   rm $OUT
done

popd

exit $RET
