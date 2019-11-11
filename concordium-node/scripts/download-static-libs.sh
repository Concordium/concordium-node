#!/usr/bin/env bash

set -e
BASEDIR="$( cd "$(dirname "$0")" ; pwd -P )"
(
  $BASEDIR/../deps/internal/consensus/consensus-rust/scripts/download-static-libs.sh $@
)