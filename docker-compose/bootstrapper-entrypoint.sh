#!/usr/bin/env bash

set -euxo pipefail

# Ensure that expected dirs exist.
mkdir -p "$CONCORDIUM_NODE_CONFIG_DIR" "$CONCORDIUM_NODE_DATA_DIR"

# Run binary - inherits env vars and args.
/p2p_bootstrapper-cli "$@"
