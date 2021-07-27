#!/usr/bin/env bash

set -euxo pipefail

# Ensure that expected dirs exist.
mkdir -p "$CONCORDIUM_NODE_CONFIG_DIR" "$CONCORDIUM_NODE_DATA_DIR"

if [ -n "$COPY_GENESIS_DAT_FILE" ]; then
	# Better solution: support flag in node to set location.
	cp "$COPY_GENESIS_DAT_FILE" "$CONCORDIUM_NODE_DATA_DIR"
fi

# Run binary - inherits env vars and args.
/concordium-node "$@"
