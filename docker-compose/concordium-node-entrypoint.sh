#!/usr/bin/env bash

set -euxo pipefail

# Ensure that expected dirs exist.
mkdir -p "$CONCORDIUM_NODE_CONFIG_DIR" "$CONCORDIUM_NODE_DATA_DIR"

if [ -n "$GENESIS_DATA_PATH" ]; then
	# Copy 'genesis.dat' - better solution: support flag in concordium-node to set location.
	cp "$GENESIS_DATA_PATH/genesis.dat" "$CONCORDIUM_NODE_DATA_DIR"
	# Select unique baker credentials file.
	id="$(curl "$BAKER_ID_GEN_NEXT_ID_URL")"
	export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE="$GENESIS_DATA_PATH/bakers/baker-$id-credentials.json"
fi

# Run binary - inherits env vars and args.
/concordium-node "$@"
