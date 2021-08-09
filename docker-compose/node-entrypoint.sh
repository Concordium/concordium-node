#!/usr/bin/env bash

set -euxo pipefail

# The correct 'genesis.dat' and baker credentials files for the network are stored in
# '/genesis-data/genesis-${NUM_BAKERS}-bakers'.
# The node expects to find 'genesis.dat' in the data dir '/var/lib/concordium/data'.
# The Compose file sets 'GENESIS_DATA_PATH' to the former path such that we can copy it here
# and also point at the correct credentials.
# If the variable isn't set, the assumption is that the files/environment has been set up by some other means.
# The node will fail on startup if this is not done correctly.
if [ -n "${GENESIS_DATA_PATH-}" ]; then
	# Copy 'genesis.dat' - better solution: support flag in concordium-node to set location.
	cp "${GENESIS_DATA_PATH}/genesis.dat" "${CONCORDIUM_NODE_DATA_DIR}"
	# Select unique baker credentials file.
	id="$(curl "http://${BAKER_ID_GEN_DNS}:8000/next_id")"
	export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE="${GENESIS_DATA_PATH}/bakers/baker-${id}-credentials.json"
fi

# Run binary - inherits env vars and args.
/concordium-node "${@}"
