#!/usr/bin/env bash

set -euxo pipefail

# Extract input values.
genesis_data_path="${GENESIS_DATA_PATH-}"
concordium_node_data_dir="${CONCORDIUM_NODE_DATA_DIR}"
if [ -n "${genesis_data_path}" ]; then
	baker_id_url="${BAKER_ID_URL}"
fi

# The correct 'genesis.dat' and baker credentials files for the network are stored in
# '/genesis-data/genesis-${NUM_BAKERS}-bakers'.
# The node expects to find 'genesis.dat' in the data dir '/var/lib/concordium/data'.
# The Compose file sets 'GENESIS_DATA_PATH' to the former path such that we can copy it here
# and also point at the correct credentials.
# If the variable isn't set, the assumption is that the files/environment has been set up by some other means.
# The node will fail on startup if this is not done correctly.
if [ -n "${genesis_data_path}" ]; then
	# Copy 'genesis.dat' - better solution: support flag in concordium-node to set location.
	cp "${genesis_data_path}/genesis.dat" "${concordium_node_data_dir}"
	# Select unique baker credentials file.
	id="$(curl -sS "${baker_id_url}")"
	export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE="${genesis_data_path}/bakers/baker-${id}-credentials.json"
fi

# Run binary - inherits env vars and args.
/concordium-node "${@}"
