#!/usr/bin/env bash

set -euxo pipefail

# Extract input values.
genesis_data_path="${GENESIS_DATA_PATH-}"
# The correct 'genesis.dat' and baker credentials files for the network are stored in
# '/genesis-data/genesis-${NUM_BAKERS}-bakers'.
# The Compose file sets 'GENESIS_DATA_PATH' to this path and the rest of this script assumes
# that the baker credentials as well as genesis.dat is contained in that folder.
# If the variable isn't set, the assumption is that the files/environment has been set up by some other means.
# The node will fail on startup if this is not done correctly.

if [ -n "${genesis_data_path}" ]; then
    # Get the baker id by querying a simple service and select the baker credentials file based on this.
	baker_id_url="${BAKER_ID_URL}"
	id="$(curl -sS "${baker_id_url}")"
	export CONCORDIUM_NODE_BAKER_CREDENTIALS_FILE="${genesis_data_path}/bakers/baker-${id}-credentials.json"
    # Set the correct genesis for the network.
    export CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE="${genesis_data_path}/genesis.dat"
fi

# Run binary - inherits env vars and args.
/concordium-node "${@}"
