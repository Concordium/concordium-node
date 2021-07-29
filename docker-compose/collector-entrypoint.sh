#!/usr/bin/env bash

set -exo pipefail

# Optionally delay startup.
if [ -n "${COLLECTOR_SLEEP}" ]; then
	echo "Sleeping for ${COLLECTOR_SLEEP}"
	sleep "${COLLECTOR_SLEEP}"
fi

# Collect data from all nodes.
url_args=()
for i in $(seq 1 "${NUM_BAKERS}"); do
	url_args=(
		"${url_args[@]}"
		--grpc-host "http://${NODE_DNS_BASE}_${i}:10000" --node-name "baker_${i}"
	)
done

# Run binary - inherits env vars and args.
/node-collector "${url_args[@]}" "${@}"
