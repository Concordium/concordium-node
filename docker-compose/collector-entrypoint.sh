#!/usr/bin/env bash

set -euxo pipefail

# Extract input values.
num_bakers="${NUM_BAKERS-1}"
if [ "${num_bakers}" -gt 0 ]; then
	node_dns_base="${NODE_DNS_BASE}"
fi

# Collect data from all nodes.
url_args=()
for i in $(seq 1 "${num_bakers}"); do
	url_args+=(
		--grpc-host "http://${node_dns_base}_${i}:10000" --node-name "baker_${i}"
	)
done

# Run binary - inherits env vars and args.
/node-collector "${url_args[@]}" "${@}"
