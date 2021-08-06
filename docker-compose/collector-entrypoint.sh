#!/usr/bin/env bash

set -euxo pipefail

num_bakers="${NUM_BAKERS-1}"

# Collect data from all nodes.
url_args=()
for i in $(seq 1 "${num_bakers}"); do
	url_args=(
		"${url_args[@]}"
		--grpc-host "http://${NODE_DNS_BASE}_${i}:10000" --node-name "baker_${i}"
	)
done

# Run binary - inherits env vars and args.
/node-collector "${url_args[@]}" "${@}"
