#!/bin/bash

# This script is intended to be run from inside a copy of the template
# directory. It will rename the files and instantiate the template according to
# the supplied environment variables.

# Expects the following environment variables to be set.
# - build_env_name (e.g., Testnet)
# - build_env_name_lower (e.g., testnet)
# - build_catchup_url (e.g. https://catchup.testnet.concordium.com/blocks.idx)
# - build_genesis_hash
# - build_collector_backend_url (e.g. https://dashboard.testnet.concordium.com/nodes/post)
# - build_grpc2_listen_port (e.g., 20001)
# - build_listen_port (e.g., 8889)
# - build_bootstrap (e.g., bootstrap.testnet.concordium.com:8888)

# Figure out the package version. We run the concordium-node binary and
# get its self-reported version to ensure consistency.

export build_version=$(./binaries/concordium-node --version | cut -d ' ' -f 2)

if [[ -z "$build_env_matrix" ]];
then
    echo '$build_env_matrix must be set.'
    exit 1
fi

# Process each key-value pair of the build_env_matrix
echo "$build_env_matrix" | jq -r 'to_entries[] | "\(.key)=\(.value)"' | while IFS="=" read -r network variables; do
    $build_env_name = $(echo "$variables" | jq -r  '.build_env_name')
    $build_env_name_lower = $(echo "$variables" | jq -r  '.build_env_name_lower')
    $build_catchup_url = $(echo "$variables" | jq -r  '.build_catchup_url')
    $build_version = $(echo "$variables" | jq -r  '.build_version')
    $build_genesis_hash = $(echo "$variables" | jq -r  '.build_genesis_hash')
    $build_genesis_collector_backend_url = $(echo "$variables" | jq -r  '.build_genesis_collector_backend_url')
    $build_genesis_grpc2_listen_port = $(echo "$variables" | jq -r  '.build_genesis_grpc2_listen_port')
    $build_genesis_listen_port = $(echo "$variables" | jq -r  '.build_genesis_listen_port')
    $build_bootstrap = $(echo "$variables" | jq -r  '.build_bootstrap')
    # The package name will be concordium-$build_env_name-node and the systemd
    # services within are analogously named. To achieve this we rename the template
    # files accordingly, and then subsitute the
    for file in debian/*
    do
        # insert environment name in template files with concordium-node in their name.
        out_file="${file/concordium-node/concordium-$(network)-node}"
        echo "$out_file"
        # Some files do not get renamed. To avoid problems with envsubst below which
        # overwrites the file we make a temporary copy for input.
        cp "$file" "$file.tmp"
        envsubst '$build_env_name
                  $build_env_name_lower
                  $build_catchup_url
                  $build_version
                  $build_genesis_hash
                  $build_collector_backend_url
                  $build_grpc2_listen_port
                  $build_listen_port
                  $build_bootstrap' < "$file.tmp" > "$out_file"
        rm "$file.tmp"
    done
done

# The install file should be executable since it renames the executables via dh-exec.
chmod +x debian/concordium-node.install
# Build the package.
dpkg-buildpackage -us -uc --build=binary

# And copy all of the resulting artifacts (three files, the .deb package being
# the main one, the other two being metadata) to ../out
mkdir ../out
mv ../concordium-node* ../out/
