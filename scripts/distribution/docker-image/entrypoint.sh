#!/bin/bash

set -euo pipefail

# This script starts the node and, when configured, the collector. It keeps the
# node as the primary process and returns the node exit status to the container.
node_pid=
collector_pid=

# Send a termination signal to each child process that is still running.
terminate_children() {
    if [[ -n "$collector_pid" ]]; then
        kill -TERM "$collector_pid" 2>/dev/null || true
    fi
    if [[ -n "$node_pid" ]]; then
        kill -TERM "$node_pid" 2>/dev/null || true
    fi
}

# Wait for one child without letting `set -e` terminate this script. Return the
# child exit status so that the caller can decide how to handle a failure.
wait_for_child() {
    local child_pid=$1
    local child_status

    set +e
    wait "$child_pid"
    child_status=$?
    set -e
    return "$child_status"
}

# Forward container termination signals to the managed child processes.
trap terminate_children INT TERM

# Use the node as the default command. Treat leading options as node options,
# in the same way as a standard Docker entrypoint.
if [[ $# -eq 0 ]]; then
    set -- /usr/local/bin/concordium-node
elif [[ "$1" == -* ]]; then
    set -- /usr/local/bin/concordium-node "$@"
fi

# Run a custom command directly. Do not apply node or collector management to
# commands such as a shell or `node-collector --help`.
if [[ "$1" != "/usr/local/bin/concordium-node" && "$1" != "concordium-node" ]]; then
    exec "$@"
fi

# Require an explicit genesis file because the image does not select a network.
if [[ -z "${CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE:-}" ]]; then
    echo "CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE must select a file in /genesis." >&2
    exit 1
fi

if [[ ! -f "$CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE" ]]; then
    echo "Genesis data file does not exist: $CONCORDIUM_NODE_CONSENSUS_GENESIS_DATA_FILE" >&2
    exit 1
fi

# Start the node before the optional collector. The entrypoint remains the
# parent process so that it can monitor both processes and forward signals.
"$@" &
node_pid=$!

# Start the collector only when the operator enables it and supplies the
# required identity and backend URL.
case "${CONCORDIUM_NODE_COLLECTOR_ENABLED:-false}" in
    1 | true | TRUE | yes | YES)
        if [[ -z "${CONCORDIUM_NODE_COLLECTOR_NODE_NAME:-}" ]]; then
            echo "CONCORDIUM_NODE_COLLECTOR_NODE_NAME is required when the collector is enabled." >&2
            terminate_children
            wait_for_child "$node_pid" || true
            exit 1
        fi
        if [[ -z "${CONCORDIUM_NODE_COLLECTOR_URL:-}" ]]; then
            echo "CONCORDIUM_NODE_COLLECTOR_URL is required when the collector is enabled." >&2
            terminate_children
            wait_for_child "$node_pid" || true
            exit 1
        fi
        /usr/local/bin/node-collector &
        collector_pid=$!
        ;;
    0 | false | FALSE | no | NO)
        ;;
    *)
        echo "CONCORDIUM_NODE_COLLECTOR_ENABLED must be true or false." >&2
        terminate_children
        wait_for_child "$node_pid" || true
        exit 1
        ;;
esac

# Monitor both children while the collector runs. A collector failure is not a
# node failure, so keep the node running and report the collector exit.
node_status=0
while [[ -n "$collector_pid" ]]; do
    exited_pid=
    exited_status=0
    wait -n -p exited_pid "$node_pid" "$collector_pid" || exited_status=$?

    if [[ "$exited_pid" == "$node_pid" ]]; then
        node_status=$exited_status
        node_pid=
        break
    fi

    if [[ "$exited_pid" == "$collector_pid" ]]; then
        echo "node-collector exited with status $exited_status; concordium-node will continue." >&2
        collector_pid=
    fi
done

# If the collector is disabled or has stopped, wait for the node by itself.
if [[ -n "$node_pid" ]]; then
    wait_for_child "$node_pid" || node_status=$?
    node_pid=
fi

# Stop the collector after the node exits, then return the node exit status.
if [[ -n "$collector_pid" ]]; then
    kill -TERM "$collector_pid" 2>/dev/null || true
    wait_for_child "$collector_pid" || true
fi

exit "$node_status"
