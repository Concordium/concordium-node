#!/bin/bash

set -e

logger -s "CONCORDIUM_NODE_STARTING_SUDO"
sudo launchctl load "/Library/LaunchDaemons/software.concordium.node.plist"
logger -s "CONCORDIUM_NODE_STARTED_SUDO"
