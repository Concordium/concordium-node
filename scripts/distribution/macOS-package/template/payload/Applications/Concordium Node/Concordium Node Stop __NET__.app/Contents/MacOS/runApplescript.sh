#!/bin/bash
set -euo pipefail

cd "${0%/*}"
osascript stop-node.applescript
