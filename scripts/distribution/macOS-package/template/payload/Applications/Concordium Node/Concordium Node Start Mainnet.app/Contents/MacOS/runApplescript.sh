#!/bin/bash
set -euo pipefail

cd "${0%/*}"
osascript start-mainnet.applescript
