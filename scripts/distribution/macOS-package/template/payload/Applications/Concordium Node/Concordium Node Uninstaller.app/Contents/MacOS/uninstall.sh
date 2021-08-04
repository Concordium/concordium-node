#!/bin/bash
set -euo pipefail

cd "${0%/*}"
osascript uninstall.applescript
