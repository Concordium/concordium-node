#!/bin/bash

# Setup any dependencies and build Haskell code
./scripts/local-setup-unix-deps.sh

# Build rust code

LD_LIBRARY_PATH=/usr/local/lib:$HOME/.stack/programs/x86_64-linux/ghc-tinfo6-8.4.4/lib/ghc-8.4.4/rts cargo build

# Restore global stack config
if [ -f ~/.stack/global-project/stack.yaml.* ]; then
    rm -f ~/.stack/global-project/stack.yaml
    mv ~/.stack/global-project/stack.yaml.* ~/.stack/global-project/stack.yaml
fi