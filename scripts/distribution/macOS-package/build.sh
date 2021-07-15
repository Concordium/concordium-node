#!/usr/bin/env bash
set -euo pipefail

readonly GREEN='\033[0;32m'
readonly NC='\033[0m' # No Color

step () {
    printf "\n${GREEN}$@${NC}\n"
}

readonly ghcVariant="x86_64-osx-ghc-8.10.4"

macPackageDir=$(pwd)
readonly macPackageDir
readonly nodeDir="$macPackageDir/../../../concordium-node"
readonly consensusDir="$macPackageDir/../../../concordium-consensus"
readonly macdylibbundlerDir="$macPackageDir/macdylibbundler-1.0.0"
readonly distDir="$macPackageDir/dist"
# readonly distLibsDir="$distDir/libs"

# Cleanup
[ -d "$distDir" ] && step "Cleaning '$distDir' folder" && rm -r "$distDir"

# Compile consensus
cd "$consensusDir"
step "Building Consensus..."
stack build
step "Done"

# Compile node
cd "$nodeDir"
step "Building Node..."
cargo build --release
step "Done"

# Get concordium-node binary
step "Copy concordium-node binary to '$distDir'.."
mkdir "$distDir"
cp "$nodeDir/target/release/concordium-node" "$distDir"
step "Done"

# Fetch dylibbundler and build it
# TODO: skip if already built
step "Getting dylibbundler..."
step " -- Downloading..."
cd "$macPackageDir"
curl -sSL "https://github.com/auriamg/macdylibbundler/archive/refs/tags/1.0.0.zip" > "dylibbundler.zip" && step " -- Unzipping..." && unzip "dylibbundler.zip" && step " -- Building..." && cd "$macdylibbundlerDir" && make
step "Done"

# Collect dylibs
step "Collecting dylibs with dylibbundler"

concordiumDylibDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
stackSnapshotDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
stackLibDirs=$(find "$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
readonly concordiumDylibDir
readonly stackSnapshotDir
readonly stackLibDirs

cd "$distDir"
"$macdylibbundlerDir/dylibbundler" --fix-file "$distDir/concordium-node" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
    -s "$concordiumDylibDir" \
    -s "$stackSnapshotDir" \
    $stackLibDirs
step "Done"
