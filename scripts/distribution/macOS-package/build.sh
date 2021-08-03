#!/usr/bin/env bash
set -euo pipefail


# Parameters
readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
readonly developerIdApplication="Developer ID Application: Concordium Software Aps (K762RM4LQ3)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps (K762RM4LQ3)"

readonly GREEN='\033[0;32m'
readonly NC='\033[0m' # No Color

logInfo () {
    printf "\n${GREEN}$@${NC}\n"
}

readonly ghcVariant="x86_64-osx-ghc-8.10.4"

# Get the location of this script.
macPackageDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
readonly macPackageDir
readonly nodeDir="$macPackageDir/../../../concordium-node"
readonly consensusDir="$macPackageDir/../../../concordium-consensus"
readonly toolsDir="$macPackageDir/tools"
readonly macdylibbundlerDir="$toolsDir/macdylibbundler-1.0.0"
readonly installDir="/Library"
readonly templateDir="$macPackageDir/template"
readonly buildDir="$macPackageDir/build"
readonly payloadDir="$buildDir/payload"
readonly versionedBinDir="$payloadDir/Concordium Node/$version"
readonly packagesDir="$buildDir/packages"
readonly pkgFile="$packagesDir/concordium-node.pkg"
readonly signedPkgFile="$packagesDir/concordium-node-signed.pkg"


function clean() {
    if [ -d "$buildDir" ]; then
        logInfo "Cleaning '$buildDir' folder"
        rm -r "$buildDir"
    fi
}

function replaceVersionPlaceholder() {
    local theFile=${1:?"replaceVersionPlaceholder expects 1 parameter: file"}
    sed -i '' -e 's/__VERSION__/'"$version"'/g' "$theFile"
}

function createBuildDirFromTemplate() {
    logInfo "Creating build folder from template..."
    cp -r "$templateDir" "$buildDir"
    mkdir "$versionedBinDir"
    replaceVersionPlaceholder "$buildDir/distribution.xml"
    replaceVersionPlaceholder "$buildDir/scripts/postinstall"
    replaceVersionPlaceholder "$payloadDir/LaunchDaemons/software.concordium.node.plist"
    replaceVersionPlaceholder "$payloadDir/LaunchDaemons/software.concordium.node-collector.plist"

    chmod -R 755 "$buildDir/scripts"

    logInfo "Done"
}

function compileConsensus() {
    cd "$consensusDir"
    logInfo "Building Consensus..."
    stack build
    logInfo "Done"
}

function compileNodeAndCollector() {
    cd "$nodeDir"
    logInfo "Building Node and Collector..."
    cargo build --bin concordium-node --bin node-collector --features collector --release
    logInfo "Done"
}

function compileInstallerPlugin() {
    logInfo "Building installer plugin..."
    xcodebuild -project "$macPackageDir/NodeConfigurationInstallerPlugin/NodeConfigurationInstallerPlugin.xcodeproj" > /dev/null
    logInfo "Done"
}

function compile() {
    compileConsensus
    compileNodeAndCollector
    compileInstallerPlugin
}

function copyInstallerPluginData() {
    logInfo "Copying installer plugin data to build folder"
    cp -r "$macPackageDir/NodeConfigurationInstallerPlugin/build/Release/NodeConfigurationInstallerPlugin.bundle" "$buildDir/plugins"
    cp "$macPackageDir/NodeConfigurationInstallerPlugin/NodeConfigurationInstallerPlugin/InstallerSections.plist" "$buildDir/plugins"
    logInfo "Done"
}

function copyBinaries() {
    logInfo "Copy concordium-node and node-collector binaries to '$versionedBinDir'.."
    cp "$nodeDir/target/release/concordium-node" "$versionedBinDir"
    cp "$nodeDir/target/release/node-collector" "$versionedBinDir"
    logInfo "Done"
}

function downloadGenesis() {
    logInfo "Downloading genesis.dat"
    curl -sSL "https://distribution.mainnet.concordium.software/data/genesis.dat" > "$payloadDir/Application Support/Concordium Node/Mainnet/Data/genesis.dat"
    logInfo "Done"
}


function getDylibbundler() {
    logInfo "Getting dylibbundler..."

    if test -f "$macdylibbundlerDir/dylibbundler"
    then
        logInfo "Done (skipped: already exists)"
    else
        logInfo " -- Downloading..."
        mkdir "$toolsDir"
        cd "$macPackageDir"
        curl -sSL "https://github.com/auriamg/macdylibbundler/archive/refs/tags/1.0.0.zip" > "$toolsDir/dylibbundler.zip" \
                    && logInfo " -- Unzipping..." \
                    && cd "$toolsDir" \
                    && unzip "dylibbundler.zip" \
                    && logInfo " -- Building..." \
                    && cd "$macdylibbundlerDir" \
                    && make
        logInfo "Done"
    fi
}

function collectDylibsFor() {
    local fileToFix=${1:?"Missing file to fix with dylibbundler"};
    cd "$versionedBinDir"
    "$macdylibbundlerDir/dylibbundler" --fix-file "$fileToFix" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
        -s "$concordiumDylibDir" \
        -s "$stackSnapshotDir" \
        $stackLibDirs # Unquoted on purpose to use as arguments correctly
}

function collectDylibs() {
    logInfo "Collecting dylibs with dylibbundler (this will take a few minutes)..."

    concordiumDylibDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
    stackSnapshotDir=$(stack --stack-yaml "$consensusDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
    stackLibDirs=$(find "$(stack --stack-yaml "$consensusDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
    readonly concordiumDylibDir
    readonly stackSnapshotDir
    readonly stackLibDirs

    logInfo " -- Processing concordium-node"
    collectDylibsFor "$versionedBinDir/concordium-node" &> /dev/null
    logInfo " -- Processing node-collector"
    collectDylibsFor "$versionedBinDir/node-collector" &> /dev/null

    logInfo "Done"
}

function signBinaries() {
    logInfo "Signing binaries..."
    find "$payloadDir" \
        -type f \
        -execdir sudo codesign -f --options runtime -s "$developerIdApplication" {} \;
    logInfo "Done"
}

function ensureDirExists() {
    local theDir=${1:?"ensureDirExists requires 1 parameter: directory"}
    if [ ! -d "$theDir" ]; then
        mkdir "$theDir"
    fi
}

function buildPackage() {
    logInfo "Building package..."
    ensureDirExists "$packagesDir"
    pkgbuild --identifier software.concordium.node \
        --version "$version" \
        --scripts "$buildDir/scripts" \
        --install-location "$installDir" \
        --root "$payloadDir" \
        "$pkgFile"
    logInfo "Done"
}

function buildProduct() {
    logInfo "Building product..."
    ensureDirExists "$packagesDir"
    productbuild \
        --distribution "$buildDir/distribution.xml" \
        --package-path "$packagesDir" \
        --resources "$buildDir/resources" \
        --plugins "$buildDir/plugins" \
        --sign "$developerIdInstaller" \
        "$signedPkgFile"
    logInfo "Done"
}

function notarize() {
    logInfo "Notarizing..."
    # FIXME: The keychain-profile part will not work on other computers
    xcrun notarytool submit \
        "$signedPkgFile" \
        --keychain-profile "notarytool" \
        --wait
    logInfo "Done"
}

function staple() {
    logInfo "Stapling..."
    xcrun stapler staple "$signedPkgFile"
    logInfo "Done"
}

function main() {
    clean
    createBuildDirFromTemplate
    compile
    copyBinaries
    copyInstallerPluginData
    downloadGenesis
    getDylibbundler
    collectDylibs
    signBinaries
    buildPackage
    buildProduct
    notarize
    staple
}

main
