#!/usr/bin/env bash
set -euo pipefail


# Parameters
readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
readonly developerIdApplication="Developer ID Application: Concordium Software Aps (K762RM4LQ3)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps (K762RM4LQ3)"
readonly year="2021" # Used for copyright notice.

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
readonly installDir="/"
readonly templateDir="$macPackageDir/template"
readonly buildDir="$macPackageDir/build"
readonly payloadDir="$buildDir/payload"
readonly libraryPayloadDir="$payloadDir/Library"
readonly packagesDir="$buildDir/packages"
readonly pkgFile="$packagesDir/concordium-node.pkg"
readonly signedPkgFile="$packagesDir/concordium-node-signed.pkg"


function clean() {
    if [ -d "$buildDir" ]; then
        logInfo "Cleaning '$buildDir' folder"
        rm -r "$buildDir"
    fi
}

function replacePlaceholderInFile() {
    local theFile=${1:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    local placeholder=${2:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    local replacement=${3:?"replacePlaceholderInFile expects 3 parameters: file, placeholder, replacement"}
    sed -i '' -e 's/'"$placeholder"'/'"$replacement"'/g' "$theFile"
}

function replaceVersionPlaceholder() {
    local theFile=${1:?"replaceVersionPlaceholder expects 1 parameter: file"}
    replacePlaceholderInFile "$theFile" "__VERSION__" "$version"
}

# Should be called after build folder has been created.
function createHelperAppsFromTemplate() {
    local startNodeMainnet="$payloadDir/Applications/Concordium Node/Concordium Node Start Mainnet.app"
    local startNodeTestnet="$payloadDir/Applications/Concordium Node/Concordium Node Start Testnet.app"
    local stopNodeMainnet="$payloadDir/Applications/Concordium Node/Concordium Node Stop Mainnet.app"
    local stopNodeTestnet="$payloadDir/Applications/Concordium Node/Concordium Node Stop Testnet.app"
    local nodeUninstaller="$payloadDir/Applications/Concordium Node/Concordium Node Uninstaller.app"

    # Use 'mv' to replace the __NET__ version template folder.
    cp -r "$payloadDir/Applications/Concordium Node/Concordium Node Start __NET__.app" "$startNodeMainnet"
    mv "$payloadDir/Applications/Concordium Node/Concordium Node Start __NET__.app" "$startNodeTestnet"
    cp -r "$payloadDir/Applications/Concordium Node/Concordium Node Stop __NET__.app" "$stopNodeMainnet"
    mv "$payloadDir/Applications/Concordium Node/Concordium Node Stop __NET__.app" "$stopNodeTestnet"

    replacePlaceholderInFile  "$startNodeMainnet/Contents/MacOS/run.applescript" "__NET__" "mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/MacOS/run.applescript" "__NET_UPPERCASE__" "MAINNET"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__NET_CAPITALISED__" "Mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__NET__" "mainnet"
    replacePlaceholderInFile  "$startNodeMainnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$startNodeMainnet/Contents/Info.plist"

    replacePlaceholderInFile  "$startNodeTestnet/Contents/MacOS/run.applescript" "__NET__" "testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/MacOS/run.applescript" "__NET_UPPERCASE__" "TESTNET"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__NET_CAPITALISED__" "Testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__NET__" "testnet"
    replacePlaceholderInFile  "$startNodeTestnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$startNodeTestnet/Contents/Info.plist"

    replacePlaceholderInFile  "$stopNodeMainnet/Contents/MacOS/run.applescript"  "__NET__" "mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__NET_CAPITALISED__" "Mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__NET__" "mainnet"
    replacePlaceholderInFile  "$stopNodeMainnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$stopNodeMainnet/Contents/Info.plist"

    replacePlaceholderInFile  "$stopNodeTestnet/Contents/MacOS/run.applescript"  "__NET__" "testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__NET_CAPITALISED__" "Testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__NET__" "testnet"
    replacePlaceholderInFile  "$stopNodeTestnet/Contents/Info.plist" "__YEAR__" "$year"
    replaceVersionPlaceholder "$stopNodeTestnet/Contents/Info.plist"

    replaceVersionPlaceholder "$nodeUninstaller/Contents/Info.plist"
}

function createBuildDirFromTemplate() {
    logInfo "Creating build folder from template..."

    cp -r "$templateDir" "$buildDir"

    mkdir "$buildDir/plugins"
    mkdir "$libraryPayloadDir/Application Support/Concordium Node/Mainnet/Config"
    mkdir "$libraryPayloadDir/Application Support/Concordium Node/Testnet/Config"

    createHelperAppsFromTemplate

    replaceVersionPlaceholder "$buildDir/resources/welcome.html"
    replacePlaceholderInFilde "$buildDir/resources/conclusion.html" "__YEAR__" "$year"

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
    logInfo "Copy concordium-node and node-collector binaries to '$libraryPayloadDir/Concordium Node/'.."
    cp "$nodeDir/target/release/concordium-node" "$libraryPayloadDir/Concordium Node"
    cp "$nodeDir/target/release/node-collector" "$libraryPayloadDir/Concordium Node"
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
    cd "$libraryPayloadDir/Concordium Node"
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
    collectDylibsFor "$libraryPayloadDir/Concordium Node/concordium-node" &> /dev/null
    logInfo " -- Processing node-collector"
    collectDylibsFor "$libraryPayloadDir/Concordium Node/node-collector" &> /dev/null

    logInfo "Done"
}

function signBinaries() {
    logInfo "Signing binaries..."

    find "$libraryPayloadDir" \
        -type f \
        -execdir sudo codesign -f --options runtime -s "$developerIdApplication" {} \;

    sudo codesign -f --options runtime -s "$developerIdApplication" \
        "$buildDir/plugins/NodeConfigurationInstallerPlugin.bundle"

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
        --component-plist "$buildDir/components.plist" \
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
    getDylibbundler
    collectDylibs
    signBinaries
    buildPackage
    buildProduct
    notarize
    staple
}

main
