# Concordium Node for MacOS

## Prerequities

- Xcode
  - Download from App Store
- Xcode toolkit
  - `$ xcode-select --install`
- [Build concordium-node dependencies](../../../concordium-node/README.md)


## Building

Simply run the build script with a version number.
For example: 

```bash
$ ./build 1.0.2
```

The build script will ask whether you want to build and sign, or simply build
the installer.

### Sign and Notarize

For releases, the code and installer should be signed and the installer
notarized.

Signing requires a valid [Developer ID Application
Certificate](https://developer.apple.com/support/certificates/) and Developer ID
Installer Certificate in the Keychain of the machine used for building, and a valid Apple Developer ID has to be logged on the same machine.

For the notarizing process to succeed, the Apple ID and it's password also needs to be available as the following
environment variables:

-   APPLEID=<example@e-mail.com>
-   APPLEIDPASS=The password for the Apple ID

For the APPLEIDPASS, setting up an [app-specific password](https://support.apple.com/en-us/HT204397) for the Apple ID is recommended.


## How the installer works

1. During the installation you configure your nodes (names, run on startup etc.)
  - This configuration pane is the "installer plugin" Xcode project
    *NodeConfigurationInstallerPlugin*.
  - It saves the configuration options in the file
    `/tmp/software.concordium.node.install.config`.
2. After clicking *install* the `preinstall` script runs.
  - It cleans up previous installs by:
    - Stopping any running node and collector services.
    - Deletes `/Library/Concordium Node` if existing.
  - `preinstall` logs to the file `/var/log/install.log` 
3. Then the payload is installed:

    ```
    .
    ├── Applications
    │   └── Concordium\ Node
    │       ├── Concordium\ Node\ Start\ Mainnet.app
    │       │   └── Contents
    │       │       ├── Info.plist
    │       │       ├── MacOS
    │       │       │   ├── run.applescript
    │       │       │   └── runApplescript.sh
    │       │       └── Resources
    │       │           └── icon.png
    │       ├── Concordium\ Node\ Start\ Testnet.app
    │       │   └── ...
    │       ├── Concordium\ Node\ Stop\ Mainnet.app
    │       │   └── ...
    │       ├── Concordium\ Node\ Stop\ Testnet.app
    │       │   └── ...
    │       └── Concordium\ Node\ Uninstaller.app
    │           └── ...
    └── Library
        ├── Application\ Support
        │   └── Concordium\ Node
        │       ├── Mainnet
        │       │   ├── Config
        │       │   └── Data
        │       │       └── genesis.dat
        │       └── Testnet
        │           ├── Config
        │           └── Data
        │               └── genesis.dat
        └── Concordium\ Node
            ├── LaunchDaemons
            │   ├── software.concordium.mainnet.node-collector.plist
            │   ├── software.concordium.mainnet.node.plist
            │   ├── software.concordium.testnet.node-collector.plist
            │   └── software.concordium.testnet.node.plist
            ├── concordium-node
            ├── libs
            │   ├── libHSQuickCheck-2.14.2-88oxj61ONgG1QbYzt1cUFu-ghc8.10.4.dylib
            │   ├── ...
            │   └── libwasm_chain_integration.dylib
            └── node-collector    
    ```

4. The `postinstall` script runs:
  - Adds `concordium-node` and `concordium-node-collector` to path by adding
    symlinks in `/usr/local/bin`. This enables advanced users to easily run
    nodes with custom options (e.g. a local chain).
  - Configures the services according to options in
    `/tmp/software.concordium.node.installer.config`
      - Run on startup: Create symlinks to the service files:
        `/Library/LaunchDaemons/software.concordium.mainnet.node.plist ->
        /Library/Concordium Node/LaunchDaemons/software.concordium.mainnet.node.plist`
        (MacOS will automatically start services in `/Library/LaunchDaemons` on
        boot).
      - Run after install: Run the service(s) with `launchctl`.
      - Report to network dashboard: Create a file which existence is checked
        by `Concordium Node Start __NET__.app`. E.g. for mainnet: `/Library/Concordium Node/REPORT_TO_NETWORK_DASHBOARD_MAINNET`
  - `postinstall` logs to the file `/var/log/install.log` 

## How the build process works
