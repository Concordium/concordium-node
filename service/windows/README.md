# Windows Concordium Node Runner Service

The Concordium Node Runner Service is an executable that runs as a Windows service and starts, stops and monitors Concordium nodes.

Assuming that it is installed and configured correctly, `node-runner-service.exe` can be invoked with one of the following command line options:

* `start` &mdash; starts the service
* `stop` &mdash; stops the service
* `configure` &mdash; opens the configuration file for editing

Note that `node-runner-service.exe` requires administrator permissions to run.

The following command line options are also provided, but it is not recommended to use them (instead, use the [installer](#installer)):

* `install` &mdash; registers `node-runner-service.exe` as a service and an event source
* `remove` &mdash; deregisters `node-runner-service.exe` as a service and an event source

Note that when the service is run, it looks for a configuration TOML file at the location specified by the `Config` value in the registry key `HKEY_LOCAL_MACHINE\SOFTWARE\Concordium\Node Runner`.
(The [installer](#installer) creates this entry and configuration file for you.)
The configuration file allows for fine control over how nodes are run.
Full details of the configuration options available are to be found in [Configuration.md](Configuration.md).

## Building
Building the Node Runner Service currently requires the `msvc` rust toolchain.
(Note, this is different to the node itself, which requires the `gnu` rust toolchain.)
Building is only supported on Windows.
Currently, we support rust version 1.53.

The service can be built with the following command:
```
cargo +1.53.0-msvc build
```

If the toolchain is not already installed, it can be done with the following command:
```
rustup toolchain install 1.53.0-x86_64-pc-windows-msvc
```

## Installer

For ease of use, we provide a Windows installer for the node, which installs the node and the node runner service.
The sources for the installer are provided in the `installer` subdirectory.
The installer is built with the [WiX Toolset](https://wixtoolset.org/releases/) (v3.11.2).
A powershell script [installer/build.ps1](installer/build.ps1) is provided to build the installer.
The build script requires that the WiX tools (`candle` and `light`) are available on the path.
The script builds a DLL from rust sources that provides custom actions that are used in the installer.
The script does not build the node or the node runner service itself, but assumes that they are already built.

### Updating for a new release

For a new release, the installer should be updated so that it can upgrade an existing installation.
At a minimum, the following two changes are required to `Node.wxs`:

1. Update the `VersionNumber` variable to the new version number.
   To count as a new version, it must be higher than the old version when restricted to the first three components.
   (In practise, we only have three components to the version number.)
2. Update the `Id` attribute of the `Product` element to a fresh GUID.
   (The GUID can be generated with any reasonable GUID-generating tool.)

The `MajorUpgrade` element should cause the new installer to remove an existing version before installation.
For this, it is important that the `UpgradeCode` should remain the same.
(Note, with the current setup, removing an existing installation will remove all installed files, but will not remove node data and logs.)
It is important that the installer should be tested for upgrading at each release.