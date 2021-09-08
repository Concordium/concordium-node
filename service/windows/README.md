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

Note that for future releases of the installer, the install script should be updated to reflect the new version, and the product GUID should be refreshed.
