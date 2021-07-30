Write-Output "Building Windows node installer..."

# Move to the location of the script so that relative paths make sense
Push-Location $PSScriptRoot

try {

    # Build the custom actions DLL. This provides custom functionality used by the installer.
    Write-Output "Building custom-actions.dll..."
    Push-Location custom-actions
    cargo build --release
    Pop-Location
    if ($LASTEXITCODE -ne 0) { throw "Failed building custom-actions.dll" }

    # Create path bindings for the various sources.
    $Binds = @(
        "-b", "consensus=..\..\..\concordium-consensus",
        "-b", "node=..\..\..\concordium-node\target\release",
        "-b", "service=..\target\x86_64-pc-windows-msvc\release"
        "-b", "ca=.\custom-actions\target\x86_64-pc-windows-msvc\release",
        "-b", "res=.\resources")

    # We add stacks extra-library-dirs as bindings so the installer can find
    # the third-party dll dependencies.
    $StackLibsStr = stack path --extra-library-dirs
    $StackLibsArr = $StackLibsStr.Split(",").Trim()
    Foreach ($LibDir in $StackLibsArr) {
        $Binds += "-b", ("lib=" + $LibDir)
    }

    Write-Output "Compiling installer..."
    candle -arch x64 -ext WixUtilExtension Node.wxs CustomDlgs.wxs
    if ($LASTEXITCODE -ne 0) { throw "Failed compiling installer" }
    Write-Output "Linking installer..."
    light $Binds -ext WixUIExtension -ext WixUtilExtension -out Node.msi Node.wixobj CustomDlgs.wixobj
    if ($LASTEXITCODE -ne 0) { throw "Failed linking installer" }

}
finally {
    Pop-Location
}