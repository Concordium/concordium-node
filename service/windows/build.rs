#[cfg(feature = "winres")]
fn main() -> std::io::Result<()> {
    let mut res = winres::WindowsResource::new();
    // Require administrator permissions
    res.set_manifest(
        r#"
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
<trustInfo xmlns="urn:schemas-microsoft-com:asm.v3">
    <security>
        <requestedPrivileges>
            <requestedExecutionLevel level="requireAdministrator" uiAccess="false" />
        </requestedPrivileges>
    </security>
</trustInfo>
</assembly>
"#,
    );
    res.set_icon("resources/icon.ico");
    res.compile()?;
    Ok(())
}

#[cfg(not(feature = "winres"))]
fn main() {}
