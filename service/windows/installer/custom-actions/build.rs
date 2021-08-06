fn main() -> std::io::Result<()> {
    println!("cargo:rustc-link-lib=dylib=Msi");
    Ok(())
}
