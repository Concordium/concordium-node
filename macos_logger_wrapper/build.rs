fn main() -> std::io::Result<()> {
    // MacOS logger
    #[cfg(target_os = "macos")]
    cc::Build::new().file("./macos_log_wrapper.c").compile("macos_log_wrapper");
    Ok(())
}
