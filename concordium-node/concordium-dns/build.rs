use std::{
    env,
    fs::File,
    io::{self, Write},
    path::PathBuf,
    process::Stdio,
};
use tempdir::TempDir;

fn available(s: &str, extra_args: &[String]) -> io::Result<bool> {
    let temp = TempDir::new(s).expect("temporary dir");
    let main = temp.path().join("main.c");
    let source = format!(
        r#"
#include <unbound.h>
int main(void) {{
    void * _ = {};
}}
"#,
        s
    );
    File::create(&main).and_then(|mut f| f.write_all(source.as_bytes()))?;
    cc::Build::new()
        .cargo_metadata(false)
        .get_compiler()
        .to_command()
        .current_dir(temp.path())
        .args(extra_args)
        .arg("-lunbound")
        .arg(main.to_string_lossy().as_ref())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .map(|o| o.status.success())
}

fn main() {
    let mut extra_args = Vec::new();

    if let Some(dir) = env::var("UNBOUND_DIR").ok().map(PathBuf::from) {
        let lib_dir = dir.join("lib").to_string_lossy().into_owned();
        let include_dir = dir.join("include").to_string_lossy().into_owned();
        extra_args.push(format!("-L{}", lib_dir));
        extra_args.push(format!("-I{}", include_dir));
        println!("cargo:include={}", include_dir);
        println!("cargo:rustc-link-search=native={}", lib_dir);
    };

    let mode = if env::var_os("UNBOUND_STATIC").is_some() {
        "static"
    } else {
        "dylib"
    };
    println!("cargo:rustc-link-lib={}=unbound", mode);

    for s in &["ub_ctx_set_stub", "ub_ctx_add_ta_autr"] {
        if available(s, &extra_args).expect(s) {
            println!("cargo:rustc-cfg={}", s);
        }
    }
}
