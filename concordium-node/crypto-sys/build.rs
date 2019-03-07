use std::env;
use std::path::{ PathBuf, Path };

/// *IMPORTANT* This packet *requires* that *Concordium-crypto* C lib is installed into your
/// system. Please check `README.md` at `https://gitlab.com/Concordium/crypto/tree/master`
/// to know how to build and install it.
fn main()
{
    println!("cargo:rustc-link-search=native=/usr/local/lib");
    println!("cargo:rustc-link-lib=concordium-crypto");

    let out_path = PathBuf::from( env::var( "OUT_DIR").unwrap());
    let lib_include_path = Path::new( "/usr/local/include/concordium-crypto");
    let builder = bindgen::Builder::default();

    // It generates Rust bindings just for functions defined using `whitelist_function` and
    // any types needed by their interface.
    let bindings = builder
        .header( lib_include_path.join( "ec_vrf_ed25519-sha256.h").to_str().unwrap())
        .whitelist_function( "priv_key")
        .whitelist_function( "public_key")
        .generate()
        .expect( "Crypto Bind Generator has been unabled to generate bindings");

    bindings.write_to_file( out_path.join("bindings.rs"))
        .expect( "Crypto Bind Generator cannot write bindings");
}
