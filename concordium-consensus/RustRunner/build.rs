fn main() {
    println!("cargo:rustc-link-search=native=../Concordium");
    println!("cargo:rustc-link-lib=dylib=HSdll");
}
