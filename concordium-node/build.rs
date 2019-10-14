extern crate protoc_grpcio;
extern crate regex;
extern crate walkdir;

#[cfg(feature = "s11n_capnp")]
extern crate capnpc;

#[cfg(feature = "s11n_fbs")]
extern crate flatc_rust;
#[cfg(feature = "s11n_fbs")]
use std::path::Path;

use regex::Regex;
use std::{fs, io::Write};

fn main() {
    // Compile the Cap'n'Proto schema
    #[cfg(feature = "s11n_capnp")]
    capnpc::CompilerCommand::new()
        .edition(::capnpc::RustEdition::Rust2018)
        .src_prefix("src/network/serialization")
        .file("src/network/serialization/p2p.capnp")
        .run()
        .expect("Can't compile the Cap'n'Proto schema");

    // Compile the flatbuffers schema
    #[cfg(feature = "s11n_fbs")]
    flatc_rust::run(flatc_rust::Args {
        inputs: &[Path::new("src/network/serialization/schema.fbs")],
        out_dir: Path::new("target/"),
        ..Default::default()
    })
    .expect("Can't compile the flatbuffers schema");

    // Build GRPC
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    let proto_root = format!("{}/src/proto", cargo_dir);
    println!(
        "cargo:rerun-if-changed={}",
        format!("{}/concordium_p2p_rpc.proto", proto_root)
    );
    protoc_grpcio::compile_grpc_protos(
        &["concordium_p2p_rpc.proto"],
        &[proto_root.clone()],
        &proto_root,
    )
    .expect("Failed to compile gRPC definitions!");

    // Walk through the proto_root directory and replace the
    // generated `allow(clippy)` directive with `allow(clippy::all)`
    // which is the new syntax.
    //
    // This can not be directly implemented into protobuf, see:
    // https://github.com/stepancheg/rust-protobuf/issues/331
    let walker = walkdir::WalkDir::new(proto_root)
        .into_iter()
        .filter_map(Result::ok);
    for entry in walker {
        if !entry.file_type().is_dir() {
            let contents =
                fs::read_to_string(entry.path()).expect("Something went wrong reading the file");
            let re = Regex::new(r"allow\(clippy\)").unwrap();
            let new_contents = re.replace(&contents, "allow(clippy::all)");

            let mut file = fs::File::create(&entry.path()).unwrap();
            file.write_all(new_contents.as_bytes()).unwrap();
        }
    }
}
