extern crate protoc_grpcio;
extern crate walkdir;

#[cfg(feature = "s11n_capnp")]
extern crate capnpc;

use std::process::Command;

fn main() {
    // Compile capnpc
    #[cfg(feature = "s11n_capnp")]
    ::capnpc::CompilerCommand::new()
        .edition(::capnpc::RustEdition::Rust2018)
        .src_prefix("src/network/serialization")
        .file("src/network/serialization/p2p.capnp")
        .run()
        .expect("CapNP P2P compiler command");

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
    #[cfg(target_family = "unix")]
    {
        let walker = walkdir::WalkDir::new(proto_root)
            .into_iter()
            .filter_map(Result::ok);
        for entry in walker {
            if !entry.file_type().is_dir() {
                Command::new("sed")
                    .args(&[
                        "-i",
                        "s/allow(clippy)/allow(clippy::all)/g",
                        &format!("{}", entry.path().display()),
                    ])
                    .spawn()
                    .expect("sed replacement command failed");
            }
        }
    }
}
