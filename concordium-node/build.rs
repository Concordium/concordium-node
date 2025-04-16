use std::{env, path::Path};
#[cfg(any(all(unix, not(feature = "static")), windows))]
use std::{process::Command, str};

#[cfg(all(not(feature = "static"), target_os = "linux"))]
const GHC_VARIANT: &str = "x86_64-linux-ghc-9.6.6";
#[cfg(all(not(feature = "static"), target_os = "macos", target_arch = "x86_64"))]
const GHC_VARIANT: &str = "x86_64-osx-ghc-9.6.6";
#[cfg(all(not(feature = "static"), target_os = "macos", target_arch = "aarch64"))]
const GHC_VARIANT: &str = "aarch64-osx-ghc-9.6.6";

#[cfg(not(feature = "static"))]
fn command_output(cmd: &mut Command) -> String {
    str::from_utf8(&cmd.output().unwrap().stdout).unwrap().trim_end().to_string()
}

fn main() -> std::io::Result<()> {
    let cargo_dir = env!("CARGO_MANIFEST_DIR");
    // Compile the flatbuffers schema
    println!("cargo:rerun-if-changed={}/src/network/serialization/schema.fbs", cargo_dir);
    flatc_rust::run(flatc_rust::Args {
        inputs: &[Path::new("src/network/serialization/schema.fbs")],
        out_dir: Path::new("target/"),
        ..Default::default()
    })
    .expect("Can't compile the flatbuffers schema");

    // Build GRPC

    let proto_root_input = format!("{}/../concordium-base/concordium-grpc-api", cargo_dir);

    #[cfg(not(feature = "static"))]
    {
        // Traverse the directory to link all of the libs in ghc.
        #[cfg(windows)]
        {
            // We assume that the consensus library was built using the stack.yaml file in
            // the root of the repository. (This is what the Windows build script does.)
            let stack_install_root_command = command_output(Command::new("stack").args([
                "--stack-yaml",
                "../stack.yaml",
                "path",
                "--local-install-root",
            ]));
            let stack_install_root = Path::new(&stack_install_root_command);
            let dll_location = stack_install_root.join("lib");

            println!("cargo:rustc-link-search=native={}", dll_location.to_string_lossy());
            println!("cargo:rustc-link-lib=dylib=concordium-consensus");
        }
        #[cfg(not(windows))]
        {
            // If the haskell root is provided we use it. In this case we assume.
            // that the libraries are put in the appropriate location and named as listed
            // below.
            if let Ok(root) = env::var("CONCORDIUM_HASKELL_ROOT") {
                if let Err(e) = std::fs::read_dir(&root) {
                    println!("Cannot read CONCORDIUM_HASKELL_ROOT: {}", e);
                    return Err(e);
                }
                println!("cargo:rustc-link-search=native={}", root);
                println!("cargo:rustc-link-lib=dylib=concordium-consensus");
                println!("cargo:rustc-link-lib=dylib=HSconcordium-consensus-0.1.0.0");
                println!("cargo:rustc-link-lib=dylib=HSconcordium-base-0.1.0.0");
                println!("cargo:rustc-link-lib=dylib=HSlmdb-0.2.5");
            } else {
                // otherwise auto-discover the directories via stack
                let stack_install_root_command = command_output(Command::new("stack").args([
                    "--stack-yaml",
                    "../concordium-consensus/stack.yaml",
                    "path",
                    "--local-install-root",
                ]));
                let stack_install_root = Path::new(&stack_install_root_command);
                let stack_install_lib = stack_install_root.join("lib");

                println!("cargo:rustc-link-search={}", stack_install_lib.to_string_lossy());
                println!("cargo:rustc-link-lib=dylib=concordium-consensus");

                let local_package = stack_install_lib.join(GHC_VARIANT);
                let dir = std::fs::read_dir(&local_package)?;

                println!("cargo:rustc-link-search={}", local_package.to_string_lossy());
                // Traverse all the files in the lib directory, and add all that end with
                // `.DYLIB_EXTENSION` to the linked libraries list.
                for dir_entry in dir {
                    let path = dir_entry?.path();
                    if let Some(true) =
                        path.extension().map(|ext| ext.to_string_lossy() == DYLIB_EXTENSION)
                    // FIXME: On a mac we should ignore case, but not on linux
                    {
                        let name = path
                            .file_stem()
                            .unwrap()
                            .to_str()
                            .unwrap()
                            .strip_prefix("lib")
                            .unwrap();
                        println!("cargo:rustc-link-lib=dylib={}", name);
                    }
                }
                if let Ok(ref extra_libs_path) = env::var("EXTRA_LIBS_PATH").as_ref() {
                    println!("cargo:rustc-link-search=native={}", extra_libs_path);
                }
                let ghc_lib_dir = link_ghc_libs()?;
                let lib_path = if cfg!(target_os = "linux") {
                    "LD_LIBRARY_PATH"
                } else {
                    "DYLD_LIBRARY_PATH"
                };

                println!(
                    "cargo:rustc-env={}={}:{}",
                    lib_path,
                    ghc_lib_dir.as_path().to_string_lossy(),
                    stack_install_lib.as_path().to_string_lossy()
                );
            }
        }
    }

    #[cfg(feature = "static")]
    link_static_libs()?;

    build_grpc2(&proto_root_input)?;
    Ok(())
}

// Compile the types for GRPC2 API and generate a service description for the
// GRPC2 interface.
fn build_grpc2(proto_root_input: &str) -> std::io::Result<()> {
    {
        let types = format!("{}/v2/concordium/types.proto", proto_root_input);
        println!("cargo:rerun-if-changed={}", types);
        let plts = format!("{}/v2/concordium/protocol-level-tokens.proto", proto_root_input);
        println!("cargo:rerun-if-changed={}", plts);
        let kernel = format!("{}/v2/concordium/kernel.proto", proto_root_input);
        println!("cargo:rerun-if-changed={}", kernel);
        prost_build::compile_protos(&[kernel, plts, types], &[proto_root_input])?;
    }

    // Because we serialize messages in Haskell we need to construct the service
    // description here manually using a custom codec. This custom codec allows us
    // to just take the message that is produced by consensus and forward it to
    // the client without any re-encoding.
    // For this reason the output type of each query is some representation of a
    // byte array.
    //
    // For most queries data is produced for that specific query, and so there is
    // only one producer, and one consumer. In such a case we use `Vec<u8>` as
    // the return type of the query. In some cases the data is produced for
    // multiple consumers. In particular this is the case for `GetBlocks` and
    // `GetFinalizedBlocks`. There data is maintained in a shared channel and then
    // sent to all the listeners. To avoid copying and retaining data for each
    // client we use `Arc<[u8]>` instead. This way only a single copy of the data to
    // be sent is retained in memory.
    let query_service = tonic_build::manual::Service::builder()
        .name("Queries")
        .package("concordium.v2")
        .method(
            tonic_build::manual::Method::builder()
                .name("get_finalized_blocks")
                .route_name("GetFinalizedBlocks")
                .input_type("crate::grpc2::types::Empty")
                .output_type("Arc<[u8]>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_blocks")
                .route_name("GetBlocks")
                .input_type("crate::grpc2::types::Empty")
                .output_type("Arc<[u8]>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_account_info")
                .route_name("GetAccountInfo")
                .input_type("crate::grpc2::types::AccountInfoRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_account_list")
                .route_name("GetAccountList")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_token_list")
                .route_name("GetTokenList")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_module_list")
                .route_name("GetModuleList")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_module_source")
                .route_name("GetModuleSource")
                .input_type("crate::grpc2::types::ModuleSourceRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_instance_list")
                .route_name("GetInstanceList")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_instance_info")
                .route_name("GetInstanceInfo")
                .input_type("crate::grpc2::types::InstanceInfoRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_instance_state")
                .route_name("GetInstanceState")
                .input_type("crate::grpc2::types::InstanceInfoRequest")
                .output_type("crate::grpc2::types::InstanceStateKvPair")
                .codec_path("tonic::codec::ProstCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("instance_state_lookup")
                .route_name("InstanceStateLookup")
                .input_type("crate::grpc2::types::InstanceStateLookupRequest")
                .output_type("crate::grpc2::types::InstanceStateValueAtKey")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_next_account_sequence_number")
                .route_name("GetNextAccountSequenceNumber")
                .input_type("crate::grpc2::types::AccountAddress")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_consensus_info")
                .route_name("GetConsensusInfo")
                .input_type("crate::grpc2::types::Empty")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_consensus_detailed_status")
                .route_name("GetConsensusDetailedStatus")
                .input_type("crate::grpc2::types::ConsensusDetailedStatusQuery")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_ancestors")
                .route_name("GetAncestors")
                .input_type("crate::grpc2::types::AncestorsRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_item_status")
                .route_name("GetBlockItemStatus")
                .input_type("crate::grpc2::types::TransactionHash")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_cryptographic_parameters")
                .route_name("GetCryptographicParameters")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("crate::grpc2::types::CryptographicParameters")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_info")
                .route_name("GetBlockInfo")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_baker_list")
                .route_name("GetBakerList")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_pool_info")
                .route_name("GetPoolInfo")
                .input_type("crate::grpc2::types::PoolInfoRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_passive_delegation_info")
                .route_name("GetPassiveDelegationInfo")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_blocks_at_height")
                .route_name("GetBlocksAtHeight")
                .input_type("crate::grpc2::types::BlocksAtHeightRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_tokenomics_info")
                .route_name("GetTokenomicsInfo")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("invoke_instance")
                .route_name("InvokeInstance")
                .input_type("crate::grpc2::types::InvokeInstanceRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_pool_delegators")
                .route_name("GetPoolDelegators")
                .input_type("crate::grpc2::types::GetPoolDelegatorsRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_pool_delegators_reward_period")
                .route_name("GetPoolDelegatorsRewardPeriod")
                .input_type("crate::grpc2::types::GetPoolDelegatorsRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_passive_delegators")
                .route_name("GetPassiveDelegators")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_passive_delegators_reward_period")
                .route_name("GetPassiveDelegatorsRewardPeriod")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_branches")
                .route_name("GetBranches")
                .input_type("crate::grpc2::types::Empty")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_election_info")
                .route_name("GetElectionInfo")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_identity_providers")
                .route_name("GetIdentityProviders")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_anonymity_revokers")
                .route_name("GetAnonymityRevokers")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_account_non_finalized_transactions")
                .route_name("GetAccountNonFinalizedTransactions")
                .input_type("crate::grpc2::types::AccountAddress")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_transaction_events")
                .route_name("GetBlockTransactionEvents")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_special_events")
                .route_name("GetBlockSpecialEvents")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_pending_updates")
                .route_name("GetBlockPendingUpdates")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_next_update_sequence_numbers")
                .route_name("GetNextUpdateSequenceNumbers")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_scheduled_release_accounts")
                .route_name("GetScheduledReleaseAccounts")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_cooldown_accounts")
                .route_name("GetCooldownAccounts")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_pre_cooldown_accounts")
                .route_name("GetPreCooldownAccounts")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_pre_pre_cooldown_accounts")
                .route_name("GetPrePreCooldownAccounts")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_baker_earliest_win_time")
                .route_name("GetBakerEarliestWinTime")
                .input_type("crate::grpc2::types::BakerId")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_chain_parameters")
                .route_name("GetBlockChainParameters")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_finalization_summary")
                .route_name("GetBlockFinalizationSummary")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("shutdown")
                .route_name("Shutdown")
                .input_type("crate::grpc2::types::Empty")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("send_block_item")
                .route_name("SendBlockItem")
                .input_type("crate::grpc2::types::SendBlockItemRequest")
                .output_type("crate::grpc2::types::TransactionHash")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_peers_info")
                .route_name("GetPeersInfo")
                .input_type("crate::grpc2::types::Empty")
                .output_type("crate::grpc2::types::PeersInfo")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("peer_connect")
                .route_name("PeerConnect")
                .input_type("crate::grpc2::types::IpSocketAddress")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("peer_disconnect")
                .route_name("PeerDisconnect")
                .input_type("crate::grpc2::types::IpSocketAddress")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_banned_peers")
                .route_name("GetBannedPeers")
                .input_type("crate::grpc2::types::Empty")
                .output_type("crate::grpc2::types::BannedPeers")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("ban_peer")
                .route_name("BanPeer")
                .input_type("crate::grpc2::types::PeerToBan")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("unban_peer")
                .route_name("UnbanPeer")
                .input_type("crate::grpc2::types::BannedPeer")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("dump_start")
                .route_name("DumpStart")
                .input_type("crate::grpc2::types::DumpRequest")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("dump_stop")
                .route_name("DumpStop")
                .input_type("crate::grpc2::types::Empty")
                .output_type("crate::grpc2::types::Empty")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_node_info")
                .route_name("GetNodeInfo")
                .input_type("crate::grpc2::types::Empty")
                .output_type("crate::grpc2::types::NodeInfo")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_account_transaction_sign_hash")
                .route_name("GetAccountTransactionSignHash")
                .input_type("crate::grpc2::types::PreAccountTransaction")
                .output_type("crate::grpc2::types::AccountTransactionSignHash")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_items")
                .route_name("GetBlockItems")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_bakers_reward_period")
                .route_name("GetBakersRewardPeriod")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_block_certificates")
                .route_name("GetBlockCertificates")
                .input_type("crate::grpc2::types::BlockHashInput")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_first_block_epoch")
                .route_name("GetFirstBlockEpoch")
                .input_type("crate::grpc2::types::EpochRequest")
                .output_type("crate::grpc2::types::BlockHash")
                .codec_path("tonic::codec::ProstCodec")
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("get_winning_bakers_epoch")
                .route_name("GetWinningBakersEpoch")
                .input_type("crate::grpc2::types::EpochRequest")
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .method(
            tonic_build::manual::Method::builder()
                .name("dry_run")
                .route_name("DryRun")
                .input_type("crate::grpc2::types::DryRunRequest")
                .client_streaming()
                .output_type("Vec<u8>")
                .codec_path("crate::grpc2::RawCodec")
                .server_streaming()
                .build(),
        )
        .build();
    // Due to the slightly hacky nature of the RawCodec (i.e., it does not support
    // deserialization) we cannot build the client. But we also don't need it in the
    // node.
    tonic_build::manual::Builder::new().build_client(false).compile(&[query_service]);

    {
        let health = format!("{}/v2/concordium/health.proto", proto_root_input);
        let descriptor_path =
            std::path::PathBuf::from(env::var("OUT_DIR").unwrap()).join("health_descriptor.bin");
        // build the health service with reflection support
        tonic_build::configure()
            .build_server(true)
            .build_client(false)
            .file_descriptor_set_path(descriptor_path)
            .compile(&[&health], &[proto_root_input])
            .expect("Failed to compile gRPC health definitions!");
    }
    {
        let grpc_health_v1 = format!("{}/grpc/health/v1/health.proto", proto_root_input);
        let descriptor_path = std::path::PathBuf::from(env::var("OUT_DIR").unwrap())
            .join("grpc_health_v1_descriptor.bin");
        // build the health service with reflection support
        tonic_build::configure()
            .build_server(true)
            .build_client(false)
            .file_descriptor_set_path(descriptor_path)
            .compile(&[&grpc_health_v1], &[proto_root_input])
            .expect("Failed to compile gRPC health definitions!");
    }
    Ok(())
}

// Each os has a diferent extesion for the Dynamic Libraries. This compiles for
// the correct ones.
#[cfg(not(any(target_os = "macos", target_os = "windows")))]
#[cfg(not(feature = "static"))]
const DYLIB_EXTENSION: &str = "so";

#[cfg(target_os = "macos")]
#[cfg(not(feature = "static"))]
const DYLIB_EXTENSION: &str = "dylib";

#[cfg(all(not(feature = "static"), not(windows)))]
/// Link with Haskell runtime libraries.
/// The RTS version defaults to the threaded one, but can be overridden by the
/// HASKELL_RTS_VARIANT environment variable
fn link_ghc_libs() -> std::io::Result<std::path::PathBuf> {
    let rts_variant =
        env::var("HASKELL_RTS_VARIANT").unwrap_or_else(|_| "libHSrts-1.0.2_thr-".to_owned());
    let ghc_lib_dir = env::var("HASKELL_GHC_LIBDIR").unwrap_or_else(|_| {
        command_output(Command::new("stack").args([
            "--stack-yaml",
            "../concordium-consensus/stack.yaml",
            "ghc",
            "--",
            "--print-libdir",
        ]))
    });
    let rts_dir = Path::new(&ghc_lib_dir).join(GHC_VARIANT);
    println!("cargo:rustc-link-search=native={}", rts_dir.to_string_lossy());
    for item in std::fs::read_dir(&rts_dir)?.filter_map(Result::ok) {
        let path = item.path();
        let file_stem = path.file_stem().unwrap().to_string_lossy();
        if file_stem.starts_with(&rts_variant) {
            if let Some(true) = item.path().extension().map(|ext| ext == DYLIB_EXTENSION) {
                let lib_name = file_stem.strip_prefix("lib").unwrap();
                println!("cargo:rustc-link-lib=dylib={}", lib_name);
            }
        }
    }
    Ok(rts_dir)
}

#[cfg(feature = "static")]
fn link_static_libs() -> std::io::Result<()> {
    let out_dir = env::var("CARGO_MANIFEST_DIR").unwrap();
    #[cfg(not(feature = "profiling"))]
    let path = format!("{}/deps/static-libs/linux/vanilla", out_dir);

    #[cfg(feature = "profiling")]
    let path = format!("{}/deps/static-libs/linux/profiling", out_dir);

    ["concordium", "dependencies", "ghc"].iter().for_each(|subdir| {
        println!("cargo:rustc-link-search=native={}/{}", path, subdir);
        let walker = walkdir::WalkDir::new(Path::new(&format!("{}/{}", path, subdir)))
            .into_iter()
            .filter_map(Result::ok);
        for item in walker {
            match item.file_name().to_str() {
                Some(lib_file) => {
                    // We link with all the libraries, but exclude the non-threaded Haskell RTS.
                    if lib_file.starts_with("lib")
                        && lib_file.ends_with(".a")
                        && (!lib_file.starts_with("libHSrts") || lib_file.contains("_thr"))
                    {
                        println!(
                            "cargo:rustc-link-lib=static={}",
                            &lib_file[3..lib_file.len() - 2]
                        );
                    }
                }
                _ => panic!("Unable to link ghc libs"),
            }
        }
    });

    println!("cargo:rustc-link-search=native={}/deps/static-libs/linux/rust", out_dir);
    println!("cargo:rustc-link-lib=static=Rcrypto");
    println!("cargo:rustc-link-lib=static=concordium_smart_contract_engine");

    println!("cargo:rustc-link-lib=dylib=gmp");

    Ok(())
}
