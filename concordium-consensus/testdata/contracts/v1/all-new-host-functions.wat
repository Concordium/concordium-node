(module

  ;; a module with no functionality, just listing the imports new in V1 contracts, for integration testing.

  ;; Function parameter
  (import "concordium" "get_parameter_size" (func $get_parameter_size (param $index i32) (result i32)))
  (import "concordium" "get_parameter_section" (func $get_parameter_section (param $index i32) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))

  ;; State functions
  (import "concordium" "state_create_entry" (func $state_create_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_size" (func $state_entry_size (param $entry i64) (result i32)))
  (import "concordium" "state_entry_read" (func $state_entry_read (param $entry i64) (param $write_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_lookup_entry" (func $state_lookup_entry (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_entry_write" (func $state_entry_write (param $entry i64) (param $read_location i32) (param $length i32) (param $offset i32) (result i32)))
  (import "concordium" "state_delete_entry" (func $state_delete_entry (param $key_start i32) (param $key_length i32) (result i32)))
  (import "concordium" "state_delete_prefix" (func $state_delete_prefix (param $key_start i32) (param $key_length i32) (result i32)))

  ;; Iterator functions
  (import "concordium" "state_iterate_prefix" (func $state_iterate_prefix (param $key_start i32) (param $key_length i32) (result i64)))
  (import "concordium" "state_iterator_next" (func $state_iterator_next (param $iter i64) (result i64)))
  (import "concordium" "state_iterator_delete" (func $state_iterator_delete (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_size" (func $state_iterator_key_size (param $iter i64) (result i32)))
  (import "concordium" "state_iterator_key_read" (func $state_iterator_key_read
                                                   (param $iter i64)
                                                   (param $write_location i32)
                                                   (param $length i32)
                                                   (param $offset i32)
                                                   (result i32)))

  ;; Fallback related functions. Only available in receive functions.
  (import "concordium" "get_receive_entrypoint_size" (func $get_ep_size (result i32)))
  (import "concordium" "get_receive_entrypoint" (func $get_ep (param $start i32)))

  ;; Invoke another contract or a transfer.
  (import "concordium" "invoke" (func $invoke (param $tag i32) (param $start i32) (param $length i32) (result i64)))

  ;; return a value
  (import "concordium" "write_output" (func $write_output (param $start i32) (param $length i32) (param $offset i32) (result i32)))

  ;; cryptographic primitives
  (import "concordium" "verify_ed25519_signature" (func $verify_ed25519_signature (param $public_key i32) (param $signature i32) (param $message i32) (param $message_len i32) (result i32)))
  (import "concordium" "verify_ecdsa_secp256k1_signature" (func $verify_ecdsa_secp256k1_signature (param $public_key i32) (param $signature i32) (param $message i32) (result i32)))
  (import "concordium" "hash_sha2_256" (func $hash_sha2_256 (param $data i32) (param $data_len i32) (param $output i32)))
  (import "concordium" "hash_sha3_256" (func $hash_sha3_256 (param $data i32) (param $data_len i32) (param $output i32)))
  (import "concordium" "hash_keccak_256" (func $hash_keccak_256 (param $data i32) (param $data_len i32) (param $output i32)))

  ;; upgrade
  (import "concordium" "upgrade" (func $upgrade (param $module_ptr i32) (result i64)))
)
