#!/usr/bin/env bash

set -exo pipefail

args=()
if [ -n "${WALLET_PROXY_GRPC_IP}" ]; then
	args+=( --grpc-ip "${WALLET_PROXY_GRPC_IP}" )
fi
if [ -n "${WALLET_PROXY_GRPC_PORT}" ]; then
	args+=( --grpc-ip "${WALLET_PROXY_GRPC_PORT}" )
fi
if [ -n "${WALLET_PROXY_DATABASE}" ]; then
	args+=( --db "${WALLET_PROXY_DATABASE}" )
fi
if [ -n "${WALLET_PROXY_ACCOUNT_FILE}" ]; then
	args+=( --drop-account "${WALLET_PROXY_ACCOUNT_FILE}" )
else
	args+=( --drop-account /genesis-complementary-bundle/additional_accounts/gtu-drop-account-0.json )
fi
if [ -n "${WALLET_PROXY_IPS_METADATA_JSON}" ]; then
	args+=( --ip-data "${WALLET_PROXY_IPS_METADATA_JSON}" )
else
	args+=( --ip-data /genesis-complementary-bundle/identity-providers-with-metadata.json )
fi
if [ -n "${DB_SLEEP}" ]; then
	echo "Sleeping for ${DB_SLEEP}"
	sleep "${DB_SLEEP}"
fi
/wallet-proxy "${args[@]}"
