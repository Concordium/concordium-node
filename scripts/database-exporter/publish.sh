#!/usr/bin/env bash

set -euxo pipefail

# Generates and publishes a database export for use with OOB catchup:
# Once the export has been generated and verified,
# it will be uploaded to the specified AWS S3 bucket with public access
# and the relevant entry of the specified AWS CloudFront distribution will be invalidated.

# Expected environment variables:
# - DB_PATH: Local path to the node's LMDB database (default: '/var/lib/concordium/data/database-v4')
# - S3_BUCKET_NAME: Name of the S3 bucket (without 's3://' prefix) to which the export should be published.
# - CF_DISTRIBUTION_ID: ID of the CloudFront distribution.

db_path="${DB_PATH-/var/lib/concordium/data/database-v4}"
s3_bucket_name="${S3_BUCKET_NAME}"
cf_distribution_id="${CF_DISTRIBUTION_ID}"

export_path=/tmp/blocks_to_import.mdb

function log {
  >&2 echo "${@}" # log to stderr
}

function do_export {
  rm -f "${export_path}"
  if ! database-exporter export --dbpath "${db_path}" --exportpath "${export_path}" > /dev/null; then
    log "Export failed."
    exit 1
  fi
}

function do_validate_db {
  if ! database-exporter check --exportpath "${export_path}" > /dev/null; then
    log "Database is invalid."
		return 1
  fi
}

function upload_to_s3_and_invalidate_cf_cache {
  aws s3 cp "${export_path}" "s3://${s3_bucket_name}/" --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
  aws cloudfront create-invalidation --distribution-id "${cf_distribution_id}" --paths "/$(basename "${export_path}")"
}

# Try exporting and validating up to 3 times. Otherwise bail out.
for _ in {1..3}; do
  do_export
  if do_validate_db; then
	  upload_to_s3_and_invalidate_cf_cache
	  break
  fi
done
