#!/usr/bin/env bash

set -euxo pipefail

# Generates and publishes a database export for use with OOB catchup:
# Once the export has been generated and verified,
# it will be uploaded to the specified AWS S3 bucket with public access
# and the relevant entry of the specified AWS CloudFront distribution will be invalidated.

# Expected environment variables:
# - BUCKET_NAME: Name of the S3 bucket (without 's3://' prefix) to which the export should be published.
# - CF_DISTRIBUTION_ID: ID of the CloudFront distribution.
# - CHUNK_SIZE: Number of blocks per file.

db_path=/var/lib/concordium/data/database-v4
s3_bucket_name="${BUCKET_NAME}"
cf_distribution_id="${CF_DISTRIBUTION_ID}"
chunk_size="${CHUNK_SIZE}"
export_path=/var/lib/database_exporter
blocks_index="${export_path}"/blocks.idx

function log {
  >&2 echo "${@}" # log to stderr
}

function do_export {
  mkdir -p "${export_path}"
  if ! database-exporter export --dbpath="${db_path}" --exportpath="${export_path}" --chunksize "${chunk_size}" > /dev/null; then
    log "Export failed."
    exit 1
  fi
}

function do_validate_db {
  #Storing the list of files from the block index into an array
  db_files=($(grep .dat "${blocks_index}"|awk -F ',' '{print $1}'))
  for i in ${db_files[@]}; do 
    if ! database-exporter check --exportpath="${export_path}/${i}" > /dev/null; then
      log "Database is invalid."
        return 1
    fi
  done
}

function upload_to_s3_and_invalidate_cf_cache {
  aws s3 sync "${export_path}" "s3://${s3_bucket_name}/" --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
  invalidation_id="$(aws cloudfront create-invalidation --distribution-id="${cf_distribution_id}" --paths="/$(basename "${blocks_index}")" | jq -r '.Invalidation.Id')"
  aws cloudfront wait invalidation-completed --distribution-id="${cf_distribution_id}" --id="${invalidation_id}"
}

# Try exporting and validating up to 3 times. Otherwise bail out.
for _ in {1..3}; do
  do_export
  if do_validate_db; then
    upload_to_s3_and_invalidate_cf_cache
    break
  fi
done
