// Parameters:
// - environment
// - image_tag

pipeline {
    agent { label 'master-node' }

    environment {
        // TODO Extract shared function for domain stuff.
        S3_BUCKET = "s3://distribution.${environment}.concordium.${environment == 'mainnet' ? 'software' : 'com'}"
        S3_VERSION_PATH = 'image/version.json'
        S3_IMAGE_PATH = "image/${environment}-node-${image_tag}.tar.gz"
        CF_DISTRIBUTION_ID = "${[stagenet: 'E2Z6VZ10YEWPDX', testnet: 'E3OE3P6NHHWU0I', mainnet: 'E1F07594M64NU0'][environment]}"
    }

    stages {
        stage('build') {
            steps {
                sh '''\
                    if ! aws s3 ls "${S3_BUCKET}/${S3_IMAGE_PATH}"; then
                        echo "Image with tag '${image_tag}' does not exist in bucket '${S3_BUCKET}'"
                        exit 1
                    fi
                    aws s3 cp - "${S3_BUCKET}/${S3_VERSION_PATH}" --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers <<EOF
                    {
                        "image_tag": "${image_tag}",
                        "file": "testnet-node-${image_tag}.tar.gz",
                        "image_name": "testnet-node"
                    }
                    EOF
                    invalidation_result="$(aws cloudfront create-invalidation --distribution-id "${CF_DISTRIBUTION_ID}" --paths "/${S3_VERSION_PATH}")"
                    # Wait for invalidation to complete. Depends on 'jq' which is currently available on the master but not the workers.
                    invalidation_id="$(echo "${invalidation_result}" | jq -r '.Invalidation.Id')"
                    aws cloudfront invalidation-completed --distribution-id "${CF_DISTRIBUTION_ID}" --id "${invalidation_id}"
                '''.stripIndent()
            }
        }
    }
}
