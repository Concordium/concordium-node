// Parameters:
// - environment
// - image_tag
@Library('concordium-pipelines') _
node('master-node') {
    def DISTRIBUTION_IDS = [
        stagenet: 'E2Z6VZ10YEWPDX',
        testnet: 'E3OE3P6NHHWU0I',
        mainnet: 'E1F07594M64NU0',
    ]
    
    if (!environment?.trim()) {
        error "No value for 'environment' provided."
    }
    
    def s3_bucket = "s3://distribution.${concordiumDomain(environment)}"
    def s3_version_path = 'image/version.json'
    def s3_image_path = "image/${environment}-node-${image_tag}.tar.gz"
    def cf_distribution_id = DISTRIBUTION_IDS[environment]
    if (!cf_distribution_id) {
        error "No value for 'cf_distribution_id' found."
    }

    stage('verify') {
        try {
            sh (script: """\
                aws s3 ls "${s3_bucket}/${s3_image_path}"
            """.stripIndent())
        } catch (e) {
            error "Image with tag '${image_tag}' does not exist in bucket '${s3_bucket}' on path '${s3_image_path}'."
        }
    }
    stage('update') {
        sh(script: """\
            aws s3 cp - "${s3_bucket}/${s3_version_path}" --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers <<EOF
            {
                "image_tag": "${image_tag}",
                "file": "testnet-node-${image_tag}.tar.gz",
                "image_name": "testnet-node"
            }
            EOF
        """.stripIndent())
    }
    stage('invalidate') {
        invalidation_result = sh(script: """\
            aws cloudfront create-invalidation --distribution-id="${cf_distribution_id}" --paths="/${s3_version_path}"
        """, returnStdout: true)
        // Wait for invalidation to complete.
        invalidation = readJSON(text: invalidation_result)
        sh(script: """\
            aws cloudfront wait invalidation-completed --distribution-id="${cf_distribution_id}" --id="${invalidation.Invalidation.Id}"
        """.stripIndent())
    }
}
