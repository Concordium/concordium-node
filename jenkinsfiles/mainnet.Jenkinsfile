pipeline {
    agent any
    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        IMAGE_TAG="${image_tag}" \
                        BASE_IMAGE_TAG="${base_image_tag}" \
                        STATIC_LIBRARIES_IMAGE_TAG="${static_libraries_image_tag}" \
                        GHC_VERSION="${ghc_version}" \
                        GENESIS_REF="${genesis_ref}" \
                        GENESIS_PATH="${genesis_path}" \
                          ./scripts/distribution/build-and-push-mainnet.sh
                    '''
                }
            }
        }
    }
}
