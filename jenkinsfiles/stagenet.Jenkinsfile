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
                        GENESIS_REF="${genesis_ref}" \
                        GENESIS_PATH="${genesis_path}" \
                        BASE_IMAGE_TAG="${base_image_tag}" \
                          ./scripts/distribution/build-and-push-stagenet.sh
                    '''
                }
            }
        }
    }
}
