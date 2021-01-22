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
                           ./scripts/download-genesis-data.sh
                           ./scripts/download-genesis-complementary-bundle.sh
                           ./scripts/build-docker-compose-image.sh latest false
                           docker push concordium/dev-client:latest
                       '''
                }
            }
        }
    }
}
