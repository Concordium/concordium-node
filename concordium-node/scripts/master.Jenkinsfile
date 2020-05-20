pipeline {
    agent any
    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('build') {
            environment {
                DOCKER_BUILDKIT = 1
            }
            steps {
                sshagent (credentials: ['jenkins']) {
                    sh './scripts/download-genesis-data.sh && ./build-testnet-production-release.sh release default false'
                }
            }
        }
    }
}
