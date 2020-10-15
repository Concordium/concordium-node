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
                sshagent (credentials: ['6a7625a8-34f4-4c39-b0be-ed5b49aabc16']) {
                    sh '''\
                           ./scripts/download-genesis-data.sh
                           ./build-testnet-develop-release.sh release default false
                       '''
                }
            }
        }
    }
}
