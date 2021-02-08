pipeline {
    agent any
    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }
        stage('dockerhub-login') {
            environment {
                CRED = credentials('jenkins-dockerhub')
            }
            steps {
                sh 'echo $CRED_PSW | docker login --username $CRED_USR --password-stdin'
            }
        }
        stage('build') {
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                           ./scripts/download-genesis-data.sh
                           ./scripts/download-genesis-complementary-bundle.sh
                           ./docker-compose/build.sh latest false
                           docker push concordium/dev-node:latest
                       '''
                }
            }
        }
    }
}
