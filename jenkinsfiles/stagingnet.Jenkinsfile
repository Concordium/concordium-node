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
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh './scripts/distribution/build-and-push-staging-client.sh'
                }
            }
        }
    }
}
