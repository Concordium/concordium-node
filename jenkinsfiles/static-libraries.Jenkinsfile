pipeline {
    agent any

    environment {
         GHC_VERSION = '8.8.4'
    }

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
                    sh './scripts/build-and-push-static-libs.sh'
                }
            }
        }
    }
}
