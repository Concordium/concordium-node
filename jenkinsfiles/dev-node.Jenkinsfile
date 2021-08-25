pipeline {
    agent any
    
    environment {
        image_repo = 'concordium/dev-node'
        image_name = "${image_repo}:${image_tag}"
    }
    
    stages {
        stage('dockerhub-login') {
            environment {
                // Defines 'CRED_USR' and 'CRED_PSW'
                // (see 'https://www.jenkins.io/doc/book/pipeline/jenkinsfile/#handling-credentials').
                CRED = credentials('jenkins-dockerhub')
            }
            steps {
                sh 'docker login --username "${CRED_USR}" --password "${CRED_PSW}"'
            }
        }
        stage('build') {
            steps {
                sh '''\
                    docker build \
                        --build-arg ghc_version="${ghc_version}" \
                        --label ghc_version="${ghc_version}" \
                        -t "concordium/dev-node:${image_tag}" \
                        -f docker-compose/Dockerfile \
                        --pull \
                        .
                '''
            }
        }
        
        stage('push') {
            steps {
                sh 'docker push "${image_name}"'
            }
        }
    }
}
