// Parameters:
// - environment (e.g., testnet, mainnet, stagenet)
// - image_tag (default: "latest")
// - static_libraries_image_tag (default: "latest")
// - ghc_version (default: "8.10.4")
// - genesis_ref (default: "master")
// - genesis_path

@Library('concordium-pipelines') _
pipeline {
    agent any
    
    environment {
        image_name = "${environment}-node"
    }

    stages {
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
                sh 'printenv'
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh './scripts/distribution/docker/build-distribution-image.sh'
                }
            }
        }

        stage('push') {
            steps {
                 sh 'docker push "concordium/${image_name}:${image_tag}"'
             }
        }
    }
}
