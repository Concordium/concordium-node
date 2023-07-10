// Parameters:
// - environment (e.g., testnet, mainnet, stagenet)
// - image_tag (default: "latest")
// - static_libraries_image_tag (default: "latest")
// - ghc_version (default: "9.2.7")
// - genesis_ref (default: "master")
// - genesis_path

@Library('concordium-pipelines') _
pipeline {
    agent any
    
    environment {
        image_name = "${environment}-node"
        static_binaries_image_tag = "${BUILD_TAG}"
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
            environment {
                // Overrides the parameter 'genesis_path'
                // Use default genesis path for each environment, if the genesis_path param has not been set.
                // Uses library function defined here: https://github.com/Concordium/concordium-infra-jenkins-library/blob/master/vars/defaultGenesis.groovy
                genesis_path = defaultGenesis(environment, genesis_path)
            }
            steps {
                sh 'printenv'
                sshagent (credentials: ['jenkins-github-ssh']) {
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
