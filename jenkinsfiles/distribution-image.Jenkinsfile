// Parameters:
// - environment (e.g., testnet, mainnet, stagenet)
// - image_tag (default: "latest")
// - base_image_tag (default: "latest")
// - static_libraries_image_tag (default: "latest")
// - ghc_version (default: "9.0.2")
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
                // Using '--no-cache' because we're cloning genesis data
                // and BuildKit (and '--ssh default') because the repo is on GitLab.
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        DOCKER_BUILDKIT=1 docker build \
                          --build-arg environment="${environment}"\
                          --build-arg base_image_tag="${base_image_tag}" \
                          --build-arg static_libraries_image_tag="${static_libraries_image_tag}" \
                          --build-arg ghc_version="${ghc_version}" \
                          --build-arg genesis_ref="${genesis_ref}" \
                          --build-arg genesis_path="${genesis_path}" \
                          --label base_image_tag="${base_image_tag}" \
                          --label static_libraries_image_tag="${static_libraries_image_tag}" \
                          --label ghc_version="${ghc_version}" \
                          --label genesis_ref="${genesis_ref}" \
                          --label genesis_path="${genesis_path}" \
                          -t "concordium/${image_name}:${image_tag}" \
                          -f scripts/distribution/docker/builder.Dockerfile \
                          --ssh default \
                          --no-cache \
                          .
                    '''.stripIndent()
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
