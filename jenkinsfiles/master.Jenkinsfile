pipeline {
    agent any

    environment {
        BUILD_TYPE = 'release'
        CONSENSUS_PROFILING = 'false'

        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }

        stage('build-universal') {
            steps {
                sh '''\
                    docker build \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg static_libraries_image_tag="${static_libraries_image_tag}" \
                      --build-arg ghc_version="${ghc_version}" \
                      --build-arg consensus_profiling="${CONSENSUS_PROFILING}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label static_libraries_image_tag="${static_libraries_image_tag}" \
                      --label ghc_version="${ghc_version}" \
                      --label consensus_profiling="${CONSENSUS_PROFILING}" \
                      -t "${universal_image_name}" \
                      -f ./scripts/testnet-deployments/universal.Dockerfile \
                      .
                '''
            }
        }
        stage('build-bootstrapper') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/bootstrapper"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      --label universal_image_name="${universal_image_name}" \
                      --label build_type="${BUILD_TYPE}" \
                      -t "${image_name}" \
                      -f scripts/testnet-deployments/bootstrapper.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }

        stage('build-node') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/node"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        # Using '--no-cache' because we're cloning genesis data
                        # and BuildKit (and '--ssh default') because the repo is on GitLab.
                        DOCKER_BUILDKIT=1 docker build \
                          --build-arg universal_image_name="${universal_image_name}" \
                          --build-arg build_type="${BUILD_TYPE}" \
                          --build-arg genesis_ref="${genesis_ref}" \
                          --build-arg genesis_path="${genesis_path}" \
                          --label universal_image_name="${universal_image_name}" \
                          --label build_type="${BUILD_TYPE}" \
                          --label genesis_ref="${genesis_ref}" \
                          --label genesis_path="${genesis_path}" \
                          -t "${image_name}" \
                          -f scripts/testnet-deployments/node.Dockerfile \
                          --ssh default \
                          --no-cache \
                          .
                        docker push "${image_name}"
                    '''
                }
            }
        }

        stage('build-collector') {
            environment {
                image_repo = "${ecr_repo_domain}/concordium/node-collector"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      --label universal_image_name="${universal_image_name}" \
                      --label build_type="${BUILD_TYPE}" \
                      -t "${image_name}" \
                      -f scripts/testnet-deployments/node-collector.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
