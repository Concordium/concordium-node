pipeline {
    agent any

    environment {
        BUILD_TYPE = 'release'
        CONSENSUS_PROFILING = 'false'

        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }

        stage('build-universal') {
            steps {
                sh '''\
                    docker build \
                      -t "${universal_image_name}" \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg consensus_profiling="${CONSENSUS_PROFILING}" \
                      --label base_image_tag="${base_image_tag}" \
                      -f ./scripts/testnet-deployments/universal.Dockerfile \
                      .
                '''
            }
        }
        stage('build-bootstrapper') {
            environment {
                image_repo = "${ecr_repo_base}/bootstrapper"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      -t "${image_name}" \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      -f scripts/testnet-deployments/bootstrapper.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }

        stage('build-node') {
            environment {
                image_repo = "${ecr_repo_base}/node"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      -t "${image_name}" \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      -f scripts/testnet-deployments/node.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }

        stage('build-collector') {
            environment {
                image_repo = "${ecr_repo_base}/node-collector"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      -t "${image_name}" \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      -f scripts/testnet-deployments/node-collector.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }

        stage('build-collector-backend') {
            environment {
                image_repo = "${ecr_repo_base}/collector-backend"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      -t "${image_name}" \
                      --build-arg universal_image_name="${universal_image_name}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      -f scripts/testnet-deployments/collector-backend.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
