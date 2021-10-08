pipeline {
    agent any

    environment {
        BUILD_TYPE = 'release'

        ecr_repo_base = '192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium'
        image_repo = "${ecr_repo_base}/collector-backend"
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                sh '$(aws --region eu-west-1 ecr get-login | sed -e \'s/-e none//g\')'
            }
        }

        stage('build-collector-backend') {
            steps {
                sh '''\
                    docker build \
                      --build-arg base_image_tag="${base_image_tag}" \
                      --build-arg build_type="${BUILD_TYPE}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label build_type="${BUILD_TYPE}" \
                      -t "${image_name}" \
                      -f scripts/testnet-deployments/collector-backend.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
