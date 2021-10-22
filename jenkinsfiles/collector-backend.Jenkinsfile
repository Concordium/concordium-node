pipeline {
    agent any

    environment {
        BUILD_TYPE = 'release'

        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        image_repo = "${ecr_repo_domain}/concordium/collector-backend"
        image_name = "${image_repo}:${image_tag}"
    }

    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
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
