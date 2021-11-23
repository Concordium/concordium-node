@Library('concordium-pipelines') _
pipeline {
    agent any

    environment {
        build_profile = 'release'

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
                      --build-arg build_profile="${build_profile}" \
                      --label base_image_tag="${base_image_tag}" \
                      --label build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/docker/collector-backend.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
