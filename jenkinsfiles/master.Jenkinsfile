@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
        build_profile = 'release'
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
                    # Using '--pull' to ensure that we build from the latest base images.
                    docker build \
                      --pull \
                      --build-arg=base_image_tag="${base_image_tag}" \
                      --build-arg=static_libraries_image_tag="${static_libraries_image_tag}" \
                      --build-arg=ghc_version="${ghc_version}" \
                      --label=base_image_tag="${base_image_tag}" \
                      --label=static_libraries_image_tag="${static_libraries_image_tag}" \
                      --label=ghc_version="${ghc_version}" \
                      -t "${universal_image_name}" \
                      -f ./scripts/docker/universal.Dockerfile \
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
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --build-arg=build_profile="${build_profile}" \
                      --label=universal_image_name="${universal_image_name}" \
                      --label=build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/docker/bootstrapper.Dockerfile \
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
                sh '''\
                    docker build \
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --build-arg=build_profile="${build_profile}" \
                      --label=universal_image_name="${universal_image_name}" \
                      --label=build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/docker/node.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
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
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --build-arg=build_profile="${build_profile}" \
                      --label=universal_image_name="${universal_image_name}" \
                      --label=build_profile="${build_profile}" \
                      -t "${image_name}" \
                      -f scripts/docker/node-collector.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
