@Library('concordium-pipelines') _
pipeline {
    agent any
    environment {
        universal_image_repo = 'concordium/universal'
        universal_image_name = "${universal_image_repo}:${image_tag}"
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
                image_repo = "concordium/bootstrapper"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --label=universal_image_name="${universal_image_name}" \
                      -t "${image_name}" \
                      -f scripts/docker/bootstrapper.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
        stage('build-node') {
            environment {
                image_repo = "concordium/node"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --label=universal_image_name="${universal_image_name}" \
                      -t "${image_name}" \
                      -f scripts/docker/node.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
        stage('build-collector') {
            environment {
                image_repo = "concordium/node-collector"
                image_name = "${image_repo}:${image_tag}"
            }
            steps {
                sh '''\
                    docker build \
                      --build-arg=universal_image_name="${universal_image_name}" \
                      --label=universal_image_name="${universal_image_name}" \
                      -t "${image_name}" \
                      -f scripts/docker/node-collector.Dockerfile \
                      .
                    docker push "${image_name}"
                '''
            }
        }
    }
}
