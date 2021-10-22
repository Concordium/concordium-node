// Parameters:
// - environment
// - image_tag (default: "latest")
// - base_image_tag (default: "latest")
// - static_libraries_image_tag (default: "latest")
// - ghc_version (default: "8.10.4")
// - genesis_ref (default: "master")
// - genesis_path

@Library('concordium-pipelines') _
pipeline {
    agent any
    
    environment {
        ecr_repo_domain = '192549843005.dkr.ecr.eu-west-1.amazonaws.com'
        image_name = "${environment}-node"
        domain = "${concordiumDomain(environment)}"
    }

    stages {
        stage('ecr-login') {
            steps {
                ecrLogin(env.ecr_repo_domain, 'eu-west-1')
            }
        }
        stage('build') {
            steps {
                // Using '--no-cache' because we're cloning genesis data
                // and BuildKit (and '--ssh default') because the repo is on GitLab.
                sshagent (credentials: ['jenkins-gitlab-ssh']) {
                    sh '''\
                        DOCKER_BUILDKIT=1 docker build \
                          --build-arg environment="${domain}"\
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
                          -f scripts/distribution/builder.Dockerfile \
                          --ssh default\
                          --no-cache \
                          .
                    '''.stripIndent()
                }
            }
        }
    
        stage('push') {
            environment {
                file = "${image_name}-${image_tag}.tar.gz"
            }
            steps {
                sh '''\
                    docker save concordium/"${image_name}:${image_tag}" | gzip > "${file}"
                    aws s3 cp "${file}" s3://distribution.${domain}/image/ --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''.stripIndent()
            }
        }
    }
}
