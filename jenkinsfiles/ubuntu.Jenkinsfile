// Parameters:
// - environment
// - version
// - genesis_path
// - ubuntu_version
// - ghc_version

@Library('concordium-pipelines') _

pipeline {
    // Use jenkins-worker, as it has the keys for pushing to AWS.
    agent { label 'jenkins-worker' }
    environment {
        CODE_VERSION = """${sh(
                returnStdout: true,
                script: '''\
                    awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml
                '''
            )}""".trim()
        OUT_VERSION = """${sh(
                returnStdout: true,
                script: '''\
                    if [ -z "$version" ]; then
                        echo "$CODE_VERSION"
                    else
                        echo "$version"
                    fi
                '''
            )}""".trim()
        DOMAIN = concordiumDomain(environment)
        BUILD_FILE = "concordium-${environment}-node_${CODE_VERSION}_amd64.deb"
        OUTFILE = "s3://distribution.${DOMAIN}/deb/concordium-${environment}-node_${OUT_VERSION}_amd64-SNM.deb"
        GENESIS_HASH_PATH = "genesis/${genesis_path}/genesis_hash"
        GENESIS_DAT_FILE = "genesis/${genesis_path}/genesis.dat"
        ENVIRONMENT_CAP = environment.capitalize()
        DATA_DIR = "./scripts/distribution/ubuntu-packages/template/data/"
    }
    stages {
        stage('Build static-node-binaries') {
            when {
                // Check if static-node-binaries image is present, if not build it.
                equals expected: "1",
                actual: "${sh script:'docker inspect --type=image static-node-binaries > /dev/null 2> /dev/null', returnStatus:true}"
            }
            steps {
                sh '''
                    export STATIC_LIBRARIES_IMAGE_TAG="latest"
                    export GHC_VERSION="${ghc_version}"
                    export EXTRA_FEATURES="collector"
                    export UBUNTU_VERSION=${ubuntu_version}
                    ./scripts/static-binaries/build-static-binaries.sh
                '''
            }
        }
        stage('Checkout genesis') {
            steps {
                // checkout([
                //     $class: 'GitSCM', 
                //     branches: [[name: '*/master']], 
                //     extensions: [[$class: 'RelativeTargetDirectory', relativeTargetDir: 'genesis'], 
                //     [$class: 'SparseCheckoutPaths', sparseCheckoutPaths: [[path: '${environment}/2022-06-24/genesis_data']]]], 
                //     userRemoteConfigs: [[credentialsId: 'jenkins-gitlab-ssh', url: 'git@gitlab.com:Concordium/genesis-data.git']]
                // ])
                dir('genesis') {
                    git credentialsId: 'jenkins-gitlab-ssh', url: 'git@gitlab.com:Concordium/genesis-data.git'
                    echo "done cloning"
                    sh 'ls'
                }
                    sh '''
                        cat ${GENESIS_HASH_PATH} | tr -cd "[:alnum:]"
                        mkdir ${DATA_DIR}
                        cp ${GENESIS_DAT_FILE} ${DATA_DIR}/${environment}-genesis.dat
                        ls ${DATA_DIR}
                    '''
            }
        }
        stage('Build node') {
            steps {
                sh '''
                    docker build\
                        --build-arg ubuntu_version=${ubuntu_version}\
                        --build-arg build_env_name=${ENVIRONMENT_CAP}\
                        --build-arg build_env_name_lower=${environment}\
                        --build-arg build_genesis_hash=$(cat ${GENESIS_HASH_PATH} | tr -cd "[:alnum:]")\
                        --build-arg build_collector_backend_url=https://dashboard.${DOMAIN}/nodes/post\
                        --build-arg build_rpc_server_port=10000\
                        --build-arg build_listen_port=8888\
                        --build-arg build_bootstrap=bootstrap.${DOMAIN}:8888\
                        -f ./scripts/distribution/ubuntu-packages/deb.Dockerfile\
                        -t ${environment}-deb ./scripts/distribution/ubuntu-packages/ --no-cache
                '''
            }
        }
        stage('Publish') {
            steps {
                // Copy out the build artifacts. We create a temporary container and use docker
                // cp. This makes the output artifacts have correct file permissions (they are
                // owned by the user who ran the script).
                sh '''
                    id=$(docker create ${environment}-deb)
                    docker cp $id:/out ${environment}-build
                    docker rm $id
                    ls ${environment}-build
                    aws s3 cp ${environment}-build/${BUILD_FILE} ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''
            }
        }
    }
}
