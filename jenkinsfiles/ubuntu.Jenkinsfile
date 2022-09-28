// Parameters:
// - ENVIRONMENT
// - VERSION
// - GENESIS_PATH
// - UBUNTU_VERSION
// - GHC_VERSION

@Library('concordium-pipelines') _

Map rpc_port = [
    mainnet: "10000",
    testnet: "10001",
    stagenet: "10500"
]

Map listen_port = [
    mainnet: "8888",
    testnet: "8889",
    stagenet: "9500"
]

Map genesis_path = [
    mainnet: "mainnet/2021-06-09",
    testnet: "testnet/2022-06-13/genesis_data",
    stagenet: "stagenet/2022-06-24/genesis_data"
]

pipeline {
    // Use jenkins-worker, as it has the keys for pushing to AWS.
    agent { label 'jenkins-worker' }
    environment {
        // Extract version from code
        CODE_VERSION = """${sh(
                returnStdout: true,
                script: '''awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml'''
            )}""".trim()
        // Use code version if the version param has not been set
        OUT_VERSION = """${sh(
                returnStdout: true,
                script: "[[ -z '${VERSION}' ]] && echo '${CODE_VERSION}' || echo '${VERSION}'"
            )}""".trim()
        // Ude default genesis path for each environment, if the GENESIS_PATH param has not been set.
        GENESIS_FULL_PATH = """${sh(
                returnStdout: true,
                script: "[[ -z '${GENESIS_PATH}' ]] && echo 'genesis/${genesis_path[ENVIRONMENT]}' || echo 'genesis/${GENESIS_PATH}'"
            )}""".trim()
        DOMAIN = concordiumDomain(ENVIRONMENT)
        BUILD_FILE = "concordium-${ENVIRONMENT}-node_${CODE_VERSION}_amd64.deb"
        OUTFILE = "s3://distribution.${DOMAIN}/deb/concordium-${ENVIRONMENT}-node_${OUT_VERSION}_amd64.deb"
        GENESIS_HASH_PATH = "${GENESIS_FULL_PATH}/genesis_hash"
        GENESIS_DAT_FILE = "${GENESIS_FULL_PATH}/genesis.dat"
        ENVIRONMENT_CAP = environment.capitalize()
        DATA_DIR = "./scripts/distribution/ubuntu-packages/template/data/"
        RPC_PORT = "${rpc_port[environment]}"
        LISTEN_PORT = "${listen_port[environment]}"
        STATIC_BINARIES_IMAGE_TAG = "${BUILD_TAG}"
    }
    stages {
        stage('Precheck') {
            steps {
                // Fail the job if the OUTFILE already exists in S3.
                sh '''\
                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls "$OUTFILE" --summarize | grep "Total Objects: " | sed "s/[^0-9]*//g")
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "$OUTFILE already exists"
                        false
                    fi
                '''.stripIndent()
            }
        }
        stage('Build static-node-binaries') {
            environment {
                STATIC_LIBRARIES_IMAGE_TAG = "latest"
                EXTRA_FEATURES = "collector"
            }
            steps {
                sh './scripts/static-binaries/build-static-binaries.sh'
            }
        }
        stage('Checkout genesis') {
            steps {
                dir('genesis') {
                    git credentialsId: 'jenkins-gitlab-ssh', url: 'git@gitlab.com:Concordium/genesis-data.git'
                }
                    // Copy genesis.dat file into place in data dir, and rename.
                    sh '''
                        mkdir ${DATA_DIR}
                        cp ${GENESIS_DAT_FILE} ${DATA_DIR}/${ENVIRONMENT}-genesis.dat
                    '''
            }
        }
        stage('Build node deb') {
            steps {
                sh '''
                    docker build\
                        --build-arg ubuntu_version=${UBUNTU_VERSION}\
                        --build-arg static_binaries_image_tag=${STATIC_BINARIES_IMAGE_TAG}\
                        --build-arg build_env_name=${ENVIRONMENT_CAP}\
                        --build-arg build_env_name_lower=${ENVIRONMENT}\
                        --build-arg build_genesis_hash=$(cat ${GENESIS_HASH_PATH} | tr -cd "[:alnum:]")\
                        --build-arg build_collector_backend_url=https://dashboard.${DOMAIN}/nodes/post\
                        --build-arg build_rpc_server_port=${RPC_PORT}\
                        --build-arg build_listen_port=${LISTEN_PORT}\
                        --build-arg build_bootstrap=bootstrap.${DOMAIN}:8888\
                        -f ./scripts/distribution/ubuntu-packages/deb.Dockerfile\
                        -t ${ENVIRONMENT}-deb ./scripts/distribution/ubuntu-packages/ --no-cache
                '''
            }
        }
        stage('Publish') {
            steps {
                // Copy out the build artifacts. We create a temporary container and use docker
                // cp. This makes the output artifacts have correct file permissions (they are
                // owned by the user who ran the script).
                sh '''
                    id=$(docker create ${ENVIRONMENT}-deb)
                    docker cp $id:/out ${ENVIRONMENT}-build
                    docker rm $id
                    aws s3 cp ${ENVIRONMENT}-build/${BUILD_FILE} ${OUTFILE} --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                '''
            }
        }
    }
}
