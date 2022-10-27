// Parameters:
// - VERSION
// - UBUNTU_VERSION (default: "20.04")
// - GHC_VERSION (default: "9.0.2")

pipeline {
    agent any
    environment {
        OUT_DIR = sh(script: 'mktemp -d', returnStdout: true).trim()
        TAG = """${sh(
                returnStdout: true,
                script: '''\
                    if [ -z "$VERSION" ]; then
                        awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml
                    else
                        echo "$VERSION"
                    fi
                '''
            )}""".trim()
        BUILD_FILE = "p2p-bootstrapper_${TAG}.deb"
        OUTFILE = "s3://distribution.concordium.software/tools/linux/${BUILD_FILE}"
        STATIC_BINARIES_IMAGE_TAG = "latest"
    }
    stages {
        stage('Precheck') {
            steps {
                // Fail the job if the TAG does not start with a number.
                sh '''\
                    re='^[0-9]'
                    if ! [[ $TAG =~ $re ]] ; then
                        echo "error: VERSION must start with a number, currently: $TAG"
                        false
                    fi
                '''.stripIndent()
                // Fail the job if the OUTFILE already exists in S3.
                sh '''\
                    # Fail if file already exists
                    totalFoundObjects=$(aws s3 ls "$OUTFILE" --summarize | grep "Total Objects: " | sed "s/[^0-9]*//g")
                    if [ "$totalFoundObjects" -ne "0" ]; then
                        echo "error: $OUTFILE already exists"
                        false
                    fi
                '''.stripIndent()
            }
        }
        stage('Build static-node-binaries') {
            environment {
                STATIC_LIBRARIES_IMAGE_TAG = "latest"
            }
            steps {
                sh './scripts/static-binaries/build-static-binaries.sh'
            }
        }
        stage('Build debian package') {
            steps {
               sh '''\
                   docker build \
                        --build-arg ubuntu_version="${UBUNTU_VERSION}" \
                        --build-arg version="${TAG}" \
                        --build-arg static_binaries_image_tag="${STATIC_BINARIES_IMAGE_TAG}" \
                        --label ubuntu_version="${UBUNTU_VERSION}" \
                        --label version="${TAG}" \
                        --label static_binaries_image_tag="${STATIC_BINARIES_IMAGE_TAG}" \
                        -f "scripts/bootstrapper/Dockerfile" \
                        -t build-deb:${BUILD_TAG} \
                        --no-cache \
                        .
               '''
               sh 'docker run -v "${OUT_DIR}":/out build-deb:${BUILD_TAG}'
            }
        }
        stage('push') {
            steps {
                sh 'aws s3 cp "${OUT_DIR}/${BUILD_FILE}" "${OUTFILE}" --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers'
            }
        }
    }
}
