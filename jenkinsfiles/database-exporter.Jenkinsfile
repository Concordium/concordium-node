// Parameters:
// - VERSION
// - UBUNTU_VERSION (default: "20.04")
// - GHC_VERSION (default: "9.2.5")

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
        BUILD_FILE = "database-exporter_${TAG}.deb"
        OUTFILE = "s3://distribution.concordium.software/tools/linux/${BUILD_FILE}"
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
        stage('build') {
            environment {
                EXTERNAL_UID = "${sh(script: 'id -u', returnStdout: true).trim()}"
                EXTERNAL_GID = "${sh(script: 'id -g', returnStdout: true).trim()}"
            }
            steps {
               sh '''\
                   docker build \
                        --build-arg ubuntu_version="${UBUNTU_VERSION}" \
                        --build-arg version="${TAG}" \
                        --build-arg ghc_version="${GHC_VERSION}" \
                        --build-arg static_libraries_image_tag="${STATIC_LIBRARIES_IMAGE_TAG}" \
                        --label ubuntu_version="${UBUNTU_VERSION}" \
                        --label version="${TAG}" \
                        --label ghc_version="${GHC_VERSION}" \
                        --label static_libraries_image_tag="${STATIC_LIBRARIES_IMAGE_TAG}" \
                        -f "scripts/db-exporter/Dockerfile" \
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
