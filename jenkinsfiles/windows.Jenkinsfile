pipeline {
    agent {
        node {
            label 'windows'
            // We set a custom workspace because the build can hit problems with long paths
            // (beyond ~260 characters) otherwise.
            customWorkspace 'C:\\node-ws'
        }
    }
    environment {
        BASE_OUTFILE = 's3://distribution.concordium.software/windows/'
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
        OUTFILE = "${BASE_OUTFILE}Node-${TAG}.msi"
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
        stage('build') {
            steps {
                sh '''\
                        # Build
                        powershell -File ./scripts/distribution/windows/build-all.ps1

                        # Push
                        aws s3 cp ./service/windows/installer/Node.msi $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

                    '''
            }
        }
    }
}
