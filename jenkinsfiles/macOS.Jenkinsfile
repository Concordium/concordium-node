pipeline {
    agent { label 'mac' }
    environment {
        BASE_OUTFILE = 's3://distribution.concordium.software/macos/'
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
        OUTFILE = "${BASE_OUTFILE}concordium-node-${TAG}.pkg"
    }
    stages {
        stage('Build') {
            steps {
                sh '''\
                        # Build (answers No to signing)
                        printf "N\n" | ./scripts/distribution/macOS-package/build.sh $TAG

                        # Push
                        aws s3 cp ./scripts/distribution/macOS-package/build/packages/concordium-node-${TAG}.pkg $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers

                   '''
            }
        }
    }
}
