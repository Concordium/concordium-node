pipeline {
    agent { label 'windows' }
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
    }
    stages {
        stage('build') {
            steps {
                powershell '''\
                        # Build
                        ./scripts/distribution/windows/build-all.ps1

                        # Push
                        $OUTFILE = "$($env:BASE_OUTFILE)Node-$($env:TAG).msi"
                        aws s3 cp ./service/windows/installer/Node.msi $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                    '''
            }
        }
    }
}
