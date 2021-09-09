pipeline {
    // Use jenkins-worker, as it has the keys for pushing to AWS.
    agent { label 'jenkins-worker' }
    environment {
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
        OUTFILE = "s3://distribution.concordium.software/macos/concordium-node-${TAG}.pkg"
    }
    stages {
        stage('Build') {
            agent { label 'mac' }
            steps {
                // Answer No to signing using printf.
                sh 'printf "N\n" | ./scripts/distribution/macOS-package/build.sh $TAG'
                stash includes: 'scripts/distribution/macOS-package/build/packages/*', name: 'release'
            }
        }
        stage('Publish') {
            steps {
                unstash 'release'
                sh ' aws s3 cp ./scripts/distribution/macOS-package/build/packages/concordium-node-${TAG}.pkg $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers'
            }
        }
    }
}
