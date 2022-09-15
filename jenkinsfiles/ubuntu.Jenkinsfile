// Parameters:
// - environment
// - version
// - genesis_path

@Library('concordium-pipelines') _

pipeline {
    // Use jenkins-worker, as it has the keys for pushing to AWS.
    agent { label 'jenkins-worker' }
    environment {
        TAG = """${sh(
                returnStdout: true,
                script: '''\
                    if [ -z "$version" ]; then
                        awk '/version = / { print substr($3, 2, length($3)-2); exit }' concordium-node/Cargo.toml
                    else
                        echo "$version"
                    fi
                '''
            )}""".trim()
        DOMAIN = concordiumDomain(environment)
        OUTFILE = "s3://distribution.${DOMAIN}/macos/concordium-${environment}-node_${TAG}_amd64.deb"
    }
    stages {
        stage('Build static-node-binaries') {
            when {
                equals expected: 1,
                actual: sh script:'docker inspect --type=image static-node-binaries > /dev/null 2> /dev/null', returnStatus:true
            }
            steps {
                echo 'Build static-node-binaries'
            }
        }
        stage('Checkout genesis') {
            git credentialsId: 'jenkins-gitlab-ssh', url: 'git@gitlab.com:Concordium/genesis-data.git'
        }
        // stage('Publish') {
        //     steps {
        //         unstash 'release'
        //         sh ' aws s3 cp ./scripts/distribution/macOS-package/build/packages/concordium-node-${TAG}-unsigned.pkg $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers'
        //     }
        // }
    }
}
