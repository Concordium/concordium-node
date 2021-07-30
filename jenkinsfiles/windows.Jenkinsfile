pipeline {
    agent { label 'windows' }
    environment {
        BASE_OUTFILE = 's3://distribution.concordium.software/windows/'
    }
    stages {
        stage('build') {
            steps {
                powershell '''\
                        # Build
                        ./scripts/distribution/windows/build-all.ps1

                        # Push
                        $OUTFILE = "$($env:BASE_OUTFILE)Node$($env:tag).msi"
                        aws s3 cp ./service/windows/installer/Node.msi $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                    '''
            }
        }
    }
}
