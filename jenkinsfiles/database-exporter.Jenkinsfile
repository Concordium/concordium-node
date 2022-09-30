pipeline {
    agent any
    environment {
        aws_s3_bucket_name = 's3://node-deb.concordium.com/'
        out_dir = sh(script: 'mktemp -d', returnStdout: true).trim()
    }
    stages {
        stage('build') {
            steps {
               sh '''\
                   docker build \
                        --build-arg ubuntu_version="${ubuntu_version}" \
                        --build-arg build_version="${build_version}" \
                        --label ubuntu_version="${ubuntu_version}" \
                        --label build_version="${build_version}" \
                        -f "scripts/db-exporter/Dockerfile" \
                        -t build-deb:${BUILD_TAG} \
                        --no-cache \
                        ./distribution/node/deb/docker
               '''
               sh 'docker run -v "${out_dir}":/out build-deb'
            }
        }
        stage('push') {
            steps {
                sh '''\
                   for f in "${out_dir}"/*.deb; do
                       aws s3 cp "\${f}" "${aws_s3_bucket_name}" --grants=read=uri=http://acs.amazonaws.com/groups/global/AllUsers
                   done
                   '''
            }
        }
    }
}
