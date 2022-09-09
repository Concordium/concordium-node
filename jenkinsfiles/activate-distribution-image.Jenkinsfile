// Parameters:
// - environment
// - source_image_tag
// - destination_image_tag
// - set_latest
// - delete_source
@Library('concordium-pipelines') _
node('any') {
    def docker_images_base = [
        stagenet: 'stagenet-node',
        testnet: 'testnet-node',
        mainnet: 'mainnet-node',
    ]
    
    if (!environment?.trim()) {
        error "No value for 'environment' provided."
    }
    if (!current_image_tag) {
        error "No value for 'current_image_tag' found."
    }
    if (!future_image_tag) {
        error "No value for 'future_image_tag' found."
    }
    
    def docker_repo = "concordium/${docker_images_base[environment]}"
    def current_image_name = "${docker_repo}:${current_image_tag}"
    def future_image_name = "${docker_repo}:${future_image_tag}"
    def latest_image_name = "${docker_repo}:latest"

    if (set_latest) {
        def latest_image_command = "--tag ${docker_repo}:latest"
    } else {
        def latest_image_command = ""
    }

    stage('verify') {
        // Verify existens of source image
        try {
            sh "curl --silent https://hub.docker.com/v2/namespaces/concordium/repositories/${docker_images_base}/tags/${source_image_tag} | jq -re '.digest'"
        } catch (e) {
            error "Image with tag '${source_image_tag}' does not exist in repo '${docker_repo}'."
        }

        // Verify tags to add don't already exist
        try {
            sh "! curl --silent https://hub.docker.com/v2/namespaces/concordium/repositories/${docker_images_base}/tags/${destination_image_tag} | jq -re '.digest'"
        } catch (e) {
            error "Image with tag '${destination_image_tag}' already exists in repo '${docker_repo}'."
        }
    }
    stage('dockerhub-login') {
        environment {
            CRED = credentials('jenkins-dockerhub')
        }
        steps {
            sh 'echo $CRED_PSW | docker login --username $CRED_USR --password-stdin'
        }
    }
    stage('update') {
        sh "docker buildx imagetools create ${source_image_tag} --tag ${destination_image_tag} ${latest_image_command}"
    }
    stage('cleanup') {
        when {
            beforeAgent true
            environment name: 'delete_source', value: 'true'
        }
        environment {
            CRED = credentials('jenkins-dockerhub')
        }
        steps {
            //https://devopsheaven.com/docker/dockerhub/2018/04/09/delete-docker-image-tag-dockerhub.html
            sh '''\
            login_data() {
            cat <<EOF
            {
            "username": "$CRED_USR",
            "password": "$CRED_PSW"
            }
            EOF
            }

            TOKEN=`curl -s -H "Content-Type: application/json" -X POST -d "$(login_data)" "https://hub.docker.com/v2/users/login/" | jq -r .token`

            curl "https://hub.docker.com/v2/repositories/${ORGANIZATION}/${IMAGE}/tags/${TAG}/" \
            -X DELETE \
            -H "Authorization: JWT ${TOKEN}"
            '''..stripIndent()
        }
    }
}
