// Parameters:
// - environment
// - source_image_tag
// - destination_image_tag
// - set_latest
// - delete_source
@Library('concordium-pipelines') _
node {
    def docker_images_base = [
        stagenet: 'stagenet-node',
        testnet: 'testnet-node',
        mainnet: 'mainnet-node',
    ][environment]
    
    if (!environment?.trim()) {
        error "No value for 'environment' provided."
    }
    if (!source_image_tag) {
        error "No value for 'source_image_tag' found."
    }
    if (!destination_image_tag) {
        error "No value for 'destination_image_tag' found."
    }
    
    
    def docker_repo = "concordium/${docker_images_base}"
    def source_image_name = "${docker_repo}:${source_image_tag}"
    def destination_image_name = "${docker_repo}:${destination_image_tag}"
    def latest_image_name = "${docker_repo}:latest"

    def latest_image_command = ""
    if (params.set_latest) {
        latest_image_command = "--tag ${docker_repo}:latest"
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
        withCredentials([usernamePassword(credentialsId: 'jenkins-dockerhub', passwordVariable: 'CRED_PSW', usernameVariable: 'CRED_USR')]) {
            sh 'docker login --username $CRED_USR --password $CRED_PSW'
        }
    }
    stage('update') {
        sh "docker buildx imagetools create ${source_image_name} --tag ${destination_image_tag} ${latest_image_command}"
    }
    stage('cleanup') {
        when {
            beforeAgent true
            environment name: 'delete_source', value: 'true'
        }
        withCredentials([usernamePassword(credentialsId: 'jenkins-dockerhub', passwordVariable: 'CRED_PSW', usernameVariable: 'CRED_USR')]) {
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
