# Install Kubernetes
For this guide Container Linux by Coreos was used.
AMI of the image used is ami-09e088627f26fd7ec

## Setup permissions on AWS
For the master create a policy with the following contents,
```
  {
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "autoscaling:DescribeAutoScalingGroups",
        "autoscaling:DescribeLaunchConfigurations",
        "autoscaling:DescribeTags",
        "ec2:DescribeInstances",
        "ec2:DescribeRegions",
        "ec2:DescribeRouteTables",
        "ec2:DescribeSecurityGroups",
        "ec2:DescribeSubnets",
        "ec2:DescribeVolumes",
        "ec2:CreateSecurityGroup",
        "ec2:CreateTags",
        "ec2:CreateVolume",
        "ec2:ModifyInstanceAttribute",
        "ec2:ModifyVolume",
        "ec2:AttachVolume",
        "ec2:AuthorizeSecurityGroupIngress",
        "ec2:CreateRoute",
        "ec2:DeleteRoute",
        "ec2:DeleteSecurityGroup",
        "ec2:DeleteVolume",
        "ec2:DetachVolume",
        "ec2:RevokeSecurityGroupIngress",
        "ec2:DescribeVpcs",
        "elasticloadbalancing:AddTags",
        "elasticloadbalancing:AttachLoadBalancerToSubnets",
        "elasticloadbalancing:ApplySecurityGroupsToLoadBalancer",
        "elasticloadbalancing:CreateLoadBalancer",
        "elasticloadbalancing:CreateLoadBalancerPolicy",
        "elasticloadbalancing:CreateLoadBalancerListeners",
        "elasticloadbalancing:ConfigureHealthCheck",
        "elasticloadbalancing:DeleteLoadBalancer",
        "elasticloadbalancing:DeleteLoadBalancerListeners",
        "elasticloadbalancing:DescribeLoadBalancers",
        "elasticloadbalancing:DescribeLoadBalancerAttributes",
        "elasticloadbalancing:DetachLoadBalancerFromSubnets",
        "elasticloadbalancing:DeregisterInstancesFromLoadBalancer",
        "elasticloadbalancing:ModifyLoadBalancerAttributes",
        "elasticloadbalancing:RegisterInstancesWithLoadBalancer",
        "elasticloadbalancing:SetLoadBalancerPoliciesForBackendServer",
        "elasticloadbalancing:AddTags",
        "elasticloadbalancing:CreateListener",
        "elasticloadbalancing:CreateTargetGroup",
        "elasticloadbalancing:DeleteListener",
        "elasticloadbalancing:DeleteTargetGroup",
        "elasticloadbalancing:DescribeListeners",
        "elasticloadbalancing:DescribeLoadBalancerPolicies",
        "elasticloadbalancing:DescribeTargetGroups",
        "elasticloadbalancing:DescribeTargetHealth",
        "elasticloadbalancing:ModifyListener",
        "elasticloadbalancing:ModifyTargetGroup",
        "elasticloadbalancing:RegisterTargets",
        "elasticloadbalancing:SetLoadBalancerPoliciesOfListener",
        "iam:CreateServiceLinkedRole",
        "kms:DescribeKey"
      ],
      "Resource": [
        "*"
      ]
    },
  ]
}
```
Assign it to the master EC2 instance.

For the worker nodes create a policy with the following contents,
```
  {
      "Version": "2012-10-17",
      "Statement": [
          {
              "Effect": "Allow",
              "Action": [
                  "ec2:DescribeInstances",
                  "ec2:DescribeRegions",
                  "ecr:GetAuthorizationToken",
                  "ecr:BatchCheckLayerAvailability",
                  "ecr:GetDownloadUrlForLayer",
                  "ecr:GetRepositoryPolicy",
                  "ecr:DescribeRepositories",
                  "ecr:ListImages",
                  "ecr:BatchGetImage"
              ],
              "Resource": "*"
          } 
      ]
  }
```
Assign it to all EC2 instances that will worker nodes.

When the EC2 instances have been spawned assign them the following tags:
For master:
KubernetesCluster = ClusterName, eg. na.prod.concordium.com
Name = MasterName, eg. master.na.prod.concordium.com
k8s.io/role/master = 1

For worker nodes:
KubernetesCluster = ClusterName, eg. na.prod.concordium.com
Name = NodeName, eg. node00.na.prod.concordium.com
k8s.io/role/node = 1

Make certain that proper security groups have been created.
Master needs to have port 443 open for API communication, and worker nodes should have 30000-32000 open for load balancer to be able to communicate with worker nodes.
SSH should to be open for all EC2 instances


## Change to root
```
sudo -s
```

## Enable Docker
```
systemctl enable docker && systemctl start docker
```

## Install kubernetes tools. Must be done on all nodes
```
CNI_VERSION="v0.6.0"
mkdir -p /opt/cni/bin
curl -L "https://github.com/containernetworking/plugins/releases/download/${CNI_VERSION}/cni-plugins-amd64-${CNI_VERSION}.tgz" | tar -C /opt/cni/bin -xz

CRICTL_VERSION="v1.11.1"
mkdir -p /opt/bin
curl -L "https://github.com/kubernetes-incubator/cri-tools/releases/download/${CRICTL_VERSION}/crictl-${CRICTL_VERSION}-linux-amd64.tar.gz" | tar -C /opt/bin -xz

RELEASE="$(curl -sSL https://dl.k8s.io/release/stable.txt)"

mkdir -p /opt/bin
cd /opt/bin
curl -L --remote-name-all https://storage.googleapis.com/kubernetes-release/release/${RELEASE}/bin/linux/amd64/{kubeadm,kubelet,kubectl}
chmod +x {kubeadm,kubelet,kubectl}

curl -sSL "https://raw.githubusercontent.com/kubernetes/kubernetes/${RELEASE}/build/debs/kubelet.service" | sed "s:/usr/bin:/opt/bin:g" > /etc/systemd/system/kubelet.service
mkdir -p /etc/systemd/system/kubelet.service.d
curl -sSL "https://raw.githubusercontent.com/kubernetes/kubernetes/${RELEASE}/build/debs/10-kubeadm.conf" | sed "s:/usr/bin:/opt/bin:g" > /etc/systemd/system/kubelet.service.d/10-kubeadm.conf

systemctl enable kubelet && systemctl start kubelet

```

At times there can be issues with the PATH variable not being properly working. Issues can be resolved by prefixing all commands with PATH=/opt/bin:$PATH

## Setup master
Load kernel modules
```
modprobe ip_vs_sh ip_vs ip_vs_rr ip_vs_wrr
```

Add to startup
```
echo -e "ip_vs_sh\nip_vs\nip_vs_rr\nip_vs_wrr" > /etc/modules-load.d/ipvs.conf
```

Initialize master
```
PATH=/opt/bin:$PATH kubeadm init --apiserver-cert-extra-sans=ExternalIP --apiserver-bind-port=443
```
Where ExternalIP is the external IP from AWS interface

## Setup networking
```
PATH=/opt/bin:$PATH kubectl apply -f "https://cloud.weave.works/k8s/net?k8s-version=$(PATH=/opt/bin:$PATH kubectl version | base64 | tr -d '\n')"
```

Check that it's up and running before starting new nodes
```
PATH=/opt/bin:$PATH kubectl get pods --all-namespaces
```

Extract the config needed to communicate with the cluster from outside the master. Download the /etc/kubernetes/admin.conf file to ~/.kube/config at your machine.
For AWS the IP needs to be corrected in the config file to use the external IP

## Set cloud provider and authentication up
We want to enable Google OAUTH authentication to give us 2FA.
Open /etc/kubernetes/manifests/kube-apiserver.yaml and add
```
    - --oidc-issuer-url=https://accounts.google.com
    - --oidc-username-claim=email
    - --oidc-client-id=812165771185-b7449eapj7ckth3f7jq28fmb858vl8gc.apps.googleusercontent.com
    - --cloud-provider=aws
```
to the arguments to kube-apiserver.

Open /etc/kubernetes/manifests/kube-controller-manager.yaml and add
```
    - --cloud-provider=aws
    - --configure-cloud-routes=false
```
to the arguments to kube-controller-manager

Open /var/lib/kubelet/kubeadm-flags.env and add
```
--cloud-provider=aws
```
as an argument to KUBELET_KUBEADM_ARGS

Restart kubelet
```
systemctl restart kubelet
```


## Setup nodes
Get token on master node
```
PATH=/opt/bin:$PATH kubeadm token list
```

Load kernel modules
```
modprobe ip_vs_sh ip_vs ip_vs_rr ip_vs_wrr
```

Add to startup
```
echo -e "ip_vs_sh\nip_vs\nip_vs_rr\nip_vs_wrr" > /etc/modules-load.d/ipvs.conf
```

Get discovery token ca hash
```
openssl x509 -pubkey -in /etc/kubernetes/pki/ca.crt | openssl rsa -pubin -outform der 2>/dev/null | \
   openssl dgst -sha256 -hex | sed 's/^.* //'
```

Now join the cluster with the output of the two previous commands

```
PATH=/opt/bin:$PATH kubeadm join --token <token> <master-ip>:443 --discovery-token-ca-cert-hash sha256:<hash>
```

Master IP is the IP of the master. Just wait and eventually the nodes will have joined.

Progress can be checked on the master(or from a local machine with .kube/config present) with 
```
PATH=/opt/bin:$PATH kubectl get nodes
```

When the nodes are up and ready, open /var/lib/kubelet/kubeadm-flags.env and add
```
--cloud-provider=aws
```
as an argument to KUBELET_KUBEADM_ARGS

Restart kubelet
```
systemctl restart kubelet
```


## Install nginx-ingress
We now assume that all operations are on a local developer machine and not on the kubernetes master

```
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/master/deploy/mandatory.yaml
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/master/deploy/provider/baremetal/service-nodeport.yaml

```

## Setup storage class
We want to be able to automatically provision EBS storage on Amazon. Therefore we have to create a storage class and mark it as default.
Apply the file scripts/create-storage-class.yaml

```
kubectl apply -f create-storage-class.yaml
```

Mark it as default
```
kubectl patch storageclass standard -p '{"metadata": {"annotations":{"storageclass.kubernetes.io/is-default-class":"true"}}}'
```


## Install helm
Follow https://docs.helm.sh/using_helm/#quickstart to get a local installation of helm on developer machine

Apply the file scripts/helm-serviceaccount.yaml
```
kubectl apply -f helm-serviceaccount.yaml
```

Execute 

```
helm init --service-account tiller
```

## Install Kubernetes Dashboard
```
kubectl apply -f https://raw.githubusercontent.com/kubernetes/dashboard/master/src/deploy/recommended/kubernetes-dashboard.yaml
```

A user needs to be created to use with the dashboard.
```
apiVersion: v1
kind: ServiceAccount
metadata:
  name: ReplaceThisWithUsername
  namespace: kube-system
```

Grant the user cluster admin rights
```
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: mj
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: cluster-admin
subjects:
- kind: ServiceAccount
  name: mj
  namespace: kube-system
```

## Modify weavenet
For AWS we want to be able to see client IPs. For now we need to modify weave to suit this need.
Under env add a new object,
```
              {
                "name": "NO_MASQ_LOCAL",
                "value": "1"
              }
```

## Install cert-manager
```
helm install \
    --name cert-manager \
    --namespace kube-system \
    stable/cert-manager
``` 

Now configure a ClusterIssuer using scripts/LetsEncryptIssuer.yaml

## Install Prometheus and Grafana
```
helm install --name prometheus --namespace prometheus --values prometheus/values.yaml stable/prometheus
helm install --name grafana --namespace prometheus --values grafana/values.yaml stable/grafana
```

If needed checkout the charts repository and changes values in values.yaml for the two packages. Otherwise omit the values parameter

## Setup OAUTH authentication
We want users to be able to use their Google account as authentication for our cluster.

Download the helper tool
```
go get github.com/micahhausler/k8s-oidc-helper
```

Run the command 
```
k8s-oidc-helper -c client_secret_812165771185-b7449eapj7ckth3f7jq28fmb858vl8gc.apps.googleusercontent.com.json
```

Login to Google using the Concordium account and copy the code given back into the program.
Now copy paste the user information into the .kube/config file. 

We now need to grant that user some permissions. For admin rights apply the file scripts/grant-admin-gaccount.yaml and correct as needed.

```
kubectl apply -f grant-admin-gaccount.yaml
```

Validate that it works
```
kubectl --user=name@domain.com get nodes
```

You should now be able to remove the user that was auto generated from your .kube/config.