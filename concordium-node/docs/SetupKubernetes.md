# Install Kubernetes
## Enable Docker
```
systemctl enable docker && systemctl start docker
```

## Install kubernetes tools. Must be done on all nodes
```
sudo -s

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
Load kernel modules, should probably be added to startup
```
modprobe ip_vs_sh ip_vs ip_vs_rr ip_vs_wrr
```
Initialize master
```
PATH=/opt/bin:$PATH kubeadm init
```

## Setup networking
```
PATH=/opt/bin:$PATH kubectl apply -f "https://cloud.weave.works/k8s/net?k8s-version=$(PATH=/opt/bin:$PATH kubectl version | base64 | tr -d '\n')"
```

Check that it's up and running before starting new nodes
```
PATH=/opt/bin:$PATH kubectl get pods --all-namespaces
```

Extract the config needed to communicate with the cluster from outside the master. Download the /etc/kubernetes/admin.conf file to ~/.kube/config at your machine

## Setup nodes
Get token on master node
```
PATH=/opt/bin:$PATH kubeadm token list
```

Load kernel modules, should probably be added to startup
```
modprobe ip_vs_sh ip_vs ip_vs_rr ip_vs_wrr
```

Get discovery token ca hash
```
openssl x509 -pubkey -in /etc/kubernetes/pki/ca.crt | openssl rsa -pubin -outform der 2>/dev/null | \
   openssl dgst -sha256 -hex | sed 's/^.* //'
```

Now join the cluster with the output of the two previous commands

```
PATH=/opt/bin:$PATH kubeadm join --token <token> <master-ip>:<master-port> --discovery-token-ca-cert-hash sha256:<hash>
```

Master IP is the IP of the master and default master port is 6443. Just wait and eventually the nodes will have joined.

Progress can be checked on the master(or from a local machine with .kube/config present) with 
```
PATH=/opt/bin:$PATH kubectl get nodes
```

## Install nginx-ingress
We now assume that all operations are on a local developer machine and not on the kubernetes master

```
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/master/deploy/mandatory.yaml
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/master/deploy/provider/baremetal/service-nodeport.yaml

```

## Install helm
Follow https://docs.helm.sh/using_helm/#quickstart to get a local installation of helm on developer machine

Execute 

```
helm init
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
  name: ReplaceThisWithUsername
  name: mj
  namespace: kube-system
```

## Modify weavenet
For AWS we want to be able to see client IPs. For now we need to modify weave to suit this need.
Change image to weaveworks/weave-kube:2.4.0 or newer. Under env add a new object,
```
              {
                "name": "NO_MASQ_LOCAL",
                "value": "1"
              }
```

Also remember to change weave-npc image to weaveworks/weave-npc:2.4.0