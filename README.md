## kube_dist

[![Build Status](https://travis-ci.org/seniverse/kube_dist.svg?branch=master)](https://travis-ci.org/seniverse/kube_dist)
[![hex.pm version](https://img.shields.io/hexpm/v/kube_dist.svg)](https://hex.pm/packages/kube_dist)
[![hex.pm downloads](https://img.shields.io/hexpm/dt/kube_dist.svg)](https://hex.pm/packages/kube_dist)
![hex.pm license](https://img.shields.io/hexpm/l/kube_dist.svg)
![GitHub top language](https://img.shields.io/github/languages/top/seniverse/kube_dist.svg)

Erlang Distribution inside Kubernetes Cluster

With `kube_dist` properly configured, name of erlang node is set to `${CONTAINER_NAME}@${POD_NAME}`.

Moreover, you may use Kubernetes's Service as your process registry.

```
gen_server:call({via, kube_endpoints, ServiceName}, Request)
```

Request would be sent to one randomly chosen pod listed in the endpoints list. If `ServiceName` is an atom, it would be sent to the container with same name. Or `ServiceName` could a tuple of two atom `{ServiceName, ContainerName}`, it would be sent to the container which name is specified in the second element of the tuple.


### Installation

add `kube_dist` dependency to your project's `rebar.config`

```
{deps, [kube_dist]}.
```

set `start_distribution` to `false` and enable `httpc` service of inets

```
[
  {kernel,
   [{start_distribution, false}]
  },
  {inets,
   [{services, [{httpc, []}]}]
  }
].
```

disable epmd daemon and set `epmd_module` to `kube_epmd` and `proto_dist` to `kube_tcp`

```
-start_epmd false
-epmd_module kube_epmd
-proto_dist kube_tcp
```

grant permission of `get` to `pods/status` and `watch` to `endpoints` to the service account of the pods

```
---

apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlang-node

---

apiVersion: rbac.authorization.k8s.io/v1beta1
kind: Role
metadata:
  name: erlang-node
rules:
- apiGroups: [""]
  resources: ["pods/status"]
  verbs: ["get"]
- apiGroups: [""]
  resources: ["endpoints"]
  verbs: ["watch"]

---

apiVersion: rbac.authorization.k8s.io/v1beta1
kind: RoleBinding
metadata:
  name: erlang-node
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: Role
  name: erlang-node
subjects:
- kind: ServiceAccount
  name: erlang-node
```

and set environment variable `CONTAINER_NAME` to container's name and add a distribution port named `${CONTAINER_NAME}-dist`

```
spec:
  serviceAccountName: erlang-node
  containers:
  - name: foo
    env:
    - name: CONTAINER_NAME
      value: foo
    ports:
    - containerPort: 4370
      name: foo-dist
```

### License

kube_dist is released under Apache 2 License. Check [LICENSE](./LICENSE) file for more information.
