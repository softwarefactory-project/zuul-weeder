# zuul-weeder: Zuul configuration analyzer

Use this service to inspect the configurations objects loaded in a [zuul](https://zuul-ci.org) system.


## Overview and scope

Zuul Weeder analyzes the configuration objects such as jobs and nodesets and provides a search interface for:

- Depencencies: what depends on an object.
- Requirements: what is needed by an object.
- URL of the configuration files that contains the object.

Zuul Weeder leverage a generic dependency graph using the data found in the ZooKeeper database
to collect every configuration elements used by any tenants.
The goal is to help evaluate the impact of a configuration change,
for example, when removing a node label or a repository.


## Usage

The service provide two functions:

- `/search/$name` returns the list of object matching the requested name.
- `/object/$type/$name` returns
  - the list of configuration file url that directly defines or uses the object,
  - the list of related objects that are reachable, either by requirement or by dependency.

For example, by visiting `/search/centos`, the service returns:

- job tripleo-centos
- nodeset centos
- label cloud-centos

And by visiting `/object/nodeset/centos`, the service returns:

- The list of zuul.yaml file url that contains a nodeset named `centos`.
- The list of jobs and project that depends on this nodeset.
- The list of node label name that is required by this nodeset.

The results can be scoped to a specific tenant by using the `/tenant/$tenant` url prefix.


## Setup

Start the service at `http://localhost:9001` (needs access to the zuul.conf):

```
podman run -p 9001:9001 -v /etc/zuul:/etc/zuul:ro --rm quay.io/software-factory/zuul-weeder
```

Serve behind a sub-path using this argument `-e WEEDER_ROOT_URL=/weeder/` and this httpd.conf:

```
<Location "/weeder">
    ProxyPass http://weeder-host:9001 nocanon
    ProxyPassReverse http://weeder-host:9001
</Location>
```

Setup monitoring by adding this prometheus configuration:

```
scrape_configs:
  - job_name: weeder
    static_configs:
      - targets:
          - weeder-host:9001
```

Configure grafana dashboard by running `./bin/create-dashboard`


## Roadmap

Here are some planned features:

- Add the remaining configuration objects to the graph project-templates.
- Add a project name object type, to resolve global project configuration (e.g. with a name regexp) and to create a graph connection for all the matching repository names.
- Connect the graphs from multiple zuul-weeder service to analyse the configuration cross deployments.
- Display the weeds, e.g. the dead configuration object that are unreachable.

Checkout the [Developper Guide](./CONTRIBUTING.md).
