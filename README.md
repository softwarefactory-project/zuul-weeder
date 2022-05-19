# zuul-weeder

Use this service to inspect Zuul configuration.

# Setup

Start the service by giving access to the zuul.conf:

```
podman run -p 9001:9001 -v /etc/zuul:/etc/zuul:ro --rm quay.io/software-factory/zuul-weeder
```

Serve behind a sub-path in apache with this argument `-e WEEDER_ROOT_URL=/weeder/` and this httpd.conf:

```
<Location "/weeder">
    ProxyPass http://weeder-host:9001
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


# Contribute

Auto reload the web ui with a demo config:

```
ghcid -W --test "ZuulWeeder.Main.runDemo"
```

After adding css class, run `nix run .#tailwind` to update the tailwind.css file. Then hard refresh the web page.

Accept golden test change with:

```
cabal test --test-option=--accept
```
