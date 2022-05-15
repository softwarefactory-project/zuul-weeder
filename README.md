# zuul-weeder

Detect dead configuration in Zuul

## Discover the configuration location:

```
$ zuul-weeder $DUMP_PATH dependency [job|nodeset] $name
- config-element (config-path)
- ...
```

# Contribute

Auto reload the web ui with a demo config:

```
ghcid -W --test "ZuulWeeder.UI.run ZuulWeeder.Main.demoConfig"
```
