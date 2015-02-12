# Test-Sandbox-Compose: Fast Development Environments Using Test-Sandbox

[![Hackage version](https://img.shields.io/hackage/v/test-sandbox-compose.svg?style=flat)](https://hackage.haskell.org/package/test-sandbox-compose)  [![Build Status](https://travis-ci.org/junjihashimoto/test-sandbox-compose.png?branch=master)](https://travis-ci.org/junjihashimoto/test-sandbox-compose) [![Coverage Status](https://coveralls.io/repos/junjihashimoto/test-sandbox-compose/badge.png)](https://coveralls.io/r/junjihashimoto/test-sandbox-compose)

Test-Sandbox-Compose makes development environments for multi-servers using Test-Sandbox.
Each server is defined in test-sandbox-compose.yml.
test-sandbox-compose.yml provides following functions.

* Mustache template for accessing each resource
* Before/After-bash-script for server-setup
* Tempolary file, directory and TCP-Port which test-sandbox provides

This project is inspired by Docker Compose(Fig).

## Getting started

Install this from Hackage.

    cabal update && cabal install test-sandbox-compose


## test-sandbox-compose.yml reference

```
<service-name1>:
  cmd: <command-name>
  args:
    - <arg1>
    - <arg2>
  confs:
    <conf1>: <conf1 contents>
    <conf2>: <conf2 contents>
  tempfiles:
    - <temp1>
    - <temp2>
  dirs:
    - <dir1>
    - <dir2>
  ports:
    - <port1>
    - <port2>
  beforescript: <script content>
  afterscript: <script content>
<service-name2>
  ...
```

Example

```
zookeeper:
  cmd: '/usr/share/zookeeper/bin/zkServer.sh'
  args:
    - 'start-foreground'
    - '{{zookeeper_conf_conf}}'
  tempfiles: []
  confs:
    conf: |
      dataDir={{zookeeper_dir_data}}
      clientPort={{zookeeper_port_2181}}
      maxClientCnxns=1000
  dirs:
    - 'data'
  ports:
    - '2181'
```


## Commands


### Up

```
test-sandbox-compose up
```

### Status

```
test-sandbox-compose status
```

### Conf

```
test-sandbox-compose conf
```

### Kill

```
test-sandbox-compose kill
```

### Logs

```
test-sandbox-compose logs
```

### Destroy

```
test-sandbox-compose destroy
```

### Daemon

```
test-sandbox-compose daemon
```
