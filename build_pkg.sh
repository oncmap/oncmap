#!/bin/bash

docker run -it -v `pwd`/..:`pwd`/.. -w `pwd` rocker/r-ver:4.5.3 R -e 'system("apt update");system("apt install -y pandoc");install.packages(c("remotes","devtools","pak"));pak::local_install_deps();devtools::build(".")'
