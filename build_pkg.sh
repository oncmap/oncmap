#!/bin/bash

docker run -it -v `pwd`/..:`pwd`/.. -w `pwd` rocker/shiny:4.3.3 R -e 'install.packages("devtools");devtools::install_deps();devtools::build(".")'

