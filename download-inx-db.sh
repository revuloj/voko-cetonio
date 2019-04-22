#!/bin/bash
set -x
swipl -s pro/sqlrevo.pl -g "sqlrevo:download,halt" -t "halt(1)"
unzip tmp/revo-inx*.zip -d sql
