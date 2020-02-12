#!/bin/bash
set -x
swipl -s pro/db/revo_download.pl -g "db_revo_download:download,halt" -t "halt(1)"
unzip -o tmp/revo-inx*.zip -d sql
