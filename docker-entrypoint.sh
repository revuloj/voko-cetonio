#!/bin/bash
set -e
set -x

if [ "$1" = 'swipl' ]; then
    if [ -z "$(ls -A sql)" ]; then
        swipl -s pro/sqlrevo.pl -g "sqlrevo:download,halt" -t "halt(1)"
        unzip tmp/revo-inx*.zip -d sql
        cp etc/redaktantoj sql/redaktantoj.db
    fi
    # exec "$@"
fi
exec "$@"