#!/bin/bash
set -e
set -x

if [ "$1" = 'swipl' ]; then
    # elŝutu index-datumbazon, se ankoraŭ mankas
    if [ ! -e sql/revo-inx.db ]; then
        bin/instalo.sh update-db
        ls -l sql
        #swipl -s pro/sqlrevo.pl -g "sqlrevo:download,halt" -t "halt(1)"
        #unzip tmp/revo-inx*.zip -d sql
    fi
    # kreu konto-datumbazon, se ankoraŭ mankas
    if [ ! -e sql/redaktantoj.db ]; then
        bin/instalo.sh kreu-db
        if [ "$CGI_USER" != "" ] && [ "$CGI_PWD" != "" ]; then
            bin/instalo.sh redaktantoj
        fi
        ls -l sql
        #sqlite3 sql/redaktantoj.db -init konto-skemo.sql
        # importi el etc/redaktantoj al sql/redaktantoj.db
        #swipl -s pro/redaktantoj.pl -g "redaktantoj:update_redaktantoj,halt" -t "halt(1)"
    fi
fi
exec "$@"