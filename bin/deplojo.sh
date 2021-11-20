#!/bin/bash

target="${1:-docker}"

HTML=pro/web/redaktilo.html

case $target in

docker)
    cetonio_id=$(docker ps --filter name=cetoniujo_cetonio -q)
    todir=/home/cetonio/pro
    echo "${HTML} -> ${cetonio_id}:${todir}/web"
    docker cp ${HTML} ${cetonio_id}:${todir}/web

esac