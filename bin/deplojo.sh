#!/bin/bash

target="${1:-docker}"

HTML=pro/web/redaktilo.html
CSS=pro/web/static/redaktilo.css

case $target in

docker)
    cetonio_id=$(docker ps --filter name=cetoniujo_cetonio -q)
    todir=/home/cetonio/pro
    echo "${HTML} -> ${cetonio_id}:${todir}/web"
    docker cp ${HTML} ${cetonio_id}:${todir}/web
    echo "${CSS} -> ${cetonio_id}:${todir}/web/static"
    docker cp ${CSS} ${cetonio_id}:${todir}/web/static
    ;;
esac