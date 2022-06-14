#!/bin/bash

target="${1:-docker}"

HTML=pro/web/redaktilo.html
HTML2=pro/web/static/*.html
CSS=pro/web/static/*.css

case $target in

docker)
    cetonio_id=$(docker ps --filter name=cetoniujo_cetonio -q)
    todir=/home/cetonio/pro

    echo "${HTML} -> ${cetonio_id}:${todir}/web"
    docker cp ${HTML} ${cetonio_id}:${todir}/web

    echo "${HTML2} -> ${cetonio_id}:${todir}/web/static"
    for html in ${HTML2}
    do
        echo "${html}"
        docker cp ${html} ${cetonio_id}:${todir}/web/static/
    done

    echo "${CSS} -> ${cetonio_id}:${todir}/web/static"
    for css in ${CSS}
    do
        echo "${css}"
        docker cp ${css} ${cetonio_id}:${todir}/web/static/
    done
    ;;
signoj)
    cd pro && swipl -s loaddtd.pl -g dtd2json_entities -g dtd2pl_entities -g halt \
    && cd .. && cp pro/voko_entities.js ../voko-grundo/jsc/x/
esac