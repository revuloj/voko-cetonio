#!/bin/bash

#
release=2e

# ni komprenas preparo 
target="${1:-helpo}"

PAGE=pro/web/redaktilo.html

case $target in
preparo)
    # kontrolu ĉu la branĉo kongruas kun la agordita versio
    branch=$(git symbolic-ref --short HEAD)
    if [ "${branch}" != "${release}" ]; then
        echo "Ne kongruas la branĉo (${branch}) kun la eldono (${release})"
        echo "Agordu la variablon 'release' en tiu ĉi skripto por prepari novan eldonon."
        exit 1
    fi

    echo "Aktualigante skriptojn al nova eldono ${release}..."
    sed -i 's,/redaktilo-[1-9][a-z]-min\.,/redaktilo-'${release}'-min\.,g' ${PAGE}
    sed -i 's/Cetonio "[1-9][a-z]";/Cetonio "'${release}'";/' ${PAGE}
    ;;
kreo)
    echo "Kreante lokan procezujon (por docker) voko-cetonio"
    docker pull ghcr.io/revuloj/voko-grundo/voko-grundo:${release}
    docker build -t voko-cetonio .
    ;;
etikedo)
    echo "Provizante la aktualan staton per etikedo (git tag) v${release}"
    echo "kaj puŝante tiun staton al la centra deponejo"
    git tag -f v${release} && git push && git push --tags -f
    ;;    
helpo | *)
    echo "---------------------------------------------------------------------------"
    echo "Per la celo 'preparo' oni povas krei git-branĉon kun nova eldono por tie "
    echo "komenci programadon de novaj funkcioj, ŝanĝoj ktp. Antaŭ adaptu en la kapo de ĉi-skripto"
    echo "la variablojn 'release' kaj 'node_release' al la nova eldono."
    ;;    
esac
